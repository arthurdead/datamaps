/**
 * vim: set ts=4 :
 * =============================================================================
 * SourceMod Sample Extension
 * Copyright (C) 2004-2008 AlliedModders LLC.  All rights reserved.
 * =============================================================================
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License, version 3.0, as published by the
 * Free Software Foundation.
 * 
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * As a special exception, AlliedModders LLC gives you permission to link the
 * code of this program (as well as its derivative works) to "Half-Life 2," the
 * "Source Engine," the "SourcePawn JIT," and any Game MODs that run on software
 * by the Valve Corporation.  You must obey the GNU General Public License in
 * all respects for all other code used.  Additionally, AlliedModders LLC grants
 * this exception to all derivative works.  AlliedModders LLC defines further
 * exceptions, found in LICENSE.txt (as of this writing, version JULY-31-2007),
 * or <http://www.sourcemod.net/license.php>.
 *
 * Version: $Id$
 */

#define swap V_swap

#if SOURCE_ENGINE == SE_TF2
	#define TF_DLL
	#define USES_ECON_ITEMS
#elif SOURCE_ENGINE == SE_LEFT4DEAD2
	#define TERROR
	#define LEFT4DEAD
	#define SWARM_DLL
#endif

#define BASEENTITY_H
#define NEXT_BOT
#define GLOWS_ENABLE
#define USE_NAV_MESH
#define RAD_TELEMETRY_DISABLED

#include "extension.h"
#include <ISDKTools.h>
#include <CDetour/detours.h>
#include <string>
#include <vector>
#include <unordered_map>
#include <mathlib/vmatrix.h>
#include <toolframework/itoolentity.h>
#include <ehandle.h>
#include <eiface.h>
#include <dt_common.h>
#include <shareddefs.h>

#if SOURCE_ENGINE == SE_LEFT4DEAD2
class CFlaggedEntitiesEnum : public IPartitionEnumerator
{
public:
	CFlaggedEntitiesEnum( CBaseEntity **pList, int listMax, int flagMask )
	{}
	
	IterationRetval_t EnumElement( IHandleEntity *pHandleEntity )
	{ return ITERATION_CONTINUE; }
};
#endif

#ifndef FMTFUNCTION
#define FMTFUNCTION(...)
#endif

#include <util.h>
#include <eiface.h>
#include <tier1/checksum_md5.h>
#include <iserver.h>
#include "protocol.h"
#include <tier1/utldict.h>

using vec3_t = vec_t[3];
using EHANDLE = CHandle<CBaseEntity>;

extern "C"
{
__attribute__((__visibility__("default"), __cdecl__)) double __pow_finite(double a, double b)
{
	return pow(a, b);
}

__attribute__((__visibility__("default"), __cdecl__)) double __log_finite(double a)
{
	return log(a);
}

__attribute__((__visibility__("default"), __cdecl__)) double __exp_finite(double a)
{
	return exp(a);
}

__attribute__((__visibility__("default"), __cdecl__)) double __atan2_finite(double a, double b)
{
	return atan2(a, b);
}

__attribute__((__visibility__("default"), __cdecl__)) float __atan2f_finite(float a, float b)
{
	return atan2f(a, b);
}

__attribute__((__visibility__("default"), __cdecl__)) float __powf_finite(float a, float b)
{
	return powf(a, b);
}

__attribute__((__visibility__("default"), __cdecl__)) float __logf_finite(float a)
{
	return logf(a);
}

__attribute__((__visibility__("default"), __cdecl__)) float __expf_finite(float a)
{
	return expf(a);
}

__attribute__((__visibility__("default"), __cdecl__)) float __acosf_finite(float a)
{
	return acosf(a);
}

__attribute__((__visibility__("default"), __cdecl__)) double __asin_finite(double a)
{
	return asin(a);
}

__attribute__((__visibility__("default"), __cdecl__)) double __acos_finite(double a)
{
	return acos(a);
}
}

#if SOURCE_ENGINE == SE_LEFT4DEAD2
char* AllocateStringHelper2( const char *pFormat, va_list marker )
{
	char str[512];
	_vsnprintf( str, sizeof( str ), pFormat, marker );
	str[ ARRAYSIZE(str) - 1 ] = 0;
	
	int len = strlen( str ) + 1;
	char *pRet = new char[len];
	memcpy( pRet, str, len );

	return pRet;
}


char* AllocateStringHelper( const char *pFormat, ... )
{
	va_list marker;
	va_start( marker, pFormat );
	char *pRet = AllocateStringHelper2( pFormat, marker );
	va_end( marker );

	return pRet;
}
#endif

/**
 * @file extension.cpp
 * @brief Implement extension code here.
 */

Sample g_Sample;		/**< Global singleton for extension's main interface */

SMEXT_LINK(&g_Sample);

int CBaseEntityPostConstructor = 0;

CGlobalVars *gpGlobals = nullptr;
CBaseEntityList *g_pEntityList = nullptr;
ISDKHooks *g_pSDKHooks = nullptr;
ISDKTools *g_pSDKTools = nullptr;
IServerTools *servertools = nullptr;
ServerClass *g_pServerClassHead = nullptr;
ServerClass *g_pServerClassTail = nullptr;
INetworkStringTableContainer *netstringtables = NULL;
INetworkStringTable *m_pInstanceBaselineTable = nullptr;
IServer *server = nullptr;
//IServerGameDLL *gamedll = nullptr;
void *EntityFactoryDictionaryPtr = nullptr;
void *CGlobalEntityListFindEntityByClassname = nullptr;
void *UTIL_RemovePtr = nullptr;

HandleType_t factory_handle = 0;
HandleType_t datamap_handle = 0;
HandleType_t removal_handle = 0;
HandleType_t serverclass_handle = 0;

template <typename T>
T void_to_func(void *ptr)
{
	union { T f; void *p; };
	p = ptr;
	return f;
}

template <typename T>
int vfunc_index(T func)
{
	SourceHook::MemFuncInfo info{};
	SourceHook::GetFuncInfo<T>(func, info);
	return info.vtblindex;
}

template <typename R, typename T, typename ...Args>
R call_mfunc(T *pThisPtr, void *offset, Args ...args)
{
	class VEmptyClass {};
	
	void **this_ptr = *reinterpret_cast<void ***>(&pThisPtr);
	
	union
	{
		R (VEmptyClass::*mfpnew)(Args...);
#ifndef PLATFORM_POSIX
		void *addr;
	} u;
	u.addr = offset;
#else
		struct  
		{
			void *addr;
			intptr_t adjustor;
		} s;
	} u;
	u.s.addr = offset;
	u.s.adjustor = 0;
#endif
	
	return (R)(reinterpret_cast<VEmptyClass *>(this_ptr)->*u.mfpnew)(args...);
}

template <typename R, typename T, typename ...Args>
R call_vfunc(T *pThisPtr, size_t offset, Args ...args)
{
	void **vtable = *reinterpret_cast<void ***>(pThisPtr);
	void *vfunc = vtable[offset];
	
	return call_mfunc<R, T, Args...>(pThisPtr, vfunc, args...);
}

int m_aThinkFunctionsOffset = -1;
int m_pfnThinkOffset = -1;
int m_iEFlagsOffset = -1;
int m_nNextThinkTickOffset = -1;
void *SimThink_EntityChangedPtr = nullptr;

void *AllocPooledStringPtr = nullptr;

void SetEdictStateChanged(CBaseEntity *pEntity, int offset);

void SimThink_EntityChanged(CBaseEntity *pEntity)
{
	(void_to_func<void(*)(CBaseEntity *)>(SimThink_EntityChangedPtr))(pEntity);
}

typedef void (CBaseEntity::*BASEPTR)(void);

struct thinkfunc_t
{
	BASEPTR		m_pfnThink;
	string_t	m_iszContext;
	int			m_nNextThinkTick;
	int			m_nLastThinkTick;

	DECLARE_SIMPLE_DATADESC();
};

string_t AllocPooledString(const char *szContext)
{
	return (void_to_func<string_t(*)(const char *)>(AllocPooledStringPtr))(szContext);
}

#define SetThink( a ) ThinkSet( static_cast <void (CBaseEntity::*)(void)> (a), 0, NULL )
#define SetContextThink( a, b, context ) ThinkSet( static_cast <void (CBaseEntity::*)(void)> (a), (b), context )

SH_DECL_MANUALHOOK0_void(GenericDtor, 1, 0, 0)

struct callback_holder_t
{
	struct callback_t
	{
		IPluginFunction *callback = nullptr;
		cell_t data = 0;
	};
	
	callback_t think{};
	
	std::unordered_map<std::string, callback_t> thinkctxs{};
	
	CBaseEntity *pEntity_ = nullptr;
	IdentityToken_t *owner = nullptr;
	bool erase = true;
	
	callback_holder_t(CBaseEntity *pEntity, IdentityToken_t *owner_);
	~callback_holder_t();
	
	void dtor(CBaseEntity *pEntity);

	void HookEntityDtor()
	{
		CBaseEntity *pEntity = META_IFACEPTR(CBaseEntity);
		dtor(pEntity);
		RETURN_META(MRES_IGNORED);
	}
};

using callback_holder_map_t = std::unordered_map<CBaseEntity *, callback_holder_t *>;
callback_holder_map_t callbackmap{};

callback_holder_t::callback_holder_t(CBaseEntity *pEntity, IdentityToken_t *owner_)
	: pEntity_{pEntity}, owner{owner_}
{
	SH_ADD_MANUALHOOK(GenericDtor, pEntity_, SH_MEMBER(this, &callback_holder_t::HookEntityDtor), false);
	
	callbackmap[pEntity_] = this;
}

callback_holder_t::~callback_holder_t()
{
	if(erase) {
		callbackmap.erase(pEntity_);
	}
}

class CBaseEntity : public IServerEntity
{
public:
	DECLARE_CLASS_NOBASE( CBaseEntity );
	DECLARE_SERVERCLASS();
	DECLARE_DATADESC();
	
	void PostConstructor(const char *classname)
	{
		call_vfunc<void, CBaseEntity, const char *>(this, CBaseEntityPostConstructor, classname);
	}
	
	using m_pfnThink_t = void (CBaseEntity::*)(void);
	using m_aThinkFunctions_t = CUtlVector<thinkfunc_t>;
	
	m_pfnThink_t &GetThinkFunc()
	{
		if(m_pfnThinkOffset == -1) {
			sm_datatable_info_t info{};
			datamap_t *pMap = gamehelpers->GetDataMap(this);
			gamehelpers->FindDataMapInfo(pMap, "m_pfnThink", &info);
			m_pfnThinkOffset = info.actual_offset;
		}
		
		return *(m_pfnThink_t *)((unsigned char *)this + m_pfnThinkOffset);
	}
	
	m_aThinkFunctions_t &GetAThinkFuncstions()
	{
		if(m_aThinkFunctionsOffset == -1) {
			sm_datatable_info_t info{};
			datamap_t *pMap = gamehelpers->GetDataMap(this);
			gamehelpers->FindDataMapInfo(pMap, "m_aThinkFunctions", &info);
			m_aThinkFunctionsOffset = info.actual_offset;
		}
		
		return *(m_aThinkFunctions_t *)((unsigned char *)this + m_aThinkFunctionsOffset);
	}
	
	int &GetIEFlags()
	{
		if(m_iEFlagsOffset == -1) {
			sm_datatable_info_t info{};
			datamap_t *pMap = gamehelpers->GetDataMap(this);
			gamehelpers->FindDataMapInfo(pMap, "m_iEFlags", &info);
			m_iEFlagsOffset = info.actual_offset;
		}
		
		return *(int *)((unsigned char *)this + m_iEFlagsOffset);
	}
	
	int &GetNextThinkTick()
	{
		if(m_nNextThinkTickOffset == -1) {
			sm_datatable_info_t info{};
			datamap_t *pMap = gamehelpers->GetDataMap(this);
			gamehelpers->FindDataMapInfo(pMap, "m_nNextThinkTick", &info);
			m_nNextThinkTickOffset = info.actual_offset;
		}
		
		return *(int *)((unsigned char *)this + m_nNextThinkTickOffset);
	}
	
	void SetNextThinkTick(int tick)
	{
		GetNextThinkTick() = tick;
		SetEdictStateChanged(this, m_nNextThinkTickOffset);
	}
	
	bool WillThink()
	{
		if ( GetNextThinkTick() > 0 )
			return true;

		for ( int i = 0; i < GetAThinkFuncstions().Count(); i++ )
		{
			if ( GetAThinkFuncstions()[i].m_nNextThinkTick > 0 )
				return true;
		}

		return false;
	}
	
	void CheckHasThinkFunction( bool isThinking )
	{
		if ( ( GetIEFlags() & EFL_NO_THINK_FUNCTION ) && isThinking )
		{
			GetIEFlags() &= ~EFL_NO_THINK_FUNCTION;
		}
		else if ( !isThinking && !( GetIEFlags() & EFL_NO_THINK_FUNCTION ) && !WillThink() )
		{
			GetIEFlags() |= EFL_NO_THINK_FUNCTION ;
		}
		
		SimThink_EntityChanged( this );
	}
	
	int	GetIndexForThinkContext( const char *pszContext )
	{
		for ( int i = 0; i < GetAThinkFuncstions().Size(); i++ )
		{
			if ( !Q_strncmp( STRING( GetAThinkFuncstions()[i].m_iszContext ), pszContext, MAX_CONTEXT_LENGTH ) )
				return i;
		}

		return NO_THINK_CONTEXT;
	}
	
	int RegisterThinkContext( const char *szContext )
	{
		int iIndex = GetIndexForThinkContext( szContext );
		if ( iIndex != NO_THINK_CONTEXT )
			return iIndex;

		// Make a new think func
		thinkfunc_t sNewFunc;
		Q_memset( &sNewFunc, 0, sizeof( sNewFunc ) );
		sNewFunc.m_pfnThink = NULL;
		sNewFunc.m_nNextThinkTick = 0;
		sNewFunc.m_iszContext = AllocPooledString(szContext);

		// Insert it into our list
		return GetAThinkFuncstions().AddToTail( sNewFunc );
	}
	
	BASEPTR	ThinkSet( BASEPTR func, float thinkTime, const char *szContext )
	{
		// Old system?
		if ( !szContext )
		{
			GetThinkFunc() = func;
			return GetThinkFunc();
		}

		// Find the think function in our list, and if we couldn't find it, register it
		int iIndex = GetIndexForThinkContext( szContext );
		if ( iIndex == NO_THINK_CONTEXT )
		{
			iIndex = RegisterThinkContext( szContext );
		}

		GetAThinkFuncstions()[ iIndex ].m_pfnThink = func;

		if ( thinkTime != 0 )
		{
			int thinkTick = ( thinkTime == TICK_NEVER_THINK ) ? TICK_NEVER_THINK : TIME_TO_TICKS( thinkTime );
			GetAThinkFuncstions()[ iIndex ].m_nNextThinkTick = thinkTick;
			CheckHasThinkFunction( thinkTick == TICK_NEVER_THINK ? false : true );
		}
		return func;
	}
	
	void SetNextThink( float thinkTime, const char *szContext )
	{
		int thinkTick = ( thinkTime == TICK_NEVER_THINK ) ? TICK_NEVER_THINK : TIME_TO_TICKS( thinkTime );

		// Are we currently in a think function with a context?
		int iIndex = 0;
		if ( !szContext )
		{
			// Old system
			SetNextThinkTick(thinkTick);
			CheckHasThinkFunction( thinkTick == TICK_NEVER_THINK ? false : true );
			return;
		}
		else
		{
			// Find the think function in our list, and if we couldn't find it, register it
			iIndex = GetIndexForThinkContext( szContext );
			if ( iIndex == NO_THINK_CONTEXT )
			{
				iIndex = RegisterThinkContext( szContext );
			}
		}

		// Old system
		GetAThinkFuncstions()[ iIndex ].m_nNextThinkTick = thinkTick;
		CheckHasThinkFunction( thinkTick == TICK_NEVER_THINK ? false : true );
	}
	
	void SetNextThinkContext( float thinkTime, int iIndex )
	{
		int thinkTick = ( thinkTime == TICK_NEVER_THINK ) ? TICK_NEVER_THINK : TIME_TO_TICKS( thinkTime );

		// Old system
		GetAThinkFuncstions()[ iIndex ].m_nNextThinkTick = thinkTick;
		CheckHasThinkFunction( thinkTick == TICK_NEVER_THINK ? false : true );
	}
	
	static int m_iCurrentThinkContext;
	
	void PluginThinkContext()
	{
		callback_holder_t *holder = callbackmap[this];
		
		if(m_iCurrentThinkContext >= 0 && m_iCurrentThinkContext < holder->thinkctxs.size()) {
			auto it = holder->thinkctxs.begin();
			std::advance(it, m_iCurrentThinkContext);
			
			IPluginFunction *func = it->second.callback;
			func->PushCell(gamehelpers->EntityToBCompatRef(this));
			func->PushString(it->first.c_str());
			func->PushCell(it->second.data);
			cell_t res = 0;
			func->Execute(&res);
		}
	}
	
	void PluginThink()
	{
		callback_holder_t *holder = callbackmap[this];
		
		IPluginFunction *func = holder->think.callback;
		func->PushCell(gamehelpers->EntityToBCompatRef(this));
		func->PushCell(holder->think.data);
		cell_t res = 0;
		func->Execute(&res);
	}
};

void callback_holder_t::dtor(CBaseEntity *pEntity)
{
	if(think.callback != nullptr) {
		pEntity->SetThink(nullptr);
	}
	
	for(auto &it : thinkctxs) {
		pEntity->SetContextThink(nullptr, 0.0f, it.first.c_str());
	}
	
	delete this;
}

int CBaseEntity::m_iCurrentThinkContext = -1;

DETOUR_DECL_MEMBER2(PhysicsRunSpecificThink, bool, int, nContextIndex, BASEPTR, thinkFunc)
{
	CBaseEntity::m_iCurrentThinkContext = nContextIndex;
	bool ret = DETOUR_MEMBER_CALL(PhysicsRunSpecificThink)(nContextIndex, thinkFunc);
	CBaseEntity::m_iCurrentThinkContext = -1;
	return ret;
}

void SetEdictStateChanged(CBaseEntity *pEntity, int offset)
{
	IServerNetworkable *pNet = pEntity->GetNetworkable();
	edict_t *edict = pNet->GetEdict();
	gamehelpers->SetEdictStateChanged(edict, offset);
}

CBaseEntity *FindEntityByClassname(CBaseEntity *pEntity, const std::string &name)
{
#if SOURCE_ENGINE == SE_TF2
	return servertools->FindEntityByClassname(pEntity, name.c_str());
#elif SOURCE_ENGINE == SE_LEFT4DEAD2
	return call_mfunc<CBaseEntity *, CBaseEntityList, CBaseEntity *, const char *>(g_pEntityList, CGlobalEntityListFindEntityByClassname, pEntity, name.c_str());
#endif
}

void RemoveEntity(CBaseEntity *pEntity)
{
#if SOURCE_ENGINE == SE_TF2
	servertools->RemoveEntity(pEntity);
#elif SOURCE_ENGINE == SE_LEFT4DEAD2
	(void_to_func<void(*)(CBaseEntity *)>(UTIL_RemovePtr))(pEntity);
#endif
}

void remove_all_entities(const std::string &name)
{
	CBaseEntity *pEntity = nullptr;
	while(pEntity = FindEntityByClassname(pEntity, name)) {
		RemoveEntity(pEntity);
	}
}

template <typename T>
void loop_all_entities(T func, const std::string &name)
{
	CBaseEntity *pEntity = nullptr;
	while((pEntity = FindEntityByClassname(pEntity, name)) != nullptr) {
		func(pEntity);
	}
}

class CEntityFactoryDictionary : public IEntityFactoryDictionary
{
public:
	CUtlDict<IEntityFactory *, unsigned short> m_Factories;
	
	const char *get_factory_name(IEntityFactory *fac)
	{
		for(int i = 0; i < m_Factories.Count(); i++) {
			IEntityFactory *it = m_Factories[i];
			if(it == fac) {
				return m_Factories.GetElementName(i);
			}
		}
		return nullptr;
	}
	
	IEntityFactory *get_name_factory(const std::string &name)
	{
		for(int i = 0; i < m_Factories.Count(); i++) {
			if(m_Factories.GetElementName(i) == name) {
				return m_Factories[i];
			}
		}
		return nullptr;
	}
	
	void remove_factory(IEntityFactory *fac, const std::string &name);
	
	void remove_factory(const std::string &name)
	{
		remove_factory(get_name_factory(name), name);
	}
	
	void remove_factory(IEntityFactory *fac)
	{
		remove_factory(fac, get_factory_name(fac));
	}
	
	static bool is_factory_custom(IEntityFactory *fac)
	{
		return (fac->GetEntitySize() == (size_t)-1);
	}
};

CEntityFactoryDictionary *dictionary = nullptr;

enum custom_prop_type
{
	custom_prop_int,
	custom_prop_float,
	custom_prop_bool,
	custom_prop_entity,
	custom_prop_vector,
};

using custom_prop_t = std::pair<std::string, custom_prop_type>;
using custom_prop_vec_t = std::vector<custom_prop_t>;

struct custom_typedescription_t : typedescription_t
{
	custom_typedescription_t()
		: typedescription_t{}
	{
		fieldName = nullptr;
	}
	
	void set_name(const std::string &name)
	{
		clear_name();
		
		size_t len = name.length();
		fieldName = (char *)malloc(len+1);
		strncpy((char *)fieldName, name.c_str(), len);
		((char *)fieldName)[len] = '\0';
	}
	
	void clear_name()
	{
		if(fieldName != nullptr) {
			free((void *)fieldName);
		}
		fieldName = nullptr;
	}
	
	~custom_typedescription_t()
	{
		//clear_name();
	}
	
	void zero()
	{
#if SOURCE_ENGINE == SE_TF2
		fieldOffset[TD_OFFSET_PACKED] = 0;
#elif SOURCE_ENGINE == SE_LEFT4DEAD2
		flatOffset[TD_OFFSET_NORMAL] = 0;
		flatOffset[TD_OFFSET_PACKED] = 0;
#endif
		externalName = nullptr;
		pSaveRestoreOps = nullptr;
		inputFunc = nullptr;
		td = nullptr;
		override_field = nullptr;
		override_count = 0;
		fieldTolerance = 0.0f;
	}
	
	int &get_offset()
	{
#if SOURCE_ENGINE == SE_TF2
		return fieldOffset[TD_OFFSET_NORMAL];
#elif SOURCE_ENGINE == SE_LEFT4DEAD2
		return fieldOffset;
#endif
	}
};

#if SOURCE_ENGINE == SE_LEFT4DEAD2
//TODO!!! m_pOptimizedDataMap
#endif

struct custom_datamap_t : datamap_t
{
	void set_name(const std::string &name)
	{
		clear_name();
		
		size_t len = name.length();
		dataClassName = (char *)malloc(len+1);
		strncpy((char *)dataClassName, name.c_str(), len);
		((char *)dataClassName)[len] = '\0';
	}
	
	void clear_name()
	{
		if(dataClassName != nullptr) {
			free((void *)dataClassName);
		}
		dataClassName = nullptr;
	}
	
	~custom_datamap_t()
	{
		clear_name();
	}
	
	void zero()
	{
#if SOURCE_ENGINE == SE_TF2
		chains_validated = 0;
		packed_offsets_computed = 0;
		packed_size = 0;
#elif SOURCE_ENGINE == SE_LEFT4DEAD2
		m_nPackedSize = 0;
		m_pOptimizedDataMap = nullptr;
#endif
	}
};

SH_DECL_HOOK1(IEntityFactory, Create, SH_NOATTRIB, 0, IServerNetworkable *, const char *);
SH_DECL_HOOK1(IVEngineServer, PvAllocEntPrivateData, SH_NOATTRIB, 0, void *, long);
SH_DECL_HOOK0(CBaseEntity, GetDataDescMap, SH_NOATTRIB, 0, datamap_t *);

int last_cb = 0;

struct custom_prop_info_t
{
	bool was_overriden = false;
	IEntityFactory *fac = nullptr;
	bool fac_is_sp = false;
	custom_datamap_t map{};
	using dataDesc_t = std::vector<custom_typedescription_t>;
	dataDesc_t dataDesc{};
	int size = 0;
	int base = 0;
	std::string clsname{};
	Handle_t hndl = BAD_HANDLE;
	IPluginContext *pContext = nullptr;
	bool erase = true;
	bool freehndl = true;
	
	custom_prop_info_t(IEntityFactory *fac_, std::string &&clsname_);
	~custom_prop_info_t();
	
	void zero(CBaseEntity *pEntity)
	{
		for(custom_typedescription_t &desc : dataDesc) {
			int offset = desc.get_offset();
			switch(desc.fieldType) {
				case FIELD_INTEGER: {
					for(int i = 0; i < desc.fieldSize; ++i) {
						*(int *)(((unsigned char *)pEntity) + offset + (i * sizeof(int))) = 0;
					}
					break;
				}
				case FIELD_FLOAT: {
					for(int i = 0; i < desc.fieldSize; ++i) {
						*(float *)(((unsigned char *)pEntity) + offset + (i * sizeof(float))) = 0.0f;
					}
					break;
				}
				case FIELD_BOOLEAN: {
					for(int i = 0; i < desc.fieldSize; ++i) {
						*(bool *)(((unsigned char *)pEntity) + offset + (i * sizeof(bool))) = false;
					}
					break;
				}
				case FIELD_EHANDLE: {
					for(int i = 0; i < desc.fieldSize; ++i) {
						new ((EHANDLE *)(((unsigned char *)pEntity) + offset + (i * sizeof(EHANDLE)))) EHANDLE();
					}
					break;
				}
				case FIELD_VECTOR: {
					for(int i = 0; i < desc.fieldSize; ++i) {
						vec3_t &vec = *(vec3_t *)(((unsigned char *)pEntity) + offset + (i * sizeof(vec3_t)));
						vec[0] = 0.0f;
						vec[1] = 0.0f;
						vec[2] = 0.0f;
					}
					break;
				}
			}
		}
	}
	
	void update_offsets()
	{
		for(custom_typedescription_t &desc : dataDesc) {
			desc.get_offset() += base;
		}
	}
	
	bool has_prop(const std::string &name)
	{
		for(custom_typedescription_t &desc : dataDesc) {
			if(desc.fieldName == name) {
				return true;
			}
		}
		
		return false;
	}
	
	void remove_prop(const std::string &name)
	{
		bool removed = false;
		
		dataDesc_t::iterator it{dataDesc.begin()};
		while(it != dataDesc.end()) {
			if(it->fieldName == name) {
				it->clear_name();
				dataDesc.erase(it);
				removed = true;
				break;
			}
			++it;
		}
		
		if(removed) {
			map.dataDesc = (typedescription_t *)dataDesc.data();
			--map.dataNumFields;
		}
	}
	
	void add_prop(const std::string &name, custom_prop_type type)
	{
		dataDesc.emplace_back();
		custom_typedescription_t &desc = dataDesc.back();
		
		desc.set_name(name);
		
		desc.flags = FTYPEDESC_PRIVATE|FTYPEDESC_VIEW_NEVER;
		desc.get_offset() = size;
		if(was_overriden && base != 0) {
			desc.get_offset() += base;
		}
		desc.fieldSize = 1;
		
		switch(type) {
			case custom_prop_int: {
				desc.fieldType = FIELD_INTEGER;
				desc.fieldSizeInBytes = (sizeof(int) * desc.fieldSize);
				break;
			}
			case custom_prop_float: {
				desc.fieldType = FIELD_FLOAT;
				desc.fieldSizeInBytes = (sizeof(float) * desc.fieldSize);
				break;
			}
			case custom_prop_bool: {
				desc.fieldType = FIELD_BOOLEAN;
				desc.fieldSizeInBytes = (sizeof(bool) * desc.fieldSize);
				break;
			}
			case custom_prop_entity: {
				desc.fieldType = FIELD_EHANDLE;
				desc.fieldSizeInBytes = (sizeof(EHANDLE) * desc.fieldSize);
				break;
			}
			case custom_prop_vector: {
				desc.fieldType = FIELD_VECTOR;
				desc.fieldSizeInBytes = (sizeof(vec3_t) * desc.fieldSize);
				break;
			}
		}
		
		size += desc.fieldSizeInBytes;
		
		desc.zero();
		
		map.dataDesc = (typedescription_t *)dataDesc.data();
		++map.dataNumFields;
	}
	
	datamap_t *HookGetDataDescMap()
	{
		RETURN_META_VALUE(MRES_SUPERCEDE, &map);
	}
	
	void HookEntityDtor()
	{
		CBaseEntity *pEntity = META_IFACEPTR(CBaseEntity);
		SH_REMOVE_HOOK(CBaseEntity, GetDataDescMap, pEntity, SH_MEMBER(this, &custom_prop_info_t::HookGetDataDescMap), false);
		SH_REMOVE_MANUALHOOK(GenericDtor, pEntity, SH_MEMBER(this, &custom_prop_info_t::HookEntityDtor), false);
		RETURN_META(MRES_IGNORED);
	}
	
	void do_override(CBaseEntity *pEntity);
	
	IServerNetworkable *HookCreate(const char *classname);
};

SH_DECL_HOOK0(CBaseEntity, GetServerClass, SH_NOATTRIB, 0, ServerClass *);

class custom_ServerClass
{
public:
	custom_ServerClass()
	{
		m_pNetworkName = nullptr;
	}
	
	void set_name(const std::string &name)
	{
		clear_name();
		
		size_t len = name.length();
		m_pNetworkName = (char *)malloc(len+1);
		strncpy((char *)m_pNetworkName, name.c_str(), len);
		((char *)m_pNetworkName)[len] = '\0';
	}
	
	void clear_name()
	{
		if(m_pNetworkName != nullptr) {
			free((void *)m_pNetworkName);
		}
		m_pNetworkName = nullptr;
	}
	
	~custom_ServerClass()
	{
		clear_name();
	}
	
	const char					*m_pNetworkName;
	SendTable					*m_pTable;
	ServerClass					*m_pNext;
	int							m_ClassID;	// Managed by the engine.

	// This is an index into the network string table (sv.GetInstanceBaselineTable()).
	int							m_InstanceBaselineIndex; // INVALID_STRING_INDEX if not initialized yet.
};

SendProp *UTIL_FindInSendTable(SendTable *pTable, const char *name, bool recursive, bool ignoreexclude)
{
	const char *pname;
	int props = pTable->GetNumProps();
	SendProp *prop;

	for (int i=0; i<props; i++)
	{
		prop = pTable->GetProp(i);
		pname = prop->GetName();
		SendTable *pInnerTable = prop->GetDataTable();
		if(!ignoreexclude || (ignoreexclude && !prop->IsExcludeProp())) {
			if (pname && strcmp(name, pname) == 0)
			{
				return prop;
			}
		}
		if (pInnerTable)
		{
			if(strcmp(pInnerTable->GetName(), name) == 0) {
				return prop;
			}

			if(recursive) {
				prop = UTIL_FindInSendTable(pInnerTable, name, recursive, ignoreexclude);
				if(prop) {
					return prop;
				}
			}
		}
	}

	return nullptr;
}

SendTable *UTIL_FindSendtableInSendTable(SendTable *pTable, const char *name)
{
	if(strcmp(pTable->GetName(), name) == 0) {
		return pTable;
	}
	
	SendProp *prop = UTIL_FindInSendTable(pTable, name, true, true);
	if(prop) {
		pTable = prop->GetDataTable();
		if(prop->GetType() != DPT_DataTable || pTable == nullptr) {
			return nullptr;
		}
		
		return pTable;
	}
	
	return nullptr;
}

void assign_prop(SendProp *prop, SendProp *realprop)
{
	if(realprop->GetType() != DPT_DataTable) {
		prop->SetProxyFn(realprop->GetProxyFn());
	} else {
		prop->SetProxyFn(nullptr);
	}
	
	if(realprop->GetType() == DPT_DataTable) {
		prop->SetDataTableProxyFn(realprop->GetDataTableProxyFn());
	} else {
		prop->SetDataTableProxyFn(nullptr);
	}
	
	prop->SetOffset(realprop->GetOffset());
	prop->SetDataTable(realprop->GetDataTable());
	prop->SetParentArrayPropName((char *)realprop->GetParentArrayPropName());
	prop->SetArrayProp(realprop->GetArrayProp());
	prop->SetArrayLengthProxy(realprop->GetArrayLengthProxy());
	prop->SetNumElements(realprop->GetNumElements());
	prop->SetExtraData(realprop->GetExtraData());
	
	prop->SetFlags(realprop->GetFlags());
	if(realprop->IsInsideArray()) {
		prop->SetInsideArray();
	}
	
	prop->m_Type = realprop->m_Type;
	prop->m_nBits = realprop->m_nBits;
	prop->m_fLowValue = realprop->m_fLowValue;
	prop->m_fHighValue = realprop->m_fHighValue;
	prop->m_ElementStride = realprop->m_ElementStride;
	prop->m_pVarName = realprop->m_pVarName;
	prop->m_fHighLowMul = realprop->m_fHighLowMul;
}

ServerClass *FindServerClass(const char *classname)
{
#if 1
	ServerClass *srvcls = gamehelpers->FindServerClass(classname);
#else
	ServerClass *srvcls = gamedll->GetAllServerClasses();
	while(srvcls) {
		if(strcmp(classname, srvcls->GetName()) == 0) {
			break;
		}
		srvcls = srvcls->m_pNext;
	}
#endif
	return srvcls;
}

struct unexclude_prop_t
{
	unexclude_prop_t(SendProp *prop_, const char *m_pExcludeDTName_, const char *m_pVarName_)
		: prop{prop_}, m_pExcludeDTName{m_pExcludeDTName_}, m_pVarName{m_pVarName_}
	{
		
	}
	
	~unexclude_prop_t()
	{
		if(prop) {
			prop->SetOffset(0);
			prop->SetProxyFn(nullptr);
			prop->SetDataTableProxyFn(nullptr);
			prop->SetDataTable(nullptr);
			prop->SetParentArrayPropName(nullptr);
			prop->SetArrayProp(nullptr);
			prop->SetArrayLengthProxy(nullptr);
			prop->SetNumElements(0);
			prop->SetExtraData(nullptr);
			
			prop->m_Type = DPT_Int;
			prop->m_nBits = 0;
			prop->m_fLowValue = 0;
			prop->m_fHighValue = 0;
			prop->m_ElementStride = 0;
			prop->m_fHighLowMul = 0;
			
			prop->m_pExcludeDTName = m_pExcludeDTName;
			prop->m_pVarName = m_pVarName;
			prop->SetFlags(SPROP_EXCLUDE);
		}
	}
	
	const char *m_pExcludeDTName = nullptr;
	const char *m_pVarName = nullptr;
	SendProp *prop = nullptr;
};

class custom_SendTable : public SendTable
{
public:
	custom_SendTable()
		: SendTable()
	{
		m_pNetTableName = nullptr;
	}
	
	void set_name(const std::string &name)
	{
		clear_name();
		
		size_t len = name.length();
		m_pNetTableName = (char *)malloc(len+1);
		strncpy((char *)m_pNetTableName, name.c_str(), len);
		((char *)m_pNetTableName)[len] = '\0';
	}
	
	void clear_name()
	{
		if(m_pNetTableName != nullptr) {
			free((void *)m_pNetTableName);
		}
		m_pNetTableName = nullptr;
	}
	
	~custom_SendTable()
	{
		clear_name();
	}
};

struct serverclass_override_t
{
	serverclass_override_t(IEntityFactory *fac_, std::string &&clsname_, ServerClass *realcls_);
	~serverclass_override_t();
	
	IServerNetworkable *HookCreate(const char *classname);
	
	ServerClass *HookGetServerClass()
	{
		RETURN_META_VALUE(MRES_SUPERCEDE, (ServerClass *)&cls);
	}
	
	void HookEntityDtor()
	{
		CBaseEntity *pEntity = META_IFACEPTR(CBaseEntity);
		SH_REMOVE_HOOK(CBaseEntity, GetServerClass, pEntity, SH_MEMBER(this, &serverclass_override_t::HookGetServerClass), false);
		SH_REMOVE_MANUALHOOK(GenericDtor, pEntity, SH_MEMBER(this, &serverclass_override_t::HookEntityDtor), false);
	}
	
	void do_override(CBaseEntity *pEntity);
	
	void override_with(ServerClass *netclass);
	void override_with(const std::string &netname, const std::string &dtname);
	void set_base_class(SendTable *table);
	void unexclude_prop(SendProp *prop, SendProp *realprop);
	
	IEntityFactory *fac = nullptr;
	bool fac_is_sp = false;
	custom_ServerClass cls{};
	custom_SendTable tbl{};
	std::vector<SendProp> props{};
	ServerClass *realcls = nullptr;
	ServerClass *fakecls = nullptr;
	std::string clsname{};
	Handle_t hndl = BAD_HANDLE;
	IPluginContext *pContext = nullptr;
	bool erase = true;
	bool freehndl = true;
	bool was_overriden = false;
	bool base_class_set = false;
	bool set_classid = false;
	SendProp *m_pProps = nullptr;
	int size = 0;
	std::vector<unexclude_prop_t> exclude_props{};
};

class sp_entity_factory : public IEntityFactory
{
	sp_entity_factory(std::string &&name_);
public:
	sp_entity_factory(std::string &&name_, IPluginFunction *func_, size_t size_)
	: sp_entity_factory(std::move(name_))
	{
		func = func_;
		size = size_;
	}
	sp_entity_factory(std::string &&name_, IEntityFactory *based_)
		: sp_entity_factory(std::move(name_))
	{
		based = based_;
		size = based->GetEntitySize();
	}
	~sp_entity_factory();
	IServerNetworkable *Create(const char *pClassName);
	void Destroy(IServerNetworkable *pNetworkable) {
		if(based) {
			based->Destroy(pNetworkable);
		} else {
			if(pNetworkable) {
				pNetworkable->Release();
			}
		}
	}
	size_t GetEntitySize() { return (size_t)-1; }
	
	std::string name{};
	IEntityFactory *based = nullptr;
	IPluginFunction *func = nullptr;
	size_t size = (size_t)-1;
	
	custom_prop_info_t *custom_prop = nullptr;
	serverclass_override_t *custom_server = nullptr;
	
	Handle_t hndl = BAD_HANDLE;
	IPluginContext *pContext = nullptr;
	bool freehndl = true;
	bool dont_delete = false;
};

struct factory_removal_t
{
	factory_removal_t(std::string &&name_, IEntityFactory *fac_)
		: name{std::move(name_)}, based{fac_}
	{
		dictionary->remove_factory(based, name);
	}
	
	~factory_removal_t()
	{
		dictionary->m_Factories.Remove(name.c_str());
		dictionary->InstallFactory(based, name.c_str());
	}
	
	std::string name{};
	IEntityFactory *based = nullptr;
};

using factory_map_t = std::unordered_map<std::string, sp_entity_factory *>;
factory_map_t factory_map{};

sp_entity_factory::sp_entity_factory(std::string &&name_)
	: name(std::move(name_))
{
	factory_map[name] = this;

	dictionary->InstallFactory(this, name.c_str());
}

sp_entity_factory::~sp_entity_factory()
{
	dont_delete = true;
	
	dictionary->remove_factory(this, name.c_str());
	
	factory_map.erase(name);
	
	if(freehndl) {
		if(hndl != BAD_HANDLE) {
			HandleSecurity security(pContext->GetIdentity(), myself->GetIdentity());
			handlesys->FreeHandle(hndl, &security);
		}
	}
}

custom_prop_info_t *curr_data_info = nullptr;
serverclass_override_t *curr_server_info = nullptr;

using info_map_t = std::unordered_map<std::string, custom_prop_info_t *>;
info_map_t info_map{};

using server_map_t = std::unordered_map<std::string, serverclass_override_t *>;
server_map_t server_map{};

void CEntityFactoryDictionary::remove_factory(IEntityFactory *fac, const std::string &name)
{
	info_map_t::iterator info_it{info_map.find(name)};
	if(info_it != info_map.end()) {
		custom_prop_info_t *prop{info_it->second};
		prop->erase = false;
		delete prop;
		info_map.erase(info_it);
	}
	
	server_map_t::iterator server_it{server_map.find(name)};
	if(server_it != server_map.end()) {
		serverclass_override_t *prop{server_it->second};
		prop->erase = false;
		delete prop;
		server_map.erase(server_it);
	}
	
	m_Factories.Remove(name.c_str());
	
	if(CEntityFactoryDictionary::is_factory_custom(fac)) {
		sp_entity_factory *sp_fac = (sp_entity_factory *)fac;
		if(!sp_fac->dont_delete) {
			delete sp_fac;
		}
	}
}

ServerClass *custom_server_head = nullptr;

class CNetworkStringTableContainer;
enum server_state_t : int;

class CBaseServer : public IServer
{
public:
	
#if SOURCE_ENGINE == SE_TF2
	server_state_t	m_State;		// some actions are only valid during load
	int				m_Socket;		// network socket 
	int				m_nTickCount;	// current server tick
	bool			m_bSimulatingTicks;		// whether or not the server is currently simulating ticks
	char			m_szMapname[64];		// map name
	char			m_szMapFilename[64];	// map filename, may bear no resemblance to map name
	char			m_szSkyname[64];		// skybox name
	char			m_Password[32];		// server password

	MD5Value_t		worldmapMD5;		// For detecting that client has a hacked local copy of map, the client will be dropped if this occurs.
	
	CNetworkStringTableContainer *m_StringTables;	// newtork string table container

	INetworkStringTable *m_pInstanceBaselineTable; 
	INetworkStringTable *m_pLightStyleTable;
	INetworkStringTable *m_pUserInfoTable;
	INetworkStringTable *m_pServerStartupTable;
	INetworkStringTable *m_pDownloadableFileTable;

	// This will get set to NET_MAX_PAYLOAD if the server is MP.
	bf_write			m_Signon;
	CUtlMemory<byte>	m_SignonBuffer;

	int			serverclasses;		// number of unique server classes
	int			serverclassbits;	// log2 of serverclasses
#endif
	
	void increment_svclasses()
	{
#if SOURCE_ENGINE == SE_TF2
		++serverclasses;
		serverclassbits = Q_log2( serverclasses ) + 1;
#endif
	}
	
	void decrement_svclasses()
	{
#if SOURCE_ENGINE == SE_TF2
		--serverclasses;
		serverclassbits = Q_log2( serverclasses ) + 1;
#endif
	}
};

void serverclass_override_t::unexclude_prop(SendProp *prop, SendProp *realprop)
{
	const char *m_pExcludeDTName = prop->m_pExcludeDTName;
	const char *m_pVarName = prop->m_pVarName;
	
	if(realprop) {
		assign_prop(prop, realprop);
	}
	
	int flags = prop->GetFlags();
	flags &= ~SPROP_EXCLUDE;
	prop->SetFlags(flags);
	
	prop->m_pExcludeDTName = nullptr;
	
	unexclude_prop_t unex{prop, m_pExcludeDTName, m_pVarName};
	exclude_props.emplace_back(std::move(unex));
}

void serverclass_override_t::set_base_class(SendTable *table2)
{
	SendTable *table1 = realcls->m_pTable;
	
	m_pProps = table1->m_pProps;
	++table1->m_nProps;
	
	table1->m_pProps = new SendProp[table1->m_nProps];
	
	for(int i = 1; i < table1->m_nProps; ++i) {
		SendProp *prop = &table1->m_pProps[i];
		SendProp *realprop = &m_pProps[i-1];
		assign_prop(prop, realprop);
	}
	
	SendProp *prop = &table1->m_pProps[0];
	prop->m_Type = DPT_DataTable;
	prop->m_pVarName = "baseclass";
	prop->SetOffset(0);
	prop->SetDataTable(table2);
	prop->SetDataTableProxyFn(SendProxy_DataTableToDataTable);
	prop->SetFlags(SPROP_PROXY_ALWAYS_YES|SPROP_COLLAPSIBLE);
	
	base_class_set = true;
}

size_t custom_server_classid = 0;

void serverclass_override_t::override_with(const std::string &netname, const std::string &dtname)
{
	fakecls = nullptr;
	
	tbl.set_name(dtname);
	
	if(!set_classid) {
		++custom_server_classid;
		
		set_classid = true;
		
		cls.m_ClassID = custom_server_classid;
	}
	
	cls.set_name(netname);
	
	char idString[32];
	Q_snprintf(idString, sizeof(idString), "%d", cls.m_ClassID);

	bool lock = engine->LockNetworkStringTables(true);
	cls.m_InstanceBaselineIndex = m_pInstanceBaselineTable->AddString(true, idString, 0, nullptr);
	engine->LockNetworkStringTables(lock);
}

void serverclass_override_t::override_with(ServerClass *netclass)
{
	fakecls = netclass;
	
	if(set_classid) {
		--custom_server_classid;
		set_classid = false;
	}
	
	tbl.set_name(fakecls->m_pTable->m_pNetTableName);
	
	cls.set_name(fakecls->m_pNetworkName);
	
	cls.m_ClassID = fakecls->m_ClassID;
	cls.m_InstanceBaselineIndex = fakecls->m_InstanceBaselineIndex;
}

serverclass_override_t::serverclass_override_t(IEntityFactory *fac_, std::string &&clsname_, ServerClass *realcls_)
	: fac{fac_}, clsname{std::move(clsname_)}, realcls{realcls_}
{
	if(CEntityFactoryDictionary::is_factory_custom(fac)) {
		fac_is_sp = true;
		sp_entity_factory *spfac = (sp_entity_factory *)fac;
		spfac->custom_server = this;
	} else {
		SH_ADD_HOOK(IEntityFactory, Create, fac, SH_MEMBER(this, &serverclass_override_t::HookCreate), false);
	}
	
	server_map[clsname] = this;
	
	props.emplace_back();
	SendProp *prop = &props.back();
	prop->m_Type = DPT_DataTable;
	prop->m_pVarName = "baseclass";
	prop->SetOffset(0);
	prop->SetDataTable(realcls->m_pTable);
	prop->SetDataTableProxyFn(SendProxy_DataTableToDataTable);
	prop->SetFlags(SPROP_PROXY_ALWAYS_YES|SPROP_COLLAPSIBLE);
	
	tbl.m_pProps = props.data();
	tbl.m_nProps = props.size();
	tbl.set_name(realcls->m_pTable->m_pNetTableName);
	tbl.m_pPrecalc = realcls->m_pTable->m_pPrecalc;
	
	cls.m_pTable = &tbl;
	
	cls.m_ClassID = realcls->m_ClassID;
	cls.set_name(realcls->m_pNetworkName);
	cls.m_InstanceBaselineIndex = realcls->m_InstanceBaselineIndex;
	
	cls.m_pNext = nullptr;
	
	if(!custom_server_head) {
		custom_server_head = (ServerClass *)&cls;
		g_pServerClassTail->m_pNext = custom_server_head;
	} else {
		custom_server_head->m_pNext = (ServerClass *)&cls;
		custom_server_head = custom_server_head->m_pNext;
	}
	
	((CBaseServer *)server)->increment_svclasses();
}

extern void remove_serverclass_from_sm_cache(ServerClass *pMap);

serverclass_override_t::~serverclass_override_t()
{
	if(erase) {
		server_map.erase(clsname);
	}
	
	for(ServerClass *cur = custom_server_head, *prev = nullptr; cur != nullptr; prev = cur, cur = cur->m_pNext) {
		if(cur == (ServerClass *)this) {
			if(prev != nullptr) {
				prev->m_pNext = cur->m_pNext;
			} else {
				custom_server_head = cur->m_pNext;
			}
			cur->m_pNext = nullptr;
			break;
		}
	}
	
	if(!custom_server_head) {
		g_pServerClassTail->m_pNext = nullptr;
	}
	
	if(set_classid) {
		--custom_server_classid;
	}
	
	exclude_props.clear();
	
	if(base_class_set) {
		SendTable *table1 = realcls->m_pTable;
		
		--table1->m_nProps;
		delete[] table1->m_pProps;
		table1->m_pProps = m_pProps;
	}
	
	((CBaseServer *)server)->decrement_svclasses();
	
	remove_serverclass_from_sm_cache((ServerClass *)&cls);
	
	if(!fac_is_sp) {
		SH_REMOVE_HOOK(IEntityFactory, Create, fac, SH_MEMBER(this, &serverclass_override_t::HookCreate), false);
	}
	
	loop_all_entities([this](CBaseEntity *pEntity){
		SH_REMOVE_HOOK(CBaseEntity, GetServerClass, pEntity, SH_MEMBER(this, &serverclass_override_t::HookGetServerClass), false);
		SH_REMOVE_MANUALHOOK(GenericDtor, pEntity, SH_MEMBER(this, &serverclass_override_t::HookEntityDtor), false);
	}, clsname);
	
	if(freehndl) {
		if(hndl != BAD_HANDLE) {
			HandleSecurity security(pContext->GetIdentity(), myself->GetIdentity());
			handlesys->FreeHandle(hndl, &security);
		}
	}
}

void *DoPvAllocEntPrivateData(long cb)
{
	last_cb = cb;
	
	if(curr_data_info != nullptr) {
		cb += curr_data_info->size;
	}
	
	if(curr_server_info != nullptr) {
		cb += curr_server_info->size;
	}
	
	return calloc(1, cb);
}

void *HookPvAllocEntPrivateData(long cb)
{
	RETURN_META_VALUE(MRES_SUPERCEDE, DoPvAllocEntPrivateData(cb));
}

custom_prop_info_t::custom_prop_info_t(IEntityFactory *fac_, std::string &&clsname_)
	: fac{fac_}, clsname{std::move(clsname_)}
{
	map.zero();
	
	if(CEntityFactoryDictionary::is_factory_custom(fac)) {
		fac_is_sp = true;
		sp_entity_factory *spfac = (sp_entity_factory *)fac;
		spfac->custom_prop = this;
	} else {
		SH_ADD_HOOK(IEntityFactory, Create, fac, SH_MEMBER(this, &custom_prop_info_t::HookCreate), false);
	}
	
	info_map[clsname] = this;
}

extern void remove_datamap_from_sm_cache(datamap_t *pMap);

custom_prop_info_t::~custom_prop_info_t()
{
	if(erase) {
		info_map.erase(clsname);
	}
	
	remove_datamap_from_sm_cache(&map);
	
	if(!fac_is_sp) {
		SH_REMOVE_HOOK(IEntityFactory, Create, fac, SH_MEMBER(this, &custom_prop_info_t::HookCreate), false);
	}
	
	loop_all_entities([this](CBaseEntity *pEntity){
		SH_REMOVE_HOOK(CBaseEntity, GetDataDescMap, pEntity, SH_MEMBER(this, &custom_prop_info_t::HookGetDataDescMap), false);
		SH_REMOVE_MANUALHOOK(GenericDtor, pEntity, SH_MEMBER(this, &custom_prop_info_t::HookEntityDtor), false);
	}, clsname);
	
	for(custom_typedescription_t &desc : dataDesc) {
		desc.clear_name();
	}
	
	if(freehndl) {
		if(hndl != BAD_HANDLE) {
			HandleSecurity security(pContext->GetIdentity(), myself->GetIdentity());
			handlesys->FreeHandle(hndl, &security);
		}
	}
}

void serverclass_override_t::do_override(CBaseEntity *pEntity)
{
	SH_ADD_HOOK(CBaseEntity, GetServerClass, pEntity, SH_MEMBER(this, &serverclass_override_t::HookGetServerClass), false);
	SH_ADD_MANUALHOOK(GenericDtor, pEntity, SH_MEMBER(this, &serverclass_override_t::HookEntityDtor), false);
}

void custom_prop_info_t::do_override(CBaseEntity *pEntity)
{
	if(!was_overriden) {
		datamap_t *basemap = gamehelpers->GetDataMap(pEntity);
		ServerClass *svcls = pEntity->GetServerClass();
		
		std::string mapname{svcls->GetName()};
		mapname += "_custom";
		map.set_name(mapname);
		map.baseMap = basemap;
		
		base = last_cb;
		update_offsets();
		
		was_overriden = true;
	}
	
	zero(pEntity);
	
	SH_ADD_HOOK(CBaseEntity, GetDataDescMap, pEntity, SH_MEMBER(this, &custom_prop_info_t::HookGetDataDescMap), false);
	SH_ADD_MANUALHOOK(GenericDtor, pEntity, SH_MEMBER(this, &custom_prop_info_t::HookEntityDtor), false);
}

IServerNetworkable *sp_entity_factory::Create(const char *pClassName)
{
	IServerNetworkable *net = nullptr;
	
	if(based != nullptr) {
		last_cb = based->GetEntitySize();
		curr_data_info = custom_prop;
		curr_server_info = custom_server;
		net = based->Create(pClassName);
		curr_data_info = nullptr;
		curr_server_info = nullptr;
		CBaseEntity *pEntity = net->GetBaseEntity();
		if(custom_prop) {
			custom_prop->do_override(pEntity);
		}
		if(custom_server) {
			custom_server->do_override(pEntity);
		}
	} else if(func != nullptr) {
		cell_t res = 0;
		func->PushCell(custom_prop ? custom_prop->size : 0);
		func->Execute(&res);
		last_cb = size;
		CBaseEntity *obj = (CBaseEntity *)res;
		if(obj != nullptr) {
			obj->PostConstructor(pClassName);
			net = obj->GetNetworkable();
			if(custom_prop) {
				custom_prop->do_override(obj);
			}
			if(custom_server) {
				custom_server->do_override(obj);
			}
		}
	}
	
	return net;
}

IServerNetworkable *custom_prop_info_t::HookCreate(const char *classname)
{
	IEntityFactory *fac = META_IFACEPTR(IEntityFactory);
	
	curr_data_info = this;
	IServerNetworkable *net = SH_CALL(fac, &IEntityFactory::Create)(classname);
	curr_data_info = nullptr;
	
	CBaseEntity *pEntity = net->GetBaseEntity();
	
	do_override(pEntity);
	
	RETURN_META_VALUE(MRES_SUPERCEDE, net);
}

IServerNetworkable *serverclass_override_t::HookCreate(const char *classname)
{
	IEntityFactory *fac = META_IFACEPTR(IEntityFactory);
	
	curr_server_info = this;
	IServerNetworkable *net = SH_CALL(fac, &IEntityFactory::Create)(classname);
	curr_server_info = nullptr;
	
	CBaseEntity *pEntity = net->GetBaseEntity();
	
	do_override(pEntity);
	
	RETURN_META_VALUE(MRES_SUPERCEDE, net);
}

cell_t IEntityFactoryCustomget(IPluginContext *pContext, const cell_t *params)
{
	IEntityFactory *factory = (IEntityFactory *)params[1];
	return CEntityFactoryDictionary::is_factory_custom(factory);
}

cell_t IEntityFactorySizeget(IPluginContext *pContext, const cell_t *params)
{
	IEntityFactory *factory = (IEntityFactory *)params[1];
	return factory->GetEntitySize();
}

cell_t EntityFactoryDictionaryremove(IPluginContext *pContext, const cell_t *params)
{
	char *name = nullptr;
	pContext->LocalToString(params[1], &name);
	
	IEntityFactory *factory = dictionary->FindFactory(name);
	if(!factory) {
		return pContext->ThrowNativeError("invalid classname %s", name);
	}
	
	if(CEntityFactoryDictionary::is_factory_custom(factory)) {
		return pContext->ThrowNativeError("cant remove custom factories using this native");
	}
	
	factory_removal_t *obj = new factory_removal_t{name, factory};
	return handlesys->CreateHandle(removal_handle, obj, pContext->GetIdentity(), myself->GetIdentity(), nullptr);
}

cell_t EntityFactoryDictionaryregister_based(IPluginContext *pContext, const cell_t *params)
{
	char *name = nullptr;
	pContext->LocalToString(params[1], &name);
	
	IEntityFactory *factory = dictionary->FindFactory(name);
	if(factory) {
		return pContext->ThrowNativeError("%s is already registered", name);
	}

	factory = (IEntityFactory *)params[2];
	
	sp_entity_factory *obj = new sp_entity_factory(name, factory);
	
	Handle_t hndl = handlesys->CreateHandle(factory_handle, obj, pContext->GetIdentity(), myself->GetIdentity(), nullptr);
	obj->hndl = hndl;
	obj->pContext = pContext;
	
	return hndl;
}

cell_t EntityFactoryDictionaryregister_function(IPluginContext *pContext, const cell_t *params)
{
	char *name = nullptr;
	pContext->LocalToString(params[1], &name);
	
	IEntityFactory *factory = dictionary->FindFactory(name);
	if(factory) {
		return pContext->ThrowNativeError("%s is already registered", name);
	}
	
	if(params[3] <= 0) {
		return pContext->ThrowNativeError("invalid size %i", params[3]);
	}
	
	IPluginFunction *callback = pContext->GetFunctionById(params[2]);
	
	sp_entity_factory *obj = new sp_entity_factory(name, callback, params[3]);
	
	Handle_t hndl = handlesys->CreateHandle(factory_handle, obj, pContext->GetIdentity(), myself->GetIdentity(), nullptr);
	obj->hndl = hndl;
	obj->pContext = pContext;
	
	return hndl;
}

cell_t CustomDatamapadd_prop(IPluginContext *pContext, const cell_t *params)
{
	HandleSecurity security(pContext->GetIdentity(), myself->GetIdentity());
	
	custom_prop_info_t *obj = nullptr;
	HandleError err = handlesys->ReadHandle(params[1], datamap_handle, &security, (void **)&obj);
	if(err != HandleError_None)
	{
		return pContext->ThrowNativeError("Invalid Handle %x (error: %d)", params[1], err);
	}
	
	char *name = nullptr;
	pContext->LocalToString(params[2], &name);
	
	obj->add_prop(name, (custom_prop_type)params[3]);
	return 0;
}

cell_t CustomDatamapfrom_classname(IPluginContext *pContext, const cell_t *params)
{
	char *name = nullptr;
	pContext->LocalToString(params[1], &name);
	
	IEntityFactory *factory = dictionary->FindFactory(name);
	if(!factory) {
		return pContext->ThrowNativeError("invalid classname %s", name);
	}
	
	if(info_map.find(name) != info_map.end()) {
		return pContext->ThrowNativeError("%s already has custom datamaps", name);
	}
	
	custom_prop_info_t *obj = new custom_prop_info_t(factory, name);

	Handle_t hndl = handlesys->CreateHandle(datamap_handle, obj, pContext->GetIdentity(), myself->GetIdentity(), nullptr);
	obj->hndl = hndl;
	obj->pContext = pContext;

	return hndl;
}

cell_t CustomDatamapfrom_factory(IPluginContext *pContext, const cell_t *params)
{
	HandleSecurity security(pContext->GetIdentity(), myself->GetIdentity());
	
	sp_entity_factory *factory = nullptr;
	HandleError err = handlesys->ReadHandle(params[1], factory_handle, &security, (void **)&factory);
	if(err != HandleError_None)
	{
		return pContext->ThrowNativeError("Invalid Handle %x (error: %d)", params[1], err);
	}
	
	std::string name{factory->name};
	
	if(info_map.find(name) != info_map.end()) {
		return pContext->ThrowNativeError("%s already has custom datamaps", name.c_str());
	}
	
	custom_prop_info_t *obj = new custom_prop_info_t(factory, std::move(name));

	Handle_t hndl = handlesys->CreateHandle(datamap_handle, obj, pContext->GetIdentity(), myself->GetIdentity(), nullptr);
	obj->hndl = hndl;
	obj->pContext = pContext;

	return hndl;
}

cell_t CustomSendtableunexclude_prop(IPluginContext *pContext, const cell_t *params)
{
	HandleSecurity security(pContext->GetIdentity(), myself->GetIdentity());
	
	serverclass_override_t *factory = nullptr;
	HandleError err = handlesys->ReadHandle(params[1], serverclass_handle, &security, (void **)&factory);
	if(err != HandleError_None)
	{
		return pContext->ThrowNativeError("Invalid Handle %x (error: %d)", params[1], err);
	}
	
	char *tablename = nullptr;
	pContext->LocalToString(params[2], &tablename);

	char *name = nullptr;
	pContext->LocalToString(params[3], &name);

	SendTable *clstable = factory->realcls->m_pTable;
	SendTable *table = UTIL_FindSendtableInSendTable(clstable, tablename);
	if(!table) {
		return pContext->ThrowNativeError("invalid table %s", tablename);
	}

	SendProp *prop = UTIL_FindInSendTable(table, name, false, false);
	if(!prop) {
		return pContext->ThrowNativeError("%s is not in %s", name, tablename);
	}

	if(!prop->IsExcludeProp()) {
		return 0;
	}

	SendProp *realprop = nullptr;
	SendTable *base = UTIL_FindSendtableInSendTable(clstable, prop->m_pExcludeDTName);
	if(base) {
		realprop = UTIL_FindInSendTable(base, name, true, true);
	}
	
	factory->unexclude_prop(prop, realprop);
	return 0;
}

cell_t CustomSendtableset_base_class(IPluginContext *pContext, const cell_t *params)
{
	HandleSecurity security(pContext->GetIdentity(), myself->GetIdentity());
	
	serverclass_override_t *factory = nullptr;
	HandleError err = handlesys->ReadHandle(params[1], serverclass_handle, &security, (void **)&factory);
	if(err != HandleError_None)
	{
		return pContext->ThrowNativeError("Invalid Handle %x (error: %d)", params[1], err);
	}
	
	if(factory->base_class_set) {
		return pContext->ThrowNativeError("base class was already set");
	}
	
	char *netname = nullptr;
	pContext->LocalToString(params[2], &netname);
	
	ServerClass *svcls = FindServerClass(netname);
	if(!svcls) {
		return pContext->ThrowNativeError("invalid netname %s", netname);
	}

	factory->set_base_class(svcls->m_pTable);
	return 0;
}

cell_t CustomSendtableoverride_with(IPluginContext *pContext, const cell_t *params)
{
	HandleSecurity security(pContext->GetIdentity(), myself->GetIdentity());
	
	serverclass_override_t *factory = nullptr;
	HandleError err = handlesys->ReadHandle(params[1], serverclass_handle, &security, (void **)&factory);
	if(err != HandleError_None)
	{
		return pContext->ThrowNativeError("Invalid Handle %x (error: %d)", params[1], err);
	}
	
	if(factory->set_classid || factory->fakecls != nullptr) {
		return pContext->ThrowNativeError("already has been overriden");
	}
	
	char *netname = nullptr;
	pContext->LocalToString(params[2], &netname);
	
	ServerClass *netclass = FindServerClass(netname);
	if(!netclass) {
		return pContext->ThrowNativeError("invalid netname %s", netname);
	}
	
	factory->override_with(netclass);
	return 0;
}

cell_t CustomSendtableoverride_with_ex(IPluginContext *pContext, const cell_t *params)
{
	HandleSecurity security(pContext->GetIdentity(), myself->GetIdentity());
	
	serverclass_override_t *factory = nullptr;
	HandleError err = handlesys->ReadHandle(params[1], serverclass_handle, &security, (void **)&factory);
	if(err != HandleError_None)
	{
		return pContext->ThrowNativeError("Invalid Handle %x (error: %d)", params[1], err);
	}
	
	if(factory->set_classid || factory->fakecls != nullptr) {
		return pContext->ThrowNativeError("already has been overriden");
	}
	
	char *netname = nullptr;
	pContext->LocalToString(params[2], &netname);
	
	char *dtname = nullptr;
	pContext->LocalToString(params[3], &dtname);
	
	factory->override_with(netname, dtname);
	return 0;
}

cell_t CustomSendtablefrom_classname(IPluginContext *pContext, const cell_t *params)
{
	char *classname = nullptr;
	pContext->LocalToString(params[1], &classname);
	
	IEntityFactory *factory = dictionary->FindFactory(classname);
	if(!factory) {
		return pContext->ThrowNativeError("invalid classname %s", classname);
	}
	
	if(server_map.find(classname) != server_map.end()) {
		return pContext->ThrowNativeError("%s already has custom sendtable", classname);
	}
	
	char *netname = nullptr;
	pContext->LocalToString(params[2], &netname);
	
	ServerClass *netclass = FindServerClass(netname);
	if(!netclass) {
		return pContext->ThrowNativeError("invalid netname %s", netname);
	}
	
	std::string clsname{classname};
	serverclass_override_t *obj = new serverclass_override_t{factory, std::move(clsname), netclass};
	Handle_t hndl = handlesys->CreateHandle(serverclass_handle, obj, pContext->GetIdentity(), myself->GetIdentity(), nullptr);
	obj->hndl = hndl;
	obj->pContext = pContext;
	
	return hndl;
}

cell_t CustomSendtablefrom_factory(IPluginContext *pContext, const cell_t *params)
{
	HandleSecurity security(pContext->GetIdentity(), myself->GetIdentity());
	
	sp_entity_factory *factory = nullptr;
	HandleError err = handlesys->ReadHandle(params[1], factory_handle, &security, (void **)&factory);
	if(err != HandleError_None)
	{
		return pContext->ThrowNativeError("Invalid Handle %x (error: %d)", params[1], err);
	}
	
	std::string name{factory->name};
	
	if(server_map.find(name) != server_map.end()) {
		return pContext->ThrowNativeError("%s already has custom sendtable", name.c_str());
	}
	
	char *netname = nullptr;
	pContext->LocalToString(params[2], &netname);
	
	ServerClass *netclass = FindServerClass(netname);
	if(!netclass) {
		return pContext->ThrowNativeError("invalid netname %s", netname);
	}
	
	serverclass_override_t *obj = new serverclass_override_t{factory, std::move(name), netclass};
	Handle_t hndl = handlesys->CreateHandle(serverclass_handle, obj, pContext->GetIdentity(), myself->GetIdentity(), nullptr);
	obj->hndl = hndl;
	obj->pContext = pContext;
	
	return hndl;
}

cell_t CustomEntityFactoryInterfaceget(IPluginContext *pContext, const cell_t *params)
{
	HandleSecurity security(pContext->GetIdentity(), myself->GetIdentity());
	
	sp_entity_factory *factory = nullptr;
	HandleError err = handlesys->ReadHandle(params[1], serverclass_handle, &security, (void **)&factory);
	if(err != HandleError_None)
	{
		return pContext->ThrowNativeError("Invalid Handle %x (error: %d)", params[1], err);
	}
	
	return (cell_t)(IEntityFactory *)factory;
}

cell_t EntityFactoryDictionaryfind(IPluginContext *pContext, const cell_t *params)
{
	char *classname = nullptr;
	pContext->LocalToString(params[1], &classname);
	
	return (cell_t)dictionary->FindFactory(classname);
}	

static cell_t SetEntityContextThink(IPluginContext *pContext, const cell_t *params)
{
	CBaseEntity *pEntity = gamehelpers->ReferenceToEntity(params[1]);
	if(!pEntity) {
		return pContext->ThrowNativeError("Invalid Entity Reference/Index %i", params[1]);
	}
	
	callback_holder_t *holder = nullptr;
	
	callback_holder_map_t::iterator it{callbackmap.find(pEntity)};
	if(it != callbackmap.end()) {
		holder = it->second;
	} else {
		holder = new callback_holder_t{pEntity, pContext->GetIdentity()};
	}
	
	char *context = nullptr;
	pContext->LocalToString(params[4], &context);
	
	holder->thinkctxs[context].callback = pContext->GetFunctionById(params[2]);
	holder->thinkctxs[context].data = params[5];
	
	pEntity->SetContextThink(&CBaseEntity::PluginThinkContext, sp_ctof(params[3]), context);
	
	return 0;
}

static cell_t SetEntityThink(IPluginContext *pContext, const cell_t *params)
{
	CBaseEntity *pEntity = gamehelpers->ReferenceToEntity(params[1]);
	if(!pEntity) {
		return pContext->ThrowNativeError("Invalid Entity Reference/Index %i", params[1]);
	}
	
	callback_holder_t *holder = nullptr;
	
	callback_holder_map_t::iterator it{callbackmap.find(pEntity)};
	if(it != callbackmap.end()) {
		holder = it->second;
	} else {
		holder = new callback_holder_t{pEntity, pContext->GetIdentity()};
	}
	
	holder->think.callback = pContext->GetFunctionById(params[2]);
	holder->think.data = params[4];
	
	pEntity->SetThink(&CBaseEntity::PluginThink);
	
	return 0;
}

static cell_t SetEntityNextThink(IPluginContext *pContext, const cell_t *params)
{
	CBaseEntity *pEntity = gamehelpers->ReferenceToEntity(params[1]);
	if(!pEntity) {
		return pContext->ThrowNativeError("Invalid Entity Reference/Index %i", params[1]);
	}
	
	pEntity->SetNextThink(sp_ctof(params[2]), nullptr);
	return 0;
}

static cell_t SetEntityNextThinkContext(IPluginContext *pContext, const cell_t *params)
{
	CBaseEntity *pEntity = gamehelpers->ReferenceToEntity(params[1]);
	if(!pEntity) {
		return pContext->ThrowNativeError("Invalid Entity Reference/Index %i", params[1]);
	}
	
	char *context = nullptr;
	pContext->LocalToString(params[3], &context);
	
	int iIndex = -1;
	
	if(context[0] == '\0') {
		if(CBaseEntity::m_iCurrentThinkContext < 0 || CBaseEntity::m_iCurrentThinkContext >= pEntity->GetAThinkFuncstions().Count()) {
			return pContext->ThrowNativeError("Theres no current context");
		} else {
			iIndex = CBaseEntity::m_iCurrentThinkContext;
		}
	} else {
		iIndex = pEntity->GetIndexForThinkContext(context);
		if(iIndex == NO_THINK_CONTEXT) {
			return pContext->ThrowNativeError("Invalid context %s", context);
		}
	}
	
	pEntity->SetNextThinkContext(sp_ctof(params[2]), iIndex);
	return 0;
}

sp_nativeinfo_t natives[] =
{
	{"IEntityFactory.Custom.get", IEntityFactoryCustomget},
	{"IEntityFactory.Size.get", IEntityFactorySizeget},
	{"CustomEntityFactory.Interface.get", CustomEntityFactoryInterfaceget},
	{"EntityFactoryDictionary.find", EntityFactoryDictionaryfind},
	{"EntityFactoryDictionary.register_based", EntityFactoryDictionaryregister_based},
	{"EntityFactoryDictionary.register_function", EntityFactoryDictionaryregister_function},
	{"EntityFactoryDictionary.remove", EntityFactoryDictionaryremove},
	{"CustomSendtable.from_factory", CustomSendtablefrom_factory},
	{"CustomSendtable.from_classname", CustomSendtablefrom_classname},
	{"CustomSendtable.override_with", CustomSendtableoverride_with},
	{"CustomSendtable.override_with_ex", CustomSendtableoverride_with_ex},
	{"CustomSendtable.unexclude_prop", CustomSendtableunexclude_prop},
	{"CustomSendtable.set_base_class", CustomSendtableset_base_class},
	{"CustomDatamap.from_classname", CustomDatamapfrom_classname},
	{"CustomDatamap.from_factory", CustomDatamapfrom_factory},
	{"CustomDatamap.add_prop", CustomDatamapadd_prop},
	{"SetEntityContextThink", SetEntityContextThink},
	{"SetEntityThink", SetEntityThink},
	{"SetEntityNextThink", SetEntityNextThink},
	{"SetEntityNextThinkContext", SetEntityNextThinkContext},
	{NULL, NULL}
};

void Sample::OnPluginUnloaded(IPlugin *plugin)
{
	callback_holder_map_t::iterator it{callbackmap.begin()};
	while(it != callbackmap.end()) {
		if(it->second->owner == plugin->GetIdentity()) {
			it->second->erase = false;
			callbackmap.erase(it);
			it->second->dtor(it->second->pEntity_);
			continue;
		}
		
		++it;
	}
}

void Sample::OnEntityDestroyed(CBaseEntity *pEntity)
{
	
}

void Sample::OnHandleDestroy(HandleType_t type, void *object)
{
	if(type == factory_handle) {
		sp_entity_factory *obj = (sp_entity_factory *)object;
		obj->freehndl = false;
		delete obj;
	} else if(type == datamap_handle) {
		custom_prop_info_t *obj = (custom_prop_info_t *)object;
		obj->freehndl = false;
		delete obj;
	} else if(type == removal_handle) {
		factory_removal_t *obj = (factory_removal_t *)object;
		delete obj;
	} else if(type == serverclass_handle) {
		serverclass_override_t *obj = (serverclass_override_t *)object;
		obj->freehndl = false;
		delete obj;
	}
}

CDetour *pPvAllocEntPrivateData = nullptr;

DETOUR_DECL_MEMBER1(DetourPvAllocEntPrivateData, void *, long, cb)
{
	return DoPvAllocEntPrivateData(cb);
}

#define SOURCEHOOK_BEING_STUPID

bool Sample::SDK_OnMetamodLoad(ISmmAPI *ismm, char *error, size_t maxlen, bool late)
{
	gpGlobals = ismm->GetCGlobals();
	GET_V_IFACE_ANY(GetServerFactory, servertools, IServerTools, VSERVERTOOLS_INTERFACE_VERSION)
	GET_V_IFACE_ANY(GetEngineFactory, engine, IVEngineServer, INTERFACEVERSION_VENGINESERVER)
#if SOURCE_ENGINE == SE_TF2
	dictionary = (CEntityFactoryDictionary *)servertools->GetEntityFactoryDictionary();
#endif
#ifndef SOURCEHOOK_BEING_STUPID
	SH_ADD_HOOK(IVEngineServer, PvAllocEntPrivateData, engine, SH_STATIC(&HookPvAllocEntPrivateData), false);
#endif
	GET_V_IFACE_ANY(GetServerFactory, gamedll, IServerGameDLL, INTERFACEVERSION_SERVERGAMEDLL)
	GET_V_IFACE_ANY(GetEngineFactory, netstringtables, INetworkStringTableContainer, INTERFACENAME_NETWORKSTRINGTABLESERVER)
	g_pServerClassHead = gamedll->GetAllServerClasses();
	g_pServerClassTail = g_pServerClassHead;
	while(g_pServerClassTail && g_pServerClassTail->m_pNext) {
		g_pServerClassTail = g_pServerClassTail->m_pNext;
	}
	custom_server_classid = g_pServerClassTail->m_ClassID;
	m_pInstanceBaselineTable = netstringtables->FindTable(INSTANCE_BASELINE_TABLENAME);
#if SOURCE_ENGINE == SE_TF2
	server = engine->GetIServer();
#endif
	return true;
}

IGameConfig *g_pGameConf = nullptr;

CDetour *pPhysicsRunSpecificThink = nullptr;

bool Sample::SDK_OnLoad(char *error, size_t maxlen, bool late)
{
	gameconfs->LoadGameConfigFile("datamaps", &g_pGameConf, error, maxlen);

	g_pGameConf->GetOffset("CBaseEntity::PostConstructor", &CBaseEntityPostConstructor);

	g_pGameConf->GetMemSig("SimThink_EntityChanged", &SimThink_EntityChangedPtr);
	g_pGameConf->GetMemSig("AllocPooledString", &AllocPooledStringPtr);
	g_pGameConf->GetMemSig("EntityFactoryDictionary", &EntityFactoryDictionaryPtr);
	g_pGameConf->GetMemSig("CGlobalEntityList::FindEntityByClassname", &CGlobalEntityListFindEntityByClassname);
	g_pGameConf->GetMemSig("UTIL_Remove", &UTIL_RemovePtr);
	
	CDetourManager::Init(g_pSM->GetScriptingEngine(), g_pGameConf);
	
#ifdef SOURCEHOOK_BEING_STUPID
	void **vtable = *(void ***)engine;
	int index = vfunc_index(&IVEngineServer::PvAllocEntPrivateData);
	pPvAllocEntPrivateData = DETOUR_CREATE_MEMBER(DetourPvAllocEntPrivateData, vtable[index])
	pPvAllocEntPrivateData->EnableDetour();
#endif
	
	pPhysicsRunSpecificThink = DETOUR_CREATE_MEMBER(PhysicsRunSpecificThink, "CBaseEntity::PhysicsRunSpecificThink")
	pPhysicsRunSpecificThink->EnableDetour();
	
	g_pEntityList = reinterpret_cast<CBaseEntityList *>(gamehelpers->GetGlobalEntityList());
	
#if SOURCE_ENGINE == SE_LEFT4DEAD2
	dictionary = (void_to_func<CEntityFactoryDictionary *(*)()>(EntityFactoryDictionaryPtr))();
#endif
	
	factory_handle = handlesys->CreateType("entity_factory", this, 0, nullptr, nullptr, myself->GetIdentity(), nullptr);
	datamap_handle = handlesys->CreateType("datamap", this, 0, nullptr, nullptr, myself->GetIdentity(), nullptr);
	removal_handle = handlesys->CreateType("factory_removal", this, 0, nullptr, nullptr, myself->GetIdentity(), nullptr);
	serverclass_handle = handlesys->CreateType("serverclass_override", this, 0, nullptr, nullptr, myself->GetIdentity(), nullptr);
	
	sharesys->AddDependency(myself, "sdkhooks.ext", true, true);
	
	plsys->AddPluginsListener(this);
	
	sharesys->RegisterLibrary(myself, "datamaps");

	return true;
}

void Sample::SDK_OnAllLoaded()
{
	SM_GET_LATE_IFACE(SDKHOOKS, g_pSDKHooks);
	SM_GET_LATE_IFACE(SDKTOOLS, g_pSDKTools);
	
	g_pSDKHooks->AddEntityListener(this);
	
#if SOURCE_ENGINE == SE_LEFT4DEAD2
	server = g_pSDKTools->GetIServer();
#endif
	
	sharesys->AddNatives(myself, natives);
}

bool Sample::QueryRunning(char *error, size_t maxlength)
{
	SM_CHECK_IFACE(SDKHOOKS, g_pSDKHooks);
	SM_CHECK_IFACE(SDKTOOLS, g_pSDKTools);
	return true;
}

bool Sample::QueryInterfaceDrop(SMInterface *pInterface)
{
	if(pInterface == g_pSDKHooks)
		return false;
	else if(pInterface == g_pSDKTools)
		return false;
	
	return IExtensionInterface::QueryInterfaceDrop(pInterface);
}

void Sample::NotifyInterfaceDrop(SMInterface *pInterface)
{
	if(strcmp(pInterface->GetInterfaceName(), SMINTERFACE_SDKHOOKS_NAME) == 0) {
		g_pSDKHooks->RemoveEntityListener(this);
		g_pSDKHooks = NULL;
	} else if(strcmp(pInterface->GetInterfaceName(), SMINTERFACE_SDKTOOLS_NAME) == 0) {
		g_pSDKTools = NULL;
	}
}

void Sample::SDK_OnUnload()
{
	if(pPvAllocEntPrivateData) {
		pPvAllocEntPrivateData->Destroy();
	}
	pPhysicsRunSpecificThink->Destroy();
	g_pSDKHooks->RemoveEntityListener(this);
	plsys->RemovePluginsListener(this);
	handlesys->RemoveType(factory_handle, myself->GetIdentity());
	handlesys->RemoveType(datamap_handle, myself->GetIdentity());
	handlesys->RemoveType(removal_handle, myself->GetIdentity());
	handlesys->RemoveType(serverclass_handle, myself->GetIdentity());
	gameconfs->CloseGameConfigFile(g_pGameConf);
}
