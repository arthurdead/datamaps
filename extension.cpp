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

#define SOURCEHOOK_BEING_STUPID

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

#include <string>
#include <vector>
#include <unordered_map>

#include "extension.h"
#include <ISDKTools.h>
#include <CDetour/detours.h>
#include <mathlib/vmatrix.h>
#include <toolframework/itoolentity.h>
#include <ehandle.h>
#include <eiface.h>
#include <dt_common.h>
#include <shareddefs.h>

#ifdef __HAS_PROXYSEND
class proxysend *proxysend = nullptr;
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
#include <variant_t.h>

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
ICvar *icvar = nullptr;
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

#define SetThink( a, b ) ThinkSet( static_cast <void (CBaseEntity::*)(void)> (a), (b), NULL )
#define SetContextThink( a, b, context ) ThinkSet( static_cast <void (CBaseEntity::*)(void)> (a), (b), context )

SH_DECL_MANUALHOOK0_void(GenericDtor, 1, 0, 0)

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

	m_pfnThink_t GetThinkFuncContext( const char *pszContext )
	{
		for ( int i = 0; i < GetAThinkFuncstions().Size(); i++ )
		{
			if ( !Q_strncmp( STRING( GetAThinkFuncstions()[i].m_iszContext ), pszContext, MAX_CONTEXT_LENGTH ) )
				return GetAThinkFuncstions()[i].m_pfnThink;
		}

		return nullptr;
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
	
	void PluginThinkContext();
	void PluginThink();
};

struct callback_holder_t
{
	struct callback_t
	{
		IChangeableForward *fwd = nullptr;
		CBaseEntity::m_pfnThink_t old_think = nullptr;
	};
	
	callback_t think{};
	
	std::unordered_map<std::string, callback_t> thinkctxs{};
	
	int ref = -1;
	std::vector<IdentityToken_t *> owners{};
	bool erase = true;
	
	callback_holder_t(CBaseEntity *pEntity, int ref_);
	~callback_holder_t();
	
	void dtor(CBaseEntity *pEntity);

	void HookEntityDtor();
};

using callback_holder_map_t = std::unordered_map<int, callback_holder_t *>;
callback_holder_map_t callbackmap{};

void CBaseEntity::PluginThinkContext()
{
	int this_ref = gamehelpers->EntityToReference(this);

	auto cb_it{callbackmap.find(this_ref)};
	if(cb_it == callbackmap.cend()) {
		return;
	}

	callback_holder_t *holder = cb_it->second;
	if(!holder) {
		return;
	}
	
	if(m_iCurrentThinkContext < 0 || m_iCurrentThinkContext >= holder->thinkctxs.size()) {
		return;
	}

	auto fnc_it = holder->thinkctxs.begin();
	std::advance(fnc_it, m_iCurrentThinkContext);
	
	IChangeableForward *fwd = fnc_it->second.fwd;
	if(!fwd || fwd->GetFunctionCount() == 0) {
		if(fnc_it->second.old_think) {
			(this->*fnc_it->second.old_think)();
		}
		return;
	}

	fwd->PushCell(gamehelpers->EntityToBCompatRef(this));
	fwd->PushStringEx((char *)fnc_it->first.c_str(), fnc_it->first.size()+1, SM_PARAM_STRING_COPY|SM_PARAM_STRING_UTF8, 0);
	cell_t res = 0;
	fwd->Execute(&res);

	if(res >= Pl_Handled) {
		return;
	}

	if(fnc_it->second.old_think) {
		(this->*fnc_it->second.old_think)();
	}
}

void CBaseEntity::PluginThink()
{
	int this_ref = gamehelpers->EntityToReference(this);

	auto it{callbackmap.find(this_ref)};
	if(it == callbackmap.cend()) {
		return;
	}

	callback_holder_t *holder = it->second;
	if(!holder) {
		return;
	}
	
	IChangeableForward *fwd = holder->think.fwd;
	if(!fwd || fwd->GetFunctionCount() == 0) {
		if(holder->think.old_think) {
			(this->*holder->think.old_think)();
		}
		return;
	}

	fwd->PushCell(gamehelpers->EntityToBCompatRef(this));
	cell_t res = 0;
	fwd->Execute(&res);

	if(res >= Pl_Handled) {
		return;
	}

	if(holder->think.old_think) {
		(this->*holder->think.old_think)();
	}
}

void callback_holder_t::HookEntityDtor()
{
	CBaseEntity *pEntity = META_IFACEPTR(CBaseEntity);
	int this_ref = gamehelpers->EntityToReference(pEntity);
	dtor(pEntity);
	callbackmap.erase(this_ref);
	erase = false;
	delete this;
	RETURN_META(MRES_HANDLED);
}

callback_holder_t::callback_holder_t(CBaseEntity *pEntity, int ref_)
	: ref{ref_}
{
	SH_ADD_MANUALHOOK(GenericDtor, pEntity, SH_MEMBER(this, &callback_holder_t::HookEntityDtor), false);

	think.old_think = pEntity->GetThinkFunc();

	callbackmap.emplace(ref, this);
}

callback_holder_t::~callback_holder_t()
{
	if(think.fwd) {
		forwards->ReleaseForward(think.fwd);
	}

	for(auto &it_ctx : thinkctxs) {
		if(it_ctx.second.fwd) {
			forwards->ReleaseForward(it_ctx.second.fwd);
		}
	}

	if(erase) {
		callbackmap.erase(ref);
	}
}

void callback_holder_t::dtor(CBaseEntity *pEntity)
{
	thinkctxs.clear();

	SH_REMOVE_MANUALHOOK(GenericDtor, pEntity, SH_MEMBER(this, &callback_holder_t::HookEntityDtor), false);
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
	while((pEntity = FindEntityByClassname(pEntity, name)) != nullptr) {
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

SH_DECL_HOOK1(IEntityFactoryDictionary, FindFactory, SH_NOATTRIB, 0, IEntityFactory *, const char *);

std::unordered_map<std::string, IEntityFactory *> factory_aliases;

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

	void init_hooks()
	{
		SH_ADD_HOOK(IEntityFactoryDictionary, FindFactory, this, SH_MEMBER(this, &CEntityFactoryDictionary::HookFindFactory), false);
	}

	IEntityFactory *HookFindFactory( const char *pClassName )
	{
		unsigned short nIndex = m_Factories.Find( pClassName );
		if ( nIndex == m_Factories.InvalidIndex() ) {
			auto it{factory_aliases.find(std::string{pClassName})};
			if(it != factory_aliases.cend()) {
				RETURN_META_VALUE(MRES_SUPERCEDE, it->second);
			}

			RETURN_META_VALUE(MRES_SUPERCEDE, NULL);
		}
		RETURN_META_VALUE(MRES_SUPERCEDE, m_Factories[nIndex]);
	}
};

CEntityFactoryDictionary *dictionary = nullptr;

using custom_prop_t = std::pair<std::string, fieldtype_t>;
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

enum custom_prop_type
{
	custom_prop_int,
	custom_prop_float,
	custom_prop_bool,
	custom_prop_ehandle,
	custom_prop_vector,
	custom_prop_string,
	custom_prop_color32,
	custom_prop_time,
	custom_prop_tick,
	custom_prop_short,
	custom_prop_char,
	custom_prop_modelname,
	custom_prop_modelindex,
	custom_prop_soundname,
	custom_prop_variant,
};

class hookobj_t;

std::unordered_map<int, std::vector<hookobj_t *>> hookobjs{};

class hookobj_t
{
public:
	std::vector<int> entities{};
	bool erase_obj{true};
	bool erase_ent{true};

	virtual ~hookobj_t()
	{
		erase_ent = false;

		for(int ref : entities) {
			CBaseEntity *pEntity{gamehelpers->ReferenceToEntity(ref)};
			if(!pEntity) {
				continue;
			}

			remove_hooks(pEntity);
		}
	}

	void add_hooks(CBaseEntity *pEntity)
	{
		int ref = gamehelpers->EntityToReference(pEntity);

		auto it_objs{hookobjs.find(ref)};
		if(it_objs == hookobjs.cend()) {
			it_objs = hookobjs.emplace(ref, std::vector<hookobj_t *>{}).first;
		}
		it_objs->second.emplace_back(this);

		entities.emplace_back(ref);
	}

	virtual void remove_hooks(CBaseEntity *pEntity)
	{
		int ref = gamehelpers->EntityToReference(pEntity);

		if(erase_obj) {
			auto it_objs{hookobjs.find(ref)};
			if(it_objs != hookobjs.cend()) {
				std::vector<hookobj_t *> &vec{it_objs->second};
				auto it_obj{std::find(vec.begin(), vec.end(), this)};
				if(it_obj != vec.end()) {
					vec.erase(it_obj);
				}
				if(vec.empty()) {
					hookobjs.erase(it_objs);
				}
			}
		}

		if(erase_ent) {
			auto it_ent{std::find(entities.begin(), entities.end(), ref)};
			if(it_ent != entities.end()) {
				entities.erase(it_ent);
			}
		}
	}
};

struct custom_prop_info_t : public hookobj_t
{
	bool was_overriden = false;
	IEntityFactory *fac = nullptr;
	bool fac_is_sp = false;
	custom_datamap_t map{};
	using dataDesc_t = std::vector<custom_typedescription_t>;
	dataDesc_t dataDesc{};
	int size = 0;
	std::string clsname{};
	Handle_t hndl = BAD_HANDLE;
	IPluginContext *pContext = nullptr;
	bool erase = true;
	bool freehndl = true;
	size_t counterid = 0;
	std::string mapname{};
	
	custom_prop_info_t(IEntityFactory *fac_, std::string &&clsname_);
	~custom_prop_info_t() override;
	
	void dtor(CBaseEntity *pEntity)
	{
		for(custom_typedescription_t &desc : dataDesc) {
			int offset = desc.get_offset();
			unsigned char *ptr = (((unsigned char *)pEntity) + offset);
			switch(desc.fieldType) {
				case FIELD_EHANDLE: {
					for(int i = 0; i < desc.fieldSize; ++i) {
						((EHANDLE *)(ptr + (i * sizeof(EHANDLE))))->~EHANDLE();
					}
					break;
				}
				case FIELD_POSITION_VECTOR:
				case FIELD_VECTOR: {
					for(int i = 0; i < desc.fieldSize; ++i) {
						((Vector *)(ptr + (i * sizeof(Vector))))->~Vector();
					}
					break;
				}
				case FIELD_VECTOR2D: {
					for(int i = 0; i < desc.fieldSize; ++i) {
						((Vector2D *)(ptr + (i * sizeof(Vector2D))))->~Vector2D();
					}
					break;
				}
				case FIELD_QUATERNION: {
					for(int i = 0; i < desc.fieldSize; ++i) {
						((Quaternion *)(ptr + (i * sizeof(Quaternion))))->~Quaternion();
					}
					break;
				}
				case FIELD_VMATRIX_WORLDSPACE:
				case FIELD_VMATRIX: {
					for(int i = 0; i < desc.fieldSize; ++i) {
						((VMatrix *)(ptr + (i * sizeof(VMatrix))))->~VMatrix();
					}
					break;
				}
				case FIELD_MATRIX3X4_WORLDSPACE: {
					for(int i = 0; i < desc.fieldSize; ++i) {
						((matrix3x4_t *)(ptr + (i * sizeof(matrix3x4_t))))->~matrix3x4_t();
					}
					break;
				}
#if SOURCE_ENGINE == SE_LEFT4DEAD2
				case FIELD_VECTOR4D: {
					for(int i = 0; i < desc.fieldSize; ++i) {
						((Vector4D *)(ptr + (i * sizeof(Vector4D))))->~Vector4D();
					}
					break;
				}
#endif
				default: {
					if(desc.fieldType == FIELD_CUSTOM && (desc.flags & FTYPEDESC_OUTPUT)) {
						for(int i = 0; i < desc.fieldSize; ++i) {
							((variant_t *)(ptr + (i * sizeof(variant_t))))->~variant_t();
						}
					}
					break;
				}
			}
		}
	}
	
	void zero(CBaseEntity *pEntity)
	{
		for(custom_typedescription_t &desc : dataDesc) {
			int offset = desc.get_offset();
			unsigned char *ptr = (((unsigned char *)pEntity) + offset);
			switch(desc.fieldType) {
				case FIELD_COLOR32: {
					for(int i = 0; i < desc.fieldSize; ++i) {
						color32 &color = *new (ptr + (i * sizeof(color32))) color32();
						color.r = 255;
						color.g = 255;
						color.b = 255;
						color.a = 255;
					}
					break;
				}
				case FIELD_EHANDLE: {
					for(int i = 0; i < desc.fieldSize; ++i) {
						new (ptr + (i * sizeof(EHANDLE))) EHANDLE();
					}
					break;
				}
				case FIELD_POSITION_VECTOR:
				case FIELD_VECTOR: {
					for(int i = 0; i < desc.fieldSize; ++i) {
						new (ptr + (i * sizeof(Vector))) Vector();
					}
					break;
				}
				case FIELD_VECTOR2D: {
					for(int i = 0; i < desc.fieldSize; ++i) {
						new (ptr + (i * sizeof(Vector2D))) Vector2D();
					}
					break;
				}
				case FIELD_QUATERNION: {
					for(int i = 0; i < desc.fieldSize; ++i) {
						new (ptr + (i * sizeof(Quaternion))) Quaternion();
					}
					break;
				}
				case FIELD_VMATRIX_WORLDSPACE:
				case FIELD_VMATRIX: {
					for(int i = 0; i < desc.fieldSize; ++i) {
						new (ptr + (i * sizeof(VMatrix))) VMatrix();
					}
					break;
				}
				case FIELD_MATRIX3X4_WORLDSPACE: {
					for(int i = 0; i < desc.fieldSize; ++i) {
						new (ptr + (i * sizeof(matrix3x4_t))) matrix3x4_t();
					}
					break;
				}
#if SOURCE_ENGINE == SE_LEFT4DEAD2
				case FIELD_VECTOR4D: {
					for(int i = 0; i < desc.fieldSize; ++i) {
						new (ptr + (i * sizeof(Vector4D))) Vector4D();
					}
					break;
				}
#endif
				default: {
					if(desc.fieldType == FIELD_CUSTOM && (desc.flags & FTYPEDESC_OUTPUT)) {
						for(int i = 0; i < desc.fieldSize; ++i) {
							new (ptr + (i * sizeof(variant_t))) variant_t();
						}
					}
					break;
				}
			}
		}
	}
	
	void update_offsets(int &base)
	{
		for(custom_typedescription_t &desc : dataDesc) {
			desc.get_offset() += base;
		}
		base += size;
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
	
	void add_prop(const std::string &name, fieldtype_t type, int num = 1, int flags = 0)
	{
		dataDesc.emplace_back();
		custom_typedescription_t &desc = dataDesc.back();
		
		desc.set_name(name);
		
		desc.get_offset() = size;
		desc.fieldSize = num;
		
		desc.fieldType = type;
		desc.flags = FTYPEDESC_PRIVATE|FTYPEDESC_VIEW_NEVER|flags;
		if(desc.fieldType == FIELD_MODELINDEX) {
			desc.flags |= FTYPEDESC_MODELINDEX;
		}
		
		switch(desc.fieldType) {
			case FIELD_FLOAT: { desc.fieldSizeInBytes = (FIELD_SIZE(FIELD_FLOAT) * desc.fieldSize); break; }
			case FIELD_STRING: { desc.fieldSizeInBytes = (FIELD_SIZE(FIELD_STRING) * desc.fieldSize); break; }
			case FIELD_VECTOR: { desc.fieldSizeInBytes = (FIELD_SIZE(FIELD_VECTOR) * desc.fieldSize); break; }
			case FIELD_VECTOR2D: { desc.fieldSizeInBytes = (FIELD_SIZE(FIELD_VECTOR2D) * desc.fieldSize); break; }
			case FIELD_QUATERNION: { desc.fieldSizeInBytes = (FIELD_SIZE(FIELD_QUATERNION) * desc.fieldSize); break; }
			case FIELD_INTEGER: { desc.fieldSizeInBytes = (FIELD_SIZE(FIELD_INTEGER) * desc.fieldSize); break; }
			case FIELD_BOOLEAN: { desc.fieldSizeInBytes = (FIELD_SIZE(FIELD_BOOLEAN) * desc.fieldSize); break; }
			case FIELD_SHORT: { desc.fieldSizeInBytes = (FIELD_SIZE(FIELD_SHORT) * desc.fieldSize); break; }
			case FIELD_CHARACTER: { desc.fieldSizeInBytes = (FIELD_SIZE(FIELD_CHARACTER) * desc.fieldSize); break; }
			case FIELD_COLOR32: { desc.fieldSizeInBytes = (FIELD_SIZE(FIELD_COLOR32) * desc.fieldSize); break; }
			case FIELD_CLASSPTR: { desc.fieldSizeInBytes = (FIELD_SIZE(FIELD_CLASSPTR) * desc.fieldSize); break; }
			case FIELD_EHANDLE: { desc.fieldSizeInBytes = (FIELD_SIZE(FIELD_EHANDLE) * desc.fieldSize); break; }
			case FIELD_EDICT: { desc.fieldSizeInBytes = (FIELD_SIZE(FIELD_EDICT) * desc.fieldSize); break; }
			case FIELD_POSITION_VECTOR: { desc.fieldSizeInBytes = (FIELD_SIZE(FIELD_POSITION_VECTOR) * desc.fieldSize); break; }
			case FIELD_TIME: { desc.fieldSizeInBytes = (FIELD_SIZE(FIELD_TIME) * desc.fieldSize); break; }
			case FIELD_TICK: { desc.fieldSizeInBytes = (FIELD_SIZE(FIELD_TICK) * desc.fieldSize); break; }
			case FIELD_MODELNAME: { desc.fieldSizeInBytes = (FIELD_SIZE(FIELD_MODELNAME) * desc.fieldSize); break; }
			case FIELD_SOUNDNAME: { desc.fieldSizeInBytes = (FIELD_SIZE(FIELD_SOUNDNAME) * desc.fieldSize); break; }
			case FIELD_INPUT: { desc.fieldSizeInBytes = (FIELD_SIZE(FIELD_INPUT) * desc.fieldSize); break; }
			case FIELD_FUNCTION: { desc.fieldSizeInBytes = (FIELD_SIZE(FIELD_FUNCTION) * desc.fieldSize); break; }
			case FIELD_VMATRIX: { desc.fieldSizeInBytes = (FIELD_SIZE(FIELD_VMATRIX) * desc.fieldSize); break; }
			case FIELD_VMATRIX_WORLDSPACE: { desc.fieldSizeInBytes = (FIELD_SIZE(FIELD_VMATRIX_WORLDSPACE) * desc.fieldSize); break; }
			case FIELD_MATRIX3X4_WORLDSPACE: { desc.fieldSizeInBytes = (FIELD_SIZE(FIELD_MATRIX3X4_WORLDSPACE) * desc.fieldSize); break; }
			case FIELD_INTERVAL: { desc.fieldSizeInBytes = (FIELD_SIZE(FIELD_INTERVAL) * desc.fieldSize); break; }
			case FIELD_MODELINDEX: { desc.fieldSizeInBytes = (FIELD_SIZE(FIELD_MODELINDEX) * desc.fieldSize); break; }
			case FIELD_MATERIALINDEX: { desc.fieldSizeInBytes = (FIELD_SIZE(FIELD_MATERIALINDEX) * desc.fieldSize); break; }
#if SOURCE_ENGINE == SE_LEFT4DEAD2
			case FIELD_VECTOR4D: { desc.fieldSizeInBytes = (FIELD_SIZE(FIELD_VECTOR4D) * desc.fieldSize); break; }
			case FIELD_INTEGER64: { desc.fieldSizeInBytes = (FIELD_SIZE(FIELD_INTEGER64) * desc.fieldSize); break; }
#endif
			default: {
				if(desc.fieldType == FIELD_CUSTOM && (desc.flags & FTYPEDESC_OUTPUT)) {
					desc.fieldSizeInBytes = (sizeof(variant_t) * desc.fieldSize);
				}
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
		dtor(pEntity);
		remove_hooks(pEntity);
		RETURN_META(MRES_HANDLED);
	}

	void remove_hooks(CBaseEntity *pEntity) override
	{
		hookobj_t::remove_hooks(pEntity);

		SH_REMOVE_HOOK(CBaseEntity, GetDataDescMap, pEntity, SH_MEMBER(this, &custom_prop_info_t::HookGetDataDescMap), false);
		SH_REMOVE_MANUALHOOK(GenericDtor, pEntity, SH_MEMBER(this, &custom_prop_info_t::HookEntityDtor), false);
	}

	void add_hooks(CBaseEntity *pEntity)
	{
		hookobj_t::add_hooks(pEntity);

		SH_ADD_HOOK(CBaseEntity, GetDataDescMap, pEntity, SH_MEMBER(this, &custom_prop_info_t::HookGetDataDescMap), false);
		SH_ADD_MANUALHOOK(GenericDtor, pEntity, SH_MEMBER(this, &custom_prop_info_t::HookEntityDtor), false);
	}
	
	void do_override(int &base, CBaseEntity *pEntity);
	
	IServerNetworkable *HookCreate(const char *classname);
};

SH_DECL_HOOK0(CBaseEntity, GetServerClass, SH_NOATTRIB, 0, ServerClass *);
SH_DECL_HOOK0(IServerNetworkable, GetServerClass, SH_NOATTRIB, 0, ServerClass *);

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
	
	void remove()
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
	
	~unexclude_prop_t()
	{
		
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

extern float AssignRangeMultiplier( int nBits, double range );

static const CStandardSendProxies *std_proxies{nullptr};

#define SPROP_HACK_ABSOFFSET (1 << SPROP_NUMFLAGBITS)

struct serverclass_override_t : public hookobj_t
{
	serverclass_override_t(IEntityFactory *fac_, std::string &&clsname_, ServerClass *realcls_);
	~serverclass_override_t() override;
	
	IServerNetworkable *HookCreate(const char *classname);
	
	ServerClass *HookGetServerClass()
	{
		RETURN_META_VALUE(MRES_SUPERCEDE, (ServerClass *)&cls);
	}
	
	void HookEntityDtor()
	{
		CBaseEntity *pEntity = META_IFACEPTR(CBaseEntity);
		remove_hooks(pEntity);
		RETURN_META(MRES_HANDLED);
	}

	void remove_hooks(CBaseEntity *pEntity) override
	{
		hookobj_t::remove_hooks(pEntity);

		IServerNetworkable *pNet = pEntity->GetNetworkable();
		SH_REMOVE_HOOK(CBaseEntity, GetServerClass, pEntity, SH_MEMBER(this, &serverclass_override_t::HookGetServerClass), false);
		SH_REMOVE_HOOK(IServerNetworkable, GetServerClass, pNet, SH_MEMBER(this, &serverclass_override_t::HookGetServerClass), false);
		SH_REMOVE_MANUALHOOK(GenericDtor, pEntity, SH_MEMBER(this, &serverclass_override_t::HookEntityDtor), false);
	}

	void add_hooks(CBaseEntity *pEntity, IServerNetworkable *pNet)
	{
		hookobj_t::add_hooks(pEntity);

		SH_ADD_HOOK(CBaseEntity, GetServerClass, pEntity, SH_MEMBER(this, &serverclass_override_t::HookGetServerClass), false);
		SH_ADD_HOOK(IServerNetworkable, GetServerClass, pNet, SH_MEMBER(this, &serverclass_override_t::HookGetServerClass), false);
		SH_ADD_MANUALHOOK(GenericDtor, pEntity, SH_MEMBER(this, &serverclass_override_t::HookEntityDtor), false);
	}
	
	void do_override(int &base, CBaseEntity *pEntity, IServerNetworkable *pNet);
	
	void remove_base_line();
	
	void override_with(ServerClass *netclass);
	void set_base_class(SendTable *table);
	void unexclude_prop(SendProp *prop, SendProp *realprop);
	
	void init();

	SendProp *emplace_prop()
	{
		SendProp *prop = &props.emplace_back();
		update_dt();
		return prop;
	}

	void update_dt()
	{
		tbl.m_pProps = props.data();
		tbl.m_nProps = props.size();
	}

	void update_offsets(int &base)
	{
		for(std::size_t i{1}; i < props.size(); ++i) {
			SendProp &prop{props[i]};
			int flags = prop.GetFlags();
			if(flags & SPROP_HACK_ABSOFFSET) {
				flags &= ~SPROP_HACK_ABSOFFSET;
				//prop.SetFlags(flags);
			} else {
				prop.SetOffset(base + prop.GetOffset());
			}
		}
		base += size;
	}

	std::vector<std::unique_ptr<std::string>> prop_names;

	void add_prop_float(std::string &&name, float fLowValue, float fHighValue, int nBits, int flags)
	{
		SendProp &prop{*emplace_prop()};

		prop.SetOffset(size);

		prop.m_pVarName = prop_names.emplace_back(new std::string{std::move(name)}).get()->c_str();

		prop.SetFlags(flags);

		prop.SetProxyFn(std_proxies->m_FloatToFloat);

		if(nBits <= 0 || nBits == 32) {
			flags |= SPROP_NOSCALE;
			fLowValue = 0.0f;
			fHighValue = 0.0f;
		} else {
			if(fHighValue == HIGH_DEFAULT) {
				fHighValue = (1 << nBits);
			}

			if(flags & SPROP_ROUNDDOWN) {
				fHighValue = fHighValue - ((fHighValue - fLowValue) / (1 << nBits));
			} else if(flags & SPROP_ROUNDUP) {
				fLowValue = fLowValue + ((fHighValue - fLowValue) / (1 << nBits));
			}
		}

		if(prop.GetFlags() & (SPROP_COORD|SPROP_NOSCALE|SPROP_NORMAL|SPROP_COORD_MP|SPROP_COORD_MP_LOWPRECISION|SPROP_COORD_MP_INTEGRAL)) {
			prop.m_nBits = 0;
		} else {
			prop.m_nBits = nBits;
		}

		prop.m_fLowValue = fLowValue;
		prop.m_fHighValue = fHighValue;
		prop.m_fHighLowMul = AssignRangeMultiplier(prop.m_nBits, prop.m_fHighValue - prop.m_fLowValue);
		prop.m_Type = DPT_Float;

		size += sizeof(float);
	}

	void add_prop_int(std::string &&name, int sizeofVar, int nBits, int flags, int offset)
	{
		SendProp &prop{*emplace_prop()};

		if(offset == -1) {
			prop.SetOffset(size);
		} else {
			prop.SetOffset(offset);
		}

		prop.m_pVarName = prop_names.emplace_back(new std::string{std::move(name)}).get()->c_str();

		if(offset == -1) {
			prop.SetFlags(flags);
		} else {
			prop.SetFlags(flags|SPROP_HACK_ABSOFFSET);
		}

		switch(sizeofVar) {
			case 1: {
				prop.SetProxyFn(flags & SPROP_UNSIGNED ? std_proxies->m_UInt8ToInt32 : std_proxies->m_Int8ToInt32);
			}
			case 2: {
				prop.SetProxyFn(flags & SPROP_UNSIGNED ? std_proxies->m_UInt16ToInt32 : std_proxies->m_Int16ToInt32);
			}
			case 4: {
				prop.SetProxyFn(flags & SPROP_UNSIGNED ? std_proxies->m_UInt32ToInt32 : std_proxies->m_Int32ToInt32);
			}
		}

		if(nBits <= 0) {
			nBits = sizeofVar * 8;
		}

		prop.m_nBits = nBits;

		prop.m_Type = DPT_Int;

		if(offset == -1) {
			size += sizeofVar;
		}
	}

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
	bool cls_name_set = false;
	bool tbl_name_set = false;
	SendProp *m_pProps = nullptr;
	int size = 0;
	std::vector<unexclude_prop_t> exclude_props{};
	size_t counterid = 0;
	int classid = 0;
};

class sp_entity_factory : public IEntityFactory
{
	sp_entity_factory(std::string &&name_);
public:
	sp_entity_factory(std::string &&name_, IPluginFunction *func_, size_t size_, cell_t data_)
	: sp_entity_factory(std::move(name_))
	{
		func = func_;
		size = size_;
		data = data_;
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
	size_t size = 0;
	cell_t data = 0;

	std::vector<std::string> aliases{};
	
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

	for(const auto &it : aliases) {
		factory_aliases.erase(it);
	}
	
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
#elif SOURCE_ENGINE == SE_LEFT4DEAD2
	char pad1[244];
#endif
	
	int			serverclasses;		// number of unique server classes
	int			serverclassbits;	// log2 of serverclasses
	
	void increment_svclasses()
	{
		++serverclasses;
		serverclassbits = Q_log2( serverclasses ) + 1;
	}
	
	void decrement_svclasses()
	{
		--serverclasses;
		if(serverclasses == 0) {
			serverclassbits = 0;
			return;
		}
		serverclassbits = Q_log2( serverclasses ) + 1;
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

void serverclass_override_t::remove_base_line()
{
	if(cls.m_InstanceBaselineIndex == INVALID_STRING_INDEX) {
		return;
	}
	
	bool lock = engine->LockNetworkStringTables(true);
	//m_pInstanceBaselineTable->RemoveString;
	engine->LockNetworkStringTables(lock);
	
	cls.m_InstanceBaselineIndex = INVALID_STRING_INDEX;
}

void serverclass_override_t::override_with(ServerClass *netclass)
{
	fakecls = netclass;
	
	cls.m_ClassID = fakecls->m_ClassID;
	classid = fakecls->m_ClassID;

	cls.m_InstanceBaselineIndex = fakecls->m_InstanceBaselineIndex;

	tbl.m_pPrecalc = fakecls->m_pTable->m_pPrecalc;
}

size_t classoverridecounter = 0;

void serverclass_override_t::init()
{
	props[0].SetDataTable(realcls->m_pTable);

	if(!tbl_name_set) {
		std::string tablename{realcls->m_pTable->m_pNetTableName};
		tbl.set_name(tablename);
	}

	if(!cls_name_set) {
		std::string netname{realcls->m_pNetworkName};
		netname += "_custom_";
		netname += std::to_string(counterid);
		cls.set_name(netname);
	}

	if(!fakecls) {
		cls.m_ClassID = realcls->m_ClassID;
		classid = realcls->m_ClassID;

		cls.m_InstanceBaselineIndex = realcls->m_InstanceBaselineIndex;

		tbl.m_pPrecalc = realcls->m_pTable->m_pPrecalc;
	}
}

static ServerClass *CBaseEntity_ServerClass = nullptr;

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
	
	counterid = classoverridecounter++;
	
	SendProp *prop = emplace_prop();
	prop->m_Type = DPT_DataTable;
	prop->m_pVarName = "baseclass";
	prop->SetOffset(0);
	prop->SetDataTableProxyFn(SendProxy_DataTableToDataTable);
	prop->SetFlags(SPROP_PROXY_ALWAYS_YES|SPROP_COLLAPSIBLE);
	
	cls.m_pTable = &tbl;
	
	cls.m_pNext = custom_server_head;
	custom_server_head = (ServerClass *)&cls;

	g_pServerClassTail->m_pNext = custom_server_head;

	((CBaseServer *)server)->increment_svclasses();
	
	if(realcls) {
		init();
	} else {
		SendTable *CBaseEntity_SendTable = CBaseEntity_ServerClass->m_pTable;

		props[0].SetDataTable(CBaseEntity_SendTable);
		tbl.m_pPrecalc = CBaseEntity_SendTable->m_pPrecalc;
		
		tbl.set_name("DT_BaseEntity");
		cls.set_name("CBaseEntity");

		cls.m_ClassID = CBaseEntity_ServerClass->m_ClassID;
		classid = CBaseEntity_ServerClass->m_ClassID;

		cls.m_InstanceBaselineIndex = CBaseEntity_ServerClass->m_InstanceBaselineIndex;
	}
}

extern void remove_serverclass_from_sm_cache(ServerClass *pMap);

serverclass_override_t::~serverclass_override_t()
{
	--classoverridecounter;
	
	if(erase) {
		server_map.erase(clsname);
	}
	
	for(ServerClass *cur = custom_server_head, *prev = nullptr; cur != nullptr; prev = cur, cur = cur->m_pNext) {
		if (cur == (ServerClass *)&cls) {
			if(prev != nullptr) {
				prev->m_pNext = cur->m_pNext;
			} else {
				custom_server_head = cur->m_pNext;
			}
			cur->m_pNext = nullptr;
			break;
		}
	}

	g_pServerClassTail->m_pNext = custom_server_head;
	
	for(auto &it : exclude_props) {
		it.remove();
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

size_t datamapoverridecounter = 0;

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
	
	counterid = datamapoverridecounter++;
}

extern void remove_datamap_from_sm_cache(datamap_t *pMap);

custom_prop_info_t::~custom_prop_info_t()
{
	--datamapoverridecounter;
	
	if(erase) {
		info_map.erase(clsname);
	}
	
	remove_datamap_from_sm_cache(&map);
	
	if(!fac_is_sp) {
		SH_REMOVE_HOOK(IEntityFactory, Create, fac, SH_MEMBER(this, &custom_prop_info_t::HookCreate), false);
	}
	
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

void serverclass_override_t::do_override(int &base, CBaseEntity *pEntity, IServerNetworkable *pNet)
{
	if(!realcls) {
		realcls = pEntity->GetServerClass();
		init();
	}

	if(!was_overriden) {
		update_offsets(base);
		was_overriden = true;
	}

	add_hooks(pEntity, pNet);
}

void custom_prop_info_t::do_override(int &base, CBaseEntity *pEntity)
{
	if(!was_overriden) {
		datamap_t *basemap = gamehelpers->GetDataMap(pEntity);
		ServerClass *svcls = pEntity->GetServerClass();
		
		if(mapname.empty()) {
			mapname = svcls->GetName();
			mapname += "_custom_";
			mapname += std::to_string(counterid);
		}
		map.set_name(mapname);
		map.baseMap = basemap;
		
		update_offsets(base);
		
		was_overriden = true;
	}
	
	//zero(pEntity);
	
	add_hooks(pEntity);
}

IServerNetworkable *sp_entity_factory::Create(const char *pClassName)
{
	IServerNetworkable *net = nullptr;
	
	if(based != nullptr) {
		curr_data_info = custom_prop;
		curr_server_info = custom_server;
		net = based->Create(pClassName);
		curr_data_info = nullptr;
		curr_server_info = nullptr;
		CBaseEntity *pEntity = net->GetBaseEntity();
		last_cb = based->GetEntitySize();
		if(custom_prop) {
			custom_prop->do_override(last_cb, pEntity);
		}
		if(custom_server) {
			custom_server->do_override(last_cb, pEntity, net);
		}
	} else if(func != nullptr) {
		cell_t res = 0;
		int add_size{0};
		if(custom_prop) {
			add_size += custom_prop->size;
		}
		if(custom_server) {
			add_size += custom_server->size;
		}
		func->PushCell(add_size);
		func->PushCell(data);
		func->Execute(&res);
		last_cb = size;
		CBaseEntity *obj = (CBaseEntity *)res;
		if(obj != nullptr) {
			obj->PostConstructor(pClassName);
			net = obj->GetNetworkable();
			if(custom_prop) {
				custom_prop->do_override(last_cb, obj);
			}
			if(custom_server) {
				custom_server->do_override(last_cb, obj, net);
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
	
	int base = fac->GetEntitySize();
	do_override(base, pEntity);
	
	RETURN_META_VALUE(MRES_SUPERCEDE, net);
}

IServerNetworkable *serverclass_override_t::HookCreate(const char *classname)
{
	IEntityFactory *fac = META_IFACEPTR(IEntityFactory);
	
	curr_server_info = this;
	IServerNetworkable *net = SH_CALL(fac, &IEntityFactory::Create)(classname);
	curr_server_info = nullptr;
	
	CBaseEntity *pEntity = net->GetBaseEntity();
	
	int base = fac->GetEntitySize();
	do_override(base, pEntity, net);
	
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
	
	sp_entity_factory *obj = new sp_entity_factory(name, callback, params[3], params[4]);
	
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
	
	fieldtype_t type = FIELD_VOID;
	
	int flags = 0;
	
	switch((custom_prop_type)params[3]) {
		case custom_prop_int: { type = FIELD_INTEGER; break; }
		case custom_prop_float: { type = FIELD_FLOAT; break; }
		case custom_prop_bool: { type = FIELD_BOOLEAN; break; }
		case custom_prop_ehandle: { type = FIELD_EHANDLE; break; }
		case custom_prop_vector: { type = FIELD_VECTOR; break; }
		case custom_prop_string: { type = FIELD_STRING; break; }
		case custom_prop_color32: { type = FIELD_COLOR32; break; }
		case custom_prop_time: { type = FIELD_TIME; break; }
		case custom_prop_tick: { type = FIELD_TICK; break; }
		case custom_prop_short: { type = FIELD_SHORT; break; }
		case custom_prop_char: { type = FIELD_CHARACTER; break; }
		case custom_prop_modelname: { type = FIELD_MODELNAME; break; }
		case custom_prop_modelindex: { type = FIELD_MODELINDEX; flags |= FTYPEDESC_MODELINDEX; break; }
		case custom_prop_soundname: { type = FIELD_SOUNDNAME; break; }
		case custom_prop_variant: { type = FIELD_CUSTOM; flags |= FTYPEDESC_OUTPUT; break; }
		default: {
			return pContext->ThrowNativeError("invalid type %i", params[3]);
		}
	}
	
	obj->add_prop(name, type, params[4], flags);
	return 0;
}

cell_t CustomSendtableadd_prop_float(IPluginContext *pContext, const cell_t *params)
{
	HandleSecurity security(pContext->GetIdentity(), myself->GetIdentity());
	
	serverclass_override_t *obj = nullptr;
	HandleError err = handlesys->ReadHandle(params[1], serverclass_handle, &security, (void **)&obj);
	if(err != HandleError_None)
	{
		return pContext->ThrowNativeError("Invalid Handle %x (error: %d)", params[1], err);
	}
	
	char *name_ptr = nullptr;
	pContext->LocalToString(params[2], &name_ptr);
	
	std::string name{name_ptr};
	obj->add_prop_float(std::move(name), sp_ctof(params[3]), sp_ctof(params[4]), params[5], params[6]);
	return 0;
}

cell_t CustomSendtableadd_prop_int(IPluginContext *pContext, const cell_t *params)
{
	HandleSecurity security(pContext->GetIdentity(), myself->GetIdentity());
	
	serverclass_override_t *obj = nullptr;
	HandleError err = handlesys->ReadHandle(params[1], serverclass_handle, &security, (void **)&obj);
	if(err != HandleError_None)
	{
		return pContext->ThrowNativeError("Invalid Handle %x (error: %d)", params[1], err);
	}
	
	char *name_ptr = nullptr;
	pContext->LocalToString(params[2], &name_ptr);
	
	std::string name{name_ptr};
	obj->add_prop_int(std::move(name), params[3], params[4], params[5], params[6]);
	return 0;
}

cell_t CustomDatamapset_name(IPluginContext *pContext, const cell_t *params)
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
	
	std::string namestr{name};
	
	obj->map.set_name(namestr);
	obj->mapname = std::move(namestr);
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
	
	if(!factory->realcls) {
		return pContext->ThrowNativeError("this table wasnt initialized yet");
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
		return pContext->ThrowNativeError("%s is not a exclude prop", name);
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
	
	if(!factory->realcls) {
		return pContext->ThrowNativeError("this table wasnt initialized yet");
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

cell_t CustomSendtableset_name(IPluginContext *pContext, const cell_t *params)
{
	HandleSecurity security(pContext->GetIdentity(), myself->GetIdentity());
	
	serverclass_override_t *factory = nullptr;
	HandleError err = handlesys->ReadHandle(params[1], serverclass_handle, &security, (void **)&factory);
	if(err != HandleError_None)
	{
		return pContext->ThrowNativeError("Invalid Handle %x (error: %d)", params[1], err);
	}
	
	char *name = nullptr;
	pContext->LocalToString(params[2], &name);

	factory->tbl.set_name(name);
	factory->tbl_name_set = true;

	return 0;
}

cell_t CustomSendtableset_network_name(IPluginContext *pContext, const cell_t *params)
{
	HandleSecurity security(pContext->GetIdentity(), myself->GetIdentity());
	
	serverclass_override_t *factory = nullptr;
	HandleError err = handlesys->ReadHandle(params[1], serverclass_handle, &security, (void **)&factory);
	if(err != HandleError_None)
	{
		return pContext->ThrowNativeError("Invalid Handle %x (error: %d)", params[1], err);
	}
	
	char *name = nullptr;
	pContext->LocalToString(params[2], &name);

	factory->cls.set_name(name);
	factory->cls_name_set = true;

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
	
	char *netname = nullptr;
	pContext->LocalToString(params[2], &netname);
	
	ServerClass *netclass = FindServerClass(netname);
	if(!netclass) {
		return pContext->ThrowNativeError("invalid netname %s", netname);
	}
	
	factory->override_with(netclass);
	return 0;
}

static ServerClass *CTFPlayer_ServerClass{nullptr};

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

	ServerClass *netclass = nullptr;
	
	if(strcmp(classname, "player") == 0) {
		netclass = CTFPlayer_ServerClass;
	} else {
		IServerNetworkable *net = factory->Create(classname);

		netclass = net->GetServerClass();

		RemoveEntity(net->GetBaseEntity());
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
	
	IServerNetworkable *net = factory->Create(factory->name.c_str());

	ServerClass *netclass = net->GetServerClass();

	RemoveEntity(net->GetBaseEntity());
	
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
	HandleError err = handlesys->ReadHandle(params[1], factory_handle, &security, (void **)&factory);
	if(err != HandleError_None)
	{
		return pContext->ThrowNativeError("Invalid Handle %x (error: %d)", params[1], err);
	}
	
	return (cell_t)(IEntityFactory *)factory;
}

cell_t CustomEntityFactoryadd_alias(IPluginContext *pContext, const cell_t *params)
{
	HandleSecurity security(pContext->GetIdentity(), myself->GetIdentity());
	
	sp_entity_factory *factory = nullptr;
	HandleError err = handlesys->ReadHandle(params[1], factory_handle, &security, (void **)&factory);
	if(err != HandleError_None)
	{
		return pContext->ThrowNativeError("Invalid Handle %x (error: %d)", params[1], err);
	}

	char *classname_ptr{nullptr};
	pContext->LocalToString(params[2], &classname_ptr);
	std::string classname{classname_ptr};

	auto it{factory_aliases.find(classname)};
	if(it != factory_aliases.cend()) {
		return pContext->ThrowNativeError("%s is already registered", classname_ptr);
	}

	factory->aliases.emplace_back(classname);
	factory_aliases.emplace(std::move(classname), (IEntityFactory *)factory);
	return 0;
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

	int ref = gamehelpers->EntityToReference(pEntity);

	char *context_ptr = nullptr;
	pContext->LocalToString(params[3], &context_ptr);
	std::string context{context_ptr};

	callback_holder_t::callback_t *callback{nullptr};
	
	callback_holder_map_t::iterator it{callbackmap.find(ref)};
	if(it != callbackmap.end()) {
		holder = it->second;

		auto it_cb{holder->thinkctxs.find(context)};
		if(it_cb != holder->thinkctxs.cend()) {
			callback = &it_cb->second;
		}
	} else {
		holder = new callback_holder_t{pEntity, ref};
	}

	if(!callback) {
		callback_holder_t::callback_t tmp_cb{};
		tmp_cb.old_think = pEntity->GetThinkFuncContext(context_ptr);
		tmp_cb.fwd = forwards->CreateForwardEx(nullptr, ET_Hook, 2, nullptr, Param_Cell, Param_String);
		callback = &holder->thinkctxs.emplace(std::move(context), std::move(tmp_cb)).first->second;

		pEntity->SetContextThink(&CBaseEntity::PluginThinkContext, 0.0f, context_ptr);
	}

	IPluginFunction *func = pContext->GetFunctionById(params[2]);
	
	callback->fwd->RemoveFunction(func);
	callback->fwd->AddFunction(func);

	IdentityToken_t *iden{pContext->GetIdentity()};
	if(std::find(holder->owners.cbegin(), holder->owners.cend(), iden) == holder->owners.cend()) {
		holder->owners.emplace_back(iden);
	}

	return 0;
}

static cell_t SetEntityThink(IPluginContext *pContext, const cell_t *params)
{
	CBaseEntity *pEntity = gamehelpers->ReferenceToEntity(params[1]);
	if(!pEntity) {
		return pContext->ThrowNativeError("Invalid Entity Reference/Index %i", params[1]);
	}
	
	callback_holder_t *holder = nullptr;
	
	int ref = gamehelpers->EntityToReference(pEntity);

	callback_holder_map_t::iterator it{callbackmap.find(ref)};
	if(it != callbackmap.end()) {
		holder = it->second;
	} else {
		holder = new callback_holder_t{pEntity, ref};
	}

	if(!holder->think.fwd) {
		holder->think.fwd = forwards->CreateForwardEx(nullptr, ET_Hook, 1, nullptr, Param_Cell);

		pEntity->SetThink(&CBaseEntity::PluginThink, 0.0f);
	}

	IPluginFunction *func = pContext->GetFunctionById(params[2]);
	
	holder->think.fwd->RemoveFunction(func);
	holder->think.fwd->AddFunction(func);

	IdentityToken_t *iden{pContext->GetIdentity()};
	if(std::find(holder->owners.cbegin(), holder->owners.cend(), iden) == holder->owners.cend()) {
		holder->owners.emplace_back(iden);
	}

	return 0;
}

static cell_t SetEntityNextThink(IPluginContext *pContext, const cell_t *params)
{
	CBaseEntity *pEntity = gamehelpers->ReferenceToEntity(params[1]);
	if(!pEntity) {
		return pContext->ThrowNativeError("Invalid Entity Reference/Index %i", params[1]);
	}

	char *context = nullptr;
	pContext->LocalToStringNULL(params[3], &context);
	if(context && context[0] == '\0') {
		context = nullptr;
	}

	int iIndex = NO_THINK_CONTEXT;
	
	if(!context) {
		if(CBaseEntity::m_iCurrentThinkContext > 0 && CBaseEntity::m_iCurrentThinkContext < pEntity->GetAThinkFuncstions().Count()) {
			iIndex = CBaseEntity::m_iCurrentThinkContext;
		}
	} else {
		iIndex = pEntity->GetIndexForThinkContext(context);
		if(iIndex == NO_THINK_CONTEXT) {
			return pContext->ThrowNativeError("Invalid context %s", context);
		}
	}

	if(iIndex != NO_THINK_CONTEXT) {
		pEntity->SetNextThinkContext(sp_ctof(params[2]), iIndex);
	} else {
		pEntity->SetNextThink(sp_ctof(params[2]), context);
	}

	return 0;
}

static cell_t native_AllocPooledString(IPluginContext *pContext, const cell_t *params)
{
	char *context = nullptr;
	pContext->LocalToString(params[1], &context);

	string_t id = AllocPooledString(context);

	return *reinterpret_cast<cell_t *>(&id);
}

sp_nativeinfo_t natives[] =
{
	{"IEntityFactory.Custom.get", IEntityFactoryCustomget},
	{"IEntityFactory.Size.get", IEntityFactorySizeget},
	{"CustomEntityFactory.Interface.get", CustomEntityFactoryInterfaceget},
	{"CustomEntityFactory.add_alias", CustomEntityFactoryadd_alias},
	{"EntityFactoryDictionary.find", EntityFactoryDictionaryfind},
	{"EntityFactoryDictionary.register_based", EntityFactoryDictionaryregister_based},
	{"EntityFactoryDictionary.register_function", EntityFactoryDictionaryregister_function},
	{"EntityFactoryDictionary.remove", EntityFactoryDictionaryremove},
	{"CustomSendtable.from_factory", CustomSendtablefrom_factory},
	{"CustomSendtable.from_classname", CustomSendtablefrom_classname},
	{"CustomSendtable.override_with", CustomSendtableoverride_with},
	{"CustomSendtable.unexclude_prop", CustomSendtableunexclude_prop},
	{"CustomSendtable.set_base_class", CustomSendtableset_base_class},
	{"CustomSendtable.set_name", CustomSendtableset_name},
	{"CustomSendtable.set_network_name", CustomSendtableset_network_name},
	{"CustomSendtable.add_prop_float", CustomSendtableadd_prop_float},
	{"CustomSendtable.add_prop_int", CustomSendtableadd_prop_int},
	{"CustomDatamap.from_classname", CustomDatamapfrom_classname},
	{"CustomDatamap.from_factory", CustomDatamapfrom_factory},
	{"CustomDatamap.add_prop", CustomDatamapadd_prop},
	{"CustomDatamap.set_name", CustomDatamapset_name},
	{"HookEntityContextThink", SetEntityContextThink},
	{"HookEntityThink", SetEntityThink},
	{"SetEntityNextThink", SetEntityNextThink},
	{"AllocPooledString", native_AllocPooledString},
	{NULL, NULL}
};

void Sample::OnPluginUnloaded(IPlugin *plugin)
{
	callback_holder_map_t::iterator it{callbackmap.begin()};
	while(it != callbackmap.end()) {
		callback_holder_t *holder = it->second;
		std::vector<IdentityToken_t *> &owners{holder->owners};

		auto it_own{std::find(owners.begin(), owners.end(), plugin->GetIdentity())};
		if(it_own != owners.cend()) {
			owners.erase(it_own);

			size_t func_count{0};

			if(holder->think.fwd) {
				holder->think.fwd->RemoveFunctionsOfPlugin(plugin);
				func_count += holder->think.fwd->GetFunctionCount();
			}

			for(auto &it_ctx : holder->thinkctxs) {
				if(it_ctx.second.fwd) {
					it_ctx.second.fwd->RemoveFunctionsOfPlugin(plugin);
					func_count += it_ctx.second.fwd->GetFunctionCount();
				}
			}

			if(func_count == 0) {
				CBaseEntity *pEntity = gamehelpers->ReferenceToEntity(holder->ref);
				if(pEntity) {
					holder->dtor(pEntity);
				}
				holder->erase = false;
				delete holder;
				it = callbackmap.erase(it);
				continue;
			}
		}
		
		++it;
	}
}

void Sample::OnEntityDestroyed(CBaseEntity *pEntity)
{
	int ref = gamehelpers->EntityToReference(pEntity);

	auto it{hookobjs.find(ref)};
	if(it != hookobjs.cend()) {
		std::vector<hookobj_t *> &vec{it->second};
		for(hookobj_t *obj : vec) {
			obj->erase_ent = false;
			obj->erase_obj = false;
			obj->remove_hooks(pEntity);
		}
		hookobjs.erase(it);
	}
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

std::unordered_map<int, ServerClass *> baselinemap{};

static ConVar *sv_parallel_packentities{nullptr};

#ifdef __HAS_PROXYSEND
bool Sample::is_allowed() const noexcept
{
	return server_map.empty();
}
#endif

static CDetour *SV_ComputeClientPacks_detour{nullptr};

class CFrameSnapshot;
class CGameClient;
DETOUR_DECL_STATIC3(SV_ComputeClientPacks, void, int, clientCount, CGameClient **, clients, CFrameSnapshot *, snapshot)
{
#ifndef __HAS_PROXYSEND
	sv_parallel_packentities->SetValue(server_map.empty());
#else
	if(!proxysend) {
		sv_parallel_packentities->SetValue(server_map.empty());
	}
#endif

	DETOUR_STATIC_CALL(SV_ComputeClientPacks)(clientCount, clients, snapshot);
}

static ConVar *sv_sendtables{nullptr};

bool Sample::SDK_OnMetamodLoad(ISmmAPI *ismm, char *error, size_t maxlen, bool late)
{
	gpGlobals = ismm->GetCGlobals();
	GET_V_IFACE_ANY(GetServerFactory, servertools, IServerTools, VSERVERTOOLS_INTERFACE_VERSION)
	GET_V_IFACE_ANY(GetEngineFactory, engine, IVEngineServer, INTERFACEVERSION_VENGINESERVER)
#if SOURCE_ENGINE == SE_TF2
	dictionary = (CEntityFactoryDictionary *)servertools->GetEntityFactoryDictionary();
	dictionary->init_hooks();
#endif
#ifndef SOURCEHOOK_BEING_STUPID
	SH_ADD_HOOK(IVEngineServer, PvAllocEntPrivateData, engine, SH_STATIC(&HookPvAllocEntPrivateData), false);
#endif
	GET_V_IFACE_ANY(GetServerFactory, gamedll, IServerGameDLL, INTERFACEVERSION_SERVERGAMEDLL)
	GET_V_IFACE_ANY(GetEngineFactory, netstringtables, INetworkStringTableContainer, INTERFACENAME_NETWORKSTRINGTABLESERVER)
	g_pServerClassHead = gamedll->GetAllServerClasses();
	g_pServerClassTail = g_pServerClassHead;
	while(true) {
		baselinemap[g_pServerClassTail->m_ClassID] = g_pServerClassTail;

		if(strcmp(g_pServerClassTail->m_pNetworkName, "CBaseEntity") == 0) {
			CBaseEntity_ServerClass = g_pServerClassTail;
		} else if(strcmp(g_pServerClassTail->m_pNetworkName, "CTFPlayer") == 0) {
			CTFPlayer_ServerClass = g_pServerClassTail;
		}
		
		if(!g_pServerClassTail->m_pNext) {
			break;
		}
		
		g_pServerClassTail = g_pServerClassTail->m_pNext;
	}
	m_pInstanceBaselineTable = netstringtables->FindTable(INSTANCE_BASELINE_TABLENAME);
#if SOURCE_ENGINE == SE_TF2
	server = engine->GetIServer();
#endif
	GET_V_IFACE_CURRENT(GetEngineFactory, icvar, ICvar, CVAR_INTERFACE_VERSION);
	g_pCVar = icvar;
	ConVar_Register(0, this);

	std_proxies = gamedll->GetStandardSendProxies();
	
	sv_sendtables = g_pCVar->FindVar("sv_sendtables");
	sv_parallel_packentities = g_pCVar->FindVar("sv_parallel_packentities");

	return true;
}

bool Sample::RegisterConCommandBase(ConCommandBase *pCommand)
{
	META_REGCVAR(pCommand);
	return true;
}

IGameConfig *g_pGameConf = nullptr;

CDetour *pPhysicsRunSpecificThink = nullptr;

CDetour *pCGameServerAssignClassIds = nullptr;

CDetour *SendTable_GetCRC_detour = nullptr;
CDetour *SV_CreateBaseline_detour{nullptr};
CDetour *pCGameClientSendSignonData = nullptr;
CDetour *pSVC_ClassInfoWriteToBuffer = nullptr;

static bool in_send_signondata{false};

#include <tier1/checksum_crc.h>

CRC32_t *g_SendTableCRC{nullptr};

void Sample::OnCoreMapStart(edict_t *pEdictList, int edictCount, int clientMax)
{
}

static void Host_Error(const char *error, ...) noexcept
{
	va_list argptr;
	char string[1024];

	va_start(argptr, error);
	Q_vsnprintf(string, sizeof(string), error, argptr);
	va_end(argptr);

	Error("Host_Error: %s", string);
}

void *SV_WriteSendTablesPtr{nullptr};
void *SV_WriteClassInfosPtr{nullptr};

void SV_WriteSendTables( ServerClass *pClasses, bf_write &pBuf )
{
	(void_to_func<void(*)(ServerClass *, bf_write &)>(SV_WriteSendTablesPtr))(pClasses, pBuf);
}

void SV_WriteClassInfos(ServerClass *pClasses, bf_write &pBuf)
{
	(void_to_func<void(*)(ServerClass *, bf_write &)>(SV_WriteClassInfosPtr))(pClasses, pBuf);
}

bf_write			m_FullSendTables;
CUtlMemory<byte>	m_FullSendTablesBuffer;

#define	NET_MAX_PAYLOAD				288000	

ConVar sv_sendcustomtables{"sv_sendcustomtables", "0"};
ConVar sv_sendclasses{"sv_sendclasses", "0"};

DETOUR_DECL_STATIC0(SV_CreateBaseline, void)
{
	DETOUR_STATIC_CALL(SV_CreateBaseline)();

	if(sv_sendtables->GetInt() == 0) {
		m_FullSendTablesBuffer.EnsureCapacity( NET_MAX_PAYLOAD );
		m_FullSendTables.StartWriting( m_FullSendTablesBuffer.Base(), m_FullSendTablesBuffer.Count() );

		if(sv_sendcustomtables.GetBool() && custom_server_head) {
			SV_WriteSendTables( custom_server_head, m_FullSendTables );

			if ( m_FullSendTables.IsOverflowed() )
			{
				Host_Error("SV_CreateBaseline: WriteSendTables overflow.\n" );
				return;
			}
		}

		if(sv_sendclasses.GetBool()) {
			// Send class descriptions.
			SV_WriteClassInfos(g_pServerClassHead, m_FullSendTables);

			if ( m_FullSendTables.IsOverflowed() )
			{
				Host_Error("SV_CreateBaseline: WriteClassInfos overflow.\n" );
				return;
			}
		}
	}
}

ConVar sv_nocustomclassids{"sv_nocustomclassids", "1"};

DETOUR_DECL_MEMBER0(DetourCGameServerAssignClassIds, void)
{
	DETOUR_MEMBER_CALL(DetourCGameServerAssignClassIds)();

	if(sv_nocustomclassids.GetBool()) {
		for(auto &it : server_map) {
			it.second->cls.m_ClassID = it.second->classid;
		}
	}
}

#include <igameevents.h>
#include <iclient.h>
#include <inetchannel.h>

class CGameClient : public IGameEventListener2, public IClient, public IClientMessageHandler
{
public:
	
};

DETOUR_DECL_MEMBER0(DetourCGameClientSendSignonData, bool)
{
	CGameClient *pThis = (CGameClient *)this;

	if(sv_sendtables->GetInt() == 0) {
		if ( m_FullSendTables.IsOverflowed() )
		{
			Host_Error( "Send Table signon buffer overflowed %i bytes!!!\n", m_FullSendTables.GetNumBytesWritten() );
			return false;
		}

		pThis->GetNetChannel()->SendData( m_FullSendTables );
	}

	in_send_signondata = true;
	bool ret = DETOUR_MEMBER_CALL(DetourCGameClientSendSignonData)();
	in_send_signondata = false;

	return ret;
}

DETOUR_DECL_STATIC0(SendTable_GetCRC, CRC32_t)
{
	if(in_send_signondata && sv_sendtables->GetInt() == 2) {
		CRC32_t tmp;
		CRC32_Init(&tmp);
		return tmp;
	} else {
		return DETOUR_STATIC_CALL(SendTable_GetCRC)();
	}
}

#include <inetmessage.h>

class CNetMessage : public INetMessage
{
public:
	bool				m_bReliable;	// true if message should be send reliable
	INetChannel			*m_NetChannel;	// netchannel this message is from/for
};

class SVC_ClassInfo : public CNetMessage
{
public:
	IServerMessageHandler *m_pMessageHandler;

	typedef struct class_s
	{
		int		classID;
		char	datatablename[256];
		char	classname[256];
	} class_t;

	bool					m_bCreateOnClient;	// if true, client creates own SendTables & classinfos from game.dll
	CUtlVector<class_t>		m_Classes;			
	int						m_nNumServerClasses;
};

ConVar sv_createclasses{"sv_createclasses", "0"};

DETOUR_DECL_MEMBER1(DetourSVC_ClassInfoWriteToBuffer, bool, bf_write &, buffer)
{
	SVC_ClassInfo *msg = reinterpret_cast<SVC_ClassInfo *>(this);

	msg->m_bCreateOnClient = sv_createclasses.GetBool();

	return DETOUR_MEMBER_CALL(DetourSVC_ClassInfoWriteToBuffer)(buffer);
}

class SVC_SendTable : public CNetMessage
{
public:
	IServerMessageHandler *m_pMessageHandler;

	bool			m_bNeedsDecoder;
	int				m_nLength;
	bf_read			m_DataIn;
	bf_write		m_DataOut;
};

#define Bits2Bytes(b) ((b+7)>>3)

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
	
	SV_ComputeClientPacks_detour = DETOUR_CREATE_STATIC(SV_ComputeClientPacks, "SV_ComputeClientPacks");

	g_pGameConf->GetMemSig("g_SendTableCRC", (void **)&g_SendTableCRC);

	g_pGameConf->GetMemSig("SV_WriteSendTables", (void **)&SV_WriteSendTablesPtr);
	g_pGameConf->GetMemSig("SV_WriteClassInfos", (void **)&SV_WriteClassInfosPtr);

	pCGameServerAssignClassIds = DETOUR_CREATE_MEMBER(DetourCGameServerAssignClassIds, "CGameServer::AssignClassIds")
	pCGameServerAssignClassIds->EnableDetour();

	SV_CreateBaseline_detour = DETOUR_CREATE_STATIC(SV_CreateBaseline, "SV_CreateBaseline");
	SV_CreateBaseline_detour->EnableDetour();

	SendTable_GetCRC_detour = DETOUR_CREATE_STATIC(SendTable_GetCRC, "SendTable_GetCRC");
	SendTable_GetCRC_detour->EnableDetour();

	pCGameClientSendSignonData = DETOUR_CREATE_MEMBER(DetourCGameClientSendSignonData, "CGameClient::SendSignonData")
	pCGameClientSendSignonData->EnableDetour();

	pSVC_ClassInfoWriteToBuffer = DETOUR_CREATE_MEMBER(DetourSVC_ClassInfoWriteToBuffer, "SVC_ClassInfo::WriteToBuffer")
	pSVC_ClassInfoWriteToBuffer->EnableDetour();

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
	dictionary->init_hooks();
#endif
	
	factory_handle = handlesys->CreateType("entity_factory", this, 0, nullptr, nullptr, myself->GetIdentity(), nullptr);
	datamap_handle = handlesys->CreateType("datamap", this, 0, nullptr, nullptr, myself->GetIdentity(), nullptr);
	removal_handle = handlesys->CreateType("factory_removal", this, 0, nullptr, nullptr, myself->GetIdentity(), nullptr);
	serverclass_handle = handlesys->CreateType("serverclass_override", this, 0, nullptr, nullptr, myself->GetIdentity(), nullptr);
	
	sharesys->AddDependency(myself, "sdkhooks.ext", true, true);
	
#ifdef __HAS_PROXYSEND
	sharesys->AddDependency(myself, "proxysend.ext", false, true);
#endif

	plsys->AddPluginsListener(this);
	
	sharesys->RegisterLibrary(myself, "datamaps");

	return true;
}

void Sample::SDK_OnUnload()
{
	SendTable_GetCRC_detour->Destroy();
	SV_CreateBaseline_detour->Destroy();
	pSVC_ClassInfoWriteToBuffer->Destroy();
	pCGameClientSendSignonData->Destroy();
	SV_ComputeClientPacks_detour->Destroy();
	if(pPvAllocEntPrivateData) {
		pPvAllocEntPrivateData->Destroy();
	}
	pPhysicsRunSpecificThink->Destroy();
	pCGameServerAssignClassIds->Destroy();
	g_pSDKHooks->RemoveEntityListener(this);
	plsys->RemovePluginsListener(this);
	handlesys->RemoveType(factory_handle, myself->GetIdentity());
	handlesys->RemoveType(datamap_handle, myself->GetIdentity());
	handlesys->RemoveType(removal_handle, myself->GetIdentity());
	handlesys->RemoveType(serverclass_handle, myself->GetIdentity());
	gameconfs->CloseGameConfigFile(g_pGameConf);
}

void Sample::SDK_OnAllLoaded()
{
	SM_GET_LATE_IFACE(SDKHOOKS, g_pSDKHooks);
	SM_GET_LATE_IFACE(SDKTOOLS, g_pSDKTools);

#ifdef __HAS_PROXYSEND
	SM_GET_LATE_IFACE(PROXYSEND, proxysend);
	if(proxysend) {
		proxysend->add_listener(this);
		SV_ComputeClientPacks_detour->DisableDetour();
	} else {
		SV_ComputeClientPacks_detour->EnableDetour();
	}
#endif

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
#ifdef __HAS_PROXYSEND
	//SM_CHECK_IFACE(PROXYSEND, proxysend);
#endif
	return true;
}

bool Sample::QueryInterfaceDrop(SMInterface *pInterface)
{
	if(pInterface == g_pSDKHooks)
		return false;
	else if(pInterface == g_pSDKTools)
		return false;
#ifdef __HAS_PROXYSEND
	else if(pInterface == proxysend)
		return false;
#endif
	
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
#ifdef __HAS_PROXYSEND
	else if(strcmp(pInterface->GetInterfaceName(), SMINTERFACE_PROXYSEND_NAME) == 0) {
		proxysend = NULL;
		SV_ComputeClientPacks_detour->EnableDetour();
	}
#endif
}

CON_COMMAND(dump_baselinemap, "")
{
	for(auto &it : baselinemap) {
		META_CONPRINTF("%i - %s\n", it.first, it.second ? it.second->GetName() : "NULL");
	}
}

CON_COMMAND(dump_baseline, "")
{
	m_pInstanceBaselineTable = netstringtables->FindTable(INSTANCE_BASELINE_TABLENAME);

	for(int i = 0; i < m_pInstanceBaselineTable->GetNumStrings(); ++i) {
		const char *str = m_pInstanceBaselineTable->GetString(i);
		int id = V_atoi(str);
		META_CONPRINTF("%i = %s = %s\n", i, str, baselinemap[id] ? baselinemap[id]->GetName() : "NULL");
	}
}

CON_COMMAND(dump_serverclasses_c, "Dumps the class list to console")
{
	ServerClass *pClass = g_pServerClassHead;
	while(pClass) {
		printf("%s\n",pClass->GetName());
		pClass = pClass->m_pNext;
	}
}

CON_COMMAND(dump_serverclasses, "Dumps the class list as a text file")
{
	if (args.ArgC() < 2)
	{
		META_CONPRINT("Usage: dump_serverclasses <file>\n");
		return;
	}

	const char *file = args.Arg(1);
	if (!file || file[0] == '\0')
	{
		META_CONPRINT("Usage: dump_serverclasses <file>\n");
		return;
	}

	char path[PLATFORM_MAX_PATH];
	g_pSM->BuildPath(Path_Game, path, sizeof(path), "%s", file);

	FILE *fp = NULL;
	if ((fp = fopen(path, "wt")) == NULL)
	{
		META_CONPRINTF("Could not open file \"%s\"\n", path);
		return;
	}

	char buffer[80];
	buffer[0] = 0;

	time_t t = g_pSM->GetAdjustedTime();
	size_t written = 0;
	{
#ifdef PLATFORM_WINDOWS
		InvalidParameterHandler p;
#endif
		written = strftime(buffer, sizeof(buffer), "%Y/%m/%d", localtime(&t));
	}

	fprintf(fp, "// Dump of all server classes for \"%s\" as at %s\n//\n\n", g_pSM->GetGameFolderName(), buffer);

	m_pInstanceBaselineTable = netstringtables->FindTable(INSTANCE_BASELINE_TABLENAME);

	ServerClass *pClass = g_pServerClassHead;
	while(pClass) {
		fprintf(fp,"%s\n",pClass->GetName());
		fprintf(fp,"  ClassID: %i (%s)\n", pClass->m_ClassID, baselinemap[pClass->m_ClassID] ? baselinemap[pClass->m_ClassID]->GetName() : "NULL");
		if(pClass->m_InstanceBaselineIndex == INVALID_STRING_INDEX) {
			fprintf(fp,"  InstanceBaselineIndex: INVALID_STRING_INDEX\n");
			fprintf(fp,"  BaselineIndex String: NULL\n");
		} else {
			const char *str = m_pInstanceBaselineTable->GetString(pClass->m_InstanceBaselineIndex);
			int id = V_atoi(str);
			fprintf(fp,"  InstanceBaselineIndex: %i\n", pClass->m_InstanceBaselineIndex);
			fprintf(fp,"  BaselineIndex String: %s (%s)\n", str, baselinemap[id] ? baselinemap[id]->GetName() : "NULL");
		}
		pClass = pClass->m_pNext;
	}

	fclose(fp);

}

CON_COMMAND(dump_classes_ent, "Dumps the class list as a text file")
{
	if (args.ArgC() < 3)
	{
		META_CONPRINT("Usage: dump_classes_ent <cls> <file>\n");
		return;
	}
	
	const char *cls = args.Arg(1);
	if (!cls || cls[0] == '\0')
	{
		META_CONPRINT("Usage: dump_classes_ent <cls> <file>\n");
		return;
	}

	const char *file = args.Arg(2);
	if (!file || file[0] == '\0')
	{
		META_CONPRINT("Usage: dump_classes_ent <cls> <file>\n");
		return;
	}

	char path[PLATFORM_MAX_PATH];
	g_pSM->BuildPath(Path_Game, path, sizeof(path), "%s", file);

	FILE *fp = NULL;
	if ((fp = fopen(path, "wt")) == NULL)
	{
		META_CONPRINTF("Could not open file \"%s\"\n", path);
		return;
	}

	char buffer[80];
	buffer[0] = 0;

	time_t t = g_pSM->GetAdjustedTime();
	size_t written = 0;
	{
#ifdef PLATFORM_WINDOWS
		InvalidParameterHandler p;
#endif
		written = strftime(buffer, sizeof(buffer), "%Y/%m/%d", localtime(&t));
	}

	fprintf(fp, "// Dump of %s classes for \"%s\" as at %s\n//\n\n", cls, g_pSM->GetGameFolderName(), buffer);

	IServerNetworkable *entity = dictionary->Create(cls);
		
	if(entity) {
		ServerClass *sclass = entity->GetServerClass();
		datamap_t *pMap = gamehelpers->GetDataMap(entity->GetBaseEntity());
		fprintf(fp,"%s\n",cls);
		fprintf(fp,"  %s - %i (%s)\n",sclass->GetName(), sclass->m_ClassID, baselinemap[sclass->m_ClassID] ? baselinemap[sclass->m_ClassID]->GetName() : "NULL");
		fprintf(fp,"  %s\n",pMap->dataClassName);
		
		sm_datatable_info_t info;
		gamehelpers->FindDataMapInfo(pMap, "m_iEFlags", &info);
		
		int *eflags = (int *)((char *)entity->GetBaseEntity() + info.actual_offset);
		*eflags |= (1<<0); // EFL_KILLME
	}

	fclose(fp);

}

CON_COMMAND(dump_classes_ex, "Dumps the class list as a text file")
{
	if (args.ArgC() < 2)
	{
		META_CONPRINT("Usage: dump_classes_ex <file>\n");
		return;
	}

	const char *file = args.Arg(1);
	if (!file || file[0] == '\0')
	{
		META_CONPRINT("Usage: dump_classes_ex <file>\n");
		return;
	}

	char path[PLATFORM_MAX_PATH];
	g_pSM->BuildPath(Path_Game, path, sizeof(path), "%s", file);

	FILE *fp = NULL;
	if ((fp = fopen(path, "wt")) == NULL)
	{
		META_CONPRINTF("Could not open file \"%s\"\n", path);
		return;
	}

	char buffer[80];
	buffer[0] = 0;

	time_t t = g_pSM->GetAdjustedTime();
	size_t written = 0;
	{
#ifdef PLATFORM_WINDOWS
		InvalidParameterHandler p;
#endif
		written = strftime(buffer, sizeof(buffer), "%Y/%m/%d", localtime(&t));
	}

	fprintf(fp, "// Dump of all classes for \"%s\" as at %s\n//\n\n", g_pSM->GetGameFolderName(), buffer);

	sm_datatable_info_t info;
	for ( int i = dictionary->m_Factories.First(); i != dictionary->m_Factories.InvalidIndex(); i = dictionary->m_Factories.Next( i ) )
	{
		IServerNetworkable *entity = dictionary->Create(dictionary->m_Factories.GetElementName(i));
		ServerClass *sclass = entity->GetServerClass();
		datamap_t *pMap = gamehelpers->GetDataMap(entity->GetBaseEntity());
		fprintf(fp,"%s\n",dictionary->m_Factories.GetElementName(i));
		fprintf(fp,"  %s - %i (%s)\n",sclass->GetName(), sclass->m_ClassID, baselinemap[sclass->m_ClassID] ? baselinemap[sclass->m_ClassID]->GetName() : "NULL");
		fprintf(fp,"  %s\n",pMap->dataClassName);
		
		if (!gamehelpers->FindDataMapInfo(pMap, "m_iEFlags", &info))
			continue;
		
		int *eflags = (int *)((char *)entity->GetBaseEntity() + info.actual_offset);
		*eflags |= (1<<0); // EFL_KILLME
	}

	fclose(fp);

}

char *UTIL_SendFlagsToString(int flags, int type)
{
	static char str[1024];
	str[0] = 0;

	if (flags & SPROP_UNSIGNED)
	{
		strcat(str, "Unsigned|");
	}
	if (flags & SPROP_COORD)
	{
		strcat(str, "Coord|");
	}
	if (flags & SPROP_NOSCALE)
	{
		strcat(str, "NoScale|");
	}
	if (flags & SPROP_ROUNDDOWN)
	{
		strcat(str, "RoundDown|");
	}
	if (flags & SPROP_ROUNDUP)
	{
		strcat(str, "RoundUp|");
	}
	if (flags & SPROP_NORMAL)
	{
		if (type == DPT_Int)
		{
			strcat(str, "VarInt|");
		}
		else
		{
			strcat(str, "Normal|");
		}
	}
	if (flags & SPROP_EXCLUDE)
	{
		strcat(str, "Exclude|");
	}
	if (flags & SPROP_XYZE)
	{
		strcat(str, "XYZE|");
	}
	if (flags & SPROP_INSIDEARRAY)
	{
		strcat(str, "InsideArray|");
	}
	if (flags & SPROP_PROXY_ALWAYS_YES)
	{
		strcat(str, "AlwaysProxy|");
	}
	if (flags & SPROP_CHANGES_OFTEN)
	{
		strcat(str, "ChangesOften|");
	}
	if (flags & SPROP_IS_A_VECTOR_ELEM)
	{
		strcat(str, "VectorElem|");
	}
	if (flags & SPROP_COLLAPSIBLE)
	{
		strcat(str, "Collapsible|");
	}
	if (flags & SPROP_COORD_MP)
	{
		strcat(str, "CoordMP|");
	}
	if (flags & SPROP_COORD_MP_LOWPRECISION)
	{
		strcat(str, "CoordMPLowPrec|");
	}
	if (flags & SPROP_COORD_MP_INTEGRAL)
	{
		strcat(str, "CoordMpIntegral|");
	}

	int len = strlen(str) - 1;
	if (len > 0)
	{
		str[len] = 0; // Strip the final '|'
	}

	return str;
}

const char *GetDTTypeName(int type)
{
	switch (type)
	{
	case DPT_Int:
		{
			return "integer";
		}
	case DPT_Float:
		{
			return "float";
		}
	case DPT_Vector:
		{
			return "vector";
		}
	case DPT_String:
		{
			return "string";
		}
	case DPT_Array:
		{
			return "array";
		}
	case DPT_DataTable:
		{
			return "datatable";
		}
#if SOURCE_ENGINE == SE_LEFT4DEAD2
	case DPT_Int64:
		{
			return "int64";
		}
#endif
	default:
		{
			return NULL;
		}
	}

	return NULL;
}

#include <stack>

const char *GetPropDatatableType(SendProp *pProp)
{
	return pProp->GetDataTable()->GetName();
}

void UTIL_DrawSendTable(FILE *fp, SendTable *pTable, int level = 1)
{
	SendProp *pProp;
	const char *type;
	
	std::stack<SendProp *> printlater{};

	for (int i = 0; i < pTable->GetNumProps(); i++)
	{
		pProp = pTable->GetProp(i);
		if (!pProp->GetDataTable())
		{
			type = GetDTTypeName(pProp->GetType());

			if (type != NULL)
			{
				fprintf(fp,
					"%*sMember: %s (offset %d) (type %s) (bits %d) (%s)\n", 
					level, "", 
					pProp->GetName(),
					pProp->GetOffset(),
					type,
					pProp->m_nBits,
					UTIL_SendFlagsToString(pProp->GetFlags(), pProp->GetType()));
			}
			else
			{
				fprintf(fp,
					"%*sMember: %s (offset %d) (type %d) (bits %d) (%s)\n", 
					level, "", 
					pProp->GetName(),
					pProp->GetOffset(),
					pProp->GetType(),
					pProp->m_nBits,
					UTIL_SendFlagsToString(pProp->GetFlags(), pProp->GetType()));
			}
		}
		else
		{
			printlater.emplace(pProp);
		}
	}
	
	while(!printlater.empty()) {
		pProp = printlater.top();
		printlater.pop();
		
		if(stricmp(pProp->GetName(), "baseclass") == 0) {
			fprintf(fp, "%*sBase Table: %s\n", 
				level, "", 
				pProp->GetDataTable()->GetName());
		} else {
			fprintf(fp, "%*sSub-Class Table: %s (offset %d) (type %s)\n", 
				level, "", 
				pProp->GetName(), 
				pProp->GetOffset(), 
				GetPropDatatableType(pProp));
		}
		
		UTIL_DrawSendTable(fp, pProp->GetDataTable(), level + 1);
	}
}

void UTIL_DrawServerClass(FILE *fp, ServerClass *pBase)
{
	fprintf(fp, "%s (table %s)(id %i (%s))\n", pBase->GetName(), pBase->m_pTable->GetName(), pBase->m_ClassID, baselinemap[pBase->m_ClassID] ? baselinemap[pBase->m_ClassID]->GetName() : "NULL");
	UTIL_DrawSendTable(fp, pBase->m_pTable);
}

CON_COMMAND(dump_netprops_inv, "Dumps the networkable property table as a text file")
{
	if (args.ArgC() < 2)
	{
		META_CONPRINT("Usage: dump_netprops_inv <file>\n");
		return;
	}

	const char *file = args.Arg(1);
	if (!file || file[0] == '\0')
	{
		META_CONPRINT("Usage: dump_netprops_inv <file>\n");
		return;
	}

	char path[PLATFORM_MAX_PATH];
	g_pSM->BuildPath(Path_Game, path, sizeof(path), "%s", file);

	FILE *fp = NULL;
	if ((fp = fopen(path, "wt")) == NULL)
	{
		META_CONPRINTF("Could not open file \"%s\"\n", path);
		return;
	}
	
	char buffer[80];
	buffer[0] = 0;

	time_t t = g_pSM->GetAdjustedTime();
	size_t written = 0;
	{
#ifdef PLATFORM_WINDOWS
		InvalidParameterHandler p;
#endif
		written = strftime(buffer, sizeof(buffer), "%Y/%m/%d", localtime(&t));
	}

	fprintf(fp, "// Dump of all network properties for \"%s\" as at %s\n//\n\n", g_pSM->GetGameFolderName(), buffer);

	ServerClass *pBase = gamedll->GetAllServerClasses();
	while (pBase != NULL)
	{
		UTIL_DrawServerClass(fp, pBase);
		pBase = pBase->m_pNext;
	}

	fclose(fp);
}

CON_COMMAND(dump_netprops_ent, "Dumps the networkable property table as a text file")
{
	if (args.ArgC() < 3)
	{
		META_CONPRINT("Usage: dump_netprops_ent <cls> <file>\n");
		return;
	}

	const char *cls = args.Arg(1);
	if (!cls || cls[0] == '\0')
	{
		META_CONPRINT("Usage: dump_netprops_ent <cls> <file>\n");
		return;
	}
	
	const char *file = args.Arg(2);
	if (!file || file[0] == '\0')
	{
		META_CONPRINT("Usage: dump_netprops_ent <cls> <file>\n");
		return;
	}

	char path[PLATFORM_MAX_PATH];
	g_pSM->BuildPath(Path_Game, path, sizeof(path), "%s", file);

	FILE *fp = NULL;
	if ((fp = fopen(path, "wt")) == NULL)
	{
		META_CONPRINTF("Could not open file \"%s\"\n", path);
		return;
	}
	
	char buffer[80];
	buffer[0] = 0;

	time_t t = g_pSM->GetAdjustedTime();
	size_t written = 0;
	{
#ifdef PLATFORM_WINDOWS
		InvalidParameterHandler p;
#endif
		written = strftime(buffer, sizeof(buffer), "%Y/%m/%d", localtime(&t));
	}

	fprintf(fp, "// Dump of %s network properties for \"%s\" as at %s\n//\n\n", cls, g_pSM->GetGameFolderName(), buffer);

	IServerNetworkable *entity = dictionary->Create(cls);
	
	if(entity) {
		ServerClass *pBase = entity->GetServerClass();
		
		UTIL_DrawServerClass(fp, pBase);
		
		datamap_t *pMap = gamehelpers->GetDataMap(entity->GetBaseEntity());
		
		static int offsEFlags = -1;
		if (offsEFlags == -1)
		{
			sm_datatable_info_t info;
			gamehelpers->FindDataMapInfo(pMap, "m_iEFlags", &info);

			offsEFlags = info.actual_offset;
		}
		
		int *eflags = (int *)((char *)entity->GetBaseEntity() + offsEFlags);
		*eflags |= (1<<0); // EFL_KILLME
	}

	fclose(fp);
}

CON_COMMAND(dump_netprops_cls, "Dumps the networkable property table as a text file")
{
	if (args.ArgC() < 3)
	{
		META_CONPRINT("Usage: dump_netprops_cls <cls> <file>\n");
		return;
	}

	const char *cls = args.Arg(1);
	if (!cls || cls[0] == '\0')
	{
		META_CONPRINT("Usage: dump_netprops_cls <cls> <file>\n");
		return;
	}
	
	const char *file = args.Arg(2);
	if (!file || file[0] == '\0')
	{
		META_CONPRINT("Usage: dump_netprops_cls <cls> <file>\n");
		return;
	}

	char path[PLATFORM_MAX_PATH];
	g_pSM->BuildPath(Path_Game, path, sizeof(path), "%s", file);

	FILE *fp = NULL;
	if ((fp = fopen(path, "wt")) == NULL)
	{
		META_CONPRINTF("Could not open file \"%s\"\n", path);
		return;
	}
	
	char buffer[80];
	buffer[0] = 0;

	time_t t = g_pSM->GetAdjustedTime();
	size_t written = 0;
	{
#ifdef PLATFORM_WINDOWS
		InvalidParameterHandler p;
#endif
		written = strftime(buffer, sizeof(buffer), "%Y/%m/%d", localtime(&t));
	}

	fprintf(fp, "// Dump of %s network properties for \"%s\" as at %s\n//\n\n", cls, g_pSM->GetGameFolderName(), buffer);

	ServerClass *pBase = gamedll->GetAllServerClasses();
	while (pBase != NULL)
	{
		if(stricmp(pBase->GetName(), cls) == 0) {
			UTIL_DrawServerClass(fp, pBase);
			break;
		}
		pBase = pBase->m_pNext;
	}

	fclose(fp);
}

char *UTIL_DataFlagsToString(int flags)
{
	static char str[1024];
	str[0] = 0;

	if (flags & FTYPEDESC_GLOBAL)
	{
		strcat(str,	"Global|");
	}
	if (flags & FTYPEDESC_SAVE)
	{
		strcat(str,	"Save|");
	}
	if (flags & FTYPEDESC_KEY)
	{
		strcat(str,	"Key|");
	}
	if (flags & FTYPEDESC_INPUT)
	{
		strcat(str,	"Input|");
	}
	if (flags & FTYPEDESC_OUTPUT)
	{
		strcat(str,	"Output|");
	}
	if (flags & FTYPEDESC_FUNCTIONTABLE)
	{
		strcat(str,	"FunctionTable|");
	}
	if (flags & FTYPEDESC_PTR)
	{
		strcat(str,	"Ptr|");
	}
	if (flags & FTYPEDESC_OVERRIDE)
	{
		strcat(str,	"Override|");
	}

	int len = strlen(str) - 1;
	if (len > 0)
	{
		str[len] = 0; // Strip the final '|'
	}

	return str;
}

#if SOURCE_ENGINE == SE_LEFT4DEAD2
inline int GetTypeDescOffs(typedescription_t *td)
{
	return td->fieldOffset;
}
#elif SOURCE_ENGINE == SE_TF2
inline int GetTypeDescOffs(typedescription_t *td)
{
	return td->fieldOffset[TD_OFFSET_NORMAL];
}
#endif

void UTIL_DrawDataTable(FILE *fp, datamap_t *pMap, int level = 1)
{
	const char *externalname;
	char *flags;

	while (pMap)
	{
		std::vector<int> printlater{};
		
		for (int i=0; i<pMap->dataNumFields; i++)
		{
			if (pMap->dataDesc[i].fieldName == NULL)
			{
				continue;
			}

			if (!pMap->dataDesc[i].td)
			{
				externalname  = pMap->dataDesc[i].externalName;
				flags = UTIL_DataFlagsToString(pMap->dataDesc[i].flags);

				if (externalname == NULL)
				{
					fprintf(fp, "%*sMember: %s (Offset %d) (%s)(%i Bytes)\n", level, "", pMap->dataDesc[i].fieldName, GetTypeDescOffs(&pMap->dataDesc[i]), flags, pMap->dataDesc[i].fieldSizeInBytes);
				}
				else
				{
					fprintf(fp, "%*sMember: %s (Offset %d) (%s)(%i Bytes) - %s\n", level, "", pMap->dataDesc[i].fieldName, GetTypeDescOffs(&pMap->dataDesc[i]), flags, pMap->dataDesc[i].fieldSizeInBytes, externalname);
				}
			}
			else
			{
				printlater.emplace_back(i);
			}
		}
		
		for(auto it : printlater) {
			int i = it;
			
			fprintf(fp, " %*sSub-Class Table: %s - %s\n", level, "", pMap->dataDesc[i].fieldName, pMap->dataDesc[i].td->dataClassName);
			UTIL_DrawDataTable(fp, pMap->dataDesc[i].td, level+1);
		}
		
		pMap = pMap->baseMap;
		
		if(pMap) {
			++level;
			
			fprintf(fp, "%*sBase Table: %s\n", 
					level, "", 
					pMap->dataClassName);
			
			++level;
		}
	}
}

CON_COMMAND(dump_datamaps_nm, "Dumps the data map list as a text file")
{
	if (args.ArgC() < 2)
	{
		META_CONPRINT("Usage: dump_datamaps_nm <file>\n");
		return;
	}

	const char *file = args.Arg(1);
	if (!file || file[0] == '\0')
	{
		META_CONPRINT("Usage: dump_datamaps_nm <file>\n");
		return;
	}

	char path[PLATFORM_MAX_PATH];
	g_pSM->BuildPath(Path_Game, path, sizeof(path), "%s", file);

	FILE *fp = NULL;
	if ((fp = fopen(path, "wt")) == NULL)
	{
		META_CONPRINTF("Could not open file \"%s\"\n", path);
		return;
	}

	char buffer[80];
	buffer[0] = 0;

	time_t t = g_pSM->GetAdjustedTime();
	size_t written = 0;
	{
#ifdef PLATFORM_WINDOWS
		InvalidParameterHandler p;
#endif
		written = strftime(buffer, sizeof(buffer), "%Y/%m/%d", localtime(&t));
	}

	fprintf(fp, "// Dump of all datamaps for \"%s\" as at %s\n//\n//\n", g_pSM->GetGameFolderName(), buffer);


	fprintf(fp, "// Flag Details:\n//\n");

	fprintf(fp, "// Global: This field is masked for global entity save/restore\n");
	fprintf(fp, "// Save: This field is saved to disk\n");
	fprintf(fp, "// Key: This field can be requested and written to by string name at load time\n");
	fprintf(fp, "// Input: This field can be written to by string name at run time, and a function called\n");
	fprintf(fp, "// Output: This field propogates it's value to all targets whenever it changes\n");
	fprintf(fp, "// FunctionTable: This is a table entry for a member function pointer\n");
	fprintf(fp, "// Ptr: This field is a pointer, not an embedded object\n");
	fprintf(fp, "// Override: The field is an override for one in a base class (only used by prediction system for now)\n");

	fprintf(fp, "//\n\n");

	static int offsEFlags = -1;
	for ( int i = dictionary->m_Factories.First(); i != dictionary->m_Factories.InvalidIndex(); i = dictionary->m_Factories.Next( i ) )
	{
		IServerNetworkable *entity = dictionary->Create(dictionary->m_Factories.GetElementName(i));
		datamap_t *pMap = gamehelpers->GetDataMap(entity->GetBaseEntity());

		fprintf(fp,"%s - %s\n", pMap->dataClassName, dictionary->m_Factories.GetElementName(i));

		UTIL_DrawDataTable(fp, pMap);

		if (offsEFlags == -1)
		{
			sm_datatable_info_t info;
			if (!gamehelpers->FindDataMapInfo(pMap, "m_iEFlags", &info))
			{
				continue;
			}

			offsEFlags = info.actual_offset;
		}
		
		int *eflags = (int *)((char *)entity->GetBaseEntity() + offsEFlags);
		*eflags |= (1<<0); // EFL_KILLME
	}

	fclose(fp);

}

CON_COMMAND(dump_datamaps_cls, "Dumps the data map list as a text file")
{
	if (args.ArgC() < 3)
	{
		META_CONPRINT("Usage: dump_datamaps_cls <cls> <file>\n");
		return;
	}
	
	const char *cls = args.Arg(1);
	if (!cls || cls[0] == '\0')
	{
		META_CONPRINT("Usage: dump_datamaps_cls <cls> <file>\n");
		return;
	}

	const char *file = args.Arg(2);
	if (!file || file[0] == '\0')
	{
		META_CONPRINT("Usage: dump_datamaps_cls <cls> <file>\n");
		return;
	}

	char path[PLATFORM_MAX_PATH];
	g_pSM->BuildPath(Path_Game, path, sizeof(path), "%s", file);

	FILE *fp = NULL;
	if ((fp = fopen(path, "wt")) == NULL)
	{
		META_CONPRINTF("Could not open file \"%s\"\n", path);
		return;
	}

	char buffer[80];
	buffer[0] = 0;

	time_t t = g_pSM->GetAdjustedTime();
	size_t written = 0;
	{
#ifdef PLATFORM_WINDOWS
		InvalidParameterHandler p;
#endif
		written = strftime(buffer, sizeof(buffer), "%Y/%m/%d", localtime(&t));
	}

	fprintf(fp, "// Dump of %s datamaps for \"%s\" as at %s\n//\n//\n", cls, g_pSM->GetGameFolderName(), buffer);


	fprintf(fp, "// Flag Details:\n//\n");

	fprintf(fp, "// Global: This field is masked for global entity save/restore\n");
	fprintf(fp, "// Save: This field is saved to disk\n");
	fprintf(fp, "// Key: This field can be requested and written to by string name at load time\n");
	fprintf(fp, "// Input: This field can be written to by string name at run time, and a function called\n");
	fprintf(fp, "// Output: This field propogates it's value to all targets whenever it changes\n");
	fprintf(fp, "// FunctionTable: This is a table entry for a member function pointer\n");
	fprintf(fp, "// Ptr: This field is a pointer, not an embedded object\n");
	fprintf(fp, "// Override: The field is an override for one in a base class (only used by prediction system for now)\n");

	fprintf(fp, "//\n\n");
	
	IServerNetworkable *entity = dictionary->Create(cls);
	
	if(entity) {
		datamap_t *pMap = gamehelpers->GetDataMap(entity->GetBaseEntity());

		fprintf(fp,"%s - %s\n", pMap->dataClassName, cls);

		UTIL_DrawDataTable(fp, pMap);

		static int offsEFlags = -1;
		if (offsEFlags == -1)
		{
			sm_datatable_info_t info;
			gamehelpers->FindDataMapInfo(pMap, "m_iEFlags", &info);

			offsEFlags = info.actual_offset;
		}
		
		int *eflags = (int *)((char *)entity->GetBaseEntity() + offsEFlags);
		*eflags |= (1<<0); // EFL_KILLME
	}

	fclose(fp);

}
