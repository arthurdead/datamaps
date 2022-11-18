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
#define GAME_DLL

#include <string>
#include <string_view>
#include <vector>
#include <unordered_map>

using namespace std::literals::string_literals;
using namespace std::literals::string_view_literals;

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
int g_iNumServerClasses = 0;
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
SH_DECL_MANUALHOOK0_void(UpdateOnRemove, 0, 0, 0)

class CBaseEntity : public IServerEntity
{
public:
	DECLARE_CLASS_NOBASE( CBaseEntity );
	DECLARE_SERVERCLASS();
	DECLARE_DATADESC();

	edict_t *edict()
	{ return GetNetworkable()->GetEdict(); }

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

	void DispatchUpdateTransmitState()
	{

	}

	void AddIEFlags(int flags)
	{
		if(m_iEFlagsOffset == -1) {
			sm_datatable_info_t info{};
			datamap_t *pMap = gamehelpers->GetDataMap(this);
			gamehelpers->FindDataMapInfo(pMap, "m_iEFlags", &info);
			m_iEFlagsOffset = info.actual_offset;
		}

		*(int *)((unsigned char *)this + m_iEFlagsOffset) |= flags;

		if ( flags & ( EFL_FORCE_CHECK_TRANSMIT | EFL_IN_SKYBOX ) )
		{
			DispatchUpdateTransmitState();
		}
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
	std::vector<int> hookids{};

	callback_holder_t(CBaseEntity *pEntity, int ref_);
	~callback_holder_t();

	void removed(CBaseEntity *pEntity);

	void HookEntityRemoved();
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

void callback_holder_t::HookEntityRemoved()
{
	CBaseEntity *pEntity = META_IFACEPTR(CBaseEntity);
	int this_ref = gamehelpers->EntityToReference(pEntity);
	removed(pEntity);
	callbackmap.erase(this_ref);
	delete this;
	RETURN_META(MRES_HANDLED);
}

callback_holder_t::callback_holder_t(CBaseEntity *pEntity, int ref_)
	: ref{ref_}
{
	hookids.emplace_back(SH_ADD_MANUALHOOK(UpdateOnRemove, pEntity, SH_MEMBER(this, &callback_holder_t::HookEntityRemoved), false));

	CBaseEntity::m_pfnThink_t old_think{pEntity->GetThinkFunc()};
	if(old_think != &CBaseEntity::PluginThink) {
		think.old_think = old_think;
	}

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
}

void callback_holder_t::removed(CBaseEntity *pEntity)
{
	pEntity->SetThink(think.old_think, 0.0f);

	for(const auto &it : thinkctxs) {
		pEntity->SetContextThink(it.second.old_think, 0.0f, it.first.c_str());
	}

	thinkctxs.clear();

	for(int id : hookids) {
		SH_REMOVE_HOOK_ID(id);
	}
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

CBaseEntity *FindEntityByClassname(CBaseEntity *pEntity, const char *name)
{
#if SOURCE_ENGINE == SE_TF2
	return servertools->FindEntityByClassname(pEntity, name);
#elif SOURCE_ENGINE == SE_LEFT4DEAD2
	return call_mfunc<CBaseEntity *, CBaseEntityList, CBaseEntity *, const char *>(g_pEntityList, CGlobalEntityListFindEntityByClassname, pEntity, name);
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

void remove_all_entities(const char *name)
{
	CBaseEntity *pEntity = nullptr;
	while((pEntity = FindEntityByClassname(pEntity, name)) != nullptr) {
		RemoveEntity(pEntity);
	}
}

template <typename T>
void loop_all_entities(T func, const char *name)
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
	
	IEntityFactory *get_name_factory(const char *name)
	{
		for(int i = 0; i < m_Factories.Count(); i++) {
			if(strcmp(m_Factories.GetElementName(i), name) == 0) {
				return m_Factories[i];
			}
		}
		return nullptr;
	}
	
	void remove_factory(IEntityFactory *fac, const std::string &name);
	
	void remove_factory(const std::string &name)
	{
		remove_factory(get_name_factory(name.c_str()), name);
	}
	
	void remove_factory(IEntityFactory *fac)
	{
		remove_factory(fac, get_factory_name(fac));
	}
	
	static bool is_factory_custom(IEntityFactory *fac);

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

	custom_typedescription_t(const custom_typedescription_t &) = delete;
	custom_typedescription_t &operator=(const custom_typedescription_t &) = delete;

	custom_typedescription_t(custom_typedescription_t &&other)
	{ operator=(std::move(other)); }

	custom_typedescription_t &operator=(custom_typedescription_t &&other)
	{
		const char *pFieldName = other.fieldName;
		typedescription_t::operator=(std::move(other));
		fieldName = pFieldName;
		other.fieldName = nullptr;
		return *this;
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
		clear_name();

		get_offset() = 0;
		fieldType = FIELD_VOID;
		fieldSize = 0;
		flags = 0;
		fieldSizeInBytes = 0;
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

static_assert(sizeof(custom_typedescription_t) == sizeof(typedescription_t));
static_assert(alignof(custom_typedescription_t) == alignof(typedescription_t));

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

		dataDesc = nullptr;
		dataNumFields = 0;
		baseMap = nullptr;
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

static_assert(sizeof(custom_datamap_t) == sizeof(datamap_t));
static_assert(alignof(custom_datamap_t) == alignof(datamap_t));

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
std::vector<int> svcls_hooks{};

class hookobj_t
{
public:
	struct hookinfo_t
	{
		std::vector<int> hookids{};
		std::vector<int> hookids_late{};

		~hookinfo_t()
		{
			for(int id : hookids) {
				SH_REMOVE_HOOK_ID(id);
			}

			for(int id : hookids_late) {
				SH_REMOVE_HOOK_ID(id);
			}
		}

		void remove_all()
		{
			for(int id : hookids) {
				SH_REMOVE_HOOK_ID(id);
			}
			hookids.clear();
		}

		void remove_all_late()
		{
			for(int id : hookids_late) {
				SH_REMOVE_HOOK_ID(id);
			}
			hookids_late.clear();
		}
	};
	std::unordered_map<int, hookinfo_t> entities{};

	hookinfo_t &add_hooks(CBaseEntity *pEntity)
	{
		int ref = gamehelpers->EntityToReference(pEntity);

		auto it_objs{hookobjs.find(ref)};
		if(it_objs == hookobjs.cend()) {
			it_objs = hookobjs.emplace(ref, std::vector<hookobj_t *>{}).first;
		}
		it_objs->second.emplace_back(this);

		hookinfo_t info{};
		info.hookids.emplace_back(SH_ADD_MANUALHOOK(UpdateOnRemove, pEntity, SH_MEMBER(this, &hookobj_t::HookEntityRemoved), false));
		return (*entities.emplace(ref, std::move(info)).first).second;
	}

	void HookEntityRemoved()
	{
		CBaseEntity *pEntity = META_IFACEPTR(CBaseEntity);

		int ref = gamehelpers->EntityToReference(pEntity);

		auto it{entities.find(ref)};
		if(it != entities.end()) {
			it->second.remove_all();
			hooks_removed(pEntity, ref);
		}

		SH_MCALL(pEntity, UpdateOnRemove)();

		if(it != entities.end()) {
			it->second.remove_all_late();
			hooks_removed_late(pEntity, ref);
			entities.erase(it);
		}

		RETURN_META(MRES_SUPERCEDE);
	}

	void remove_all_hooks()
	{
		for(auto it : entities) {
			it.second.remove_all();
			it.second.remove_all_late();
			CBaseEntity *pEntity = gamehelpers->ReferenceToEntity(it.first);
			if(pEntity) {
				hooks_removed(pEntity, it.first);
				hooks_removed_late(pEntity, it.first);
				RemoveEntity(pEntity);
			}
		}

		entities.clear();
	}

	virtual ~hookobj_t()
	{
		remove_all_hooks();
	}

	virtual void hooks_removed_late(CBaseEntity *pEntity, int ref)
	{

	}

	virtual void hooks_removed(CBaseEntity *pEntity, int ref)
	{
		auto it_objs{hookobjs.find(ref)};
		if(it_objs != hookobjs.end()) {
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
};

struct custom_prop_info_t : public hookobj_t
{
	bool was_overriden = false;
	IEntityFactory *hooked_fac = nullptr;
	bool fac_is_sp = false;
	custom_datamap_t map{};
	using dataDesc_t = std::vector<custom_typedescription_t>;
	dataDesc_t dataDesc{};
	int size = 0;
	std::string clsname{};
	Handle_t hndl = BAD_HANDLE;
	IPluginContext *pContext = nullptr;
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
	
	bool has_prop(const char *name)
	{
		for(custom_typedescription_t &desc : dataDesc) {
			if(strcmp(desc.fieldName, name) == 0) {
				return true;
			}
		}
		
		return false;
	}
	
	void remove_prop(const char *name)
	{
		bool removed = false;
		
		dataDesc_t::iterator it{dataDesc.begin()};
		while(it != dataDesc.end()) {
			if(strcmp(it->fieldName, name) == 0) {
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
	
	void add_prop(const char *name, fieldtype_t type, int num = 1, int flags = 0)
	{
		dataDesc.emplace_back();
		custom_typedescription_t &desc = dataDesc.back();
		desc.zero();
		
		desc.set_name(name);
		
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
		
		desc.get_offset() = size;
		
		size += desc.fieldSizeInBytes;
		
		map.dataDesc = (typedescription_t *)dataDesc.data();
		++map.dataNumFields;
	}
	
	datamap_t *HookGetDataDescMap()
	{
		RETURN_META_VALUE(MRES_SUPERCEDE, &map);
	}

	void factory_removed(IEntityFactory *fac);

	void hooks_removed(CBaseEntity *pEntity, int ref) override
	{
		hookobj_t::hooks_removed(pEntity, ref);
	}

	void hooks_removed_late(CBaseEntity *pEntity, int ref) override
	{
		hookobj_t::hooks_removed_late(pEntity, ref);

		dtor(pEntity);
	}

	void add_hooks(CBaseEntity *pEntity)
	{
		hookinfo_t &info{hookobj_t::add_hooks(pEntity)};
		info.hookids_late.emplace_back(SH_ADD_HOOK(CBaseEntity, GetDataDescMap, pEntity, SH_MEMBER(this, &custom_prop_info_t::HookGetDataDescMap), false));
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

		m_pTable = nullptr;
		m_pNext = nullptr;
	}

	const char*	GetName() { return m_pNetworkName; }
	
	const char					*m_pNetworkName;
	SendTable					*m_pTable;
	ServerClass					*m_pNext;
	int							m_ClassID;	// Managed by the engine.

	// This is an index into the network string table (sv.GetInstanceBaselineTable()).
	int							m_InstanceBaselineIndex; // INVALID_STRING_INDEX if not initialized yet.
};

static_assert(sizeof(custom_ServerClass) == sizeof(ServerClass));
static_assert(alignof(custom_ServerClass) == alignof(ServerClass));

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

		m_pProps = nullptr;
		m_nProps = 0;
	}
};

static_assert(sizeof(custom_SendTable) == sizeof(SendTable));
static_assert(alignof(custom_SendTable) == alignof(SendTable));

extern float AssignRangeMultiplier( int nBits, double range );

static const CStandardSendProxies *std_proxies{nullptr};

void SendProxy_EHandleToInt( const SendProp *pProp, const void *pStruct, const void *pVarData, DVariant *pOut, int iElement, int objectID)
{
	CBaseHandle *pHandle = (CBaseHandle*)pVarData;

	if ( pHandle && pHandle->Get() )
	{
		int iSerialNum = pHandle->GetSerialNumber() & ( (1 << NUM_NETWORKED_EHANDLE_SERIAL_NUMBER_BITS) - 1 );
		pOut->m_Int = pHandle->GetEntryIndex() | (iSerialNum << MAX_EDICT_BITS);
	}
	else
	{
		pOut->m_Int = INVALID_NETWORKED_EHANDLE_VALUE;
	}
}

static void SendProxy_Empty( const SendProp *pProp, const void *pStruct, const void *pData, DVariant *pOut, int iElement, int objectID)
{
}

class custom_SendProp : public SendProp
{
public:
	struct extra_data_t
	{
		bool absoffset = false;
		fieldtype_t type = FIELD_VOID;
		int elementCount = 1;
		int var_size = 0;
	};

	custom_SendProp(const custom_SendProp &) = delete;
	custom_SendProp &operator=(const custom_SendProp &) = delete;

	custom_SendProp(custom_SendProp &&other)
	{ operator=(std::move(other)); }

	custom_SendProp &operator=(custom_SendProp &&other)
	{
		const void *pExtraData = other.GetExtraData();
		const char *pVarName = other.m_pVarName;
		SendProp::operator=(std::move(other));
		m_pVarName = pVarName;
		other.m_pVarName = nullptr;
		SetExtraData(pExtraData);
		other.SetExtraData(nullptr);
		return *this;
	}

	void set_name(const std::string &name)
	{
		clear_name();
		
		size_t len = name.length();
		m_pVarName = (char *)malloc(len+1);
		strncpy((char *)m_pVarName, name.c_str(), len);
		((char *)m_pVarName)[len] = '\0';
	}
	
	void clear_name()
	{
		if(m_pVarName != nullptr) {
			free((void *)m_pVarName);
		}
		m_pVarName = nullptr;
	}

	custom_SendProp()
		: SendProp{}
	{
		m_pVarName = nullptr;

		SetExtraData(new extra_data_t{});
	}

	~custom_SendProp()
	{
		clear_name();

		delete (extra_data_t *)GetExtraData();

		m_Type = DPT_DataTable;
		m_nBits = 0;
		m_pArrayProp = nullptr;
		m_ArrayLengthProxy = nullptr;
		m_nElements = 0;
		m_ElementStride = 0;

		SetFlags(0);
		SetOffset(0);
		SetProxyFn(nullptr);
		SetDataTableProxyFn(nullptr);
		SetDataTable(nullptr);
		SetExtraData(nullptr);
	}

	extra_data_t &extra_data()
	{ return *(extra_data_t *)GetExtraData(); }
};

static_assert(sizeof(custom_SendProp) == sizeof(SendProp));
static_assert(alignof(custom_SendProp) == alignof(SendProp));

char *UTIL_SendFlagsToString(int flags, int type);

template<>
FORCEINLINE void NetworkVarConstruct<QAngle>( QAngle &x ) { x = QAngle(); }

class NetworkVar_Generic
{
public:
	static inline void NetworkStateChanged(void *ptr)
	{
	}
};

template <typename T>
class NetworkVar_ArrayUnknownSize
{
public:
	static size_t size(size_t S)
	{
		return (sizeof(T) * S);
	}
	inline NetworkVar_ArrayUnknownSize(size_t S)
	{
		for ( int i = 0 ; i < S ; ++i ) {
			NetworkVarConstruct( GetPtr()[i] );
		}
	}
	void Destruct(size_t S)
	{
		for ( int i = 0 ; i < S ; ++i ) {
			GetPtr()[i].~T();
		}
	}
	const T& operator[]( int i ) const
	{
		return Get( i );
	}
	const T& Get( int i ) const
	{
		return GetPtr()[i];
	}
	T& GetForModify( int i )
	{
		NetworkStateChanged( i );
		return GetPtr()[i];
	}
	void Set( int i, const T &val )
	{
		if( memcmp( &GetPtr()[i], &val, sizeof(T) ) )
		{
			NetworkStateChanged( i );
			GetPtr()[i] = val;
		}
	}
	const T* Base() const { return GetPtr(); }
protected:
	inline void NetworkStateChanged( int net_change_index )
	{
	}
	T *GetPtr()
	{ return (T *)this; }
};

#include <tier1/checksum_crc.h>

CRC32_t *g_SendTableCRC{nullptr};
CUtlVector<SendTable *> *g_SendTables{nullptr};

CRC32_t SendTable_ComputeCRC();

struct serverclass_override_t : public hookobj_t
{
	serverclass_override_t(IEntityFactory *fac_, std::string &&clsname_, ServerClass *realcls_);
	~serverclass_override_t() override;
	
	IServerNetworkable *HookCreate(const char *classname);
	
	ServerClass *HookGetServerClass()
	{
		RETURN_META_VALUE(MRES_SUPERCEDE, (ServerClass *)&cls);
	}

	void factory_removed(IEntityFactory *fac);

	void hooks_removed(CBaseEntity *pEntity, int ref) override
	{
		hookobj_t::hooks_removed(pEntity, ref);
	}

	void hooks_removed_late(CBaseEntity *pEntity, int ref) override
	{
		hookobj_t::hooks_removed_late(pEntity, ref);

		dtor(pEntity);

		auto hsvcls_it{std::find(svcls_hooks.begin(), svcls_hooks.end(), ref)};
		if(hsvcls_it != svcls_hooks.end()) {
			svcls_hooks.erase(hsvcls_it);
		}
	}

	void add_hooks(CBaseEntity *pEntity, IServerNetworkable *pNet)
	{
		hookinfo_t &info{hookobj_t::add_hooks(pEntity)};
		info.hookids_late.emplace_back(SH_ADD_HOOK(CBaseEntity, GetServerClass, pEntity, SH_MEMBER(this, &serverclass_override_t::HookGetServerClass), false));
		info.hookids_late.emplace_back(SH_ADD_HOOK(IServerNetworkable, GetServerClass, pNet, SH_MEMBER(this, &serverclass_override_t::HookGetServerClass), false));

		svcls_hooks.emplace_back(gamehelpers->EntityToReference(pEntity));
	}

	void setup_datatable();
	void term_datatable();

	void do_override(int &base, CBaseEntity *pEntity, IServerNetworkable *pNet);

	void set_client_class_id(ServerClass *netclass);
	void set_client_class_name(std::string &&name);
	void set_client_table_name(std::string &&name);

	void set_base_class(SendTable *table);
	void unexclude_prop(SendProp *prop, SendProp *realprop);
	
	void init();

	void init_classid();

	void update_props()
	{
		tbl.m_pProps = (SendProp *)props.data();
		tbl.m_nProps = props.size();
	}

	template <typename T>
	using netvar_t = CNetworkVarBase<T, NetworkVar_Generic>;

	template <typename T>
	using netvar_vec_t = CNetworkVectorBase<T, NetworkVar_Generic>;

	template <typename T>
	using netvar_ehndl_t = CNetworkHandleBase<T, NetworkVar_Generic>;

	template <typename T>
	using netvar_arr_t = NetworkVar_ArrayUnknownSize<T>;

	void dtor(CBaseEntity *pEntity)
	{
		for(std::size_t i{1}; i < props.size(); ++i) {
			custom_SendProp &prop{props[i]};
			int offset = prop.GetOffset();
			custom_SendProp::extra_data_t &data{prop.extra_data()};
			unsigned char *ptr = (((unsigned char *)pEntity) + offset);
			switch(data.type) {
				case FIELD_VECTOR: {
					bool ang = (prop.m_fLowValue == 0.0f && prop.m_fHighValue == 360.0f);
					if(ang) {
						if(data.elementCount == 1) {
							((netvar_vec_t<QAngle> *)ptr)->~netvar_vec_t<QAngle>();
						} else {
							((netvar_arr_t<QAngle> *)ptr)->Destruct(data.elementCount);
						}
					} else {
						if(data.elementCount == 1) {
							((netvar_vec_t<Vector> *)ptr)->~netvar_vec_t<Vector>();
						} else {
							((netvar_arr_t<Vector> *)ptr)->Destruct(data.elementCount);
						}
					}
				} break;
				case FIELD_POSITION_VECTOR: {
					if(data.elementCount == 1) {
						((netvar_vec_t<Vector> *)ptr)->~netvar_vec_t<Vector>();
					} else {
						((netvar_arr_t<Vector> *)ptr)->Destruct(data.elementCount);
					}
				} break;
				case FIELD_FLOAT: {
					if(data.elementCount == 1) {
						((netvar_t<float> *)ptr)->~netvar_t<float>();
					} else {
						((netvar_arr_t<float> *)ptr)->Destruct(data.elementCount);
					}
				} break;
				case FIELD_EHANDLE: {
					if(data.elementCount == 1) {
						((netvar_ehndl_t<EHANDLE> *)ptr)->~netvar_ehndl_t<EHANDLE>();
					} else {
						((netvar_arr_t<EHANDLE> *)ptr)->Destruct(data.elementCount);
					}
				} break;
				case FIELD_BOOLEAN: {
					if(data.elementCount == 1) {
						((netvar_t<bool> *)ptr)->~netvar_t<bool>();
					} else {
						((netvar_arr_t<bool> *)ptr)->Destruct(data.elementCount);
					}
				} break;
				case FIELD_SHORT: {
					if(data.elementCount == 1) {
						((netvar_t<short> *)ptr)->~netvar_t<short>();
					} else {
						((netvar_arr_t<short> *)ptr)->Destruct(data.elementCount);
					}
				} break;
				case FIELD_INTEGER: {
					if(data.elementCount == 1) {
						((netvar_t<int> *)ptr)->~netvar_t<int>();
					} else {
						((netvar_arr_t<int> *)ptr)->Destruct(data.elementCount);
					}
				} break;
			}
		}
	}

	void zero(CBaseEntity *pEntity)
	{
		for(std::size_t i{1}; i < props.size(); ++i) {
			custom_SendProp &prop{props[i]};
			int offset = prop.GetOffset();
			custom_SendProp::extra_data_t &data{prop.extra_data()};
			unsigned char *ptr = (((unsigned char *)pEntity) + offset);
			switch(data.type) {
				case FIELD_VECTOR: {
					bool ang = (prop.m_fLowValue == 0.0f && prop.m_fHighValue == 360.0f);
					if(ang) {
						if(data.elementCount == 1) {
							new (ptr) netvar_vec_t<QAngle>{};
						} else {
							new (ptr) netvar_arr_t<QAngle>{data.elementCount};
						}
					} else {
						if(data.elementCount == 1) {
							new (ptr) netvar_vec_t<Vector>{};
						} else {
							new (ptr) netvar_arr_t<Vector>{data.elementCount};
						}
					}
				} break;
				case FIELD_POSITION_VECTOR: {
					if(data.elementCount == 1) {
						new (ptr) netvar_vec_t<Vector>{};
					} else {
						new (ptr) netvar_arr_t<Vector>{data.elementCount};
					}
				} break;
				case FIELD_FLOAT: {
					if(data.elementCount == 1) {
						new (ptr) netvar_t<float>{};
					} else {
						new (ptr) netvar_arr_t<float>{data.elementCount};
					}
				} break;
				case FIELD_EHANDLE: {
					if(data.elementCount == 1) {
						new (ptr) netvar_ehndl_t<EHANDLE>{};
					} else {
						new (ptr) netvar_arr_t<EHANDLE>{data.elementCount};
					}
				} break;
				case FIELD_BOOLEAN: {
					if(data.elementCount == 1) {
						new (ptr) netvar_t<bool>{};
					} else {
						new (ptr) netvar_arr_t<bool>{data.elementCount};
					}
				} break;
				case FIELD_SHORT: {
					if(data.elementCount == 1) {
						new (ptr) netvar_t<short>{};
					} else {
						new (ptr) netvar_arr_t<short>{data.elementCount};
					}
				} break;
				case FIELD_INTEGER: {
					if(data.elementCount == 1) {
						new (ptr) netvar_t<int>{};
					} else {
						new (ptr) netvar_arr_t<int>{data.elementCount};
					}
				} break;
			}
		}
	}

	void calc_size(int &base);

	void update_offsets(int &base);

	custom_SendProp *emplace_prop()
	{
		custom_SendProp *prop = &props.emplace_back();
		update_props();
		return prop;
	}

	template <typename T>
	int get_var_size(int elementCount)
	{
		if(elementCount == 1) {
			return sizeof(netvar_t<T>);
		} else {
			return netvar_arr_t<T>::size(elementCount);
		}
	}

	template <>
	int get_var_size<EHANDLE>(int elementCount)
	{
		if(elementCount == 1) {
			return sizeof(netvar_ehndl_t<EHANDLE>);
		} else {
			return netvar_arr_t<EHANDLE>::size(elementCount);
		}
	}

	template <>
	int get_var_size<QAngle>(int elementCount)
	{
		if(elementCount == 1) {
			return sizeof(netvar_vec_t<QAngle>);
		} else {
			return netvar_arr_t<QAngle>::size(elementCount);
		}
	}

	template <>
	int get_var_size<Vector>(int elementCount)
	{
		if(elementCount == 1) {
			return sizeof(netvar_vec_t<Vector>);
		} else {
			return netvar_arr_t<Vector>::size(elementCount);
		}
	}

	#define PROP_OFFSET_EXISTING -1
	#define PROP_OFFSET_NEW -2

	int get_prop_offset(const char *cls, const char *name)
	{
		sm_sendprop_info_t info{};
		if(gamehelpers->FindSendPropInfo(cls, name, &info)) {
			if(info.prop->GetType() == DPT_Array) {
				return info.prop->GetArrayProp()->GetOffset();
			} else {
				return info.actual_offset;
			}
		}
		return -1;
	}

	int get_prop_offset(const char *name)
	{
		int offset = -1;
		if(cl_classid_cls) {
			offset = get_prop_offset(cl_classid_cls->m_pNetworkName, name);
			if(offset != -1) {
				return offset;
			}

			if(!cls_cl_name.empty() && strcmp(cls_cl_name.c_str(), cl_classid_cls->m_pNetworkName) != 0) {
				offset = get_prop_offset(cls_cl_name.c_str(), name);
				if(offset != -1) {
					return offset;
				}
			}
		} else if(!cls_cl_name.empty()) {
			offset = get_prop_offset(cls_cl_name.c_str(), name);
			if(offset != -1) {
				return offset;
			}
		}
		if(realcls) {
			offset = get_prop_offset(realcls->m_pNetworkName, name);
			if(offset != -1) {
				return offset;
			}
		}
		return -1;
	}

	template <typename T>
	custom_SendProp *emplace_prop(const std::string &name, int offset, int elementCount)
	{
		custom_SendProp *prop = &props.emplace_back();
		update_props();
		prop->set_name(name);
		int var_size{get_var_size<T>(elementCount)};
		custom_SendProp::extra_data_t &data{prop->extra_data()};
		if(offset == PROP_OFFSET_EXISTING) {
			offset = get_prop_offset(name.c_str());
			if(offset == -1) {
				offset = PROP_OFFSET_NEW;
			}
		}
		if(offset >= 0) {
			prop->SetOffset(offset);
			data.absoffset = true;
			int final_offset{offset + var_size};
			if(final_offset > largest_offset) {
				largest_offset = final_offset;
			}
		} else if(offset == PROP_OFFSET_NEW) {
			prop->SetOffset(size);
			size += var_size;
		}
		data.var_size = var_size;
		data.elementCount = elementCount;
		return prop;
	}

	custom_SendProp &add_prop_array(const std::string &name, int elementCount, int elementStride)
	{
		custom_SendProp &prop{*emplace_prop()};

		prop.set_name(name);

		prop.SetProxyFn(SendProxy_Empty);

		prop.m_Type = DPT_Array;
		prop.m_nElements = elementCount;
		prop.m_ElementStride = elementStride;

		prop.m_pArrayProp = nullptr;

		prop.SetArrayLengthProxy( nullptr );

		return prop;
	}

	void setup_array(const std::string &name, custom_SendProp &prop, int elementCount, int elementStride)
	{
		if(elementCount <= 1) {
			return;
		}

		add_prop_array(name, elementCount, elementStride);
	}

	void add_prop_qangles(const std::string &name, int nBits, int flags, int elementCount, int offset)
	{
		if(nBits == 32) {
			flags |= SPROP_NOSCALE;
		}

		custom_SendProp &prop{*emplace_prop<QAngle>(name, offset, elementCount)};

		prop.SetFlags(flags);

		prop.SetProxyFn(SendProxy_QAngles);

		prop.m_nBits = nBits;

		prop.m_fLowValue = 0.0f;
		prop.m_fHighValue = 360.0f;
		prop.m_fHighLowMul = AssignRangeMultiplier(nBits, 360.0f);
		prop.m_Type = DPT_Vector;

		prop.extra_data().type = FIELD_VECTOR;

		int elementStride = sizeof(QAngle);

		setup_array(name, prop, elementCount, elementStride);
	}

	void add_prop_vector(const std::string &name, float fLowValue, float fHighValue, int nBits, int flags, int elementCount, int offset)
	{
		if(nBits == 32) {
			flags |= SPROP_NOSCALE;
		}

		int actual_bits{nBits};
		if(flags & (SPROP_COORD|SPROP_NOSCALE|SPROP_NORMAL|SPROP_COORD_MP|SPROP_COORD_MP_LOWPRECISION|SPROP_COORD_MP_INTEGRAL)) {
			actual_bits = 0;
		}

		custom_SendProp &prop{*emplace_prop<Vector>(name, offset, elementCount)};

		prop.SetFlags(flags);

		prop.SetProxyFn(std_proxies->m_VectorToVector);

		prop.m_nBits = actual_bits;

		prop.m_fLowValue = fLowValue;
		prop.m_fHighValue = fHighValue;
		prop.m_fHighLowMul = AssignRangeMultiplier(nBits, fHighValue - fLowValue);
		prop.m_Type = DPT_Vector;

		if(flags & (SPROP_COORD|SPROP_COORD_MP|SPROP_COORD_MP_LOWPRECISION|SPROP_COORD_MP_INTEGRAL)) {
			prop.extra_data().type = FIELD_POSITION_VECTOR;
		} else {
			prop.extra_data().type = FIELD_VECTOR;
		}

		int elementStride = sizeof(Vector);

		setup_array(name, prop, elementCount, elementStride);
	}

	void add_prop_float(const std::string &name, float fLowValue, float fHighValue, int nBits, int flags, int elementCount, int offset)
	{
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

		int actual_bits{nBits};
		if(flags & (SPROP_COORD|SPROP_NOSCALE|SPROP_NORMAL|SPROP_COORD_MP|SPROP_COORD_MP_LOWPRECISION|SPROP_COORD_MP_INTEGRAL)) {
			actual_bits = 0;
		}

		custom_SendProp &prop{*emplace_prop<float>(name, offset, elementCount)};

		prop.SetProxyFn(std_proxies->m_FloatToFloat);

		prop.SetFlags(flags);

		prop.m_nBits = actual_bits;

		prop.m_fLowValue = fLowValue;
		prop.m_fHighValue = fHighValue;
		prop.m_fHighLowMul = AssignRangeMultiplier(nBits, fHighValue - fLowValue);
		prop.m_Type = DPT_Float;

		prop.extra_data().type = FIELD_FLOAT;

		int elementStride = sizeof(float);

		setup_array(name, prop, elementCount, elementStride);
	}

	custom_SendProp &add_prop_int(const std::string &name, int sizeofVar, int nBits, int flags, SendVarProxyFn proxy, int elementCount, int offset)
	{
		if(nBits <= 0) {
			nBits = (sizeofVar * 8);
		}

		custom_SendProp *prop{nullptr};

		if(sizeofVar == sizeof(EHANDLE) &&
			nBits == NUM_NETWORKED_EHANDLE_BITS &&
			proxy == SendProxy_EHandleToInt &&
			(flags & SPROP_UNSIGNED)) {
			prop = emplace_prop<EHANDLE>(name, offset, elementCount);

			prop->extra_data().type = FIELD_EHANDLE;
		} else {
			switch(sizeofVar) {
				case sizeof(bool): {
					prop = emplace_prop<bool>(name, offset, elementCount);

					prop->extra_data().type = FIELD_BOOLEAN;
				} break;
				case sizeof(short): {
					prop = emplace_prop<short>(name, offset, elementCount);

					prop->extra_data().type = FIELD_SHORT;
				} break;
				case sizeof(int): {
					prop = emplace_prop<int>(name, offset, elementCount);

					prop->extra_data().type = FIELD_INTEGER;
				} break;
			}
		}

		prop->SetFlags(flags);
		prop->SetProxyFn(proxy);
		prop->m_nBits = nBits;
		prop->m_Type = DPT_Int;

		int elementStride = sizeofVar;

		setup_array(name, *prop, elementCount, elementStride);

		return *prop;
	}

	void add_prop_ehandle(const std::string &name, int flags, int elementCount, int offset)
	{
		add_prop_int(name, sizeof(EHANDLE), NUM_NETWORKED_EHANDLE_BITS, SPROP_UNSIGNED|flags, SendProxy_EHandleToInt, elementCount, offset);
	}

	void add_prop_int(const std::string &name, int sizeofVar, int nBits, int flags, int elementCount, int offset)
	{
		SendVarProxyFn proxy{nullptr};

		switch(sizeofVar) {
			case sizeof(bool): {
				proxy = ((flags & SPROP_UNSIGNED) ? std_proxies->m_UInt8ToInt32 : std_proxies->m_Int8ToInt32);
			}
			case sizeof(short): {
				proxy = ((flags & SPROP_UNSIGNED) ? std_proxies->m_UInt16ToInt32 : std_proxies->m_Int16ToInt32);
			}
			case sizeof(int): {
				proxy = ((flags & SPROP_UNSIGNED) ? std_proxies->m_UInt32ToInt32 : std_proxies->m_Int32ToInt32);
			}
		}

		add_prop_int(name, sizeofVar, nBits, flags, proxy, elementCount, offset);
	}

	IEntityFactory *hooked_fac = nullptr;
	bool fac_is_sp = false;
	std::string cls_name{};
	custom_ServerClass cls{};
	std::string cls_cl_name{};
	std::string tbl_name{};
	custom_SendTable tbl{};
	std::string tbl_cl_name{};
	int cl_classid = -1;
	int classid = -1;
	std::vector<custom_SendProp> props{};
	ServerClass *realcls = nullptr;
	ServerClass *cl_classid_cls = nullptr;
	std::string clsname{};
	Handle_t hndl = BAD_HANDLE;
	IPluginContext *pContext = nullptr;
	bool freehndl = true;
	bool was_overriden = false;
	bool cls_inited = false;
	bool base_class_set = false;
	SendProp *m_pProps = nullptr;
	int size = 0;
	int largest_offset = 0;
	std::vector<unexclude_prop_t> exclude_props{};
	size_t counterid = 0;
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
	size_t GetEntitySize() {
		int realsiz = size;
		if(custom_server) {
			custom_server->calc_size(realsiz);
		}
		if(custom_prop) {
			realsiz += custom_prop->size;
		}
		return realsiz;
	}
	
	std::string name{};
	IEntityFactory *based = nullptr;
	IPluginFunction *func = nullptr;
	size_t size = 0;
	cell_t data = 0;

	std::vector<std::string> aliases{};

	ServerClass *svclass{nullptr};

	custom_prop_info_t *custom_prop = nullptr;
	serverclass_override_t *custom_server = nullptr;
	
	Handle_t hndl = BAD_HANDLE;
	IPluginContext *pContext = nullptr;
	bool freehndl = true;
	bool dont_delete = false;
};

bool CEntityFactoryDictionary::is_factory_custom(IEntityFactory *fac)
{
	return dynamic_cast<sp_entity_factory *>(fac) != nullptr;
}

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
	
	dictionary->remove_factory(this, name);
	
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

std::unordered_map<int, ServerClass *> baselinemap{};

using info_map_t = std::unordered_map<std::string, custom_prop_info_t *>;
info_map_t info_map{};

using server_map_t = std::unordered_map<std::string, serverclass_override_t *>;
server_map_t server_map{};

using server_ptr_map_t = std::unordered_map<ServerClass *, serverclass_override_t *>;
server_ptr_map_t server_ptr_map{};

void CEntityFactoryDictionary::remove_factory(IEntityFactory *fac, const std::string &name)
{
	sp_entity_factory *sp_fac{nullptr};
	if(CEntityFactoryDictionary::is_factory_custom(fac)) {
		sp_fac = (sp_entity_factory *)fac;
	}

	info_map_t::iterator info_it{info_map.find(name)};
	if(info_it != info_map.end()) {
		custom_prop_info_t *prop{info_it->second};
		if(prop->hooked_fac == fac) {
			if(sp_fac) {
				sp_fac->custom_server = nullptr;
			} else {
				prop->factory_removed(fac);
			}
		}
	}

	server_map_t::iterator server_it{server_map.find(name)};
	if(server_it != server_map.end()) {
		serverclass_override_t *prop{server_it->second};
		if(prop->hooked_fac == fac) {
			if(sp_fac) {
				sp_fac->custom_server = nullptr;
			} else {
				prop->factory_removed(fac);
			}
		}
	}

	m_Factories.Remove(name.c_str());

	if(sp_fac && !sp_fac->dont_delete) {
		delete sp_fac;
	}
}

ServerClass *custom_server_head = nullptr;

class CNetworkStringTableContainer;
enum server_state_t : int;
class CEventInfo;

#include <igameevents.h>
#include <iclient.h>

class CBaseClient : public IGameEventListener2, public IClient, public IClientMessageHandler
{
public:
	
};

class CGameClient : public CBaseClient
{
public:
	
};

class CClientFrame;

class CBaseServer : public IServer
{
public:
	virtual float	GetCPUUsage( void ) = 0;
	virtual void	BroadcastPrintf ( PRINTF_FORMAT_STRING const char *fmt, ...) = 0;
	virtual void	SetMaxClients( int number ) = 0;
	virtual void	WriteDeltaEntities( CBaseClient *client, CClientFrame *to, CClientFrame *from,	bf_write &pBuf ) = 0;

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

#if SOURCE_ENGINE == SE_TF2
	int			m_nUserid;			// increases by one with every new client

	int			m_nMaxclients;         // Current max #
	int			m_nSpawnCount;			// Number of servers spawned since start,
									// used to check late spawns (e.g., when d/l'ing lots of
									// data)
	float		m_flTickInterval;		// time for 1 tick in seconds

	CUtlVector<CBaseClient*>	m_Clients;		// array of up to [maxclients] client slots.
	
	bool		m_bIsDedicated;

	uint32		m_CurrentRandomNonce;
	uint32		m_LastRandomNonce;
	float		m_flLastRandomNumberGenerationTime;
	float		m_fCPUPercent;
	float		m_fStartTime;
	float		m_fLastCPUCheckTime;

	// This is only used for Steam's master server updater to refer to this server uniquely.
	bool		m_bRestartOnLevelChange;
	
	bool		m_bMasterServerRulesDirty;
	double		m_flLastMasterServerUpdateTime;

	int			m_nNumConnections;		//Number of successful client connections.

	bool		m_bReportNewFakeClients; // Whether or not newly created fake clients should be included in server browser totals
	float		m_flPausedTimeEnd;
#endif

	void increment_svclasses()
	{
		++serverclasses;
		serverclassbits = Q_log2( serverclasses ) + 1;
	}
	
	void decrement_svclasses()
	{
		if(--serverclasses > 0) {
			serverclassbits = Q_log2( serverclasses ) + 1;
		} else {
			serverclassbits = 0;
		}
	}
};

class CGameServer : public CBaseServer
{
public:
	bool		m_bLoadgame;			// handle connections specially
	
	char		m_szStartspot[64];
	
	int			num_edicts;
	int			max_edicts;
	int			free_edicts; // how many edicts in num_edicts are free, in use is num_edicts - free_edicts
	edict_t		*edicts;			// Can array index now, edict_t is fixed
	IChangeInfoAccessor *edictchangeinfo; // HACK to allow backward compat since we can't change edict_t layout

	int			m_nMaxClientsLimit;    // Max allowed on server.
	
	bool		allowsignonwrites;
	bool	    dll_initialized;    // Have we loaded the game dll.

	bool		m_bIsLevelMainMenuBackground;	// true if the level running only as the background to the main menu

	CUtlVector<CEventInfo*>	m_TempEntities;		// temp entities

	bf_write			m_FullSendTables;
	CUtlMemory<byte>	m_FullSendTablesBuffer;
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
	prop->SetDataTableProxyFn(std_proxies->m_DataTableToDataTable);
	prop->SetFlags(SPROP_PROXY_ALWAYS_YES|SPROP_COLLAPSIBLE);
	
	base_class_set = true;
}

static int classid_last{0};
static int def_classid_last{0};
static bool classids_assigned{false};

size_t classoverridecounter = 0;

static ServerClass *CBaseEntity_ServerClass = nullptr;

class CSendNode
{
public:
					CSendNode();
					~CSendNode();

	inline int GetNumChildren() const
	{
		return m_Children.Count(); 
	}

	inline CSendNode* GetChild( int i ) const
	{
		return m_Children[i];
	}

	inline unsigned short GetDataTableProxyIndex() const
	{
		return m_DataTableProxyIndex;
	}

	inline void SetDataTableProxyIndex( unsigned short val )
	{
		m_DataTableProxyIndex = val;
	}

	inline unsigned short GetRecursiveProxyIndex() const
	{
		return m_RecursiveProxyIndex;
	}

	inline void SetRecursiveProxyIndex( unsigned short val )
	{
		m_RecursiveProxyIndex = val;
	}

	// Child datatables.
	CUtlVector<CSendNode*>	m_Children;

	// The datatable property that leads us to this CSendNode.
	// This indexes the CSendTablePrecalc or CRecvDecoder's m_DatatableProps list.
	// The root CSendNode sets this to -1.
	short					m_iDatatableProp;

	// The SendTable that this node represents.
	// ALL CSendNodes have this.
	const SendTable	*m_pTable;

	//
	// Properties in this table.
	//

	// m_iFirstRecursiveProp to m_nRecursiveProps defines the list of propertise
	// of this node and all its children.
	unsigned short	m_iFirstRecursiveProp;
	unsigned short	m_nRecursiveProps;


	// See GetDataTableProxyIndex().
	unsigned short	m_DataTableProxyIndex;
	
	// See GetRecursiveProxyIndex().
	unsigned short	m_RecursiveProxyIndex;
};

CSendNode::CSendNode()
{
	m_iDatatableProp = -1;
	m_pTable = NULL;
	
	m_iFirstRecursiveProp = m_nRecursiveProps = 0;

	m_DataTableProxyIndex = DATATABLE_PROXY_INDEX_INVALID; // set it to a questionable value.
}

CSendNode::~CSendNode()
{
	int c = GetNumChildren();
	for ( int i = c - 1 ; i >= 0 ; i-- )
	{
		delete GetChild( i );
	}
	m_Children.Purge();
}

#define DELTA_DISTANCE_BAND			200
#define NUM_DELTA_DISTANCE_BANDS	(8000/DELTA_DISTANCE_BAND)

class CDTISendTable
{
public:
	// Which SendTable we're interested in.
	CUtlString		m_NetTableName;

	// How many cycles we've spent in certain calls.
	CCycleCount		m_nCalcDeltaCycles;
	int				m_nCalcDeltaCalls;

	CCycleCount		m_nEncodeCycles;
	int				m_nEncodeCalls;

	CCycleCount		m_nShouldTransmitCycles;
	int				m_nShouldTransmitCalls;

	CCycleCount		m_nWriteDeltaPropsCycles;

	// Used to determine how much the class uses manual mode.
	int m_nChangeAutoDetects;
	int m_nNoChanges;

	// This tracks how many times an entity was delta'd for each distance from a client.
	unsigned short	m_DistanceDeltaCounts[NUM_DELTA_DISTANCE_BANDS];
};

class CFastLocalTransferPropInfo
{
public:
	unsigned short	m_iRecvOffset;
	unsigned short	m_iSendOffset;
	unsigned short	m_iProp;
};

class CFastLocalTransferInfo
{
public:
	CUtlVector<CFastLocalTransferPropInfo> m_FastInt32;
	CUtlVector<CFastLocalTransferPropInfo> m_FastInt16;
	CUtlVector<CFastLocalTransferPropInfo> m_FastInt8;
	CUtlVector<CFastLocalTransferPropInfo> m_FastVector;
	CUtlVector<CFastLocalTransferPropInfo> m_OtherProps;	// Props that must be copied slowly (proxies and all).
};

void *CSendTablePrecalcCTOR{nullptr};

#define MAX_EXCLUDE_PROPS		512
#define MAX_PROXY_RESULTS 256

class CSendTablePrecalc
{
public:
	virtual ~CSendTablePrecalc() = 0;

	inline int GetNumProps() const
	{
		return m_Props.Count();
	}

	inline const SendProp* GetProp( int i ) const
	{
		return m_Props[i]; 
	}

	inline int GetNumDataTableProxies() const
	{
		return m_nDataTableProxies;
	}

	inline void SetNumDataTableProxies( int count )
	{
		m_nDataTableProxies = count;
	}

	inline SendTable* GetSendTable() const
	{
		return m_pSendTable; 
	}

	inline CSendNode* GetRootNode()
	{
		return &m_Root; 
	}

	bool SetupFlatPropertyArray();

	class CProxyPathEntry
	{
	public:
		unsigned short m_iDatatableProp;	// Lookup into CSendTablePrecalc or CRecvDecoder::m_DatatableProps.
		unsigned short m_iProxy;
	};
	class CProxyPath
	{
	public:
		unsigned short m_iFirstEntry;	// Index into m_ProxyPathEntries.
		unsigned short m_nEntries;
	};
	
	CUtlVector<CProxyPathEntry> m_ProxyPathEntries;	// For each proxy index, this is all the DT proxies that generate it.
	CUtlVector<CProxyPath> m_ProxyPaths;			// CProxyPathEntries lookup into this.
	
	// These are what CSendNodes reference.
	// These are actual data properties (ints, floats, etc).
	CUtlVector<const SendProp*>	m_Props;

	// Each datatable in a SendTable's tree gets a proxy index, and its properties reference that.
	CUtlVector<unsigned char> m_PropProxyIndices;
	
	// CSendNode::m_iDatatableProp indexes this.
	// These are the datatable properties (SendPropDataTable).
	CUtlVector<const SendProp*>	m_DatatableProps;

	// This is the property hierarchy, with the nodes indexing m_Props.
	CSendNode				m_Root;

	// From whence we came.
	SendTable				*m_pSendTable;

	// For instrumentation.
	CDTISendTable			*m_pDTITable;

	// This is precalculated in single player to allow faster direct copying of the entity data
	// from the server entity to the client entity.
	CFastLocalTransferInfo	m_FastLocalTransfer;

	// This tells how many data table properties there are without SPROP_PROXY_ALWAYS_YES.
	// Arrays allocated with this size can be indexed by CSendNode::GetDataTableProxyIndex().
	int						m_nDataTableProxies;
	
	// Map prop offsets to indices for properties that can use it.
	CUtlMap<unsigned short, unsigned short> m_PropOffsetToIndexMap;
};

static void SendTable_CalcNextVectorElems( SendTable *pTable )
{
	for ( int i=0; i < pTable->GetNumProps(); i++ )
	{
		SendProp *pProp = pTable->GetProp( i );
		
		if ( pProp->GetType() == DPT_DataTable )
		{
			SendTable_CalcNextVectorElems( pProp->GetDataTable() );
		}
		else if ( pProp->GetOffset() < 0 )
		{
			pProp->SetOffset( -pProp->GetOffset() );
			pProp->SetFlags( pProp->GetFlags() | SPROP_IS_A_VECTOR_ELEM );
		}
	}
}

static void SendTable_Validate( CSendTablePrecalc *pPrecalc )
{
	SendTable *pTable = pPrecalc->m_pSendTable;

	for ( int i = 0; i < pPrecalc->GetNumProps(); ++i )
	{
		const SendProp *pProp = pPrecalc->GetProp( i );
		if ( pProp->GetFlags() & SPROP_ENCODED_AGAINST_TICKCOUNT )
		{
			pTable->SetHasPropsEncodedAgainstTickcount( true );
			break;
		}
	}
}

void *ServerDTI_HookTablePtr{nullptr};

CDTISendTable* ServerDTI_HookTable( SendTable *pTable )
{
	return (void_to_func<CDTISendTable *(*)(SendTable *)>(ServerDTI_HookTablePtr))(pTable);
}

CRC32_t SendTable_CRCTable( CRC32_t &crc, SendTable *pTable )
{
	CRC32_ProcessBuffer( &crc, (void *)pTable->m_pNetTableName, Q_strlen( pTable->m_pNetTableName) );

	int nProps = LittleLong( pTable->m_nProps );
	CRC32_ProcessBuffer( &crc, (void *)&nProps, sizeof( pTable->m_nProps ) );

	// Send each property.
	for ( int iProp=0; iProp < pTable->m_nProps; iProp++ )
	{
		const SendProp *pProp = &pTable->m_pProps[iProp];

		int type = LittleLong( pProp->m_Type );
		CRC32_ProcessBuffer( &crc, (void *)&type, sizeof( type ) );
		CRC32_ProcessBuffer( &crc, (void *)pProp->GetName() , Q_strlen( pProp->GetName() ) );

		int flags = LittleLong( pProp->GetFlags() );
		CRC32_ProcessBuffer( &crc, (void *)&flags, sizeof( flags ) );

		if( pProp->m_Type == DPT_DataTable )
		{
			CRC32_ProcessBuffer( &crc, (void *)pProp->GetDataTable()->m_pNetTableName, Q_strlen( pProp->GetDataTable()->m_pNetTableName ) );
		}
		else
		{
			if ( pProp->IsExcludeProp() )
			{
				CRC32_ProcessBuffer( &crc, (void *)pProp->GetExcludeDTName(), Q_strlen( pProp->GetExcludeDTName() ) );
			}
			else if ( pProp->GetType() == DPT_Array )
			{
				int numelements = LittleLong( pProp->GetNumElements() );
				CRC32_ProcessBuffer( &crc, (void *)&numelements, sizeof( numelements ) );
			}
			else
			{	
				float lowvalue;
				LittleFloat( &lowvalue, &pProp->m_fLowValue );
				CRC32_ProcessBuffer( &crc, (void *)&lowvalue, sizeof( lowvalue ) );

				float highvalue;
				LittleFloat( &highvalue, &pProp->m_fHighValue );
				CRC32_ProcessBuffer( &crc, (void *)&highvalue, sizeof( highvalue ) );

				int	bits = LittleLong( pProp->m_nBits );
				CRC32_ProcessBuffer( &crc, (void *)&bits, sizeof( bits ) );
			}
		}
	}

	return crc;
}

int SV_BuildSendTablesArray( ServerClass *pClasses, SendTable **pTables, int nMaxTables )
{
	int nTables = 0;

	for( ServerClass *pCur=pClasses; pCur; pCur=pCur->m_pNext )
	{
		pTables[nTables] = pCur->m_pTable;
		++nTables;
	}

	return nTables;
}

CRC32_t SendTable_ComputeCRC()
{
	CRC32_t result;
	CRC32_Init( &result );

	// walk the tables and checksum them
	int c = g_SendTables->Count();
	for ( int i = 0 ; i < c; i++ )
	{
		SendTable *st = (*g_SendTables)[ i ];
		result = SendTable_CRCTable( result, st );
	}

	CRC32_Final( &result );

	return result;
}

static void SendTable_TermTable( SendTable *pTable )
{
	if( !pTable->m_pPrecalc )
		return;

	delete pTable->m_pPrecalc;
}

serverclass_override_t::serverclass_override_t(IEntityFactory *fac_, std::string &&clsname_, ServerClass *realcls_)
	: hooked_fac{fac_}, clsname{std::move(clsname_)}, realcls{realcls_}
{
	if(CEntityFactoryDictionary::is_factory_custom(hooked_fac)) {
		fac_is_sp = true;
		sp_entity_factory *spfac = (sp_entity_factory *)hooked_fac;
		spfac->custom_server = this;
	} else {
		SH_ADD_HOOK(IEntityFactory, Create, hooked_fac, SH_MEMBER(this, &serverclass_override_t::HookCreate), false);
	}
	
	server_map.emplace(clsname, this);
	server_ptr_map.emplace((ServerClass *)&cls, this);
	
	counterid = classoverridecounter++;
	
	custom_SendProp *prop = emplace_prop();
	prop->m_Type = DPT_DataTable;
	prop->set_name("baseclass"s);
	prop->SetOffset(0);
	prop->SetDataTableProxyFn(std_proxies->m_DataTableToDataTable);
	prop->SetFlags(SPROP_PROXY_ALWAYS_YES|SPROP_COLLAPSIBLE);
	
	cls.m_pTable = &tbl;
	cls.m_InstanceBaselineIndex = INVALID_STRING_INDEX;

	cls.m_pNext = custom_server_head;
	custom_server_head = (ServerClass *)&cls;

	g_pServerClassTail->m_pNext = custom_server_head;

	init_classid();

	if(realcls) {
		init();
		cls_inited = true;
	} else {
		props[0].SetDataTable(CBaseEntity_ServerClass->m_pTable);
		
		tbl.set_name("DT_BaseEntity"s);
		cls.set_name("CBaseEntity"s);
	}
}

extern void remove_serverclass_from_sm_cache(ServerClass *pMap);

void serverclass_override_t::factory_removed(IEntityFactory *fac)
{
	SH_REMOVE_HOOK(IEntityFactory, Create, fac, SH_MEMBER(this, &serverclass_override_t::HookCreate), false);
}

serverclass_override_t::~serverclass_override_t()
{
	server_map.erase(clsname);

	server_ptr_map.erase((ServerClass *)&cls);

	baselinemap.erase(classid);

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

	if(!custom_server_head) {
		--classoverridecounter;
		--classid_last;
	}

	if(server_map.empty()) {
		classoverridecounter = 0;
		classid_last = def_classid_last;
	}

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

#ifdef __HAS_PROXYSEND
	if(proxysend) {
		proxysend->remove_serverclass_from_cache((ServerClass *)&cls);
	}
#endif

	if(!fac_is_sp && hooked_fac) {
		factory_removed(hooked_fac);
	}

	cls.clear_name();

	term_datatable();

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

	int new_cb{last_cb};

	if(curr_server_info != nullptr) {
		curr_server_info->calc_size(new_cb);
	}
	
	if(curr_data_info != nullptr) {
		new_cb += curr_data_info->size;
	}
	
	return calloc(1, new_cb);
}

void *HookPvAllocEntPrivateData(long cb)
{
	RETURN_META_VALUE(MRES_SUPERCEDE, DoPvAllocEntPrivateData(cb));
}

size_t datamapoverridecounter = 0;

custom_prop_info_t::custom_prop_info_t(IEntityFactory *fac_, std::string &&clsname_)
	: hooked_fac{fac_}, clsname{std::move(clsname_)}
{
	map.zero();
	
	if(CEntityFactoryDictionary::is_factory_custom(hooked_fac)) {
		fac_is_sp = true;
		sp_entity_factory *spfac = (sp_entity_factory *)hooked_fac;
		spfac->custom_prop = this;
	} else {
		SH_ADD_HOOK(IEntityFactory, Create, hooked_fac, SH_MEMBER(this, &custom_prop_info_t::HookCreate), false);
	}
	
	info_map.emplace(clsname, this);
	
	counterid = datamapoverridecounter++;
}

extern void remove_datamap_from_sm_cache(datamap_t *pMap);

void custom_prop_info_t::factory_removed(IEntityFactory *fac)
{
	SH_REMOVE_HOOK(IEntityFactory, Create, fac, SH_MEMBER(this, &custom_prop_info_t::HookCreate), false);
}

custom_prop_info_t::~custom_prop_info_t()
{
	info_map.erase(clsname);

	if(info_map.empty()) {
		datamapoverridecounter = 0;
	}

	remove_datamap_from_sm_cache(&map);
	
	if(!fac_is_sp && hooked_fac) {
		factory_removed(hooked_fac);
	}
	
	for(custom_typedescription_t &desc : dataDesc) {
		desc.clear_name();
	}

	map.clear_name();

	if(freehndl) {
		if(hndl != BAD_HANDLE) {
			HandleSecurity security(pContext->GetIdentity(), myself->GetIdentity());
			handlesys->FreeHandle(hndl, &security);
		}
	}
}

void serverclass_override_t::calc_size(int &base)
{
	if(largest_offset >= base) {
		base = largest_offset;
	}

	base += size;
}

void serverclass_override_t::update_offsets(int &base)
{
	if(largest_offset >= base) {
		base = largest_offset;
	}

	for(std::size_t i{1}; i < props.size(); ++i) {
		custom_SendProp &prop{props[i]};
		if(prop.GetType() == DPT_Array ||
			prop.extra_data().absoffset) {
			continue;
		}
		prop.SetOffset(base + prop.GetOffset());
	}

	base += size;
}

void serverclass_override_t::do_override(int &base, CBaseEntity *pEntity, IServerNetworkable *pNet)
{
	if(!was_overriden) {
		if(!cls_inited) {
			realcls = pEntity->GetServerClass();
			init();
			cls_inited = true;
		}

		update_offsets(base);

		setup_datatable();

		was_overriden = true;
	}

	zero(pEntity);

	add_hooks(pEntity, pNet);
}

void custom_prop_info_t::do_override(int &base, CBaseEntity *pEntity)
{
	if(!was_overriden) {
		datamap_t *basemap = gamehelpers->GetDataMap(pEntity);
		ServerClass *svcls = pEntity->GetServerClass();
		
		if(mapname.empty()) {
			mapname = svcls->GetName();
			mapname += "_custom_"s;
			mapname += std::to_string(counterid);
		}
		map.set_name(mapname);
		map.baseMap = basemap;
		
		update_offsets(base);
		
		was_overriden = true;
	}
	
	zero(pEntity);
	
	add_hooks(pEntity);
}

#define protected public
#define private public
#include <entitylist.h>
#undef protected
#undef private

SH_DECL_HOOK2_void(CGlobalEntityList, OnAddEntity, SH_NOATTRIB, 0, IHandleEntity *, CBaseHandle);

static bool ignore_entity_listeners{false};

class CGlobalEntityListHack : public CGlobalEntityList
{
public:
	void HookOnAddEntity( IHandleEntity *pEnt, CBaseHandle handle )
	{
		int i = handle.GetEntryIndex();

		// record current list details
		m_iNumEnts++;
		if ( i > m_iHighestEnt )
			m_iHighestEnt = i;

		// If it's a CBaseEntity, notify the listeners.
		CBaseEntity *pBaseEnt = static_cast<IServerUnknown*>(pEnt)->GetBaseEntity();
		if ( pBaseEnt->edict() )
			m_iNumEdicts++;
		
		// NOTE: Must be a CBaseEntity on server
		Assert( pBaseEnt );
		//DevMsg(2,"Created %s\n", pBaseEnt->GetClassname() );

		if(!ignore_entity_listeners) {
			for ( i = m_entityListeners.Count()-1; i >= 0; i-- )
			{
				m_entityListeners[i]->OnEntityCreated( pBaseEnt );
			}
		}

		RETURN_META(MRES_SUPERCEDE);
	}

	void SendEntityListeners(CBaseEntity *pBaseEnt)
	{
		for ( int i = m_entityListeners.Count()-1; i >= 0; i-- )
		{
			m_entityListeners[i]->OnEntityCreated( pBaseEnt );
		}
	}
};

IServerNetworkable *sp_entity_factory::Create(const char *pClassName)
{
	IServerNetworkable *net = nullptr;
	
	if(based != nullptr) {
		curr_data_info = custom_prop;
		curr_server_info = custom_server;
		ignore_entity_listeners = true;
		net = based->Create(pClassName);
		ignore_entity_listeners = false;
		curr_data_info = nullptr;
		curr_server_info = nullptr;
		CBaseEntity *pEntity = net->GetBaseEntity();
		//last_cb = based->GetEntitySize();
		int new_cb{last_cb};
		if(custom_server) {
			custom_server->do_override(new_cb, pEntity, net);
		}
		if(custom_prop) {
			custom_prop->do_override(new_cb, pEntity);
		}
		((CGlobalEntityListHack *)g_pEntityList)->SendEntityListeners(pEntity);
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
		int new_cb{last_cb};
		CBaseEntity *obj = (CBaseEntity *)res;
		if(obj != nullptr) {
			ignore_entity_listeners = true;
			obj->PostConstructor(pClassName);
			ignore_entity_listeners = false;
			net = obj->GetNetworkable();
			if(custom_server) {
				custom_server->do_override(new_cb, obj, net);
			}
			if(custom_prop) {
				custom_prop->do_override(new_cb, obj);
			}
			((CGlobalEntityListHack *)g_pEntityList)->SendEntityListeners(obj);
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
	
	//int base = fac->GetEntitySize();
	int base = last_cb;

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
	
	//int base = fac->GetEntitySize();
	int base = last_cb;

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

static ServerClass *CTFPlayer_ServerClass{nullptr};
static ServerClass *CDynamicProp_ServerClass{nullptr};
static ServerClass *CWeaponMedigun_ServerClass{nullptr};

static bool is_player_classname(const char *classname)
{
	return (strcmp(classname, "player") == 0 ||
			strcmp(classname, "tf_bot") == 0);
}

static bool is_prop_classname(const char *classname)
{
	return (strcmp(classname, "dynamic_prop") == 0 ||
			strcmp(classname, "prop_dynamic") == 0 ||
			strcmp(classname, "prop_dynamic_override") == 0);
}

static bool is_medgun_classname(const char *classname)
{
	return (strcmp(classname, "tf_weapon_medigun") == 0);
}

static ServerClass *get_factory_serverclass(IEntityFactory *factory, const char *classname)
{
	if(classname) {
		if(is_player_classname(classname)) {
			return CTFPlayer_ServerClass;
		} else if(is_prop_classname(classname)) {
			return CDynamicProp_ServerClass;
		} else if(is_medgun_classname(classname)) {
			return CWeaponMedigun_ServerClass;
		}
	}
	ignore_entity_listeners = true;
	IServerNetworkable *net = factory->Create("__hack_getsvclass__");
	ignore_entity_listeners = false;
	ServerClass *svclass = net->GetServerClass();
	RemoveEntity(net->GetBaseEntity());
	return svclass;
}

cell_t EntityFactoryDictionaryregister_based(IPluginContext *pContext, const cell_t *params)
{
	char *name = nullptr;
	pContext->LocalToString(params[1], &name);
	
	if(dictionary->FindFactory(name) != nullptr) {
		return pContext->ThrowNativeError("%s is already registered", name);
	}

	char *based = nullptr;
	pContext->LocalToString(params[2], &based);

	IEntityFactory *factory = dictionary->FindFactory(based);
	if(!factory) {
		return pContext->ThrowNativeError("invalid classname %s", based);
	}

	sp_entity_factory *obj = new sp_entity_factory(name, factory);

	if(!CEntityFactoryDictionary::is_factory_custom(factory)) {
		obj->svclass = get_factory_serverclass(factory, based);
	} else {
		obj->svclass = ((sp_entity_factory *)factory)->svclass;
	}

	Handle_t hndl = handlesys->CreateHandle(factory_handle, obj, pContext->GetIdentity(), myself->GetIdentity(), nullptr);
	obj->hndl = hndl;
	obj->pContext = pContext;
	
	return hndl;
}

cell_t EntityFactoryDictionaryregister_function(IPluginContext *pContext, const cell_t *params)
{
	char *name = nullptr;
	pContext->LocalToString(params[1], &name);
	
	if(dictionary->FindFactory(name) != nullptr) {
		return pContext->ThrowNativeError("%s is already registered", name);
	}
	
	if(params[3] <= 0) {
		return pContext->ThrowNativeError("invalid size %i", params[3]);
	}
	
	IPluginFunction *callback = pContext->GetFunctionById(params[2]);
	
	sp_entity_factory *obj = new sp_entity_factory(name, callback, params[3], params[4]);

	obj->svclass = get_factory_serverclass(obj, name);

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

cell_t CustomSendtableadd_prop_vector(IPluginContext *pContext, const cell_t *params)
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

	int elementCount = params[7];
	if(elementCount == 0) {
		return pContext->ThrowNativeError("cannot have 0 elements");
	}

	std::string name{name_ptr};
	obj->add_prop_vector(name, sp_ctof(params[3]), sp_ctof(params[4]), params[5], params[6], elementCount, params[8]);
	return 0;
}

cell_t CustomSendtableadd_prop_qangles(IPluginContext *pContext, const cell_t *params)
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

	int elementCount = params[5];
	if(elementCount == 0) {
		return pContext->ThrowNativeError("cannot have 0 elements");
	}

	std::string name{name_ptr};
	obj->add_prop_qangles(name, params[3], params[4], elementCount, params[6]);
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

	int elementCount = params[7];
	if(elementCount == 0) {
		return pContext->ThrowNativeError("cannot have 0 elements");
	}

	std::string name{name_ptr};
	obj->add_prop_float(name, sp_ctof(params[3]), sp_ctof(params[4]), params[5], params[6], elementCount, params[8]);
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

	int elementCount = params[6];
	if(elementCount == 0) {
		return pContext->ThrowNativeError("cannot have 0 elements");
	}

	std::string name{name_ptr};
	obj->add_prop_int(name, params[3], params[4], params[5], elementCount, params[7]);
	return 0;
}

cell_t CustomSendtableadd_prop_ehandle(IPluginContext *pContext, const cell_t *params)
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

	int elementCount = params[4];
	if(elementCount == 0) {
		return pContext->ThrowNativeError("cannot have 0 elements");
	}

	std::string name{name_ptr};
	obj->add_prop_ehandle(name, params[3], elementCount, params[5]);
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
	
	ServerClass *svcls = gamehelpers->FindServerClass(netname);
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

	std::string namestr{name};

	if(factory->cls_inited) {
		factory->tbl.set_name(namestr);
	}
	factory->tbl_name = std::move(namestr);

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

	std::string namestr{name};

	if(factory->cls_inited) {
		factory->cls.set_name(namestr);
	}
	factory->cls_name = std::move(namestr);

	return 0;
}

cell_t CustomSendtableset_client_class_name(IPluginContext *pContext, const cell_t *params)
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

	std::string namestr{name};

	factory->set_client_class_name(std::move(namestr));
	return 0;
}

cell_t CustomSendtableset_client_name(IPluginContext *pContext, const cell_t *params)
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

	std::string namestr{name};

	factory->set_client_table_name(std::move(namestr));
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
	
	ServerClass *netclass = gamehelpers->FindServerClass(netname);
	if(!netclass) {
		return pContext->ThrowNativeError("invalid netname %s", netname);
	}
	
	factory->set_client_class_id(netclass);
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

	ServerClass *netclass = nullptr;

	char *forced = nullptr;
	pContext->LocalToStringNULL(params[2], &forced);

	if(!forced || forced[0] == '\0') {
		if(!CEntityFactoryDictionary::is_factory_custom(factory)) {
			netclass = get_factory_serverclass(factory, classname);
		} else {
			netclass = ((sp_entity_factory *)factory)->svclass;
			if(!netclass) {
				return pContext->ThrowNativeError("invalid classname %s", classname);
			}
		}
	} else {
		netclass = gamehelpers->FindServerClass(forced);
		if(!netclass) {
			return pContext->ThrowNativeError("invalid netname %s", forced);
		}
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

	ServerClass *netclass{nullptr};

	char *forced = nullptr;
	pContext->LocalToStringNULL(params[2], &forced);

	if(forced && forced[0] != '\0') {
		netclass = gamehelpers->FindServerClass(forced);
		if(!netclass) {
			return pContext->ThrowNativeError("invalid netname %s", forced);
		}
	} else {
		netclass = factory->svclass;
		if(!netclass) {
			return pContext->ThrowNativeError("invalid factory %s", name.c_str());
		}
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
		CBaseEntity::m_pfnThink_t old_think{pEntity->GetThinkFuncContext(context_ptr)};
		if(old_think != &CBaseEntity::PluginThinkContext) {
			tmp_cb.old_think = old_think;
		}
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

static cell_t RemoveEntityImmediate(IPluginContext *pContext, const cell_t *params)
{
	CBaseEntity *pEntity = gamehelpers->ReferenceToEntity(params[1]);
	if(!pEntity) {
		return pContext->ThrowNativeError("Invalid Entity Reference/Index %i", params[1]);
	}

	servertools->RemoveEntityImmediate(pEntity);
	return 0;
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
	{"CustomSendtable.set_client_class_id", CustomSendtableoverride_with},
	{"CustomSendtable.unexclude_prop", CustomSendtableunexclude_prop},
	{"CustomSendtable.set_base_class", CustomSendtableset_base_class},
	{"CustomSendtable.set_name", CustomSendtableset_name},
	{"CustomSendtable.set_class_name", CustomSendtableset_network_name},
	{"CustomSendtable.set_client_name", CustomSendtableset_client_name},
	{"CustomSendtable.set_client_class_name", CustomSendtableset_client_class_name},
	{"CustomSendtable.add_prop_float", CustomSendtableadd_prop_float},
	{"CustomSendtable.add_prop_int", CustomSendtableadd_prop_int},
	{"CustomSendtable.add_prop_ehandle", CustomSendtableadd_prop_ehandle},
	{"CustomSendtable.add_prop_vector", CustomSendtableadd_prop_vector},
	{"CustomSendtable.add_prop_qangles", CustomSendtableadd_prop_qangles},
	{"CustomDatamap.from_classname", CustomDatamapfrom_classname},
	{"CustomDatamap.from_factory", CustomDatamapfrom_factory},
	{"CustomDatamap.add_prop", CustomDatamapadd_prop},
	{"CustomDatamap.set_name", CustomDatamapset_name},
	{"HookEntityContextThink", SetEntityContextThink},
	{"HookEntityThink", SetEntityThink},
	{"SetEntityNextThink", SetEntityNextThink},
	{"AllocPooledString", native_AllocPooledString},
	{"RemoveEntityImmediate", RemoveEntityImmediate},
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
					holder->removed(pEntity);
				}
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
	if(!pEntity) {
		return;
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
		obj->remove_all_hooks();
		delete obj;
	} else if(type == removal_handle) {
		factory_removal_t *obj = (factory_removal_t *)object;
		delete obj;
	} else if(type == serverclass_handle) {
		serverclass_override_t *obj = (serverclass_override_t *)object;
		obj->freehndl = false;
		obj->remove_all_hooks();
		delete obj;
	}
}

CDetour *pPvAllocEntPrivateData = nullptr;

DETOUR_DECL_MEMBER1(DetourPvAllocEntPrivateData, void *, long, cb)
{
	return DoPvAllocEntPrivateData(cb);
}

static ConVar *sv_parallel_packentities{nullptr};

#ifdef __HAS_PROXYSEND
bool Sample::is_allowed() const noexcept
{
	return svcls_hooks.empty();
}
#endif

static CDetour *SV_ComputeClientPacks_detour{nullptr};

class CFrameSnapshot;
class CGameClient;
DETOUR_DECL_STATIC3(SV_ComputeClientPacks, void, int, clientCount, CGameClient **, clients, CFrameSnapshot *, snapshot)
{
#ifndef __HAS_PROXYSEND
	sv_parallel_packentities->SetValue(svcls_hooks.empty());
#else
	if(!proxysend) {
		sv_parallel_packentities->SetValue(svcls_hooks.empty());
	}
#endif

	DETOUR_STATIC_CALL(SV_ComputeClientPacks)(clientCount, clients, snapshot);
}

DETOUR_DECL_MEMBER4(CBaseServer_WriteDeltaEntities, void, CBaseClient *, client, CClientFrame *, to, CClientFrame *, from, bf_write &, pBuf)
{
	g_Sample.pre_write_deltas();
	DETOUR_MEMBER_CALL(CBaseServer_WriteDeltaEntities)(client, to, from, pBuf);
	g_Sample.post_write_deltas();
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
		++g_iNumServerClasses;

		if(strcmp(g_pServerClassTail->m_pNetworkName, "CBaseEntity") == 0) {
			CBaseEntity_ServerClass = g_pServerClassTail;
		} else if(strcmp(g_pServerClassTail->m_pNetworkName, "CTFPlayer") == 0) {
			CTFPlayer_ServerClass = g_pServerClassTail;
		} else if(strcmp(g_pServerClassTail->m_pNetworkName, "CDynamicProp") == 0) {
			CDynamicProp_ServerClass = g_pServerClassTail;
		} else if(strcmp(g_pServerClassTail->m_pNetworkName, "CWeaponMedigun") == 0) {
			CWeaponMedigun_ServerClass = g_pServerClassTail;
		}
		
		if(!g_pServerClassTail->m_pNext) {
			break;
		}
		
		g_pServerClassTail = g_pServerClassTail->m_pNext;
	}
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
CDetour *pCGameClientSendSignonData = nullptr;
CDetour *SV_CreateBaseline_detour{nullptr};
CDetour *SV_EnsureInstanceBaseline_detour{nullptr};

static bool in_send_signondata{false};

void Sample::OnCoreMapStart(edict_t *pEdictList, int edictCount, int clientMax)
{
	m_pInstanceBaselineTable = netstringtables->FindTable(INSTANCE_BASELINE_TABLENAME);
}

void Sample::OnCoreMapEnd()
{
	classids_assigned = false;
	classid_last = 0;
	def_classid_last = 0;
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

#define	NET_MAX_PAYLOAD				288000	

#include <inetmessage.h>

class CNetMessage : public INetMessage
{
public:
	bool				m_bReliable;	// true if message should be send reliable
	INetChannel			*m_NetChannel;	// netchannel this message is from/for
	char pad[4];
};

#define NETMSG_TYPE_BITS	6	// must be 2^NETMSG_TYPE_BITS > SVC_LASTMSG

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

	bool WriteToBuffer_impl( bf_write &buffer )
	{
		if ( !m_bCreateOnClient )
		{
			m_nNumServerClasses = m_Classes.Count();	// use number from list list	
		}
		
		buffer.WriteUBitLong( svc_ClassInfo, NETMSG_TYPE_BITS );

		buffer.WriteShort( m_nNumServerClasses );

		int serverClassBits = Q_log2( m_nNumServerClasses ) + 1;

		buffer.WriteOneBit( m_bCreateOnClient?1:0 );

		if ( m_bCreateOnClient )
			return !buffer.IsOverflowed();

		for ( int i=0; i< m_nNumServerClasses; i++ )
		{
			class_t * serverclass = &m_Classes[i];

			buffer.WriteUBitLong( serverclass->classID, serverClassBits );
			buffer.WriteString( serverclass->classname );
			buffer.WriteString( serverclass->datatablename );
		}

		return !buffer.IsOverflowed();
	}
};

static void handle_classinfos(SVC_ClassInfo &classinfomsg)
{
	int nClasses = 0;

	for ( ServerClass *pClass=g_pServerClassHead; pClass && pClass != custom_server_head; pClass=pClass->m_pNext )
	{
		SVC_ClassInfo::class_t svclass;

		svclass.classID = pClass->m_ClassID;
		Q_strncpy( svclass.datatablename, pClass->m_pTable->GetName(), sizeof(svclass.datatablename) );
		Q_strncpy( svclass.classname, pClass->m_pNetworkName, sizeof(svclass.classname) );

		classinfomsg.m_Classes.AddToTail( svclass );  // add all known classes to message

		++nClasses;
	}

	for ( ServerClass *pClass=custom_server_head; pClass; pClass=pClass->m_pNext )
	{
		SVC_ClassInfo::class_t svclass;

		const char *netname{pClass->m_pNetworkName};
		const char *tblname{pClass->m_pTable->GetName()};

		auto it{server_ptr_map.find(pClass)};
		if(it != server_ptr_map.end()) {
			serverclass_override_t &overr{*it->second};
			if(!overr.cls_cl_name.empty()) {
				netname = overr.cls_cl_name.c_str();
			}
			if(!overr.tbl_cl_name.empty()) {
				tblname = overr.tbl_cl_name.c_str();
			}
		}

		svclass.classID = pClass->m_ClassID;
		Q_strncpy( svclass.datatablename, tblname, sizeof(svclass.datatablename) );
		Q_strncpy( svclass.classname, netname, sizeof(svclass.classname) );

		classinfomsg.m_Classes.AddToTail( svclass );  // add all known classes to message

		++nClasses;
	}

	classinfomsg.m_nNumServerClasses = nClasses;
}

#include <inetchannel.h>

SH_DECL_HOOK3(INetChannel, SendNetMsg, SH_NOATTRIB, 0, bool, INetMessage &, bool, bool);

bool HookSendNetMsg(INetMessage &msg, bool bForceReliable, bool bVoice);

DETOUR_DECL_STATIC2(DetourSV_WriteClassInfos, void, ServerClass *,pClasses, bf_write &,pBuf)
{
	SVC_ClassInfo &classinfomsg = *(SVC_ClassInfo *)alloca(sizeof(SVC_ClassInfo));
	new (&classinfomsg.m_Classes) CUtlVector<SVC_ClassInfo::class_t>{};

	classinfomsg.m_bCreateOnClient = false;

	handle_classinfos(classinfomsg);

	classinfomsg.WriteToBuffer_impl(pBuf);

	classinfomsg.m_Classes.~CUtlVector<SVC_ClassInfo::class_t>();
}

DETOUR_DECL_STATIC1(DetourSV_ComputeClassInfosCRC, void, CRC32_t*, crc)
{
	for ( ServerClass *pClass=g_pServerClassHead; pClass && pClass != custom_server_head; pClass=pClass->m_pNext )
	{
		CRC32_ProcessBuffer( crc, (void *)pClass->m_pNetworkName, Q_strlen( pClass->m_pNetworkName ) );
		CRC32_ProcessBuffer( crc, (void *)pClass->m_pTable->GetName(), Q_strlen(pClass->m_pTable->GetName() ) );
	}

	for ( ServerClass *pClass=custom_server_head; pClass; pClass=pClass->m_pNext )
	{
		const char *netname{pClass->m_pNetworkName};
		const char *tblname{pClass->m_pTable->GetName()};

		CRC32_ProcessBuffer( crc, (void *)netname, Q_strlen( netname ) );
		CRC32_ProcessBuffer( crc, (void *)tblname, Q_strlen( tblname ) );
	}
}

void DataTable_ClearWriteFlags_R( SendTable *pTable )
{
	pTable->SetWriteFlag( false );

	for(int i=0; i < pTable->m_nProps; i++)
	{
		SendProp *pProp = &pTable->m_pProps[i];

		if( pProp->m_Type == DPT_DataTable )
		{
			DataTable_ClearWriteFlags_R( pProp->GetDataTable() );
		}
	}
}

void DataTable_ClearWriteFlags( ServerClass *pClasses )
{
	for ( ServerClass *pCur=pClasses; pCur; pCur=pCur->m_pNext )
	{
		DataTable_ClearWriteFlags_R( pCur->m_pTable );
	}
}

void *SV_MaybeWriteSendTablePtr{nullptr};

static bool write_needs_decoder = false;

#define PROPINFOBITS_NUMPROPS			10
#define PROPINFOBITS_TYPE				5
#define PROPINFOBITS_FLAGS				SPROP_NUMFLAGBITS_NETWORKED
#define PROPINFOBITS_NUMBITS			7
#define PROPINFOBITS_NUMELEMENTS		10	// For arrays.

class SVC_SendTable : public CNetMessage
{
public:
	IServerMessageHandler *m_pMessageHandler;

	bool			m_bNeedsDecoder;
	int				m_nLength;
	bf_read			m_DataIn;
	bf_write		m_DataOut;
};

char writeinfos_buf[4096 * 2];

void SV_MaybeWriteSendTable( SendTable *pTable, bf_write &pBuf, bool bNeedDecoder )
{
	write_needs_decoder = bNeedDecoder;

	(void_to_func<void(*)(SendTable *, bf_write &, bool)>(SV_MaybeWriteSendTablePtr))(pTable, pBuf, bNeedDecoder);
}

#if SOURCE_ENGINE == SE_TF2
const char *CONDTABLE_NAME{nullptr};
int CONDTABLE_LIMIT{-1};
#endif

DETOUR_DECL_STATIC2(DetourSendTable_WriteInfos, bool, SendTable *,pTable, bf_write *,pBuf)
{
	pBuf->StartWriting( writeinfos_buf, sizeof(writeinfos_buf) );

	const char *tblname{pTable->GetName()};
	if(curr_server_info) {
		if(!curr_server_info->tbl_cl_name.empty()) {
			tblname = curr_server_info->tbl_cl_name.c_str();
		}
	}

#if SOURCE_ENGINE == SE_TF2
	if(strcmp(pTable->GetName(), CONDTABLE_NAME) == 0) {
		if(CONDTABLE_LIMIT > pTable->m_nProps) {
			Host_Error( "SendTable_WriteInfos: condition table limit is larger than condition table itself %i vs %i.\n", CONDTABLE_LIMIT, pTable->m_nProps );
		}
	}

	int numprops{pTable->m_nProps};
	if(strcmp(pTable->GetName(), CONDTABLE_NAME) == 0) {
		numprops = CONDTABLE_LIMIT;
	}
#else
	int numprops{pTable->m_nProps};
#endif

	pBuf->WriteString( tblname );
	pBuf->WriteUBitLong( numprops, PROPINFOBITS_NUMPROPS );

	// Send each property.
	for ( int iProp=0; iProp < pTable->m_nProps; iProp++ )
	{
		const SendProp *pProp = &pTable->m_pProps[iProp];

	#if SOURCE_ENGINE == SE_TF2
		if(strcmp(pTable->GetName(), CONDTABLE_NAME) == 0) {
			if(iProp >= numprops) {
				Warning( "SendTable_WriteInfos: skipped condition table prop %s (%i).\n", pProp->GetName(), iProp );
				continue;
			}
		}
	#endif

		pBuf->WriteUBitLong( (unsigned int)pProp->m_Type, PROPINFOBITS_TYPE );
		pBuf->WriteString( pProp->GetName() );
		// we now have some flags that aren't networked so strip them off
		unsigned int networkFlags = pProp->GetFlags() & ((1<<PROPINFOBITS_FLAGS)-1);
		pBuf->WriteUBitLong( networkFlags, PROPINFOBITS_FLAGS );

		if( pProp->m_Type == DPT_DataTable )
		{
			// Just write the name and it will be able to reuse the table with a matching name.
			pBuf->WriteString( pProp->GetDataTable()->m_pNetTableName );
		}
		else
		{
			if ( pProp->IsExcludeProp() )
			{
				pBuf->WriteString( pProp->GetExcludeDTName() );
			}
			else if ( pProp->GetType() == DPT_Array )
			{
				pBuf->WriteUBitLong( pProp->GetNumElements(), PROPINFOBITS_NUMELEMENTS );
			}
			else
			{
				pBuf->WriteBitFloat( pProp->m_fLowValue );
				pBuf->WriteBitFloat( pProp->m_fHighValue );
				pBuf->WriteUBitLong( pProp->m_nBits, PROPINFOBITS_NUMBITS );
			}
		}

		if(pBuf->IsOverflowed()) {
			Host_Error( "SendTable_WriteInfos: overflow after writing %s %s.\n", pTable->GetName(), pProp->GetName() );
		}
	}

	return !pBuf->IsOverflowed();
}

void SV_MaybeWriteSendTable_R( SendTable *pTable, bf_write &pBuf, bool root )
{
	bool root_custom = (root && curr_server_info);

	if(!root) {
		curr_server_info = nullptr;
	}

	SV_MaybeWriteSendTable( pTable, pBuf, false );

	// Make sure we send child send tables..
	for(int i=0; i < pTable->m_nProps; i++)
	{
		SendProp *pProp = &pTable->m_pProps[i];

		if( pProp->m_Type == DPT_DataTable ) {
			SV_MaybeWriteSendTable_R( pProp->GetDataTable(), pBuf, false );
		}
	}
}

DETOUR_DECL_STATIC2(DetourSV_WriteSendTables, void, ServerClass *,pClasses, bf_write &,pBuf)
{
	ServerClass *pCur;

	DataTable_ClearWriteFlags( pClasses );

	// First, we send all the leaf classes. These are the ones that will need decoders
	// on the client.
	for ( pCur=pClasses; pCur; pCur=pCur->m_pNext )
	{
		auto it{server_ptr_map.find(pCur)};
		if(it != server_ptr_map.end()) {
			serverclass_override_t &overr{*it->second};
			curr_server_info = &overr;
		} else {
			curr_server_info = nullptr;
		}

		SV_MaybeWriteSendTable( pCur->m_pTable, pBuf, true );
	}

	// Now, we send their base classes. These don't need decoders on the client
	// because we will never send these SendTables by themselves.
	for ( pCur=pClasses; pCur; pCur=pCur->m_pNext )
	{
		auto it{server_ptr_map.find(pCur)};
		if(it != server_ptr_map.end()) {
			serverclass_override_t &overr{*it->second};
			curr_server_info = &overr;
		} else {
			curr_server_info = nullptr;
		}

		SV_MaybeWriteSendTable_R( pCur->m_pTable, pBuf, true );
	}

	curr_server_info = nullptr;
}

#include <tier0/icommandline.h>

static CDetour *CBaseServer_WriteDeltaEntities_detour{nullptr};

std::unordered_map<ServerClass *, int> old_classids{};

void Sample::pre_write_deltas() const noexcept
{
	if(sv_sendtables->GetInt() == 0) {
		for ( ServerClass *pCur=custom_server_head; pCur; pCur=pCur->m_pNext )
		{
			auto it{server_ptr_map.find(pCur)};
			if(it != server_ptr_map.end()) {
				serverclass_override_t &overr{*it->second};
				old_classids.emplace(pCur, pCur->m_ClassID);
				pCur->m_ClassID = overr.cl_classid;
			}
		}
	}
}

void Sample::post_write_deltas() const noexcept
{
	for(auto it : old_classids) {
		it.first->m_ClassID = it.second;
	}
	old_classids.clear();
}

void *SendTable_EncodePtr{nullptr};

bool SendTable_Encode(const SendTable *pTable, const void *pStruct, bf_write *pOut, int objectID, CUtlMemory<CSendProxyRecipients> *pRecipients, bool bNonZeroOnly)
{
	return (void_to_func<bool(*)(const SendTable *, const void *, bf_write *, int, CUtlMemory<CSendProxyRecipients> *, bool)>(SendTable_EncodePtr))(pTable, pStruct, pOut, objectID, pRecipients, bNonZeroOnly);
}

static void add_baseline_for_class(ServerClass *pClass, int entnum)
{
	if(pClass->m_InstanceBaselineIndex != INVALID_STRING_INDEX) {
		return;
	}

	SendTable *pSendTable = pClass->m_pTable;

	edict_t *edict = gamehelpers->EdictOfIndex(entnum);

	ALIGN4 char packedData[MAX_PACKEDENTITY_DATA] ALIGN4_POST;
	bf_write writeBuf( "SV_CreateBaseline->writeBuf", packedData, sizeof( packedData ) );

	// create basline from zero values
	if ( !SendTable_Encode(
		pSendTable,
		edict->GetUnknown(),
		&writeBuf,
		entnum,
		NULL,
		false
		) )
	{
		Host_Error("SV_CreateBaseline: SendTable_Encode returned false (ent %d).\n", entnum);
	}

	// copy baseline into baseline stringtable
	char idString[32];
	Q_snprintf( idString, sizeof( idString ), "%d", pClass->m_ClassID );

	m_pInstanceBaselineTable->AddString(true, idString, writeBuf.GetNumBytesWritten(), packedData);
}

DETOUR_DECL_STATIC4(DetourSV_EnsureInstanceBaseline, void, ServerClass *,pServerClass, int, iEdict, const void *,pData, int, nBytes)
{
	ServerClass *pClass = gamehelpers->EdictOfIndex(iEdict)->GetNetworkable()->GetServerClass();

	if(sv_sendtables->GetInt() == 0) {
		auto it{server_ptr_map.find(pClass)};
		if(it != server_ptr_map.end()) {
			serverclass_override_t &overr{*it->second};
			if(overr.cl_classid_cls) {
				add_baseline_for_class(overr.cl_classid_cls, iEdict);
			} else if(overr.realcls) {
				add_baseline_for_class(overr.realcls, iEdict);
			}
		}
	}

	DETOUR_STATIC_CALL(DetourSV_EnsureInstanceBaseline)(pServerClass, iEdict, pData, nBytes);
}

DETOUR_DECL_MEMBER0(DetourCGameServerAssignClassIds, void)
{
	bool bSpew = CommandLine()->FindParm( "-netspike" ) != 0;

	int nClasses = 0;

	classid_last = 0;
	for ( ServerClass *pClass=g_pServerClassHead; pClass && pClass != custom_server_head; pClass=pClass->m_pNext )
	{
		pClass->m_ClassID = classid_last++;

		baselinemap[pClass->m_ClassID] = pClass;

		++nClasses;

		if ( bSpew )
		{
			Msg( "%d == '%s'\n", pClass->m_ClassID, pClass->GetName() );
		}
	}

	def_classid_last = classid_last;

	for ( ServerClass *pClass=custom_server_head; pClass; pClass=pClass->m_pNext )
	{
		pClass->m_ClassID = classid_last++;

		baselinemap[pClass->m_ClassID] = pClass;

		++nClasses;

		auto it{server_ptr_map.find(pClass)};
		if(it != server_ptr_map.end()) {
			serverclass_override_t &overr{*it->second};
			overr.classid = pClass->m_ClassID;
			if(overr.cl_classid_cls) {
				overr.cl_classid = overr.cl_classid_cls->m_ClassID;
			} else if(overr.realcls) {
				overr.cl_classid = overr.realcls->m_ClassID;
			}
		}
	}

	((CBaseServer *)this)->serverclasses = nClasses;
	((CBaseServer *)this)->serverclassbits = Q_log2( nClasses ) + 1;

	classids_assigned = true;
}

void serverclass_override_t::init_classid()
{
	if(classids_assigned) {
		((CBaseServer *)server)->increment_svclasses();

		cls.m_ClassID = classid_last++;
		classid = cls.m_ClassID;

		baselinemap[cls.m_ClassID] = (ServerClass *)&cls;
	} else {
		cls.m_ClassID = -1;
	}
}

void serverclass_override_t::set_client_class_id(ServerClass *netclass)
{
	cl_classid_cls = netclass;

	cl_classid = cl_classid_cls->m_ClassID;

	if(cls_cl_name.empty()) {
		cls_cl_name = cl_classid_cls->m_pNetworkName;
	}

	if(tbl_cl_name.empty()) {
		tbl_cl_name = cl_classid_cls->m_pTable->m_pNetTableName;
	}
}

void serverclass_override_t::set_client_class_name(std::string &&name)
{
	cls_cl_name = std::move(name);

	if(!cl_classid_cls) {
		ServerClass *netclass = gamehelpers->FindServerClass(name.c_str());
		if(netclass) {
			set_client_class_id(netclass);
		}
	}
}

void serverclass_override_t::set_client_table_name(std::string &&name)
{
	tbl_cl_name = std::move(name);
}

void serverclass_override_t::init()
{
	props[0].SetDataTable(realcls->m_pTable);

	if(tbl_name.empty()) {
		tbl_name = realcls->m_pTable->m_pNetTableName;
		tbl_name += "_custom_";
		tbl_name += std::to_string(counterid);
	}
	tbl.set_name(tbl_name);

	if(cls_name.empty()) {
		cls_name = realcls->m_pNetworkName;
		cls_name += "_custom_";
		cls_name += std::to_string(counterid);
	}
	cls.set_name(cls_name);

	if(!cl_classid_cls) {
		cl_classid = realcls->m_ClassID;
	}
}

void serverclass_override_t::term_datatable()
{
	g_SendTables->FindAndRemove(&tbl);

	SendTable_TermTable(&tbl);

	for(custom_SendProp &prop : props) {
		prop.clear_name();
	}

	tbl.clear_name();

	*g_SendTableCRC = SendTable_ComputeCRC();
}

template< class TableType, class PropType >
void SetupArrayProps_R( TableType *pTable )
{
	// If this table has already been initialized in here, then jump out.
	if ( pTable->IsInitialized() )
		return;

	pTable->SetInitialized( true );

	for ( int i=0; i < pTable->GetNumProps(); i++ )
	{
		PropType *pProp = pTable->GetProp( i );

		if ( pProp->GetType() == DPT_Array )
		{
			// Get the property defining the elements in the array.
			PropType *pArrayProp = pTable->GetProp( i-1 );
			pArrayProp->SetInsideArray();
			pProp->SetArrayProp( pArrayProp );
		}
		else if ( pProp->GetType() == DPT_DataTable )
		{
			// Recurse into children datatables.
			SetupArrayProps_R<TableType,PropType>( pProp->GetDataTable() );
		}
	}
}

class ExcludeProp
{
public:
	char const	*m_pTableName;
	char const	*m_pPropName;
};

static bool SendTable_GetPropsExcluded( const SendTable *pTable, ExcludeProp *pExcludeProps, int &nExcludeProps, int nMaxExcludeProps )
{
	for(int i=0; i < pTable->m_nProps; i++)
	{
		SendProp *pProp = &pTable->m_pProps[i];

		if ( pProp->IsExcludeProp() )
		{
			char const *pName = pProp->GetExcludeDTName();

			pExcludeProps[nExcludeProps].m_pTableName = pName;
			pExcludeProps[nExcludeProps].m_pPropName = pProp->GetName();
			nExcludeProps++;
		}
		else if ( pProp->GetDataTable() )
		{
			if( !SendTable_GetPropsExcluded( pProp->GetDataTable(), pExcludeProps, nExcludeProps, nMaxExcludeProps ) )
				return false;
		}
	}

	return true;
}

#define PROPINDEX_NUMBITS 12
#define MAX_TOTAL_SENDTABLE_PROPS	(1 << PROPINDEX_NUMBITS)

class CBuildHierarchyStruct
{
public:
	const ExcludeProp	*m_pExcludeProps;
	int					m_nExcludeProps;

	const SendProp		*m_pDatatableProps[MAX_TOTAL_SENDTABLE_PROPS];
	int					m_nDatatableProps;
	
	const SendProp		*m_pProps[MAX_TOTAL_SENDTABLE_PROPS];
	unsigned char		m_PropProxyIndices[MAX_TOTAL_SENDTABLE_PROPS];
	int					m_nProps;

	unsigned char m_nPropProxies;
};

void SendTable_BuildHierarchy( 
	CSendNode *pNode,
	const SendTable *pTable, 
	CBuildHierarchyStruct *bhs
	);

const ExcludeProp* FindExcludeProp(
	char const *pTableName,
	char const *pPropName,
	const ExcludeProp *pExcludeProps, 
	int nExcludeProps)
{
	for ( int i=0; i < nExcludeProps; i++ )
	{
		if ( stricmp(pExcludeProps[i].m_pTableName, pTableName) == 0 && stricmp(pExcludeProps[i].m_pPropName, pPropName ) == 0 )
			return &pExcludeProps[i];
	}

	return NULL;
}

void SendTable_BuildHierarchy_IterateProps(
	CSendNode *pNode,
	const SendTable *pTable, 
	CBuildHierarchyStruct *bhs,
	const SendProp *pNonDatatableProps[MAX_TOTAL_SENDTABLE_PROPS],
	int &nNonDatatableProps )
{
	int i;
	for ( i=0; i < pTable->m_nProps; i++ )
	{
		const SendProp *pProp = &pTable->m_pProps[i];

		if ( pProp->IsExcludeProp() || 
			pProp->IsInsideArray() || 
			FindExcludeProp( pTable->GetName(), pProp->GetName(), bhs->m_pExcludeProps, bhs->m_nExcludeProps ) )
		{
			continue;
		}

		if ( pProp->GetType() == DPT_DataTable )
		{
			if ( pProp->GetFlags() & SPROP_COLLAPSIBLE )
			{
				// This is a base class.. no need to make a new CSendNode (and trigger a bunch of
				// unnecessary send proxy calls in the datatable stacks).
				SendTable_BuildHierarchy_IterateProps( 
					pNode,
					pProp->GetDataTable(), 
					bhs, 
					pNonDatatableProps, 
					nNonDatatableProps );
			}
			else
			{
				// Setup a child datatable reference.
				CSendNode *pChild = new CSendNode;

				// Setup a datatable prop for this node to reference (so the recursion
				// routines can get at the proxy).
				if ( bhs->m_nDatatableProps >= ARRAYSIZE( bhs->m_pDatatableProps ) )
					Error( "Overflowed datatable prop list in SendTable '%s'.", pTable->GetName() );
				
				bhs->m_pDatatableProps[bhs->m_nDatatableProps] = pProp;
				pChild->m_iDatatableProp = bhs->m_nDatatableProps;
				++bhs->m_nDatatableProps;

				pNode->m_Children.AddToTail( pChild );

				// Recurse into the new child datatable.
				SendTable_BuildHierarchy( pChild, pProp->GetDataTable(), bhs );
			}
		}
		else
		{
			if ( nNonDatatableProps >= MAX_TOTAL_SENDTABLE_PROPS )
				Error( "SendTable_BuildHierarchy: overflowed non-datatable props with '%s'.", pProp->GetName() );
			
			pNonDatatableProps[nNonDatatableProps] = pProp;
			++nNonDatatableProps;
		}
	}
}

void SendTable_BuildHierarchy( 
	CSendNode *pNode,
	const SendTable *pTable, 
	CBuildHierarchyStruct *bhs
	)
{
	pNode->m_pTable = pTable;
	pNode->m_iFirstRecursiveProp = bhs->m_nProps;
	
	unsigned char curPropProxy = bhs->m_nPropProxies;
	++bhs->m_nPropProxies;

	const SendProp *pNonDatatableProps[MAX_TOTAL_SENDTABLE_PROPS];
	int nNonDatatableProps = 0;
	
	// First add all the child datatables.
	SendTable_BuildHierarchy_IterateProps(
		pNode,
		pTable,
		bhs,
		pNonDatatableProps,
		nNonDatatableProps );

	// Now add the properties.

	for ( int i=0; i < nNonDatatableProps; i++ )
	{
		bhs->m_pProps[bhs->m_nProps] = pNonDatatableProps[i];
		bhs->m_PropProxyIndices[bhs->m_nProps] = curPropProxy;
		++bhs->m_nProps;
	}

	pNode->m_nRecursiveProps = bhs->m_nProps - pNode->m_iFirstRecursiveProp;
}

void SendTable_SortByPriority(CBuildHierarchyStruct *bhs)
{
	int i, start = 0;

	while( true )
	{
		for ( i = start; i < bhs->m_nProps; i++ )
		{
			const SendProp *p = bhs->m_pProps[i];
			unsigned char c = bhs->m_PropProxyIndices[i];

			if ( p->GetFlags() & SPROP_CHANGES_OFTEN )
			{
				bhs->m_pProps[i] = bhs->m_pProps[start];
				bhs->m_PropProxyIndices[i] = bhs->m_PropProxyIndices[start];
				bhs->m_pProps[start] = p;
				bhs->m_PropProxyIndices[start] = c;
				start++;
				break;
			}
		}

		if ( i == bhs->m_nProps )
			return; 
	}
}

static void SetDataTableProxyIndices_R( 
	CSendTablePrecalc *pMainTable, 
	CSendNode *pCurTable,
	CBuildHierarchyStruct *bhs )
{
	for ( int i=0; i < pCurTable->GetNumChildren(); i++ )
	{
		CSendNode *pNode = pCurTable->GetChild( i );
		const SendProp *pProp = bhs->m_pDatatableProps[pNode->m_iDatatableProp];

		if ( pProp->GetFlags() & SPROP_PROXY_ALWAYS_YES )
		{
			pNode->SetDataTableProxyIndex( DATATABLE_PROXY_INDEX_NOPROXY );
		}
		else
		{
			pNode->SetDataTableProxyIndex( pMainTable->GetNumDataTableProxies() );
			pMainTable->SetNumDataTableProxies( pMainTable->GetNumDataTableProxies() + 1 );
		}

		SetDataTableProxyIndices_R( pMainTable, pNode, bhs );
	}
}

static void SetRecursiveProxyIndices_R( 
	SendTable *pBaseTable,
	CSendNode *pCurTable,
	int &iCurProxyIndex )
{
	if ( iCurProxyIndex >= MAX_PROXY_RESULTS )
		Error( "Too many proxies for datatable %s.", pBaseTable->GetName() );

	pCurTable->SetRecursiveProxyIndex( iCurProxyIndex );
	iCurProxyIndex++;
	
	for ( int i=0; i < pCurTable->GetNumChildren(); i++ )
	{
		CSendNode *pNode = pCurTable->GetChild( i );
		SetRecursiveProxyIndices_R( pBaseTable, pNode, iCurProxyIndex );
	}
}

void CalcPathLengths_R( CSendNode *pNode, CUtlVector<int> &pathLengths, int curPathLength, int &totalPathLengths )
{
	pathLengths[pNode->GetRecursiveProxyIndex()] = curPathLength;
	totalPathLengths += curPathLength;
	
	for ( int i=0; i < pNode->GetNumChildren(); i++ )
	{
		CalcPathLengths_R( pNode->GetChild( i ), pathLengths, curPathLength+1, totalPathLengths );
	}
}

void FillPathEntries_R( CSendTablePrecalc *pPrecalc, CSendNode *pNode, CSendNode *pParent, int &iCurEntry )
{
	// Fill in this node's path.
	CSendTablePrecalc::CProxyPath &outProxyPath = pPrecalc->m_ProxyPaths[ pNode->GetRecursiveProxyIndex() ];
	outProxyPath.m_iFirstEntry = (unsigned short)iCurEntry;

	// Copy all the proxies leading to the parent.
	if ( pParent )
	{
		CSendTablePrecalc::CProxyPath &parentProxyPath = pPrecalc->m_ProxyPaths[pParent->GetRecursiveProxyIndex()];
		outProxyPath.m_nEntries = parentProxyPath.m_nEntries + 1;

		for ( int i=0; i < parentProxyPath.m_nEntries; i++ )
			pPrecalc->m_ProxyPathEntries[iCurEntry++] = pPrecalc->m_ProxyPathEntries[parentProxyPath.m_iFirstEntry+i];
		
		// Now add this node's own proxy.
		pPrecalc->m_ProxyPathEntries[iCurEntry].m_iProxy = pNode->GetRecursiveProxyIndex();
		pPrecalc->m_ProxyPathEntries[iCurEntry].m_iDatatableProp = pNode->m_iDatatableProp;
		++iCurEntry;
	}
	else
	{
		outProxyPath.m_nEntries = 0;
	}

	for ( int i=0; i < pNode->GetNumChildren(); i++ )
	{
		FillPathEntries_R( pPrecalc, pNode->GetChild( i ), pNode, iCurEntry );
	}
}

void SendTable_GenerateProxyPaths( CSendTablePrecalc *pPrecalc, int nProxyIndices )
{
	// Initialize the array.
	pPrecalc->m_ProxyPaths.SetSize( nProxyIndices );
	for ( int i=0; i < nProxyIndices; i++ )
		pPrecalc->m_ProxyPaths[i].m_iFirstEntry = pPrecalc->m_ProxyPaths[i].m_nEntries = 0xFFFF;
	
	// Figure out how long the path down the tree is to each node.
	int totalPathLengths = 0;
	CUtlVector<int> pathLengths;
	pathLengths.SetSize( nProxyIndices );
	memset( pathLengths.Base(), 0, sizeof( pathLengths[0] ) * nProxyIndices );
	CalcPathLengths_R( pPrecalc->GetRootNode(), pathLengths, 0, totalPathLengths );
	
	// 
	int iCurEntry = 0;
	pPrecalc->m_ProxyPathEntries.SetSize( totalPathLengths );
	FillPathEntries_R( pPrecalc, pPrecalc->GetRootNode(), NULL, iCurEntry );
}

bool CSendTablePrecalc::SetupFlatPropertyArray()
{
	SendTable *pTable = GetSendTable();

	// First go through and set SPROP_INSIDEARRAY when appropriate, and set array prop pointers.
	SetupArrayProps_R<SendTable, SendTable::PropType>( pTable );

	// Make a list of which properties are excluded.
	ExcludeProp excludeProps[MAX_EXCLUDE_PROPS];
	int nExcludeProps = 0;
	if( !SendTable_GetPropsExcluded( pTable, excludeProps, nExcludeProps, MAX_EXCLUDE_PROPS ) )
		return false;

	// Now build the hierarchy.
	CBuildHierarchyStruct bhs;
	bhs.m_pExcludeProps = excludeProps;
	bhs.m_nExcludeProps = nExcludeProps;
	bhs.m_nProps = bhs.m_nDatatableProps = 0;
	bhs.m_nPropProxies = 0;
	SendTable_BuildHierarchy( GetRootNode(), pTable, &bhs );

	SendTable_SortByPriority( &bhs );
	
	// Copy the SendProp pointers into the precalc.	
	MEM_ALLOC_CREDIT();
	m_Props.CopyArray( bhs.m_pProps, bhs.m_nProps );
	m_DatatableProps.CopyArray( bhs.m_pDatatableProps, bhs.m_nDatatableProps );
	m_PropProxyIndices.CopyArray( bhs.m_PropProxyIndices, bhs.m_nProps );

	// Assign the datatable proxy indices.
	SetNumDataTableProxies( 0 );
	SetDataTableProxyIndices_R( this, GetRootNode(), &bhs );
	
	int nProxyIndices = 0;
	SetRecursiveProxyIndices_R( pTable, GetRootNode(), nProxyIndices );

	SendTable_GenerateProxyPaths( this, nProxyIndices );
	return true;
}

static bool SendTable_InitTable( SendTable *pTable )
{
	if( pTable->m_pPrecalc )
		return true;
	
	// Create the CSendTablePrecalc.
	CSendTablePrecalc *pPrecalc = (CSendTablePrecalc *)calloc(1, sizeof(CSendTablePrecalc));
	call_mfunc<void, CSendTablePrecalc>(pPrecalc, CSendTablePrecalcCTOR);

	pPrecalc->m_pSendTable = pTable;
	pTable->m_pPrecalc = pPrecalc;

	SendTable_CalcNextVectorElems( pTable );

	// Bind the instrumentation if -dti was specified.
	pPrecalc->m_pDTITable = ServerDTI_HookTable( pTable );

	// Setup its flat property array.
	bool ret = pPrecalc->SetupFlatPropertyArray();
	if ( !ret )
		return false;

	SendTable_Validate( pPrecalc );
	return true;
}

void serverclass_override_t::setup_datatable()
{
	curr_server_info = this;
	SendTable_InitTable(&tbl);
	curr_server_info = nullptr;

	g_SendTables->AddToTail(&tbl);

	*g_SendTableCRC = SendTable_ComputeCRC();
}

static void dump_classinfo(SVC_ClassInfo &classinfomsg)
{
	printf("\nm_bCreateOnClient == %i\n", classinfomsg.m_bCreateOnClient);
	printf("m_Classes == %i\n", classinfomsg.m_Classes.Count());
	printf("m_nNumServerClasses == %i\n", classinfomsg.m_nNumServerClasses);
	for(int i = 0; i < classinfomsg.m_Classes.Count(); ++i) {
		SVC_ClassInfo::class_t &svclass{classinfomsg.m_Classes[i]};
		printf("  %i = %s - %s\n", i, svclass.classname, svclass.datatablename);
	}
}

bf_write			m_FullSendTables;
CUtlMemory<byte>	m_FullSendTablesBuffer;

DETOUR_DECL_STATIC0(SV_CreateBaseline, void)
{
	if(sv_sendtables->GetInt() == 1 ||
		sv_sendtables->GetInt() == 2) {
		sv_sendtables->SetValue(3);
	}

	if(sv_sendtables->GetInt() == 3) {
		ServerClass *pClasses = g_pServerClassHead;

		m_FullSendTablesBuffer.EnsureCapacity( NET_MAX_PAYLOAD );
		m_FullSendTables.StartWriting( m_FullSendTablesBuffer.Base(), m_FullSendTablesBuffer.Count() );

		DetourSV_WriteSendTables( pClasses, m_FullSendTables );

		if ( m_FullSendTables.IsOverflowed() )
		{
			Host_Error("SV_CreateBaseline: WriteSendTables overflow.\n" );
			return;
		}

		// Send class descriptions.
		DetourSV_WriteClassInfos(pClasses, m_FullSendTables);

		if ( m_FullSendTables.IsOverflowed() )
		{
			Host_Error("SV_CreateBaseline: WriteClassInfos overflow.\n" );
			return;
		}
	}

	bool always_send = (sv_sendtables->GetInt() == 3);
	if(always_send) {
		sv_sendtables->SetValue(0);
	}

	DETOUR_STATIC_CALL(SV_CreateBaseline)();

	if(always_send) {
		sv_sendtables->SetValue(3);
	}
}

bool sendfulltables{false};

bool HookSendNetMsg(INetMessage &msg, bool bForceReliable, bool bVoice)
{
	if(msg.GetType() != svc_ClassInfo) {
		RETURN_META_VALUE(MRES_IGNORED, false);
	}

	INetChannel *netchan = META_IFACEPTR(INetChannel);

	SVC_ClassInfo &classinfomsg = (SVC_ClassInfo &)msg;

	if(sendfulltables) {
		if ( m_FullSendTables.IsOverflowed() )
		{
			Host_Error( "Send Table signon buffer overflowed %i bytes!!!\n", m_FullSendTables.GetNumBytesWritten() );
			return false;
		}

		bool ret = netchan->SendData( m_FullSendTables );

		RETURN_META_VALUE(MRES_SUPERCEDE, ret);
	} else {
		RETURN_META_VALUE(MRES_HANDLED, false);
	}
}

DETOUR_DECL_MEMBER0(DetourCGameClientSendSignonData, bool)
{
	CGameClient *pThis = (CGameClient *)this;

	INetChannel *netchan = pThis->GetNetChannel();

	bool always_send = (sv_sendtables->GetInt() == 3);

	int hid = SH_ADD_HOOK(INetChannel, SendNetMsg, netchan, SH_STATIC(HookSendNetMsg), false);

	if(always_send) {
		sv_sendtables->SetValue(0);
		sendfulltables = true;
	}

	in_send_signondata = true;
	bool ret = DETOUR_MEMBER_CALL(DetourCGameClientSendSignonData)();
	in_send_signondata = false;

	sendfulltables = false;

	if(always_send) {
		sv_sendtables->SetValue(3);
	}

	SH_REMOVE_HOOK_ID(hid);

	return ret;
}

CRC32_t invalid_crc;
CRC32_t original_crc;

DETOUR_DECL_STATIC0(SendTable_GetCRC, CRC32_t)
{
	if(in_send_signondata) {
		switch(sv_sendtables->GetInt()) {
			case 2: return invalid_crc;
			case 0: return original_crc;
		}
	}

	return DETOUR_STATIC_CALL(SendTable_GetCRC)();
}

#define Bits2Bytes(b) ((b+7)>>3)

CDetour *SV_WriteClassInfos_detour{nullptr};
CDetour *SV_ComputeClassInfosCRC_detour{nullptr};
CDetour *SV_WriteSendTables_detour{nullptr};
CDetour *SendTable_WriteInfos_detour{nullptr};

bool Sample::SDK_OnLoad(char *error, size_t maxlen, bool late)
{
	if(!gameconfs->LoadGameConfigFile("datamaps", &g_pGameConf, error, maxlen)) {
		return false;
	}

	CONDTABLE_NAME = g_pGameConf->GetKeyValue("CONDTABLE_NAME");
	if(CONDTABLE_NAME == nullptr) {
		snprintf(error, maxlen, "could not get CONDTABLE_NAME key");
		return false;
	}

	const char *CONDTABLE_LIMIT_key = g_pGameConf->GetKeyValue("CONDTABLE_LIMIT");
	if(CONDTABLE_LIMIT_key == nullptr) {
		snprintf(error, maxlen, "could not get CONDTABLE_LIMIT key");
		return false;
	}

	g_pGameConf->GetOffset("CBaseEntity::PostConstructor", &CBaseEntityPostConstructor);
	if(CBaseEntityPostConstructor == -1) {
		snprintf(error, maxlen, "could not get CBaseEntity::PostConstructor offset");
		return false;
	}

	int CBaseEntityUpdateOnRemove{-1};
	g_pGameConf->GetOffset("CBaseEntity::UpdateOnRemove", &CBaseEntityUpdateOnRemove);
	if(CBaseEntityUpdateOnRemove == -1) {
		snprintf(error, maxlen, "could not get CBaseEntity::UpdateOnRemove offset");
		return false;
	}

	g_pGameConf->GetMemSig("SimThink_EntityChanged", &SimThink_EntityChangedPtr);
	if(SimThink_EntityChangedPtr == nullptr) {
		snprintf(error, maxlen, "could not get SimThink_EntityChanged address");
		return false;
	}

	g_pGameConf->GetMemSig("AllocPooledString", &AllocPooledStringPtr);
	if(AllocPooledStringPtr == nullptr) {
		snprintf(error, maxlen, "could not get AllocPooledString address");
		return false;
	}

	g_pGameConf->GetMemSig("EntityFactoryDictionary", &EntityFactoryDictionaryPtr);
	if(EntityFactoryDictionaryPtr == nullptr) {
		snprintf(error, maxlen, "could not get EntityFactoryDictionary address");
		return false;
	}

	g_pGameConf->GetMemSig("CGlobalEntityList::FindEntityByClassname", &CGlobalEntityListFindEntityByClassname);
	if(CGlobalEntityListFindEntityByClassname == nullptr) {
		snprintf(error, maxlen, "could not get CGlobalEntityList::FindEntityByClassname address");
		return false;
	}

	g_pGameConf->GetMemSig("UTIL_Remove", &UTIL_RemovePtr);
	if(UTIL_RemovePtr == nullptr) {
		snprintf(error, maxlen, "could not get UTIL_Remove address");
		return false;
	}

	g_pGameConf->GetMemSig("g_SendTableCRC", (void **)&g_SendTableCRC);
	if(g_SendTableCRC == nullptr) {
		snprintf(error, maxlen, "could not get g_SendTableCRC address");
		return false;
	}

	g_pGameConf->GetMemSig("g_SendTables", (void **)&g_SendTables);
	if(g_SendTables == nullptr) {
		snprintf(error, maxlen, "could not get g_SendTables address");
		return false;
	}

	g_pGameConf->GetMemSig("SV_MaybeWriteSendTable", &SV_MaybeWriteSendTablePtr);
	if(SV_MaybeWriteSendTablePtr == nullptr) {
		snprintf(error, maxlen, "could not get SV_MaybeWriteSendTable address");
		return false;
	}

	g_pGameConf->GetMemSig("CSendTablePrecalc::CSendTablePrecalc", &CSendTablePrecalcCTOR);
	if(CSendTablePrecalcCTOR == nullptr) {
		snprintf(error, maxlen, "could not get CSendTablePrecalc::CSendTablePrecalc address");
		return false;
	}

	g_pGameConf->GetMemSig("ServerDTI_HookTable", &ServerDTI_HookTablePtr);
	if(ServerDTI_HookTablePtr == nullptr) {
		snprintf(error, maxlen, "could not get ServerDTI_HookTable address");
		return false;
	}

	g_pGameConf->GetMemSig("SendTable_Encode", &SendTable_EncodePtr);
	if(SendTable_EncodePtr == nullptr) {
		snprintf(error, maxlen, "could not get SendTable_Encode address");
		return false;
	}

	CDetourManager::Init(g_pSM->GetScriptingEngine(), g_pGameConf);

	SV_EnsureInstanceBaseline_detour = DETOUR_CREATE_STATIC(DetourSV_EnsureInstanceBaseline, "SV_EnsureInstanceBaseline");
	if(!SV_EnsureInstanceBaseline_detour) {
		snprintf(error, maxlen, "could not create SV_EnsureInstanceBaseline detour");
		return false;
	}

	SV_WriteSendTables_detour = DETOUR_CREATE_STATIC(DetourSV_WriteSendTables, "SV_WriteSendTables");
	if(!SV_WriteSendTables_detour) {
		snprintf(error, maxlen, "could not create SV_WriteSendTables detour");
		return false;
	}

	SendTable_WriteInfos_detour = DETOUR_CREATE_STATIC(DetourSendTable_WriteInfos, "SendTable_WriteInfos");
	if(!SendTable_WriteInfos_detour) {
		snprintf(error, maxlen, "could not create SendTable_WriteInfos detour");
		return false;
	}

	SV_WriteClassInfos_detour = DETOUR_CREATE_STATIC(DetourSV_WriteClassInfos, "SV_WriteClassInfos");
	if(!SV_WriteClassInfos_detour) {
		snprintf(error, maxlen, "could not create SV_WriteClassInfos detour");
		return false;
	}

	SV_ComputeClassInfosCRC_detour = DETOUR_CREATE_STATIC(DetourSV_ComputeClassInfosCRC, "SV_ComputeClassInfosCRC");
	if(!SV_ComputeClassInfosCRC_detour) {
		snprintf(error, maxlen, "could not create SV_ComputeClassInfosCRC detour");
		return false;
	}

	SV_ComputeClientPacks_detour = DETOUR_CREATE_STATIC(SV_ComputeClientPacks, "SV_ComputeClientPacks");
	if(!SV_ComputeClientPacks_detour) {
		snprintf(error, maxlen, "could not create SV_ComputeClientPacks detour");
		return false;
	}

	pCGameServerAssignClassIds = DETOUR_CREATE_MEMBER(DetourCGameServerAssignClassIds, "CGameServer::AssignClassIds")
	if(!pCGameServerAssignClassIds) {
		snprintf(error, maxlen, "could not create CGameServer::AssignClassIds detour");
		return false;
	}

	SV_CreateBaseline_detour = DETOUR_CREATE_STATIC(SV_CreateBaseline, "SV_CreateBaseline");
	if(!SV_CreateBaseline_detour) {
		snprintf(error, maxlen, "could not create SV_CreateBaseline detour");
		return false;
	}

	SendTable_GetCRC_detour = DETOUR_CREATE_STATIC(SendTable_GetCRC, "SendTable_GetCRC");
	if(!SendTable_GetCRC_detour) {
		snprintf(error, maxlen, "could not create SendTable_GetCRC detour");
		return false;
	}

	pCGameClientSendSignonData = DETOUR_CREATE_MEMBER(DetourCGameClientSendSignonData, "CGameClient::SendSignonData")
	if(!pCGameClientSendSignonData) {
		snprintf(error, maxlen, "could not create CGameClient::SendSignonData detour");
		return false;
	}

	pPhysicsRunSpecificThink = DETOUR_CREATE_MEMBER(PhysicsRunSpecificThink, "CBaseEntity::PhysicsRunSpecificThink")
	if(!pPhysicsRunSpecificThink) {
		snprintf(error, maxlen, "could not create CBaseEntity::PhysicsRunSpecificThink detour");
		return false;
	}

	pCGameServerAssignClassIds->EnableDetour();
	SendTable_GetCRC_detour->EnableDetour();
	pCGameClientSendSignonData->EnableDetour();
	pPhysicsRunSpecificThink->EnableDetour();
	SV_ComputeClassInfosCRC_detour->EnableDetour();
	SV_WriteClassInfos_detour->EnableDetour();
	SV_WriteSendTables_detour->EnableDetour();
	SendTable_WriteInfos_detour->EnableDetour();
	SV_CreateBaseline_detour->EnableDetour();
	SV_EnsureInstanceBaseline_detour->EnableDetour();

#if SOURCE_ENGINE == SE_TF2
	{
		void **vtable = *(void ***)server;
		int index = vfunc_index(&CBaseServer::WriteDeltaEntities);
		CBaseServer_WriteDeltaEntities_detour = DETOUR_CREATE_MEMBER(CBaseServer_WriteDeltaEntities, vtable[index]);
	}
#endif

#ifdef SOURCEHOOK_BEING_STUPID
	{
		void **vtable = *(void ***)engine;
		int index = vfunc_index(&IVEngineServer::PvAllocEntPrivateData);
		pPvAllocEntPrivateData = DETOUR_CREATE_MEMBER(DetourPvAllocEntPrivateData, vtable[index])
		pPvAllocEntPrivateData->EnableDetour();
	}
#endif

	original_crc = *g_SendTableCRC;

	invalid_crc = CRC32_ProcessSingleBuffer("no", 3);

	CONDTABLE_LIMIT = V_atoi(CONDTABLE_LIMIT_key);

	SH_MANUALHOOK_RECONFIGURE(UpdateOnRemove, CBaseEntityUpdateOnRemove, 0, 0);

	g_pEntityList = reinterpret_cast<CGlobalEntityList *>(gamehelpers->GetGlobalEntityList());

	SH_ADD_HOOK(CGlobalEntityList, OnAddEntity, ((CGlobalEntityList *)g_pEntityList), SH_MEMBER(((CGlobalEntityListHack *)g_pEntityList), &CGlobalEntityListHack::HookOnAddEntity), false);

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
	SH_REMOVE_HOOK(CGlobalEntityList, OnAddEntity, ((CGlobalEntityList *)g_pEntityList), SH_MEMBER(((CGlobalEntityListHack *)g_pEntityList), &CGlobalEntityListHack::HookOnAddEntity), false);

	SendTable_GetCRC_detour->Destroy();
	pCGameClientSendSignonData->Destroy();
	SV_ComputeClientPacks_detour->Destroy();
	SV_WriteClassInfos_detour->Destroy();
	SV_ComputeClassInfosCRC_detour->Destroy();
	SV_WriteSendTables_detour->Destroy();
	SendTable_WriteInfos_detour->Destroy();
	CBaseServer_WriteDeltaEntities_detour->Destroy();
	SV_CreateBaseline_detour->Destroy();
	SV_EnsureInstanceBaseline_detour->Destroy();
	if(pPvAllocEntPrivateData) {
		pPvAllocEntPrivateData->Destroy();
	}
	pPhysicsRunSpecificThink->Destroy();
	pCGameServerAssignClassIds->Destroy();
	g_pSDKHooks->RemoveEntityListener(this);
#ifdef __HAS_PROXYSEND
	if(proxysend) {
		proxysend->remove_listener(this);
	}
#endif
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
	} else {
		SV_ComputeClientPacks_detour->EnableDetour();
		CBaseServer_WriteDeltaEntities_detour->EnableDetour();
	}
#else
	SV_ComputeClientPacks_detour->EnableDetour();
	CBaseServer_WriteDeltaEntities_detour->EnableDetour();
#endif

	g_pSDKHooks->AddEntityListener(this);
	
#if SOURCE_ENGINE == SE_LEFT4DEAD2
	server = g_pSDKTools->GetIServer();
	void **vtable = *(void ***)server;
	int index = vfunc_index(&CBaseServer::WriteDeltaEntities);
	CBaseServer_WriteDeltaEntities_detour = DETOUR_CREATE_MEMBER(CBaseServer_WriteDeltaEntities, vtable[index]);
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
		proxysend->remove_listener(this);
		proxysend = NULL;
		SV_ComputeClientPacks_detour->EnableDetour();
		CBaseServer_WriteDeltaEntities_detour->EnableDetour();
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
	for(int i = 0; i < m_pInstanceBaselineTable->GetNumStrings(); ++i) {
		const char *str = m_pInstanceBaselineTable->GetString(i);
		int id = V_atoi(str);
		META_CONPRINTF("%i = %s = %s\n", i, str, baselinemap[id] ? baselinemap[id]->GetName() : "NULL");
	}
}

CON_COMMAND(dump_dt_precalc, "")
{
	if (args.ArgC() < 2)
	{
		META_CONPRINT("Usage: dump_dt_precalc <cls>\n");
		return;
	}

	const char *cls = args.Arg(1);
	if (!cls || cls[0] == '\0')
	{
		META_CONPRINT("Usage: dump_dt_precalc <cls>\n");
		return;
	}

	ServerClass *pClass = gamehelpers->FindServerClass(cls);
	if (!pClass)
	{
		META_CONPRINT("invalid cls\n");
		return;
	}

	for(int i = 0; i < pClass->m_pTable->m_pPrecalc->GetNumProps(); ++i) {
		const SendProp *prop = pClass->m_pTable->m_pPrecalc->GetProp(i);
		printf("%i %s\n", i, prop->GetName());
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

CON_COMMAND(dump_serverclass_edict, "")
{
	if (args.ArgC() < 2)
	{
		META_CONPRINT("Usage: dump_serverclass_edict <edict>\n");
		return;
	}

	const char *edict_str = args.Arg(1);
	if (!edict_str || edict_str[0] == '\0')
	{
		META_CONPRINT("Usage: dump_serverclass_edict <edict>\n");
		return;
	}

	CBaseEntity *pEntity = gamehelpers->ReferenceToEntity(V_atoi(edict_str));
	if (!pEntity)
	{
		META_CONPRINT("invalid edict index\n");
		return;
	}

	ServerClass *pClass = pEntity->GetServerClass();

	FILE *fp = stdout;

	fprintf(fp,"%s\n",pClass->GetName());
	fprintf(fp,"  Entity Classname: %s\n",gamehelpers->GetEntityClassname(pEntity));
	fprintf(fp,"  ClassID: %i (%s)\n", pClass->m_ClassID, baselinemap[pClass->m_ClassID] ? baselinemap[pClass->m_ClassID]->GetName() : "NULL");
	if(pClass->m_InstanceBaselineIndex == INVALID_STRING_INDEX) {
		fprintf(fp,"  InstanceBaselineIndex: INVALID_STRING_INDEX\n");
	} else {
		const char *str = m_pInstanceBaselineTable->GetString(pClass->m_InstanceBaselineIndex);
		int id = V_atoi(str);
		fprintf(fp,"  InstanceBaselineIndex: %i = %s (%s)\n", pClass->m_InstanceBaselineIndex, str, baselinemap[id] ? baselinemap[id]->GetName() : "NULL");
	}
	fprintf(fp,"  Table Name: %s\n", pClass->m_pTable->GetName());
	auto it{server_ptr_map.find(pClass)};
	if(it != server_ptr_map.end()) {
		serverclass_override_t &overr{*it->second};
		fprintf(fp,"  Server-Side ClassID: %i (%s)\n", overr.classid, baselinemap[overr.classid] ? baselinemap[overr.classid]->GetName() : "NULL");
		fprintf(fp,"  Server-Side Class Name: %s\n", overr.cls_name.c_str());
		fprintf(fp,"  Server-Side Table Name: %s\n", overr.tbl_name.c_str());
		fprintf(fp,"  Client-Side ClassID: %i (%s)\n", overr.cl_classid, baselinemap[overr.cl_classid] ? baselinemap[overr.cl_classid]->GetName() : "NULL");
		fprintf(fp,"  Client-Side Class Name: %s\n", overr.cls_cl_name.c_str());
		fprintf(fp,"  Client-Side Table Name: %s\n", overr.tbl_cl_name.c_str());
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

	ServerClass *pClass = g_pServerClassHead;
	while(pClass) {
		fprintf(fp,"%s\n",pClass->GetName());
		fprintf(fp,"  ClassID: %i (%s)\n", pClass->m_ClassID, baselinemap[pClass->m_ClassID] ? baselinemap[pClass->m_ClassID]->GetName() : "NULL");
		if(pClass->m_InstanceBaselineIndex == INVALID_STRING_INDEX) {
			fprintf(fp,"  InstanceBaselineIndex: INVALID_STRING_INDEX\n");
		} else {
			const char *str = m_pInstanceBaselineTable->GetString(pClass->m_InstanceBaselineIndex);
			int id = V_atoi(str);
			fprintf(fp,"  InstanceBaselineIndex: %i = %s (%s)\n", pClass->m_InstanceBaselineIndex, str, baselinemap[id] ? baselinemap[id]->GetName() : "NULL");
		}
		fprintf(fp,"  Table Name: %s\n", pClass->m_pTable->GetName());
		auto it{server_ptr_map.find(pClass)};
		if(it != server_ptr_map.end()) {
			serverclass_override_t &overr{*it->second};
			fprintf(fp,"  Server-Side ClassID: %i (%s)\n", overr.classid, baselinemap[overr.classid] ? baselinemap[overr.classid]->GetName() : "NULL");
			fprintf(fp,"  Server-Side Class Name: %s\n", overr.cls_name.c_str());
			fprintf(fp,"  Server-Side Table Name: %s\n", overr.tbl_name.c_str());
			fprintf(fp,"  Client-Side ClassID: %i (%s)\n", overr.cl_classid, baselinemap[overr.cl_classid] ? baselinemap[overr.cl_classid]->GetName() : "NULL");
			fprintf(fp,"  Client-Side Class Name: %s\n", overr.cls_cl_name.c_str());
			fprintf(fp,"  Client-Side Table Name: %s\n", overr.tbl_cl_name.c_str());
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
