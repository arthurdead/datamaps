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

#define BASEENTITY_H
#define NEXT_BOT
#define GLOWS_ENABLE
#define TF_DLL
#define USES_ECON_ITEMS
#define USE_NAV_MESH
#define RAD_TELEMETRY_DISABLED

#include "extension.h"
#include <string>
#include <vector>
#include <unordered_map>
#include <mathlib/vmatrix.h>
#include <toolframework/itoolentity.h>
#include <ehandle.h>
#include <eiface.h>
#include <dt_common.h>
#include <shareddefs.h>
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
IServerTools *servertools = nullptr;
ServerClass *g_pServerClassHead = nullptr;
ServerClass *g_pServerClassTail = nullptr;
INetworkStringTableContainer *netstringtables = NULL;
INetworkStringTable *m_pInstanceBaselineTable = nullptr;
IServer *server = nullptr;
//IServerGameDLL *gamedll = nullptr;

HandleType_t factory_handle = 0;
HandleType_t datamap_handle = 0;
HandleType_t removal_handle = 0;
HandleType_t serverclass_handle = 0;

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
};

void remove_all_entities(const std::string &name)
{
	CBaseEntity *pEntity = nullptr;
	while(pEntity = servertools->FindEntityByClassname(pEntity, name.c_str())) {
		servertools->RemoveEntity(pEntity);
	}
}

template <typename T>
void loop_all_entities(T func, const std::string &name)
{
	CBaseEntity *pEntity = nullptr;
	while((pEntity = servertools->FindEntityByClassname(pEntity, name.c_str())) != nullptr) {
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
};

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
};

SH_DECL_MANUALHOOK0_void(GenericDtor, 0, 0, 0)
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
			int offset = desc.fieldOffset[TD_OFFSET_NORMAL];
			switch(desc.fieldType) {
				case FIELD_INTEGER: { *(int *)(((unsigned char *)pEntity) + offset) = 0; break; }
				case FIELD_FLOAT: { *(float *)(((unsigned char *)pEntity) + offset) = 0.0f; break; }
				case FIELD_BOOLEAN: { *(bool *)(((unsigned char *)pEntity) + offset) = false; break; }
				case FIELD_EHANDLE: { *(EHANDLE *)(((unsigned char *)pEntity) + offset) = nullptr; break; }
				case FIELD_VECTOR: {
					vec3_t &vec = *(vec3_t *)(((unsigned char *)pEntity) + offset);
					vec[0] = 0.0f;
					vec[1] = 0.0f;
					vec[2] = 0.0f;
					break;
				}
			}
		}
	}
	
	void update_offsets()
	{
		for(custom_typedescription_t &desc : dataDesc) {
			desc.fieldOffset[TD_OFFSET_NORMAL] += base;
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
		desc.fieldOffset[TD_OFFSET_NORMAL] = size;
		if(was_overriden && base != 0) {
			desc.fieldOffset[TD_OFFSET_NORMAL] += base;
		}
		desc.fieldSize = 1;
		
		switch(type) {
			case custom_prop_int: {
				desc.fieldType = FIELD_INTEGER;
				desc.fieldSizeInBytes = sizeof(int);
				break;
			}
			case custom_prop_float: {
				desc.fieldType = FIELD_FLOAT;
				desc.fieldSizeInBytes = sizeof(float);
				break;
			}
			case custom_prop_bool: {
				desc.fieldType = FIELD_BOOLEAN;
				desc.fieldSizeInBytes = sizeof(bool);
				break;
			}
			case custom_prop_entity: {
				desc.fieldType = FIELD_EHANDLE;
				desc.fieldSizeInBytes = sizeof(EHANDLE);
				break;
			}
			case custom_prop_vector: {
				desc.fieldType = FIELD_VECTOR;
				desc.fieldSizeInBytes = sizeof(vec3_t);
				break;
			}
		}
		
		size += desc.fieldSizeInBytes;
		
		desc.fieldOffset[TD_OFFSET_PACKED] = 0;
		desc.externalName = nullptr;
		desc.pSaveRestoreOps = nullptr;
		desc.inputFunc = nullptr;
		desc.td = nullptr;
		desc.override_field = nullptr;
		desc.override_count = 0;
		desc.fieldTolerance = 0.0f;
		
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
	prop->SetOffset(realprop->GetOffset());
	prop->SetProxyFn(realprop->GetProxyFn());
	prop->SetDataTableProxyFn(realprop->GetDataTableProxyFn());
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

SendTable *get_send_table(const char *classname, const char *tablename, SendTable **clstable = nullptr)
{
	ServerClass *srvcls = gamehelpers->FindServerClass(classname);
	if(!srvcls) {
		return nullptr;
	}
	
	if(clstable) {
		*clstable = srvcls->m_pTable;
	}
	
	return UTIL_FindSendtableInSendTable(srvcls->m_pTable, tablename);
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
	
	void increment_svclasses()
	{
		++serverclasses;
		serverclassbits = Q_log2( serverclasses ) + 1;
	}
	
	void decrement_svclasses()
	{
		--serverclasses;
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

custom_prop_info_t::custom_prop_info_t(IEntityFactory *fac_, std::string &&clsname_)
	: fac{fac_}, clsname{std::move(clsname_)}
{
	map.chains_validated = 0;
	map.packed_offsets_computed = 0;
	map.packed_size = 0;
	
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
	
	bool has_custom_prop = custom_prop != nullptr;
	bool has_custom_server = custom_server != nullptr;
	
	if(based != nullptr) {
		last_cb = based->GetEntitySize();
		net = based->Create(pClassName);
		CBaseEntity *pEntity = net->GetBaseEntity();
		if(has_custom_prop) {
			if(pEntity) {
				custom_prop->do_override(pEntity);
			}
		}
		if(has_custom_server) {
			custom_server->do_override(pEntity);
		}
	} else if(func != nullptr) {
		cell_t res = 0;
		func->PushCell(has_custom_prop ? custom_prop->size : 0);
		func->Execute(&res);
		last_cb = size;
		CBaseEntity *obj = (CBaseEntity *)res;
		if(obj != nullptr) {
			obj->PostConstructor(pClassName);
			net = obj->GetNetworkable();
			if(has_custom_prop) {
				custom_prop->do_override(obj);
			}
			if(has_custom_server) {
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
	IServerNetworkable *net = SH_CALL(fac, &IEntityFactory::Create)(classname);
	
	CBaseEntity *pEntity = net->GetBaseEntity();
	
	do_override(pEntity);
	
	RETURN_META_VALUE(MRES_SUPERCEDE, net);
}

void *HookPvAllocEntPrivateData(long cb)
{
	if(curr_data_info != nullptr) {
		last_cb = cb;
		cb += curr_data_info->size;
		RETURN_META_VALUE_NEWPARAMS(MRES_HANDLED, nullptr, &IVEngineServer::PvAllocEntPrivateData, (cb));
	} else {
		RETURN_META_VALUE(MRES_IGNORED, nullptr);
	}
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

	SendTable *clstable = nullptr;
	SendTable *table = get_send_table(factory->clsname.c_str(), tablename, &clstable);
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
	
	ServerClass *svcls = gamehelpers->FindServerClass(netname);
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
	
	ServerClass *netclass = gamehelpers->FindServerClass(netname);
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
	
	ServerClass *netclass = gamehelpers->FindServerClass(netname);
	if(!netclass) {
		return pContext->ThrowNativeError("invalid netname %s", netname);
	}
	
	serverclass_override_t *obj = new serverclass_override_t{factory, classname, netclass};
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
	
	ServerClass *netclass = gamehelpers->FindServerClass(netname);
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
	{"CustomSendtable.override_with", CustomSendtableoverride_with},
	{"CustomSendtable.override_with_ex", CustomSendtableoverride_with_ex},
	{"CustomSendtable.unexclude_prop", CustomSendtableunexclude_prop},
	{"CustomSendtable.set_base_class", CustomSendtableset_base_class},
	{"CustomDatamap.from_classname", CustomDatamapfrom_classname},
	{"CustomDatamap.from_factory", CustomDatamapfrom_factory},
	{"CustomDatamap.add_prop", CustomDatamapadd_prop},
	{NULL, NULL}
};

void Sample::OnPluginUnloaded(IPlugin *plugin)
{
	
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

bool Sample::SDK_OnMetamodLoad(ISmmAPI *ismm, char *error, size_t maxlen, bool late)
{
	gpGlobals = ismm->GetCGlobals();
	GET_V_IFACE_ANY(GetServerFactory, servertools, IServerTools, VSERVERTOOLS_INTERFACE_VERSION)
	GET_V_IFACE_ANY(GetEngineFactory, engine, IVEngineServer, INTERFACEVERSION_VENGINESERVER)
	dictionary = (CEntityFactoryDictionary *)servertools->GetEntityFactoryDictionary();
	SH_ADD_HOOK(IVEngineServer, PvAllocEntPrivateData, engine, SH_STATIC(&HookPvAllocEntPrivateData), false);
	GET_V_IFACE_ANY(GetServerFactory, gamedll, IServerGameDLL, INTERFACEVERSION_SERVERGAMEDLL)
	GET_V_IFACE_ANY(GetEngineFactory, netstringtables, INetworkStringTableContainer, INTERFACENAME_NETWORKSTRINGTABLESERVER)
	g_pServerClassHead = gamedll->GetAllServerClasses();
	g_pServerClassTail = g_pServerClassHead;
	while(g_pServerClassTail && g_pServerClassTail->m_pNext) {
		g_pServerClassTail = g_pServerClassTail->m_pNext;
	}
	custom_server_classid = g_pServerClassTail->m_ClassID;
	m_pInstanceBaselineTable = netstringtables->FindTable(INSTANCE_BASELINE_TABLENAME);
	server = engine->GetIServer();
	return true;
}

bool Sample::SDK_OnLoad(char *error, size_t maxlen, bool late)
{
	IGameConfig *g_pGameConf = nullptr;
	gameconfs->LoadGameConfigFile("datamaps", &g_pGameConf, error, maxlen);

	g_pGameConf->GetOffset("CBaseEntity::PostConstructor", &CBaseEntityPostConstructor);

	gameconfs->CloseGameConfigFile(g_pGameConf);
	
	g_pEntityList = reinterpret_cast<CBaseEntityList *>(gamehelpers->GetGlobalEntityList());
	
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

	g_pSDKHooks->AddEntityListener(this);
	
	sharesys->AddNatives(myself, natives);
}

bool Sample::QueryRunning(char *error, size_t maxlength)
{
	SM_CHECK_IFACE(SDKHOOKS, g_pSDKHooks);
	return true;
}

bool Sample::QueryInterfaceDrop(SMInterface *pInterface)
{
	if(pInterface == g_pSDKHooks)
		return false;

	return IExtensionInterface::QueryInterfaceDrop(pInterface);
}

void Sample::NotifyInterfaceDrop(SMInterface *pInterface)
{
	if(strcmp(pInterface->GetInterfaceName(), SMINTERFACE_SDKHOOKS_NAME) == 0)
	{
		g_pSDKHooks->RemoveEntityListener(this);
		g_pSDKHooks = NULL;
	}
}

void Sample::SDK_OnUnload()
{
	g_pSDKHooks->RemoveEntityListener(this);
	plsys->RemovePluginsListener(this);
	handlesys->RemoveType(factory_handle, myself->GetIdentity());
	handlesys->RemoveType(datamap_handle, myself->GetIdentity());
	handlesys->RemoveType(removal_handle, myself->GetIdentity());
	handlesys->RemoveType(serverclass_handle, myself->GetIdentity());
}
