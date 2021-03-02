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
#include <string_view>
#include <vector>
#include <unordered_map>
#include <mathlib/vmatrix.h>
#include <toolframework/itoolentity.h>
#include <ehandle.h>
#include <eiface.h>
#include <dt_common.h>
#include <shareddefs.h>
#include <util.h>
#include <tier1/utldict.h>

using vec3_t = vec_t[3];
using EHANDLE = CHandle<CBaseEntity>;

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

HandleType_t factory_handle = 0;
HandleType_t datamap_handle = 0;
HandleType_t removal_handle = 0;

template <typename T>
T void_to_func(void *ptr)
{
	union { T f; void *p; };
	p = ptr;
	return f;
}

class CBaseEntity : public IServerEntity
{
public:
	DECLARE_CLASS_NOBASE( CBaseEntity );
	DECLARE_SERVERCLASS();
	DECLARE_DATADESC();
	
	void PostConstructor(const char *classname)
	{
		void **vtable = *(void ***)this;
		(this->*void_to_func<void (CBaseEntity::*)(const char *classname)>(vtable[CBaseEntityPostConstructor]))(classname);
	}
};

void remove_all_entities(std::string_view name)
{
	CBaseEntity *pEntity = nullptr;
	while(pEntity = servertools->FindEntityByClassname(pEntity, name.data())) {
		servertools->RemoveEntity(pEntity);
	}
}

template <typename T>
void loop_all_entities(T func, std::string_view name)
{
	CBaseEntity *pEntity = nullptr;
	while(pEntity = servertools->FindEntityByClassname(pEntity, name.data())) {
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
	
	IEntityFactory *get_name_factory(std::string_view name)
	{
		for(int i = 0; i < m_Factories.Count(); i++) {
			if(m_Factories.GetElementName(i) == name) {
				return m_Factories[i];
			}
		}
		return nullptr;
	}
	
	void remove_factory(IEntityFactory *fac, std::string_view name, bool remove_entities = true);
	
	void remove_factory(std::string_view name, bool remove_entities = true)
	{
		remove_factory(get_name_factory(name), name, remove_entities);
	}
	
	void remove_factory(IEntityFactory *fac, bool remove_entities = true)
	{
		remove_factory(fac, get_factory_name(fac), remove_entities);
	}
	
	static bool is_factory_custom(IEntityFactory *fac)
	{
		return (fac->GetEntitySize() == (size_t)-1);
	}
};

CEntityFactoryDictionary *dictionary = nullptr;

class sp_entity_factory : public IEntityFactory
{
	sp_entity_factory(std::string &&name_, bool remove_entities_);
public:
	sp_entity_factory(std::string &&name_, IPluginFunction *func_, size_t size_, bool remove_entities_)
	: sp_entity_factory(std::move(name_), remove_entities_)
	{
		func = func_;
		size = size_;
	}
	sp_entity_factory(std::string &&name_, IEntityFactory *based_, bool remove_entities_)
		: sp_entity_factory(std::move(name_), remove_entities_)
	{
		based = based_;
		size = based->GetEntitySize();
	}
	~sp_entity_factory();
	IServerNetworkable *Create(const char *pClassName)
	{
		IServerNetworkable *net = nullptr;
		
		if(based != nullptr) {
			net = based->Create(pClassName);
		} else if(func != nullptr) {
			cell_t res = 0;
			func->Execute(&res);
			CBaseEntity *obj = (CBaseEntity *)res;
			if(obj != nullptr) {
				obj->PostConstructor(pClassName);
				net = obj->GetNetworkable();
			}
		}
		
		return net;
	}
	void Destroy(IServerNetworkable *pNetworkable) {}
	size_t GetEntitySize() { return (size_t)-1; }
	
	std::string name;
	IEntityFactory *based = nullptr;
	IPluginFunction *func = nullptr;
	bool remove_entities = true;
	size_t size = 0;
	
	Handle_t hndl = BAD_HANDLE;
	IPluginContext *pContext = nullptr;
	bool freehndl = true;
	bool dont_delete = false;
};

struct factory_removal_t
{
	factory_removal_t(std::string &&name_, IEntityFactory *fac_, bool remove_entities)
		: name{std::move(name_)}, based{fac_}
	{
		dictionary->remove_factory(based, name, remove_entities);
	}
	
	~factory_removal_t()
	{
		dictionary->m_Factories.Remove(name.c_str());
		dictionary->InstallFactory(based, name.c_str());
	}
	
	std::string name;
	IEntityFactory *based;
};

using factory_map_t = std::unordered_map<std::string, sp_entity_factory *>;
factory_map_t factory_map{};

sp_entity_factory::sp_entity_factory(std::string &&name_, bool remove_entities_)
	: name(std::move(name_)), remove_entities{remove_entities_}
{
	factory_map[name] = this;

	dictionary->InstallFactory(this, name.c_str());
}

sp_entity_factory::~sp_entity_factory()
{
	dont_delete = true;
	
	dictionary->remove_factory(this, name.c_str(), remove_entities);
	
	factory_map.erase(name);
	
	if(freehndl) {
		if(hndl != BAD_HANDLE) {
			HandleSecurity security(pContext->GetIdentity(), myself->GetIdentity());
			handlesys->FreeHandle(hndl, &security);
		}
	}
}

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
	
	void set_name(std::string_view name)
	{
		size_t len = name.length();
		fieldName = (char *)malloc(len+1);
		strncpy((char *)fieldName, name.data(), len);
		((char *)fieldName)[len] = '\0';
	}
	
	void clear_name()
	{
		if(fieldName != nullptr) {
			free((void *)fieldName);
		}
	}
	
	~custom_typedescription_t()
	{
		//clear_name();
	}
};

struct custom_datamap_t : datamap_t
{
	void set_name(std::string_view name)
	{
		size_t len = name.length();
		dataClassName = (char *)malloc(len+1);
		strncpy((char *)dataClassName, name.data(), len);
		((char *)dataClassName)[len] = '\0';
	}
	
	void clear_name()
	{
		if(dataClassName != nullptr) {
			free((void *)dataClassName);
		}
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
	bool remove_entities = true;
	bool erase = true;
	bool freehndl = true;
	
	custom_prop_info_t(IEntityFactory *fac_, std::string &&clsname_);
	~custom_prop_info_t();
	
	void zero(CBaseEntity *pEntity)
	{
		for(custom_typedescription_t &desc : dataDesc) {
			switch(desc.fieldType) {
				case FIELD_INTEGER: { *(int *)(((unsigned char *)pEntity) + desc.fieldOffset[TD_OFFSET_NORMAL]) = 0; break; }
				case FIELD_FLOAT: { *(float *)(((unsigned char *)pEntity) + desc.fieldOffset[TD_OFFSET_NORMAL]) = 0.0f; break; }
				case FIELD_BOOLEAN: { *(bool *)(((unsigned char *)pEntity) + desc.fieldOffset[TD_OFFSET_NORMAL]) = false; break; }
				case FIELD_EHANDLE: { *(EHANDLE *)(((unsigned char *)pEntity) + desc.fieldOffset[TD_OFFSET_NORMAL]) = nullptr; break; }
				case FIELD_VECTOR: {
					vec3_t &vec = *(vec3_t *)(((unsigned char *)pEntity) + desc.fieldOffset[TD_OFFSET_NORMAL]);
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
	
	bool has_prop(std::string_view name)
	{
		for(custom_typedescription_t &desc : dataDesc) {
			if(desc.fieldName == name) {
				return true;
			}
		}
		
		return false;
	}
	
	void remove_prop(std::string_view name)
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
	
	void add_prop(std::string_view name, custom_prop_type type)
	{
		custom_typedescription_t &desc = dataDesc.emplace_back();
		
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
	
	IServerNetworkable *HookCreate(const char *classname);
};

custom_prop_info_t *currinfo = nullptr;

using info_map_t = std::unordered_map<std::string, custom_prop_info_t *>;
info_map_t info_map{};

void CEntityFactoryDictionary::remove_factory(IEntityFactory *fac, std::string_view name, bool remove_entities)
{
	std::string clsname{name};
	info_map_t::iterator it{info_map.find(clsname)};
	if(it != info_map.end()) {
		custom_prop_info_t *prop{it->second};
		prop->erase = false;
		if(remove_entities) {
			prop->remove_entities = false;
		}
		delete prop;
		info_map.erase(it);
	}
	
	m_Factories.Remove(name.data());
	
	if(CEntityFactoryDictionary::is_factory_custom(fac)) {
		sp_entity_factory *sp_fac = (sp_entity_factory *)fac;
		if(!sp_fac->dont_delete) {
			delete sp_fac;
		}
	}
	
	if(remove_entities) {
		remove_all_entities(name);
	}
}

custom_prop_info_t::custom_prop_info_t(IEntityFactory *fac_, std::string &&clsname_)
	: fac{fac_}, clsname{std::move(clsname_)}
{
	map.chains_validated = 0;
	map.packed_offsets_computed = 0;
	map.packed_size = 0;
	
	SH_ADD_HOOK(IEntityFactory, Create, fac, SH_MEMBER(this, &custom_prop_info_t::HookCreate), false);
	
	info_map[clsname] = this;
}

custom_prop_info_t::~custom_prop_info_t()
{
	if(erase) {
		info_map.erase(clsname);
	}
	
	SH_REMOVE_HOOK(IEntityFactory, Create, fac, SH_MEMBER(this, &custom_prop_info_t::HookCreate), false);
	
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
	
	if(remove_entities) {
		remove_all_entities(clsname);
	}
}

IServerNetworkable *custom_prop_info_t::HookCreate(const char *classname)
{
	IEntityFactory *fac = META_IFACEPTR(IEntityFactory);
	
	currinfo = this;
	IServerNetworkable *net = SH_CALL(fac, &IEntityFactory::Create)(classname);
	currinfo = nullptr;
	
	CBaseEntity *pEntity = net->GetBaseEntity();
	
	if(!was_overriden) {
		datamap_t *basemap = gamehelpers->GetDataMap(pEntity);
		
		std::string mapname{basemap->dataClassName};
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
	
	RETURN_META_VALUE(MRES_SUPERCEDE, net);
}

void *HookPvAllocEntPrivateData(long cb)
{
	if(currinfo != nullptr) {
		last_cb = cb;
		cb += currinfo->size;
		RETURN_META_VALUE_NEWPARAMS(MRES_HANDLED, nullptr, &IVEngineServer::PvAllocEntPrivateData, (cb));
	} else {
		RETURN_META_VALUE(MRES_IGNORED, nullptr);
	}
}

cell_t entity_factory_exists(IPluginContext *pContext, const cell_t *params)
{
	char *name = nullptr;
	pContext->LocalToString(params[1], &name);
	
	return dictionary->FindFactory(name) != nullptr;
}

cell_t entity_factory_is_custom(IPluginContext *pContext, const cell_t *params)
{
	char *name = nullptr;
	pContext->LocalToString(params[1], &name);
	
	IEntityFactory *factory = dictionary->FindFactory(name);
	if(!factory) {
		return pContext->ThrowNativeError("invalid classname %s", name);
	}
	
	return CEntityFactoryDictionary::is_factory_custom(factory);
}

cell_t get_entity_factory_size(IPluginContext *pContext, const cell_t *params)
{
	char *name = nullptr;
	pContext->LocalToString(params[1], &name);
	
	IEntityFactory *factory = dictionary->FindFactory(name);
	if(!factory) {
		return pContext->ThrowNativeError("invalid classname %s", name);
	}
	
	return factory->GetEntitySize();
}

cell_t remove_entity_factory(IPluginContext *pContext, const cell_t *params)
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
	
	factory_removal_t *obj = new factory_removal_t{name, factory, params[3]};
	return handlesys->CreateHandle(removal_handle, obj, pContext->GetIdentity(), myself->GetIdentity(), nullptr);
}

cell_t register_entity_factory(IPluginContext *pContext, const cell_t *params)
{
	char *name = nullptr;
	pContext->LocalToString(params[1], &name);
	
	IEntityFactory *factory = dictionary->FindFactory(name);
	if(factory) {
		return pContext->ThrowNativeError("%s is already registered", name);
	}
	
	char *based = nullptr;
	pContext->LocalToString(params[2], &based);

	factory = dictionary->FindFactory(based);
	if(!factory) {
		return pContext->ThrowNativeError("invalid classname %s", based);
	}
	
	sp_entity_factory *obj = new sp_entity_factory(name, factory, params[3]);
	
	Handle_t hndl = handlesys->CreateHandle(factory_handle, obj, pContext->GetIdentity(), myself->GetIdentity(), nullptr);
	obj->hndl = hndl;
	obj->pContext = pContext;
	
	return hndl;
}

cell_t register_entity_factory_ex(IPluginContext *pContext, const cell_t *params)
{
	char *name = nullptr;
	pContext->LocalToString(params[1], &name);
	
	IEntityFactory *factory = dictionary->FindFactory(name);
	if(factory) {
		return pContext->ThrowNativeError("%s is already registered", name);
	}
	
	IPluginFunction *callback = pContext->GetFunctionById(params[2]);
	
	sp_entity_factory *obj = new sp_entity_factory(name, callback, params[3], params[4]);
	
	Handle_t hndl = handlesys->CreateHandle(factory_handle, obj, pContext->GetIdentity(), myself->GetIdentity(), nullptr);
	obj->hndl = hndl;
	obj->pContext = pContext;
	
	return hndl;
}

cell_t has_prop(IPluginContext *pContext, const cell_t *params)
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
	
	return obj->has_prop(name);
}

cell_t remove_prop(IPluginContext *pContext, const cell_t *params)
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
	
	obj->remove_prop(name);
	return 0;
}

cell_t add_prop(IPluginContext *pContext, const cell_t *params)
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

cell_t from_classname(IPluginContext *pContext, const cell_t *params)
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

cell_t from_factory(IPluginContext *pContext, const cell_t *params)
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

sp_nativeinfo_t natives[] =
{
	{"entity_factory_exists", entity_factory_exists},
	{"entity_factory_is_custom", entity_factory_is_custom},
	{"register_entity_factory", register_entity_factory},
	{"register_entity_factory_ex", register_entity_factory_ex},
	{"remove_entity_factory", remove_entity_factory},
	{"get_entity_factory_size", get_entity_factory_size},
	{"CustomDatamap.from_classname", from_classname},
	{"CustomDatamap.from_factory", from_factory},
	{"CustomDatamap.has_prop", has_prop},
	{"CustomDatamap.remove_prop", remove_prop},
	{"CustomDatamap.add_prop", add_prop},
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
	}
}

bool Sample::SDK_OnMetamodLoad(ISmmAPI *ismm, char *error, size_t maxlen, bool late)
{
	gpGlobals = ismm->GetCGlobals();
	GET_V_IFACE_ANY(GetServerFactory, servertools, IServerTools, VSERVERTOOLS_INTERFACE_VERSION)
	GET_V_IFACE_ANY(GetEngineFactory, engine, IVEngineServer, INTERFACEVERSION_VENGINESERVER)
	dictionary = (CEntityFactoryDictionary *)servertools->GetEntityFactoryDictionary();
	SH_ADD_HOOK(IVEngineServer, PvAllocEntPrivateData, engine, SH_STATIC(&HookPvAllocEntPrivateData), false);
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
}
