#if defined datamaps_included
	#endinput
#endif
#define datamaps_included

typeset datamaps_functions
{
	//register_entity_factory_ex
	function Address (int size_modifier);
};

native bool entity_factory_exists(const char[] classname);
native bool entity_factory_is_custom(const char[] classname);
native int get_entity_factory_size(const char[] classname);
native Handle register_entity_factory(const char[] classname, const char[] based);
native Handle register_entity_factory_ex(const char[] classname, datamaps_functions func, int size);
native Handle remove_entity_factory(const char[] classname);
native Handle unexclude_sendprop(const char[] netname, const char[] dtname, const char[] name);
native Handle sendtable_addbaseclass(const char[] netname1, const char[] dtname1, const char[] netname2, const char[] dtname2);
native Handle override_serverclass_name(const char[] classname, const char[] netname);
native Handle override_serverclass_factory(Handle factory, const char[] netname);

enum custom_prop_type
{
	custom_prop_int,
	custom_prop_float,
	custom_prop_bool,
	custom_prop_entity,
	custom_prop_vector,
};

methodmap CustomDatamap < Handle
{
	public static native CustomDatamap from_classname(const char[] classname);
	public static native CustomDatamap from_factory(Handle factory);

	public native bool has_prop(const char[] name);
	public native void remove_prop(const char[] name);
	public native void add_prop(const char[] name, custom_prop_type type);
};

stock void SetEntCustomProp(int entity, const char[] name, any value)
{
	SetEntProp(entity, Prop_Data, name, value);
}

stock void SetEntCustomPropFloat(int entity, const char[] name, float value)
{
	SetEntPropFloat(entity, Prop_Data, name, value);
}

stock void SetEntCustomPropEnt(int entity, const char[] name, int value)
{
	SetEntPropEnt(entity, Prop_Data, name, value);
}

stock void SetEntCustomPropVector(int entity, const char[] name, float value[3])
{
	SetEntPropVector(entity, Prop_Data, name, value);
}

stock any GetEntCustomProp(int entity, const char[] name)
{
	return GetEntProp(entity, Prop_Data, name);
}

stock float GetEntCustomPropFloat(int entity, const char[] name)
{
	return GetEntPropFloat(entity, Prop_Data, name);
}

stock int GetEntCustomPropEnt(int entity, const char[] name)
{
	return GetEntPropEnt(entity, Prop_Data, name);
}

stock void GetEntCustomPropVector(int entity, const char[] name, float value[3])
{
	GetEntPropVector(entity, Prop_Data, name, value);
}

#if !defined REQUIRE_EXTENSIONS
public void __ext_datamaps_SetNTVOptional()
{
	
}
#endif

public Extension __ext_datamaps = 
{
	name = "datamaps", 
	file = "datamaps.ext", 
	
#if defined AUTOLOAD_EXTENSIONS
	autoload = 1,
#else
	autoload = 0,
#endif

#if defined REQUIRE_EXTENSIONS
	required = 1,
#else
	required = 0,
#endif
};
