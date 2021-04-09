#if defined datamaps_included
	#endinput
#endif
#define datamaps_included

typeset datamaps_functions
{
	//EntityFactoryDictionary.register_function
	function Address (int size_modifier);

	//SetEntityContextThink
	function void (int entity, const char[] context, any data);

	//SetEntityThink
	function void (int entity, any data);
};

methodmap IEntityFactory
{
	property bool Custom
	{
		public native get();
	}

	property int Size
	{
		public native get();
	}
};

#define IEntityFactory_Null (view_as<IEntityFactory>(Address_Null))

methodmap CustomEntityFactory < Handle
{
	property IEntityFactory Interface
	{
		public native get();
	}
};

methodmap EntityFactoryDictionary
{
	public static native IEntityFactory find(const char[] classname);

	public static native CustomEntityFactory register_based(const char[] classname, IEntityFactory based);
	public static CustomEntityFactory register_based_name(const char[] classname, const char[] based)
	{
		IEntityFactory factory = EntityFactoryDictionary.find(based);
		if(factory != IEntityFactory_Null) {
			return EntityFactoryDictionary.register_based(classname, factory);
		} else {
			ThrowError("invalid classname %s", based);
			return null;
		}
	}
	public static native CustomEntityFactory register_function(const char[] classname, datamaps_functions func, int size);

	public static native Handle remove(const char[] classname);
};

methodmap CustomSendtable < Handle
{
	public static native CustomSendtable from_classname(const char[] classname, const char[] netname = "");
	public static native CustomSendtable from_factory(CustomEntityFactory fac, const char[] netname = "");

	public native void override_with(const char[] netname);
	public native void unexclude_prop(const char[] dtname, const char[] name);
	public native void set_base_class(const char[] netname);

	public native void set_name(const char[] name);
	public native void set_network_name(const char[] name);
};

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

methodmap CustomDatamap < Handle
{
	public static native CustomDatamap from_classname(const char[] classname);
	public static native CustomDatamap from_factory(CustomEntityFactory fac);

	public native void set_name(const char[] name);

	public native void add_prop(const char[] name, custom_prop_type type, int num = 1);
};

native void SetEntityContextThink(int entity, datamaps_functions func, float time, const char[] context, any data = 0);
native void SetEntityThink(int entity, datamaps_functions func, float time, any data = 0);
native void SetEntityNextThink(int entity, float time);
native void SetEntityNextThinkContext(int entity, float time, const char[] context = "");

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
