#if defined datamaps_included
	#endinput
#endif
#define datamaps_included

typedef entityalloc_func_t = function Address (int size_modifier, any data);
typedef ctxthink_func_t = function void (int entity, const char[] context, any data);
typedef think_func_t = function void (int entity, any data);

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
	public static native CustomEntityFactory register_function(const char[] classname, entityalloc_func_t func, int size, any data = 0);

	public static native Handle remove(const char[] classname);
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

#define HIGH_DEFAULT -121121.121121

#define SPROP_UNSIGNED (1 << 0)
#define SPROP_NOSCALE  (1 << 2)

methodmap CustomSendtable < Handle
{
	public static native CustomSendtable from_classname(const char[] classname);
	public static native CustomSendtable from_factory(CustomEntityFactory fac);

	public native void override_with(const char[] netname);
	public native void unexclude_prop(const char[] dtname, const char[] name);
	public native void set_base_class(const char[] netname);

	public native void set_name(const char[] name);
	public native void set_network_name(const char[] name);

	public native void add_prop_float(const char[] name, float low_value = 0.0, float high_value = HIGH_DEFAULT, int bits = 32, int flags = 0);
	public native void add_prop_int(const char[] name, int size, int bits = -1, int flags = 0, int offset = -1);
};

methodmap CustomDatamap < Handle
{
	public static native CustomDatamap from_classname(const char[] classname);
	public static native CustomDatamap from_factory(CustomEntityFactory fac);

	public native void set_name(const char[] name);

	public native void add_prop(const char[] name, custom_prop_type type, int num = 1);
};

native void SetEntityContextThink(int entity, ctxthink_func_t func, float time, const char[] context, any data = 0);
native void SetEntityThink(int entity, think_func_t func, any data = 0);
native void SetEntityNextThink(int entity, float time);
native void SetEntityNextThinkContext(int entity, float time, const char[] context = "");

native Address AllocPooledString(const char[] str);

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
