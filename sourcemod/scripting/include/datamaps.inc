#if defined datamaps_included
	#endinput
#endif
#define datamaps_included

typeset datamaps_functions
{
	//EntityFactoryDictionary.register_function
	function Address (int size_modifier);
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

#define HIGH_DEFAULT -121121.121121

#define SPROP_NOSCALE (1 << 2)

methodmap CustomSendtable < Handle
{
	public static native CustomSendtable from_classname(const char[] classname, const char[] netname);
	public static native CustomSendtable from_factory(CustomEntityFactory fac, const char[] netname);

	public native void override_with(const char[] netname);
	public native void override_with_ex(const char[] netname, const char[] dtname);
	public native void unexclude_prop(const char[] dtname, const char[] name);
	public native void set_base_class(const char[] netname);

	public native void SendPropFloat(const char[] name, int nBits = 32, int flags = 0, float fLowValue = 0.0, float fHighValue = HIGH_DEFAULT);
};

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
	public static native CustomDatamap from_factory(CustomEntityFactory fac);

	public native void add_prop(const char[] name, custom_prop_type type);
};

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
