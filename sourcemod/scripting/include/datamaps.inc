#if defined datamaps_included
	#endinput
#endif
#define datamaps_included

#define TIME_NEVER_THINK -1.0

typedef entityalloc_func_t = function Address (int size_modifier, any data);
typedef ctxthink_func_t = function Action (int entity, const char[] context);
typedef think_func_t = function Action (int entity);

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

	public native void add_alias(const char[] classname);
};

methodmap EntityFactoryDictionary
{
	public static native IEntityFactory find(const char[] classname);

	public static native CustomEntityFactory register_based(const char[] classname, const char[] based);
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

#define SPROP_UNSIGNED  (1 << 0)
#define SPROP_COORD     (1 << 1)
#define SPROP_NOSCALE   (1 << 2)
#define SPROP_NORMAL    (1 << 5)
#define SPROP_ROUNDDOWN (1 << 3)
#define SPROP_COORD_MP  (1 << 13)
#define SPROP_VARINT    SPROP_NORMAL

#define PROP_OFFSET_EXISTING -1
#define PROP_OFFSET_NEW -2

methodmap CustomSendtable < Handle
{
	public static native CustomSendtable from_classname(const char[] classname, const char[] forced = NULL_STRING);
	public static native CustomSendtable from_factory(CustomEntityFactory fac, const char[] forced = NULL_STRING);

	public native void unexclude_prop(const char[] dtname, const char[] name);
	public native void set_base_class(const char[] netname);

	public native void set_name(const char[] name);
	public native void set_class_name(const char[] name);

	public void set_shared_name(const char[] name)
	{
		char netname[64];
		strcopy(netname, sizeof(netname), "C");
		StrCat(netname, sizeof(netname), name);
		this.set_class_name(netname);

		char tblname[64];
		strcopy(tblname, sizeof(tblname), "DT_");
		StrCat(tblname, sizeof(tblname), name);
		this.set_name(tblname);
	}

	public native void set_client_name(const char[] name);
	public native void set_client_class_id(const char[] name);
	public native void set_client_class_name(const char[] name);

	public void set_shared_client_name(const char[] name)
	{
		char netname[64];
		strcopy(netname, sizeof(netname), "C");
		StrCat(netname, sizeof(netname), name);
		this.set_client_class_name(netname);

		char tblname[64];
		strcopy(tblname, sizeof(tblname), "DT_");
		StrCat(tblname, sizeof(tblname), name);
		this.set_client_name(tblname);
	}

	public native void add_prop_float(const char[] name, float low_value = 0.0, float high_value = HIGH_DEFAULT, int bits = 32, int flags = 0, int elements = 1, int offset = PROP_OFFSET_EXISTING);

	public native void add_prop_vector(const char[] name, float low_value = 0.0, float high_value = HIGH_DEFAULT, int bits = 32, int flags = SPROP_NOSCALE, int elements = 1, int offset = PROP_OFFSET_EXISTING);
	public native void add_prop_qangles(const char[] name, int bits = 32, int flags = 0, int elements = 1, int offset = PROP_OFFSET_EXISTING);

	public native void add_prop_int(const char[] name, int size, int bits = -1, int flags = 0, int elements = 1, int offset = PROP_OFFSET_EXISTING);
	public void add_prop_bool(const char[] name, int flags = 0, int elements = 1, int offset = PROP_OFFSET_EXISTING)
	{ this.add_prop_int(name, 1, 1, SPROP_UNSIGNED|flags, elements, offset); }

	public native void add_prop_ehandle(const char[] name, int flags = 0, int elements = 1, int offset = PROP_OFFSET_EXISTING);
};

methodmap CustomDatamap < Handle
{
	public static native CustomDatamap from_classname(const char[] classname);
	public static native CustomDatamap from_factory(CustomEntityFactory fac);

	public native void set_name(const char[] name);

	public void set_shared_name(const char[] name)
	{
		char netname[64];
		strcopy(netname, sizeof(netname), "C");
		StrCat(netname, sizeof(netname), name);
		this.set_name(netname);
	}

	public native void add_prop(const char[] name, custom_prop_type type, int num = 1);
};

native void HookEntityContextThink(int entity, ctxthink_func_t func, const char[] context);
native void HookEntityThink(int entity, think_func_t func);

native void SetEntityNextThink(int entity, float time, const char[] context = NULL_STRING);

native Address AllocPooledString(const char[] str);

native void RemoveEntityImmediate(int entity);

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
