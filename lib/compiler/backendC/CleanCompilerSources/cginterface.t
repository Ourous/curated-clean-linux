
typedef enum { MAC_II, SUN_3, MAC_I, MAC_IISANE, MAC_CURRENT = 100 } target_machine_type;

/* code generator flags */
#define ASSEMBLY		1
#define KEEP_ABC		2
#define STACK_CHECKS	4
#define DO_PARALLEL		8
#define	CHECK_INDICES  16

/* application and linker flags */
#define SHOW_BASIC_ONLY				1
#define SHOW_GARBAGE_COLLECTIONS	2
#define SHOW_STACK_SIZE				4
#define SHOW_EXECUTION_TIME			8

