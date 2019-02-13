#define FASTER_ARRAY_COMPREHENSIONS

typedef	enum {
	kGeneratorList,kGeneratorFrom,kGeneratorFromTo,kGeneratorFromThen,kGeneratorFromThenTo,kGeneratorArrayOnly
} GeneratorKind;

extern	int	gGeneratorNumber;
extern	int	gListFunctionNumber;

STRUCT (generator, Generator)
{
	GeneratorKind	gen_kind;
	Bool			gen_arrayCombined;
	int			gen_array_index_n;
	NodeP		gen_array;
	NodeP		*gen_array_size_node_h;
	NodeP		gen_pattern;
	union
	{
		NodeP	val_list;
		struct
		{
			ArgP	fbt_from;
			ArgP	fbt_by;
			ArgP	fbt_to;
		} val_fbt;
	} val;
};

# define	gen_list		val.val_list
# define	gen_from		val.val_fbt.fbt_from
# define	gen_by		val.val_fbt.fbt_by
# define	gen_to		val.val_fbt.fbt_to

# define	kMaxParallelGenerators	16

extern unsigned ConvertGenerators (unsigned n, GeneratorS generators[],IdentP updatedArrayIdent,NodeP *arrayNodePtr,
							int *const array_index_generator_n_p,NodeP result_node_p,NodeP guard_p,ScopeP scope);
extern void GenerateComprehensionFunction (ImpRules impRule, unsigned n, GeneratorS djennereeturs [], NodeP guard,
								NodeP result_node_p,int array_index_generator_n,
								NodeP *begin, NodeP end, NodeP **successP, unsigned line, NodeP *array);
extern NodeP ComputeNewArrayLength (unsigned n, GeneratorS generators [], ScopeP scope);
