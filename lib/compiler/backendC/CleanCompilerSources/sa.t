/*
#define _DB_

#define _DB_TEST_
*/

/* Debug Options */

#ifdef _DB_
# ifndef _DB_TEST_
#  define _DB_TEST_
# endif
# define _DB_RED_
/* # define _DB_EQ_ */
#endif

/* Expressions */

typedef enum {
	Bottom, Ind, FunValue, Value, Lub, Top, Argument, Dep
} ExpKind;

typedef struct _exp *Exp;

typedef struct _dependency *Dependency;

typedef struct _dependency {
	Exp			dep_exp;
	Dependency	dep_next;
} DependencyRepr;

typedef Exp *ExpP;

typedef struct _exp {
	union {
		unsigned short	u_sym;			
		struct _fun *	u_fun;			/* if a value, a function id	*/
	} e_u;
	ExpKind			e_kind;				/* the kind of expression		*/
	unsigned char	e_hnf:1,			/* set if reduced to hnf		*/
	 				e_spechnf:1,		/* set if reduced in spec context */
	 				e_hasind:1,			/* used for indirections		*/
					e_red:1,			/* used for reductions			*/
		 			e_imark:1,			/* for marking use with Inds	*/
		 			e_mark:1,			/* for general use				*/
		 			e_mark2:1;			/* not for general use			*/
	 Exp			*e_args;			/* the arguments of the exp		*/
	 Exp			e_fwd;				/* for forwarding pointers		*/
	 Dependency		e_deps;				/* the current dependency list	*/
#ifdef _DB_
	 unsigned		e_mmark:1,			/* used for testing				*/
		 			e_dmark:1,			/* used for dumping				*/
		 			e_shared:1;			/* used for dumping				*/
	 unsigned		e_add;				/* the address of the exp		*/
#endif /* _DB_ */
} ExpRepr;

#define e_sym e_u.u_sym
#define e_fun e_u.u_fun
		
typedef enum {
	Function, Constructor,
	IfFunction, ApFunction, SelFunction,
	StrictFunction, FailFunction
} FunKind;

typedef enum {
	NotStrict = 0, HnfStrict = 1, SpineStrict = 2, TailStrict = 3
} StrictKind;

typedef struct _strictinfo {
	int							strict_arity;
	union {
		StrictKind				info_kinds[3];
	 	struct {
	 		StrictKind			info_kind;
			struct _strictinfo *info_args;
		} strict_tuple;
	} strict_info;
} StrictInfo;

typedef struct _context *Context;

typedef struct _context {
	unsigned		context_arity:8,
					context_kind:2,
	 				context_speculative:1;
	Context *		context_args;
} ContextRepr;

#define IsStrictContext(C)		((C)->context_kind != NotStrict)
#define IsSpeculativeContext(C)	((C)->context_speculative)

#define IsTupleInfo(A)			((A)->strict_arity != 1)
#define GetTupleStrictKind(A)	((A)->strict_info.strict_tuple.info_kind)
#define GetTupleInfos(A)		((A)->strict_info.strict_tuple.info_args)
#define GetTupleInfo(A,B)		((A)->strict_info.strict_tuple.info_args[B])
#define GetStrictKinds(A)		((A)->strict_info.info_kinds)
#define GetStrictKind(A,B)		((A)->strict_info.info_kinds[B])
#define InitStrictInfo(A,B)		((A)->strict_info.info_kinds[0]    = \
									(A)->strict_info.info_kinds[1] = \
									(A)->strict_info.info_kinds[2] = (B))
#define ContextToIndex(A)		((A) == NotStrict ? 0 : (A) - 1)

typedef struct _alts *Alts;

typedef struct _alts {
	Exp		fun_lhs;
	Exp		fun_rhs;
#ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
	Alts	fun_switch_alts;
#endif
	Alts	fun_next;
	Bool	fun_has_fail;
#ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
	Bool	fun_is_guard; /* if fun_switch_alts!=NULL */
#endif
} AltsRepr;

typedef struct _fun {
	 SymbDef		fun_symbol;
	 StrictInfo*	fun_strictargs;
	 Alts			fun_alts;
	 StrictInfo		fun_strictresult;
	 unsigned short	fun_arity;
	 unsigned short	fun_single:1;			/* TRUE if pattern matching on symbol always succeeds	*/
	 FunKind		fun_kind;
} Fun;

/* paths used in less-then operator */

typedef struct _apath *APath;

typedef struct _apath {
	Exp		ap_e1;
	Exp		ap_e2;
	APath	ap_next;
} APathRepr;

/* paths used during reduction */

typedef struct _path *Path;

typedef struct _path {
	Exp		p_exp;
	Exp		p_root;
	Path	p_next;
} PathRepr;

/* abstract matching results */

typedef enum {
	NoMatch, InfiniteMatch, PartialMatch, PartialInfiniteMatch,
	TotalMatch, LubMatch, ReduceMatch
} MatchKind;
