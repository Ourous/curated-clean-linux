
#undef _TYPESBUG_

/*
	Type definitions
*/

typedef
	enum
	{	US_OK, US_OrdinaryFailure, US_SpecificationError, US_UniquenessError, US_UniquenessSpecificationError,
		US_StrictnessError, US_LiftedTypeVarError, US_LiftedAttrVarError, US_OverloadingError,
		US_ExistentialError, US_ConstructorVarError
	} UnificationStatus;

/*
	Global variables
*/

extern ImpRules *LastNewImpRule;

extern SymbDef *LastNewDependency;

extern Symbol *LastNewSymbol;

typedef struct overloaded_variable_list_elem
{
	TypeCell							ovle_temp_type;
	TypeCell							ovle_copy_type;
	struct overloaded_variable_list_elem *	ovle_next;

} *OverloadedVariableList;

extern OverloadedVariableList OverloadedTypeVars;

extern Bool DoDeriveUniAttributes;

extern int ErroneousArgumentNumber;

extern HeapDescr TCWorkSpace, TCTempSpace;

extern void *AllocInTCWorkSpace (SizeT size);
/*
	Global functions
*/

#ifdef _MEMORY_CHECK_
#define SkipIndirections(type) \
	if (MemoryCheck (type), (type) -> tc_kind == Indirection)\
		(type) = (TypeCell) SkipIndirectionChain ((type) -> contents_indirect)
#else
#define SkipIndirections(type) \
	if ((type) -> tc_kind == Indirection)\
		(type) = (TypeCell) SkipIndirectionChain ((type) -> contents_indirect)
#endif

extern TypeCell SkipIndirectionChain (TypeCell type);

#define SkipSC_Indirections(type) \
	if ((type) -> tc_strict == SC_Indirection)\
		(type) = (TypeCell) Skip_SCI_Chain ((type) -> contents_si_indirect)

extern TypeCell Skip_SCI_Chain (TypeCell type);

#define SkipAttrVarIndirections(avar) \
	if ((avar) -> tav_varkind == AC_Indirection)\
		(avar) = SkipAttrVarIndirectionChain ((avar) -> tav_indirection)

extern TempAttrVar SkipAttrVarIndirectionChain (TempAttrVar avar);

#ifdef _MEMORY_CHECK_
extern void MemoryCheck (void * ptr);
#else
#define MemoryCheck(ptr)
#endif

extern PlainAttrVar NewPlainAttrVar (void);

extern TempAttrVar NewTempAttrVar (void);

extern AttrVarWithEquations NewAttrVarWithEquations (unsigned attrnr);

extern TypeCell NewTypeVariableCell (HeapDescr hd, AttributeCellKind attrkind);

extern TypeCell NewConstructorTypeCell (SymbDef type_cons, int act_arity, AttributeCellKind attrkind);

#define cAddExtraArgument	True
#define cDontAddExtraArgument	False

extern TypeCell NewTemporaryConstructorTypeCell (TypeConsRepr type_cons, int act_arity, AttributeCellKind attrkind, Bool extra_argument);

extern void CreateAttributeEquationsForConstructorVariables (SymbDef cons_def, TypeCell arg_cells []);

extern AttributeCellKind DetermineAttrkindOfTemporaryTypeCell (TypeCell type);

extern TypeCell NewAttributedTypeVariable (HeapDescr hd, AttrVarKind av_kind);

extern TypeCell BasicCells [], UniqueBasicCells [], StrictBasicCells [Nr_Of_Basic_Types];

extern TypeCell NewTypeCell (CellKind kind, HeapDescr hd);

extern TypeCell NewBasicTypeCell (Symbol symbol, HeapDescr hd);

extern TypeCell NewAttributedBasicTypeCell (Symbol symbol, HeapDescr hd, AttributeCellKind attrkind);

extern TypeCell NewFunctionTypeCell (HeapDescr hd, AttributeCellKind attrkind);

extern TypeCell NewVoidTypeCell (AttributeCellKind attrkind, HeapDescr hd);

extern TypeConsRepr NewTypeConstructorRepr (SymbDef symbol, int arity, HeapDescr hd);

extern TypeCell NewConstructorVariableCell (int arity, HeapDescr hd);

extern TypeCell NewEmptyTypeCell (Bool is_strict, HeapDescr hd);

extern SymbDef GetTupleDef (int arity);

extern SymbolType NewSymbolType (HeapDescr hd, int arity);

extern void DumpSymbolType (Symbol symbol, SymbolType stype, int arity);

typedef enum {	LhsConstructor, RhsConstructor, AnySymbol } SymbolApplKind;

extern TypeCell CreateInstance (TypeCell type, Bool marking, SymbolApplKind symb_appl, int inst_depth, unsigned group_nr);

extern TypeCell CreateInstanceOfTypeCell (TypeCell elemtype);

extern SymbolType CreateInstanceOfSymbolType (SymbolType stype, int demarity, int offarity, SymbolApplKind symb_appl);

extern Bool ExpandSynonymTypesIfNecessary (TypeCell *t1, TypeCell *t2);

extern UnificationStatus CompareSymbolTypes (SymbolType orig_type, SymbolType temp_type);

extern UnificationStatus SubstituteVariable (TypeCell alpha, TypeCell beta);

extern Bool CopySymbolType (SymbDef fun_symb, SymbolType dst_type);

extern TypeCell RetrieveInstanceOfTypeVar (TypeCell type_var);

extern void AdjustOverloadedNode (Node old_node, Node new_node);

extern void AdjustRecursionNode (Node old_node, Node new_node);

extern void CheckMarkingsOfSymbolType (SymbolType stype);

extern AttrEquationList NewAttributeEquation  (TempAttrVar demvar, TempAttrVar offvar,
	AttrEquationList nextdem, AttrEquationList nextoff, Bool implicit);

#define cEquationIsImplicit		True
#define cEquationIsNotImplicit	False

extern void CreateAttributeEquation (TempAttrVar demvar, TempAttrVar offvar, Bool implicit);

extern Bool AdjustDemandedAttributeList (TempAttrVar demvar);

extern PolyList *CollectPropagatingTypeArgs (ConsVarList cons_var, TypeCell type_args [], int arity,
	PolyList extra_args [], int nr_of_extra_args);

extern Bool HasObservingResultType (TypeCell type);

extern Bool AdjustAttributeOfTypeCell (TypeCell type, AttributeCellKind attr_kind);

extern Bool MakeAttributeNotUnique (TypeCell type);

extern SymbolType InstantiateRecordSelectorType (Symbol sel_symb, int sel_kind);

typedef enum
	{	UEK_OK, UEK_MultiToUni, UEK_NonCoercible, UEK_UniqueRequired, UEK_EssentiallyUnique, UEK_ExistentionalAttr
	} UniquenessErrorKind;

extern UniquenessErrorKind DetermineAttributeDependencies (TypeCell demtype, TypeCell offtype, Bool write_access, Bool non_coercible);

extern UniquenessErrorKind ExpandSubstitutedTypes (TypeCell type, TypeCell * result_cell_p);

extern UniquenessErrorKind EquateAttributesOfType (TypeCell type1, AttributeCellKind attr1, TypeCell type2, AttributeCellKind attr2);

extern TypeCell gErroneousTypeCell;
extern Bool gDemandedIsErroneous;
