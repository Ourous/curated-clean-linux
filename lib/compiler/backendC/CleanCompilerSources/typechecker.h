/*

Version 1.0 25/04/1994

Author: Sjaak Smetsers 

*/
extern Bool TypeError;

extern Bool TypeChecker (ImpMod imod);

extern void ListTypes (ImpMod imod);


extern SymbDef ListDef, TupleDefs [], ArrayDefs [];

extern Symbol EmptySymbol;

extern PolyList UserDefinedArrayFunctions;

extern void InitTypeChecker (void);

extern void PrintNodeSymbol (Node node, int arg_nr, File file);

extern void PrintTCType (struct type_cell * type, struct type_cell * sub_type);

extern unsigned ArityOfTypeSymbol (Symbol type_symb);

extern unsigned long ConvertTypeToTypeVector (TypeNode type);

extern FlatType RetrieveLhsOfTypeDefinition (SymbDef tdef);

extern Ident IdentOfOverloadedInstance (Symbol inst_symb);