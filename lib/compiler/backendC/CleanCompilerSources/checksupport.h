
#define cTypeDelimiter	';'	/* also in optimisations.c */
#define cTypeFirstArg	'<'
#define cTypeLastArg	'>'

extern char *ConvertSymbolKindToString (SymbKind skind);

extern void CheckError (char *msg1,char *msg2);
extern void CheckSymbolError (struct symbol *symbol,char *msg);
extern void CheckWarning (char *msg1,char *msg2);
extern void CheckSymbolWarning (struct symbol *symbol,char *msg);
extern void CheckWarningOrError (Bool error,char *msg1,char *msg2);
extern void CheckWarningOrError2 (Bool error,char *msg1,char *msg2,char *msg3);
extern void CheckSymbolWarningOrError (Bool error,struct symbol *symbol,char *msg);

#define NameOfSymbol(symb)	((symb)->symb_def ->sdef_ident->ident_name)

extern void PrintSymbolOfIdent (Ident sid,unsigned line_nr,File file);

