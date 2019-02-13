/*
	(Concurrent) Clean Compiler: ABC instructions
	Authors:  Sjaak Smetsers & John van Groningen
*/

#include "compiledefines.h"
#include "comsupport.h"

#include <ctype.h>

#include "syntaxtr.t"
#include "checksupport.h"

#include "settings.h"
#include "sizes.h"
#include "codegen_types.h"
#include "codegen1.h"
#include "codegen2.h"
#include "instructions.h"
#include "statesgen.h"
#include "version.h"

#define for_l(v,l,n) for(v=(l);v!=NULL;v=v->n)

#define BINARY_ABC 0
#undef MEMORY_PROFILING_WITH_N_STRING

#define PutSOutFile(s) FPutS ((s),OutFile)
#define PutCOutFile(s) FPutC ((s),OutFile)

static void error_in_function (char *m)
{
	ErrorInCompiler ("instructions.c",m,"");
}

/* also defined in project.c, only needed for stand alone compiler */

#define N_DoDebug				0
#define N_DoReuseUniqueNodes	1
#define N_DoParallel			2

#define N_NoDescriptors 3
/*
#define N_NoMemoryProfiling		3
*/
#define N_DoStrictnessAnalysis	4
#define N_NoTimeProfiling		5

#define N_ExportLocalLabels 6
/*
#define N_DoVerbose				6
*/
#define N_DoWarning				7
#define N_System				8
#define N_DoFusion				9
#define N_Do64BitArch			10
#define N_Dynamics				11
#define N_DoGenericFusion		12

#define MINIMUM_N_OPTIONS 9
#define N_OPTIONS 13

static void ConvertOptionsToString (char *optstring)
{
	optstring[N_DoDebug]              = DoDebug ? '1' : '0';
	optstring[N_DoReuseUniqueNodes]   = !DoReuseUniqueNodes ? '1' : '0';
	optstring[N_DoParallel]           = DoParallel ? '1' : '0';

	optstring[N_NoDescriptors] = !DoDescriptors ? '1' : '0';
/*
	optstring[N_NoMemoryProfiling]    = !DoProfiling ? '1' : '0';
*/
	optstring[N_DoStrictnessAnalysis] = DoStrictnessAnalysis ? '1' : '0';

	optstring[N_NoTimeProfiling]      = !DoTimeProfiling ? '1' : '0';
	optstring[N_ExportLocalLabels] = ExportLocalLabels ? '1' : '0';
/*
	optstring[N_DoVerbose]            = DoVerbose ? '1' : '0';
*/
	optstring[N_DoWarning]            = DoWarning ? '1' : '0';
	optstring[N_System]               = '0';

	if (DoFusion || ObjectSizes[RealObj]!=2 || Dynamics || DoGenericFusion){
		optstring[N_DoFusion] = DoFusion ? '1' : '0';
		optstring[N_Do64BitArch] = ObjectSizes[RealObj]!=2 ? '1' : '0';
		optstring[N_Dynamics] = Dynamics ? '1' : '0';
		optstring[N_DoGenericFusion] = DoGenericFusion ? '1' : '0';
		optstring[N_OPTIONS]='\0';
	} else
		optstring[MINIMUM_N_OPTIONS]='\0';
}

#define D_PREFIX "d"
#define N_PREFIX "n"
#define L_PREFIX "l"

#define EA_PREFIX "ea"
#define EU_PREFIX "eu"
#define S_PREFIX "s"

#define R_PREFIX "r"
#define RECORD_N_PREFIX "c"
#define RECORD_D_PREFIX "t"
#define CONSTRUCTOR_R_PREFIX "k"

#define LOCAL_D_PREFIX "d"

File OutFile;
char *ABCFileName;

Bool OpenABCFile (char *fname)
{
	OutFile = FOpen (fname, abcFile, "w");

	if (OutFile!=NULL){
#if defined (THINK_C) || defined (POWER)
		setvbuf ((FILE*) OutFile, NULL, _IOFBF, 8192);
#endif
		OpenedFile = OutFile;
		ABCFileName = fname;
		return True;
	} else
		return False;
}

void WriteLastNewlineToABCFile (void)
{
	FPutC ('\n',OutFile);
}

void CloseABCFile (char *fname)
{
	if (OutFile){
#ifdef THINK_C
		int file_io_error;
		
		file_io_error=ferror (OutFile);
#endif
		if (FClose (OutFile) != 0
#ifdef THINK_C
		||	file_io_error
#endif
		){
			CompilerError = True;
			CurrentLine = 0;
			
			StaticMessage (True, "<open file>", "Write error (disk full?)");
		}
		if (CompilerError)
			FDelete (fname, abcFile); 
		OpenedFile = (File) NIL;
	}
}

static Bool DescriptorNeeded (SymbDef sdef)
{
	return (sdef->sdef_exported || 
			(sdef->sdef_kind!=IMPRULE && sdef->sdef_kind!=SYSRULE) || 
			sdef->sdef_mark & SDEF_USED_CURRIED_MASK) ||
			((DoParallel || DoDescriptors) && (sdef->sdef_mark & (SDEF_USED_CURRIED_MASK | SDEF_USED_LAZILY_MASK)));
}

static void GenLabel (Label label)
{
	if (label->lab_issymbol){
		SymbDef def;
		char *module_name;

		def=label->lab_symbol;
		module_name = label->lab_mod;
		
		if (module_name!=NULL)
			FPrintF (OutFile,"e_%s_%s%s",module_name,label->lab_pref,def->sdef_ident->ident_name);
		else if (DoDebug){
			if (def->sdef_kind==IMPRULE)
				FPrintF (OutFile, "%s%s.%u",label->lab_pref,def->sdef_ident->ident_name,def->sdef_number);
			else
				FPrintF (OutFile, "%s%s",label->lab_pref,def->sdef_ident->ident_name);
		} else if (def->sdef_number==0)
			FPrintF (OutFile, "%s%s",label->lab_pref,def->sdef_ident->ident_name);
		else if (label->lab_pref[0] == '\0')
			FPrintF (OutFile,LOCAL_D_PREFIX "%u",def->sdef_number);
		else
			FPrintF (OutFile,"%s%u",label->lab_pref,def->sdef_number);
	} else {
		FPutS (label->lab_pref,OutFile);
		FPutS (label->lab_name,OutFile);
	}
	if (label->lab_post!=0)
		FPrintF (OutFile,".%u",label->lab_post);
}

static void GenDescriptorOrNodeEntryLabel (Label label)
{
	if (label->lab_issymbol){
		SymbDef def;
		char *module_name;

		def=label->lab_symbol;
		module_name = label->lab_mod;
		
		if (module_name!=NULL)
			FPrintF (OutFile,"e_%s_%s%s",module_name,label->lab_pref,def->sdef_ident->ident_name);
		else if (ExportLocalLabels){
			if (def->sdef_kind==IMPRULE)
				FPrintF (OutFile,"e_%s_%s%s.%u",CurrentModule,label->lab_pref,def->sdef_ident->ident_name,def->sdef_number);
			else
				FPrintF (OutFile,"e_%s_%s%s",CurrentModule,label->lab_pref,def->sdef_ident->ident_name);
		} else if (DoDebug){
			if (def->sdef_kind==IMPRULE)
				FPrintF (OutFile, "%s%s.%u",label->lab_pref,def->sdef_ident->ident_name,def->sdef_number);
			else
				FPrintF (OutFile, "%s%s",label->lab_pref,def->sdef_ident->ident_name);
		} else if (def->sdef_number==0)
			FPrintF (OutFile, "%s%s",label->lab_pref,def->sdef_ident->ident_name);
		else if (label->lab_pref[0] == '\0')
			FPrintF (OutFile,LOCAL_D_PREFIX "%u",def->sdef_number);
		else
			FPrintF (OutFile,"%s%u",label->lab_pref,def->sdef_number);
	} else {
		FPutS (label->lab_pref,OutFile);
		FPutS (label->lab_name,OutFile);
	}
	if (label->lab_post!=0)
		FPrintF (OutFile,".%u",label->lab_post);
}

static void GenGetWL (int offset)
{
	FPrintF (OutFile, "\n\tgetWL %d", offset);
}

static void GenPutWL (int offset)
{
	FPrintF (OutFile, "\n\tputWL %d", offset);
}

static void GenRelease (void)
{
	FPutS ("\n\trelease", OutFile);
}

static void TreatWaitListBeforeFill (int offset, FillKind fkind)
{
	if (DoParallel && fkind != NormalFill)
		GenGetWL (offset);	
}

static void TreatWaitListAfterFill (int offset, FillKind fkind)
{
	if (DoParallel){
		switch (fkind){
			case ReleaseAndFill:GenRelease ();		break;
			case PartialFill:	GenPutWL (offset);	break;
			default:							break;
		}
	}
}

#if !BINARY_ABC

#define put_instructionb(a) put_instruction(I##a)
#define put_instruction_b(a) put_instruction_(I##a)
#define put_directive_b(a) put_directive_(D##a)
#define put_arguments_i_b(i1) FPrintF (OutFile,"%s",(i1))
#define put_arguments_in_b(i1,n1) FPrintF (OutFile,"%s %d",(i1),(n1))
#define put_arguments_n_b(n1) FPrintF (OutFile,"%d",(n1))
#define put_arguments_nn_b(n1,n2) FPrintF (OutFile,"%d %d",(n1),(n2))
#define put_arguments_nnn_b(n1,n2,n3) FPrintF (OutFile,"%d %d %d",(n1),(n2),(n3))
#define put_arguments_nnnn_b(n1,n2,n3,n4) FPrintF (OutFile,"%d %d %d %d",(n1),(n2),(n3),(n4))
#define put_arguments_nnnnn_b(n1,n2,n3,n4,n5) FPrintF (OutFile,"%d %d %d %d %d",(n1),(n2),(n3),(n4),(n5))
#define put_arguments_nnnnnnn_b(n1,n2,n3,n4,n5,n6,n7) FPrintF (OutFile,"%d %d %d %d %d %d %d",(n1),(n2),(n3),(n4),(n5),(n6),(n7))
#define put_arguments_n__b(n1) FPrintF (OutFile,"%d ",(n1))
#define put_arguments_nn__b(n1,n2) FPrintF (OutFile,"%d %d ",(n1),(n2))
#define put_arguments__n_b(n1) FPrintF (OutFile," %d",(n1))
#define put_arguments__nn_b(n1,n2) FPrintF (OutFile," %d %d",(n1),(n2))
#define put_arguments__n__b(n1) FPrintF (OutFile," %d ",(n1))

#else

/*
#define put_instructionb(a) put_instruction_code(C##a)
#define put_instruction_b(a) put_instruction_code(C##a)
#define put_directive_b(a) put_instruction_code(C##a)
*/

#define put_instructionb(a) if (DoDebug) put_instruction(I##a); else put_instruction_code(C##a)
#define put_instruction_b(a) if (DoDebug) put_instruction_(I##a); else put_instruction_code(C##a)
#define put_directive_b(a) if (DoDebug) put_directive_(D##a); else put_instruction_code(C##a)

static void put_n (long n)
{
	while (!(n>=-64 && n<=63)){
		FPutC (128+(n & 127),OutFile);
		n=n>>7;
	}

	FPutC (n+64,OutFile);
}

static long integer_string_to_integer (char *s_p)
{
	long integer;
	int minus_sign,last_char;
	
	minus_sign=0;
	last_char=*s_p++;
	if (last_char=='+' || last_char=='-'){
		if (last_char=='-')
			minus_sign=!minus_sign;
		last_char=*s_p++;;
	}
			
	integer=last_char-'0';
	last_char=*s_p++;;
	
	while ((unsigned)(last_char-'0')<10u){
		integer*=10;
		integer+=last_char-'0';
		last_char=*s_p++;;
	}
		
	if (minus_sign)
		integer=-integer;
	
	return integer;
}

static void put_arguments_i_b (char *i1)
{
	if (DoDebug)
		FPrintF (OutFile,"%s",(i1));
	else
		put_n (integer_string_to_integer (i1));
}

static void put_arguments_in_b (char *i1,long n1)
{
	if (DoDebug)
		FPrintF (OutFile,"%s %d",(i1),(n1));
	else {
		put_n (integer_string_to_integer (i1));
		put_n (n1);
	}
}

static void put_arguments_n_b (long n1)
{
	if (DoDebug)
		FPrintF (OutFile,"%d",(n1));
	else
		put_n (n1);
}

static void put_arguments_nn_b (long n1,long n2)
{
	if (DoDebug)
		FPrintF (OutFile,"%d %d",(n1),(n2));
	else {
		put_n (n1);
		put_n (n2);
	}
}

static void put_arguments_nnn_b (long n1,long n2,long n3)
{
	if (DoDebug)
		FPrintF (OutFile,"%d %d %d",(n1),(n2),(n3));
	else {
		put_n (n1);
		put_n (n2);
		put_n (n3);
	}
}

static void put_arguments_nnnn_b (long n1,long n2,long n3,long n4)
{
	if (DoDebug)	
		FPrintF (OutFile,"%d %d %d %d",(n1),(n2),(n3),(n4));
	else {
		put_n (n1);
		put_n (n2);
		put_n (n3);
		put_n (n4);
	}
}

static void put_arguments_nnnnn_b (long n1,long n2,long n3,long n4,long n5)
{
	if (DoDebug)
		FPrintF (OutFile,"%d %d %d %d %d",(n1),(n2),(n3),(n4),(n5));
	else {
		put_n (n1);
		put_n (n2);
		put_n (n3);
		put_n (n4);
		put_n (n5);
	}
}

static void put_arguments_n__b (long n1)
{
	if (DoDebug)
		FPrintF (OutFile,"%d ",(n1));
	else
		put_n (n1);
}

static void put_arguments__n_b (long n1)
{
	if (DoDebug)
		FPrintF (OutFile," %d",(n1));
	else {
		FPutC (' ',OutFile);
		put_n (n1);
	}
}

static void put_arguments__n__b (long n1)
{
	if (DoDebug)
		FPrintF (OutFile," %d ",(n1));
	else {
		FPutC (' ',OutFile);
		put_n (n1);
	}
}

static void put_arguments_nn__b (long n1,long n2)
{
	if (DoDebug)
		FPrintF (OutFile,"%d %d ",(n1),(n2));
	else {
		put_n (n1);
		put_n (n2);
	}
}

static void put_arguments__nn_b (long n1,long n2)
{
	if (DoDebug)
		FPrintF (OutFile," %d %d",(n1),(n2));
	else {
		FPutC (' ',OutFile);
		put_n (n1);
		put_n (n2);
	}
}

enum {
	Cbuild=136,
	Cbuildh,
	CbuildI,
	CbuildB_b,
	CbuildC_b,
	CbuildI_b,
	CbuildR_b,
	CbuildF_b,
	Ceq_desc,
	CeqD_b,
	CeqI_a,
	CeqI_b,
	Cfill,
	Cfillh,
	CfillI,
	CfillB_b,
	CfillC_b,
	CfillF_b,
	CfillI_b,
	CfillR_b,
	Cfill_a,
	Cjmp,
	Cjmp_false,
	Cjmp_true,
	Cjsr,
	Cjsr_eval,
	Cpop_a,
	Cpop_b,
	CpushB_a,
	CpushC_a,
	CpushI_a,
	CpushF_a,
	CpushR_a,
	CpushD,
	CpushI,
	Cpush_a,
	Cpush_b,
	Cpush_arg,
	Cpush_args,
	Cpush_args_u,
	Cpush_node,
	Cpush_node_u,
	Cpush_r_args,
	Cpush_r_args_a,
	Cpush_r_args_b,
	Cpush_r_args_u,
	Crepl_arg,
	Crepl_args,
	Crepl_r_args,
	Crepl_r_args_a,
	Crtn,
	Cupdate_a,
	Cupdate_b,
	Cupdatepop_a,
	Cupdatepop_b,
	
	Cd,
	Co,
	Cimpdesc,
	Cimplab,
	Cimpmod,
	Cn
};
#endif

#define IbuildB "buildB"
#define IbuildC "buildC"
#define IbuildI "buildI"
#define IbuildR "buildR"
#define IbuildS "buildS"

#define IbuildB_b "buildB_b"
#define IbuildC_b "buildC_b"
#define IbuildF_b "buildF_b"
#define IbuildI_b "buildI_b"
#define IbuildR_b "buildR_b"

#define IfillB "fillB"
#define IfillC "fillC"
#define IfillI "fillI"
#define IfillR "fillR"
#define IfillS "fillS"

#define IfillB_b "fillB_b"
#define IfillC_b "fillC_b"
#define IfillI_b "fillI_b"
#define IfillR_b "fillR_b"
#define IfillF_b "fillF_b"

#define IeqB_a "eqB_a"
#define IeqC_a "eqC_a"
#define IeqI_a "eqI_a"
#define IeqR_a "eqR_a"
#define IeqS_a "eqS_a"

#define IeqAC_a "eqAC_a"

#define IeqB_b "eqB_b"
#define IeqC_b "eqC_b"
#define IeqI_b "eqI_b"
#define IeqR_b "eqR_b"

#define InotB "notB"

#define IpushB "pushB"
#define IpushI "pushI"
#define IpushC "pushC"
#define IpushR "pushR"
#define IpushZR "pushZR"
#define IpushZ "pushZ"

#define IpushD "pushD"

#define IpushB_a "pushB_a"
#define IpushC_a "pushC_a"
#define IpushI_a "pushI_a"
#define IpushR_a "pushR_a"
#define IpushF_a "pushF_a"

#define IpushD_a "pushD_a"

#define Ipush_array "push_array"
#define Ipush_arraysize "push_arraysize"
#define Iselect "select"
#define Iupdate "update"
#define Ireplace "replace"

#define Ipush_arg "push_arg"
#define Ipush_args "push_args"
#define Ipush_args_u "push_args_u"
#define Ipush_r_args "push_r_args"
#define Ipush_r_args_u "push_r_args_u"
#define Ipush_r_args_a "push_r_args_a"
#define Ipush_r_args_b "push_r_args_b"
#define Ipush_r_arg_u "push_r_arg_u"
#define Irepl_arg "repl_arg"
#define Irepl_args "repl_args"
#define Irepl_r_args "repl_r_args"
#define Irepl_r_args_a "repl_r_args_a"

#define Ipush_node "push_node"
#define Ipush_node_u "push_node_u"

#define Ifill "fill"
#define Ifillcp "fillcp"
#define Ifill_u "fill_u"
#define Ifillcp_u "fillcp_u"
#define Ifillh "fillh"
#define Ifill1 "fill1"
#define Ifill2 "fill2"
#define Ifill3 "fill3"

#define Ibuild "build"
#define Ibuildh "buildh"
#define Ibuild_u "build_u"
#define IbuildAC "buildAC"

#define Ifill_r "fill_r"
#define Ifill1_r "fill1_r"
#define Ifill2_r "fill2_r"
#define Ifill3_r "fill3_r"

#define Ibuildhr "buildhr"
#define Ibuild_r "build_r"

#define Ifill_a "fill_a"

#define Ipush_a "push_a"
#define Ipush_b "push_b"

#define Ijsr_eval "jsr_eval"
#define Ijsr_ap "jsr_ap"
#define Ijsr_i "jsr_i"

#define Ipop_a "pop_a"
#define Ipop_b "pop_b"
#define Ieq_desc "eq_desc"
#define IeqD_b "eqD_b"

#define Ijmp_false "jmp_false"
#define Ijmp_true "jmp_true"
#define Ijmp "jmp"
#define Ijsr "jsr"

#define Icreate "create"
#define Iprint "print"

#define Iupdate_a "update_a"
#define Iupdate_b "update_b"
#define Iupdatepop_a "updatepop_a"
#define Iupdatepop_b "updatepop_b"
#define Iupdate_b "update_b"
#define Ipop_a "pop_a"
#define Ipop_b "pop_b"

#define Iget_node_arity "get_node_arity"
#define Iget_desc_arity "get_desc_arity"

#define Ipush_arg_b "push_arg_b"

#define Irtn "rtn"

#define Ijmp_eval "jmp_eval"
#define Ijmp_eval_upd "jmp_eval_upd"
#define Ijmp_ap "jmp_ap"
#define Ijmp_ap_upd "jmp_ap_upd"
#define Ijmp_i "jmp_i"
#define Ijmp_not_eqZ "jmp_not_eqZ"
#define Ijmp_upd "jmp_upd"

#define Ihalt "halt"

#define Itestcaf "testcaf"
#define Ipushcaf "pushcaf"
#define Ifillcaf "fillcaf"

#define Iin "in"
#define Iout "out"

static void put_instruction (char *instruction)
{
	FPutC ('\n',OutFile);
	FPutC ('\t',OutFile);
	FPutS (instruction,OutFile);
}

static void put_instruction_ (char *instruction)
{
	FPutC ('\n',OutFile);
	FPutC ('\t',OutFile);
	FPutS (instruction,OutFile);
	FPutC (' ',OutFile);
}

static void put_instruction_code (int instruction_code)
{
	FPutC (instruction_code,OutFile);
}

#define Da "a"
#define Dai "ai"
#define Dkeep "keep"
#define Dd "d"
#define Do "o"
#define Dimpdesc "impdesc"
#define Dimplab "implab"
#define Dimpmod "impmod"
#define Dexport "export"
#define Dn "n"
#define Dnu "nu"
#define Dn_string "n_string"
#define Ddesc "desc"
#define Ddesc0 "desc0"
#define Ddescn "descn"
#define Ddescs "descs"
#define Ddescexp "descexp"
#define Drecord "record"
#define Dmodule "module"
#define Ddepend "depend"
#define Dcomp "comp"
#define Dstart "start"
#define Dstring "string"
#define Dcaf "caf"
#define Dendinfo "endinfo"

#define Dpb "pb"
#define Dpd "pd"
#define Dpn "pn"
#define Dpl "pl"
#define Dpld "pld"
#define Dpt "pt"
#define Dpe "pe"

static void put_directive (char *directive)
{
	FPutC ('\n',OutFile);
	FPutC ('.',OutFile);
	FPutS (directive,OutFile);
}

static void put_directive_ (char *directive)
{
	FPutC ('\n',OutFile);
	FPutC ('.',OutFile);
	FPutS (directive,OutFile);
	FPutC (' ',OutFile);
}

static void put_first_directive_ (char *directive)
{
	FPutC ('.',OutFile);
	FPutS (directive,OutFile);
	FPutC (' ',OutFile);
}

void BuildBasicFromB (ObjectKind kind,int b_offset)
{
	switch (kind){
		case IntObj:
		case ProcIdObj:
		case RedIdObj:
			put_instruction_b (buildI_b); break;
		case BoolObj:
			put_instruction_b (buildB_b); break;
		case CharObj:
			put_instruction_b (buildC_b); break;
		case RealObj:
			put_instruction_b (buildR_b); break;
		case FileObj:
			put_instruction_b (buildF_b); break;
		default:
			error_in_function ("BuildBasicFromB");
			return;
	}

	put_arguments_n_b (b_offset);
}

void FillBasicFromB (ObjectKind kind, int boffs, int aoffs, FillKind fkind)
{
	TreatWaitListBeforeFill (aoffs, fkind);
	switch (kind){
		case IntObj:
		case ProcIdObj:			/* we assume proc_id and red_id	*/
		case RedIdObj:			/* to be integers				*/
			put_instruction_b (fillI_b); break;
		case BoolObj:
			put_instruction_b (fillB_b); break;
		case CharObj:
			put_instruction_b (fillC_b); break;
		case RealObj:
			put_instruction_b (fillR_b); break;
		case FileObj:
			put_instruction_b (fillF_b); break;
		default:
			error_in_function ("FillBasicFromB");
			return;
	}
	put_arguments_nn_b (boffs,aoffs);
	TreatWaitListAfterFill (aoffs, fkind);
}

void BuildBasic (ObjectKind obj,SymbValue val)
{
	switch (obj){
		case IntObj:
			put_instruction_b (buildI);
			put_arguments_i_b (val.val_int);
			break;
		case BoolObj:
			put_instruction_ (IbuildB);
			if (val.val_bool)
				FPrintF (OutFile, "TRUE");
			else
				FPrintF (OutFile, "FALSE");
			break;
		case CharObj:
			put_instruction_ (IbuildC);
			FPrintF (OutFile, "%s", val.val_char);
			break;
		case RealObj:
			put_instruction_ (IbuildR);
			FPrintF (OutFile, "%s", val.val_real);
			break;
		case StringObj:
			put_instruction_ (IbuildS);
			FPrintF (OutFile, "%s", val.val_string);
			break;
		default:
			error_in_function ("BuildBasic");
			return;
	}
}

void FillBasic (ObjectKind obj, SymbValue val, int offset, FillKind fkind)
{
	TreatWaitListBeforeFill (offset, fkind);
	switch (obj){
		case IntObj:
			put_instruction_b (fillI);
			put_arguments_in_b (val.val_int,offset);
			break;
		case BoolObj:
			put_instruction_ (IfillB);
			if (val.val_bool)
				FPrintF (OutFile, "TRUE %d", offset);
			else
				FPrintF (OutFile, "FALSE %d", offset);
			break;
		case CharObj:
			put_instruction_ (IfillC);
			FPrintF (OutFile, "%s %d", val.val_char, offset);
			break;
		case RealObj:
			put_instruction_ (IfillR);
			FPrintF (OutFile, "%s %d", val.val_real, offset);
			break;
		case StringObj:
			put_instruction_ (IfillS);
			FPrintF (OutFile, "%s %d", val.val_string, offset);
			break;
		default:
			error_in_function ("FillBasic");
			return;
	}
	TreatWaitListAfterFill (offset, fkind);
}

void IsBasic (ObjectKind obj, SymbValue val, int offset)
{
	switch (obj){
		case IntObj:
			put_instruction_b (eqI_a);
			put_arguments_in_b (val.val_int,offset);
			break;
		case BoolObj:
			put_instruction_ (IeqB_a);
			if (val.val_bool)
				FPrintF (OutFile, "TRUE %d", offset);
			else
				FPrintF (OutFile, "FALSE %d", offset);
			break;
		case CharObj:
			put_instruction_ (IeqC_a);
			FPrintF (OutFile, "%s %d", val.val_char, offset); break;
		case RealObj:
			put_instruction_ (IeqR_a);
			FPrintF (OutFile, "%s %d", val.val_real, offset); break;
		case StringObj:
			put_instruction_ (IeqS_a);
			FPrintF (OutFile, "%s %d", val.val_string, offset); break;
		default:
			error_in_function ("IsBasic");
			return;
	}
}

void IsString (SymbValue val)
{
	put_instruction_ (IeqAC_a);
	FPrintF (OutFile, "%s",val.val_string);
}

void PushBasic (ObjectKind obj, SymbValue val)
{
	switch (obj){
		case IntObj:
			put_instruction_b (pushI);
			put_arguments_i_b (val.val_int);
			break;
		case BoolObj:
			put_instruction_ (IpushB);
			if (val.val_bool)
				FPutS ("TRUE", OutFile);
			else
				FPutS ("FALSE", OutFile);
			break;
		case CharObj:
			put_instruction_ (IpushC);
			FPrintF (OutFile, "%s", val.val_char); break;
		case RealObj:
			put_instruction_ (IpushR);
			FPrintF (OutFile, "%s", val.val_real); break;
		default:
			error_in_function ("PushBasic");
			return;
	}
}

void GenPushReducerId (int i)
{
	put_instruction_b (pushI);
	put_arguments_n_b (i);
}

void GenPushArgNr (int argnr)
{
	put_instruction_b (pushI);
	put_arguments_n_b (argnr);
}

void EqBasic (ObjectKind obj, SymbValue val, int offset)
{
	switch (obj){
		case IntObj:
			put_instruction_b (eqI_b);
			put_arguments_in_b (val.val_int,offset);
			break;
		case BoolObj:
			put_instruction_ (IeqB_b);
			if (val.val_bool)
				FPrintF (OutFile, "TRUE %d", offset);
			else
				FPrintF (OutFile, "FALSE %d", offset);
			break;
		case CharObj:
			put_instruction_ (IeqC_b);
			FPrintF (OutFile, "%s %d", val.val_char, offset); break;
		case RealObj:
			put_instruction_ (IeqR_b);
			FPrintF (OutFile, "%s %d", val.val_real, offset); break;
		default:
			error_in_function ("EqBasic");
			return;
	}
}

void GenNotB (void)
{
	put_instruction (InotB);
}

void PushBasicFromAOnB (ObjectKind kind,int offset)
{
	switch (kind){
		case IntObj:
		case ProcIdObj:
		case RedIdObj:
			put_instruction_b (pushI_a);
			break;
		case BoolObj:
			put_instruction_b (pushB_a);
			break;
		case CharObj:
			put_instruction_b (pushC_a);
			break;
		case RealObj:
			put_instruction_b (pushR_a);
			break;
		case FileObj:
			put_instruction_b (pushF_a);
			break;
		default:
			error_in_function ("PushBasicFromAOnB");
			return;
	}
	put_arguments_n_b (offset);
}

void GenPushD_a (int a_offset)
{
	put_instruction_ (IpushD_a);
	FPrintF (OutFile,"%d",a_offset);
}
	
void PushBasicOnB (ObjectKind obj, int offset)
{
	int i;

	for (i = ObjectSizes[obj]; i > 0; i--)
		GenPushB (offset + ObjectSizes[obj] - 1);
}

void UpdateBasic (int size, int srcoffset, int dstoffset)
{
	if (srcoffset < dstoffset){
		int i;
		
		for (i=size-1; i >= 0; i--)
			GenUpdateB (srcoffset+i, dstoffset+i);
	} else if (srcoffset > dstoffset){
		int i;
		
		for (i=0; i < size; i++)
			GenUpdateB (srcoffset+i, dstoffset+i);
	}
}

static Bool IsDirective (Instructions instruction, char *directive)
{
	char *s;

	s=instruction->instr_this;
	while (isspace(*s))
		++s;
	if (*s!='.')
		return False;

	for (; *directive; ++directive)
		if (*directive!=*++s)
			return False;

	return True;
}

static Bool IsInlineFromCurrentModule (SymbDef def)
{
	RuleAlts alt;
	Instructions instruction, next;
	/*
	if (def->sdef_kind!=IMPRULE)
		return False;
	*/
	alt=def->sdef_rule->rule_alts;

	if (alt->alt_kind!=ExternalCall || !alt->alt_rhs_code->co_is_abc_code)
		return False;
	
	instruction=alt->alt_rhs_code->co_instr;

	if (!IsDirective(instruction, "inline"))
		return False;

	for (instruction=instruction->instr_next;(next=instruction->instr_next)!=NULL;instruction=next)
		;

	return (IsDirective(instruction, "end"));
}

/*
	For ABC to target machine code generation we supply the abc code
	with special stack layout directives. The routines for doing this
	are 'GenBStackElems', 'GenStackLayoutOfNode' and 'GenStackLayoutOfState'.
*/

static char BElems[] = BASIC_ELEMS_STRING;

static void GenBStackElems (StateS state)
{
	if (IsSimpleState (state)){
		if (state.state_kind == OnB)
			FPutC (BElems [(int) state.state_object], OutFile);			
	} else {
		int arity;
		States argstates;
		
		switch (state.state_type){
			case TupleState:
				argstates = state.state_tuple_arguments;
				break;
			case RecordState:
				argstates = state.state_record_arguments;
				break;
			case ArrayState:
				return;
			default:
				error_in_function ("GenBStackElems");
				return;
		}
		for (arity=0; arity < state.state_arity; ++arity)
			GenBStackElems (argstates[arity]);
	}
}

static void GenABStackElems (StateS state)
{
	if (IsSimpleState (state)){
		if (state.state_kind == OnB)
			FPutC (BElems [(int) state.state_object], OutFile);
		else
			FPutC ('a', OutFile);
	} else {
		int arity;
		States argstates;
		
		switch (state.state_type){
			case TupleState:
				argstates = state.state_tuple_arguments;			
				FPutC ('(', OutFile);
				if (state.state_arity>0){
					GenABStackElems (argstates[0]);
					for (arity=1; arity < state.state_arity; arity++){
						FPutC (',', OutFile);
						GenABStackElems (argstates[arity]);
					}
				}
				FPutC (')', OutFile);
				break;
			case RecordState:
				argstates = state.state_record_arguments;
				FPutC ('(', OutFile);
				for (arity=0; arity < state.state_arity; arity++)
					GenABStackElems (argstates[arity]);
				FPutC (')', OutFile);
				return;
			case ArrayState:
				FPutC ('a', OutFile);
				return;
			default:
				error_in_function ("GenABStackElems");
				return;
		}
	}
}

static void GenABStackElemsForRecordDesc (StateS state)
{
	if (IsSimpleState (state)){
		if (state.state_kind == OnB)
			FPutC (BElems [(int) state.state_object], OutFile);
		else
			FPutC ('a', OutFile);
	} else {
		int arity;
		States argstates;
		
		switch (state.state_type){
			case TupleState:
				argstates = state.state_tuple_arguments;			
				FPutC ('(', OutFile);
				if (state.state_arity>0){
					GenABStackElemsForRecordDesc (argstates[0]);
					for (arity=1; arity < state.state_arity; ++arity){
						FPutC (',', OutFile);
						GenABStackElemsForRecordDesc (argstates[arity]);
					}
				}
				FPutC (')', OutFile);
				return;
			case RecordState:
				argstates = state.state_record_arguments;
				FPutC ('{', OutFile);
				for (arity=0; arity < state.state_arity; ++arity)
					GenABStackElemsForRecordDesc (argstates[arity]);
				FPutC ('}', OutFile);
				return;
			case ArrayState:
				FPutC ('a', OutFile);
				return;
			default:
				error_in_function ("GenABStackElemsForRecordDesc");
		}
	}
}

static void GenABStackElemsOfRecord (StateS state)
{
	if (state.state_type==RecordState){
		int arity;
		States argstates;

		argstates = state.state_record_arguments;
		for (arity=0; arity < state.state_arity; ++arity)
			GenABStackElemsForRecordDesc (argstates[arity]);
	} else
		GenABStackElemsForRecordDesc (state);
}

static int AddSizeOfStatesAndImportRecords (int arity, States states, int *asize, int *bsize);

static int AddSizeOfStateAndImportRecords (StateS state, int *asize, int *bsize)
{
	if (IsSimpleState (state)){
		if (state.state_kind == OnB)
			*bsize += ObjectSizes [state.state_object];
		else if (state.state_kind != Undefined)
			*asize += SizeOfAStackElem;
		return 0;
	} else {
		switch (state.state_type){
			case RecordState:
			{
				SymbDef record_sdef;

				record_sdef = state.state_record_symbol;
				if (record_sdef->sdef_exported || record_sdef->sdef_module!=CurrentModule || ExportLocalLabels){
					if ((record_sdef->sdef_mark & SDEF_RECORD_R_LABEL_IMPORTED_MASK)!=0){
						record_sdef->sdef_mark |= SDEF_USED_STRICTLY_MASK;
					} else {
						record_sdef->sdef_mark |= SDEF_USED_STRICTLY_MASK;
						record_sdef->sdef_mark |= SDEF_USED_STRICTLY_MASK | SDEF_RECORD_R_LABEL_IMPORTED_MASK;
						GenImpRecordDesc (record_sdef->sdef_module,record_sdef->sdef_ident->ident_name);
					}
				}

				(void) AddSizeOfStatesAndImportRecords (state.state_arity, state.state_record_arguments, asize, bsize);
				return 1;
			}
			case TupleState:
				return AddSizeOfStatesAndImportRecords (state.state_arity, state.state_tuple_arguments, asize, bsize);
			case ArrayState:
				*asize += SizeOfAStackElem;
				return 0;
		}
	}
	return 0;
}

static int AddSizeOfStatesAndImportRecords (int arity, States states, int *asize, int *bsize)
{
	int has_unboxed_record;
	
	has_unboxed_record=0;
	for (; arity; arity--)
		has_unboxed_record |= AddSizeOfStateAndImportRecords (states [arity-1], asize, bsize);
	return has_unboxed_record;
}

static void GenUnboxedRecordLabelsReversed (StateS state)
{
	if (!IsSimpleState (state)){
		int arity;
		States argstates;
		
		switch (state.state_type){
			case TupleState:
				argstates = state.state_tuple_arguments;			
				for (arity=state.state_arity-1; arity>=0; --arity)
					GenUnboxedRecordLabelsReversed (argstates[arity]);
				return;
			case RecordState:
			{
				SymbDef record_sdef;

				argstates = state.state_record_arguments;
				for (arity=state.state_arity-1; arity>=0 ; --arity)
					GenUnboxedRecordLabelsReversed (argstates[arity]);

				record_sdef = state.state_record_symbol;
				if (!record_sdef->sdef_exported && record_sdef->sdef_module==CurrentModule && !ExportLocalLabels){
					if (DoDebug)
						FPrintF (OutFile, R_PREFIX "%s ",record_sdef->sdef_ident->ident_name);
					else
						FPrintF (OutFile, R_PREFIX "%u ",record_sdef->sdef_number);
				} else
					FPrintF (OutFile, "e_%s_" R_PREFIX "%s ",record_sdef->sdef_module,record_sdef->sdef_ident->ident_name);
				return;
			}
			case ArrayState:
				return;
			default:
				error_in_function ("GenUnboxedRecordLabelsReversed");
				return;
		}
	}
}

static void GenUnboxedRecordLabelsReversedForRecord (StateS state)
{
	if (state.state_type==RecordState){
		int arity;
		States argstates;

		argstates = state.state_record_arguments;
		for (arity=state.state_arity-1; arity>=0; --arity)
			GenUnboxedRecordLabelsReversed (argstates[arity]);
	} else
		GenUnboxedRecordLabelsReversed (state);
}

void GenDStackLayout (int asize,int bsize,Args fun_args)
{
	if (DoStackLayout){
		put_directive_b (d);
		if (bsize > 0){
			put_arguments_nn__b (asize,bsize);

			while (fun_args!=NULL){
				GenBStackElems (fun_args->arg_state);
				fun_args=fun_args->arg_next;
			}
		} else
			put_arguments_nn_b (asize,0);
	}
}

void GenOStackLayout (int asize,int bsize,Args fun_args)
{
	if (DoStackLayout){
		put_directive_b (o);
		if (bsize > 0){
			put_arguments_nn__b (asize,bsize);

			while (fun_args!=NULL){
				GenBStackElems (fun_args->arg_state);
				fun_args=fun_args->arg_next;
			}
		} else
			put_arguments_nn_b (asize,0);
	}
}

static void CallFunction2 (Label label, SymbDef def, Bool isjsr, StateS root_state, Args fun_args, int arity)
{
	int ain,aout,bin,bout;
	Args arg;
	
	ain=0;
	bin=0;
	
	if (fun_args != NULL){
		for (arg = fun_args; arg; arg = arg -> arg_next)
			AddSizeOfState  (arg -> arg_state, &ain, &bin);
	} else
		ain = arity;
	DetermineSizeOfState (root_state, &aout, &bout);

	if (IsSimpleState (root_state) && (root_state.state_kind!=OnB && root_state.state_kind!=StrictRedirection))
		ain++;

	if (label->lab_mod && label->lab_mod==CurrentModule)
		label->lab_mod = NULL;

	label->lab_pref = s_pref;

	if (def->sdef_kind==SYSRULE){
		char  *instr;
	
		instr= def->sdef_ident->ident_instructions;

		if (instr && *instr!='\0'){
			char *tail;

			for (; *instr != '\0'; instr = tail){
				for (tail = instr; *tail != '\n'; tail++)
					;
				*tail = '\0';
				FPrintF (OutFile, "\n%s", instr);
				*tail++ = '\n';
			}
			if (!isjsr)
				GenRtn (aout, bout, root_state);
		} else {
			if (def->sdef_ident->ident_environ && instr==NULL)
			{	char *previous_module = CurrentModule;
				char *previous_ext = CurrentExt;
			
				CurrentModule = def->sdef_module;
				CurrentExt    = GetFileExtension (abcFile);
				
				StaticMessage (False, "%D", "no inline code for this function", def);

				CurrentModule = previous_module;
				CurrentExt = previous_ext;

               	def->sdef_ident->ident_environ = (char *) NIL;	
			}
			GenDStackLayout (ain, bin, fun_args);
			if (isjsr){
				GenJsr (label);
				GenOStackLayoutOfState (aout, bout, root_state);
			} else
				GenJmp (label);
		}
		return;
	}
	if (def->sdef_kind==IMPRULE){
		if ((def->sdef_mark & SDEF_INLINE_IS_CONSTRUCTOR)!=0){
			generate_is_constructor (def->sdef_rule);
			if (!isjsr)
				GenRtn (aout, bout, root_state);
			return;
		} else if (IsInlineFromCurrentModule (def)){
			Instructions instruction, last, first, next;

			instruction=def->sdef_rule->rule_alts->alt_rhs_code->co_instr;
			instruction=instruction->instr_next;
			first=instruction;

			last=NULL;
			for (;(next=instruction->instr_next)!=NULL;instruction=next)
				last=instruction;

			last->instr_next=NULL;
			GenInstructions (first);
			last->instr_next=instruction;

			if (!isjsr)
				GenRtn (aout, bout, root_state);
			return;
		}
	}

	GenDStackLayout (ain, bin, fun_args);
	if (isjsr){
		GenJsr (label);
		GenOStackLayoutOfState (aout, bout, root_state);
	} else
		GenJmp (label);
}

void CallFunction (Label label, SymbDef def, Bool isjsr, Node root)
{
	if (def->sdef_arfun<NoArrayFun)
		CallArrayFunction (def,isjsr,&root->node_state);
	else
		CallFunction2 (label, def, isjsr, root->node_state, root->node_arguments, root->node_arity);
}

void CallFunction1 (Label label, SymbDef def, StateS root_state, Args fun_args, int arity)
{
	CallFunction2 (label, def, True, root_state, fun_args, arity);
}

static void GenArraySize (Label elemdesc, int asize, int bsize)
{
	put_instruction_ (Ipush_arraysize);
	GenLabel (elemdesc);
	FPrintF (OutFile, " %d %d", asize, bsize);
}

static void GenArraySelect (Label elemdesc, int asize, int bsize)
{
	put_instruction_ (Iselect);
	GenLabel (elemdesc);
	FPrintF (OutFile, " %d %d", asize, bsize);
}

static void GenArrayUpdate (Label elemdesc, int asize, int bsize)
{
	put_instruction_ (Iupdate);
	GenLabel (elemdesc);
	FPrintF (OutFile, " %d %d", asize, bsize);	
}

static void GenArrayReplace (Label elemdesc, int asize, int bsize)
{
	put_instruction_ (Ireplace);
	GenLabel (elemdesc);
	FPrintF (OutFile, " %d %d", asize, bsize);
}

#if CLEAN2
static int CaseFailNumber;
#endif

void CallArrayFunction (SymbDef array_def,Bool is_jsr,StateP node_state_p)
{
	LabDef elem_desc;
	int asize, bsize;
	Bool elem_is_lazy;
	StateS array_state;
	ArrayFunKind fkind;
	StateP function_state_p;
	
	fkind = (ArrayFunKind)array_def->sdef_arfun;

	switch (array_def->sdef_kind)
	{
		case DEFRULE:
		case SYSRULE:
			function_state_p = array_def->sdef_rule_type->rule_type_state_p;
			break;
		case IMPRULE:
			function_state_p = array_def->sdef_rule->rule_state_p;
			break;
		default:
			error_in_function ("CallArrayFunction");
			break;
	}

	switch (fkind){
		case CreateArrayFun:
		case _CreateArrayFun:
			array_state = function_state_p[-1];
			break;
	 	case _UnqArraySelectNextFun:
		case _UnqArraySelectLastFun:
		case _ArrayUpdateFun:
			if (function_state_p[0].state_type==TupleState)
				array_state=function_state_p[0].state_tuple_arguments[0];
			else
				error_in_function ("CallArrayFunction");
			break;
		default:
			array_state = function_state_p[0];
	}

	if (array_state.state_type == ArrayState){
		StateS elem_state = array_state.state_array_arguments [0];
		DetermineArrayElemDescr (elem_state, & elem_desc);
		DetermineSizeOfState	(elem_state, & asize, & bsize);
		elem_is_lazy = elem_state.state_type==SimpleState && elem_state.state_kind==OnA;
	} else
		error_in_function ("CallArrayFunction");

	switch (fkind){
		case CreateArrayFun:
			put_instruction_ ("create_array");
			GenLabel (&elem_desc);
			FPrintF (OutFile," %d %d",asize,bsize);
			break;
		case _CreateArrayFun:
			put_instruction_ ("create_array_");
			GenLabel (&elem_desc);
			FPrintF (OutFile," %d %d",asize,bsize);
			break;
		case ArraySelectFun:
			GenArraySelect (&elem_desc,asize,bsize);
			if (elem_is_lazy){
				if (is_jsr)
					GenJsrEval (0);
				else {
					GenJmpEval ();
					return;
				}
			}
			break;
		case UnqArraySelectFun:
#ifdef OBSERVE_ARRAY_SELECTS_IN_PATTERN
			if (! (node_state_p->state_type==TupleState
				&& node_state_p->state_tuple_arguments[1].state_type==SimpleState
				&& node_state_p->state_tuple_arguments[1].state_kind==Undefined))
			{
				GenPushA (0);
			}
			GenArraySelect (&elem_desc,asize,bsize);
			break;
#endif
		case _UnqArraySelectFun:
			GenPushA (0);
			GenArraySelect (&elem_desc,asize,bsize);
			break;
		case _UnqArraySelectNextFun:
		case _UnqArraySelectLastFun:
		{
			int record_or_array_a_size,record_or_array_b_size;
			
			if (node_state_p->state_type!=TupleState)
				error_in_function ("CallArrayFunction");

			DetermineSizeOfState (node_state_p->state_tuple_arguments[1],&record_or_array_a_size,&record_or_array_b_size);
			
			if (record_or_array_b_size>0){
				int i;
				
				GenPushB (record_or_array_b_size);
				
				for (i=record_or_array_b_size; i>=0; --i)
					GenUpdateB (i,i+1);
				
				GenPopB (1);
			}

			GenArraySelect (&elem_desc,asize,bsize);
			break;
		}
		case _ArrayUpdateFun:
		{
			int i,result_a_size,result_b_size;
			
			DetermineSizeOfState (*node_state_p,&result_a_size,&result_b_size);

			if (asize!=0){
				for (i=0; i<asize; ++i)
					GenPushA (result_a_size+asize);
			
				for (i=result_a_size-1; i>=0; --i)
					GenUpdateA (i+asize+1,i+asize+1+asize);

				for (i=asize-1; i>=0; --i)
					GenUpdateA (i,i+1+asize);

				GenPopA (asize);
			}			
			
			if (result_b_size!=0){
				int b_size_with_index;

				b_size_with_index=bsize+1;

				for (i=0; i<b_size_with_index; ++i)
					GenPushB (result_b_size+b_size_with_index-1);
				
				for (i=result_b_size-1; i>=0; --i)
					GenUpdateB (i+b_size_with_index,i+b_size_with_index+b_size_with_index);

				for (i=b_size_with_index-1; i>=0; --i)
					GenUpdateB (i,i+b_size_with_index);

				GenPopB (b_size_with_index);				
			}

			GenArrayUpdate (&elem_desc,asize,bsize);
							
			for (i=0; i<result_a_size; ++i)
				GenKeep (0,i+1);
			
			GenPopA (1);

			break;
		}
		case ArrayUpdateFun:
			GenArrayUpdate (& elem_desc, asize, bsize);
			break;
		case ArrayReplaceFun:
			GenArrayReplace (& elem_desc, asize, bsize);
			break;
		case ArraySizeFun:
			GenArraySize (& elem_desc, asize, bsize);
			break;
		case UnqArraySizeFun:
			GenPushA (0);
			GenArraySize (& elem_desc, asize, bsize);
			break;
	}
	
	if (! is_jsr){
		DetermineSizeOfState (*node_state_p,&asize,&bsize);
		GenRtn (asize,bsize,*node_state_p);
	}	
}

void GenNewContext (Label contlab, int offset)
{
	FPrintF (OutFile, "\n\tset_entry ");
	GenLabel (contlab);
	FPrintF (OutFile, " %d", offset);
}

void GenSetDefer (int offset)
{
	FPrintF (OutFile, "\n\tset_defer %d", offset);
}

void GenReplArgs (int arity, int nrargs)
{
	if (nrargs > 0){
		put_instruction_b (repl_args);
		put_arguments_nn_b (arity,nrargs);
	} else
		GenPopA (1);
}

void GenReplArg (int arity, int argnr)
{
	put_instruction_b (repl_arg);
	put_arguments_nn_b (arity,argnr);
}

void GenPushArgs (int offset, int arity, int nrargs)
{
	if (nrargs > 0){
		put_instruction_b (push_args);
		put_arguments_nnn_b (offset,arity,nrargs);
	}
}

void GenPushArgsU (int offset, int arity, int nrargs)
{
	if (nrargs > 0){
		put_instruction_b (push_args_u);
		put_arguments_nnn_b (offset,arity,nrargs);
	}
}

void GenPushArg (int offset, int arity, int argnr)
{
	put_instruction_b (push_arg);
	put_arguments_nnn_b (offset,arity,argnr);
}

void GenPushRArgs (int offset, int nr_a_args, int nr_b_args)
{
	if (nr_a_args + nr_b_args > 0){
		put_instruction_b (push_r_args);
		put_arguments_nnn_b (offset,nr_a_args,nr_b_args);
	}
}

void GenPushRArgsU (int offset,int n_a_args,int n_b_args)
{
	if (n_a_args + n_b_args > 0){
		put_instruction_b (push_r_args_u);
		put_arguments_nnn_b (offset,n_a_args,n_b_args);
	}
}

void GenPushRArgA (int offset,int tot_nr_a_args,int tot_nr_b_args,int args_nr,int nr_a_args)
{
	if (nr_a_args > 0){
		put_instruction_b (push_r_args_a);
		put_arguments_nnnnn_b (offset,tot_nr_a_args,tot_nr_b_args,args_nr,nr_a_args);
	}
}

void GenPushRArgB (int offset,int tot_nr_a_args,int tot_nr_b_args,int args_nr,int nr_b_args)
{
	if (nr_b_args > 0){
		put_instruction_b (push_r_args_b);
		put_arguments_nnnnn_b (offset,tot_nr_a_args,tot_nr_b_args,args_nr,nr_b_args);
	}
}

void GenPushRArgU (int offset,int tot_nr_a_args,int tot_nr_b_args,int args_a_nr,int nr_a_args,int args_b_nr,int nr_b_args)
{
	put_instruction_b (push_r_arg_u);
	put_arguments_nnnnnnn_b (offset,tot_nr_a_args,tot_nr_b_args,args_a_nr,nr_a_args,args_b_nr,nr_b_args);
}

void GenReplRArgs (int nr_a_args, int nr_b_args)
{
	if (nr_a_args +  nr_b_args > 0){
		put_instruction_b (repl_r_args);
		put_arguments_nn_b (nr_a_args,nr_b_args);
	} else
		GenPopA (1);
}

void GenReplRArgA (int tot_nr_a_args, int tot_nr_b_args, int args_nr, int nr_a_args)
{
	if (nr_a_args > 0){
		put_instruction_b (repl_r_args_a);
		put_arguments_nnnn_b (tot_nr_a_args,tot_nr_b_args,args_nr,nr_a_args);
	} else
		GenPopA (1);
}

void GenPushNode (Label contlab, int arity)
{
	put_instruction_b (push_node);
	GenLabel (contlab);
	put_arguments__n_b (arity);
}

void GenPushNodeU (Label contlab,int a_size,int b_size)
{
	put_instruction_b (push_node_u);
	GenLabel (contlab);
	put_arguments__nn_b (a_size,b_size);
}

void GenFill (Label symblab,int arity,Label contlab,int offset,FillKind fkind)
{
	TreatWaitListBeforeFill (offset, fkind);

	put_instruction_b (fill);
	
	if (!symblab->lab_issymbol || DescriptorNeeded (symblab->lab_symbol))
		GenDescriptorOrNodeEntryLabel (symblab);
	else
		FPutS (empty_lab.lab_name, OutFile);
	
	put_arguments__n__b (arity);
	
	GenDescriptorOrNodeEntryLabel (contlab);
	
	put_arguments__n_b (offset);

	if (arity < 0)
		arity = 1;
	TreatWaitListAfterFill (offset-arity, fkind);
}

void GenFillU (Label symblab,int a_size,int b_size,Label contlab,int offset)
{
	put_instruction_ (Ifill_u);
	
	if (!symblab->lab_issymbol || DescriptorNeeded (symblab->lab_symbol))
		GenDescriptorOrNodeEntryLabel (symblab);
	else
		FPutS (empty_lab.lab_name, OutFile);
	
	FPrintF (OutFile," %d %d ",a_size,b_size);
		
	GenDescriptorOrNodeEntryLabel (contlab);
	
	put_arguments__n_b (offset);
}

void GenFillcp (Label symblab,int arity,Label contlab,int offset,char bits[])
{
	put_instruction_b (fillcp);
	
	if (!symblab->lab_issymbol || DescriptorNeeded (symblab->lab_symbol))
		GenDescriptorOrNodeEntryLabel (symblab);
	else
		FPutS (empty_lab.lab_name, OutFile);
	
	put_arguments__n__b (arity);
	
	GenDescriptorOrNodeEntryLabel (contlab);
	
	put_arguments__n_b (offset);

	FPrintF (OutFile," %s",bits);
}

void GenFillcpU (Label symblab,int a_size,int b_size,Label contlab,int offset,char bits[])
{
	put_instruction_b (fillcp_u);
	
	if (!symblab->lab_issymbol || DescriptorNeeded (symblab->lab_symbol))
		GenDescriptorOrNodeEntryLabel (symblab);
	else
		FPutS (empty_lab.lab_name, OutFile);
		
	FPrintF (OutFile," %d %d ",a_size,b_size);

	GenDescriptorOrNodeEntryLabel (contlab);
	
	put_arguments__n_b (offset);

	FPrintF (OutFile," %s",bits);
}

void GenFillh (Label symblab, int arity, int offset, FillKind fkind)
{
	TreatWaitListBeforeFill (offset, fkind);

	put_instruction_b (fillh);
	
	if (!symblab->lab_issymbol || DescriptorNeeded (symblab->lab_symbol))
		GenDescriptorOrNodeEntryLabel (symblab);
	else
		FPutS (empty_lab.lab_name, OutFile);
	
	put_arguments__nn_b (arity,offset);

	if (arity < 0)
		arity = 1;
	TreatWaitListAfterFill (offset-arity, fkind);
}

void GenFill1 (Label symblab,int arity,int offset,char bits[])
{
	put_instruction_ (Ifill1);
	
	if (!symblab->lab_issymbol || DescriptorNeeded (symblab->lab_symbol))
		GenLabel (symblab);
	else
		FPutS (empty_lab.lab_name, OutFile);
	
	FPrintF (OutFile," %d %d %s",arity,offset,bits);
}

void GenFill2 (Label symblab,int arity,int offset,char bits[])
{	
	put_instruction_ (Ifill2);
	GenLabel (symblab);
	FPrintF (OutFile," %d %d %s",arity,offset,bits);
}

void GenFill3 (Label symblab,int arity,int offset,char bits[])
{	
	put_instruction_ (Ifill3);
	GenLabel (symblab);	
	FPrintF (OutFile, " %d %d %s",arity,offset,bits);
}

void GenBuild (Label symblab,int arity,Label contlab)
{
	put_instruction_b (build);
	
	if (!symblab->lab_issymbol || DescriptorNeeded (symblab->lab_symbol))
		GenDescriptorOrNodeEntryLabel (symblab);
	else
		FPutS (empty_lab.lab_name, OutFile);
	
	put_arguments__n__b (arity);
	
	GenDescriptorOrNodeEntryLabel (contlab);
}

void GenBuildh (Label symblab,int arity)
{
	put_instruction_b (buildh);
	
	if (!symblab->lab_issymbol || DescriptorNeeded (symblab->lab_symbol))
		GenLabel (symblab);
	else
		FPutS (empty_lab.lab_name, OutFile);
	
	put_arguments__n_b (arity);
}

void GenBuildPartialFunctionh (Label symblab,int arity)
{
	put_instruction_b (buildh);
	
	if (!symblab->lab_issymbol || DescriptorNeeded (symblab->lab_symbol))
		GenDescriptorOrNodeEntryLabel (symblab);
	else
		FPutS (empty_lab.lab_name, OutFile);
	
	put_arguments__n_b (arity);
}

void GenBuildU (Label symblab,int a_size,int b_size,Label contlab)
{
	put_instruction_ (Ibuild_u);
	
	if (!symblab->lab_issymbol || DescriptorNeeded (symblab->lab_symbol))
		GenDescriptorOrNodeEntryLabel (symblab);
	else
		FPutS (empty_lab.lab_name, OutFile);
	
	FPrintF (OutFile," %d %d ",a_size,b_size);
	
	GenDescriptorOrNodeEntryLabel (contlab);
}

void GenBuildArrayPop (void)
{
	GenBuildh (& BasicDescriptors [ArrayObj], 1);
}

void GenBuildArray (int argoffset)
{
	GenPushA (argoffset);
	GenBuildArrayPop();
}

void GenBuildString (SymbValue val)
{
	put_instruction_ (IbuildAC);
	FPrintF (OutFile, "%s", val.val_string);
}

void GenPushZ (SymbValue val)
{
	put_instruction_ (IpushZ);
	FPrintF (OutFile, "%s", val.val_string);
}

void GenPushZR (SymbValue val)
{
	put_instruction_ (IpushZR);
	FPrintF (OutFile, "%s", val.val_string);
}

static void GenFieldLabel (Label label,char *record_name)
{
	SymbDef def;
	
	def = (SymbDef) label->lab_name;
		
	if (label->lab_mod)
		FPrintF (OutFile,"e_%s_%s%s.%s",label->lab_mod,label->lab_pref,record_name,def->sdef_ident->ident_name);
	else if (ExportLocalLabels)
		FPrintF (OutFile,"e_%s_%s%s.%s",CurrentModule,label->lab_pref,record_name,def->sdef_ident->ident_name);
	else if (DoDebug){
		if (def->sdef_kind==IMPRULE)
			FPrintF (OutFile, "%s%s.%s.%u",label->lab_pref,record_name,def->sdef_ident->ident_name,def->sdef_number);
		else
			FPrintF (OutFile, "%s%s.%s",label->lab_pref,record_name,def->sdef_ident->ident_name);
	} else if (def->sdef_number==0)
		FPrintF (OutFile, "%s%s",label->lab_pref,def->sdef_ident->ident_name);
	else if (label->lab_pref[0] == '\0')
		FPrintF (OutFile,LOCAL_D_PREFIX "%u",def->sdef_number);
	else
		FPrintF (OutFile,"%s%u",label->lab_pref,def->sdef_number);
}

void GenBuildFieldSelector (Label symblab,Label contlab,char *record_name,int arity)
{
	put_instruction_b (build);

	if (!symblab->lab_issymbol || DescriptorNeeded (symblab->lab_symbol))
		GenFieldLabel (symblab,record_name);
	else
		FPutS (empty_lab.lab_name, OutFile);

	put_arguments__n__b (arity);

	GenFieldLabel (contlab,record_name);
}

void GenFieldLabelDefinition (Label label,char *record_name)
{
	FPutS ("\n", OutFile);
	GenFieldLabel (label,record_name);
}

void GenFillFieldSelector (Label symblab,Label contlab,char *record_name,int arity,int offset,FillKind fkind)
{
	TreatWaitListBeforeFill (offset,fkind);
	
	put_instruction_b (fill);

	if (!symblab->lab_issymbol || DescriptorNeeded (symblab->lab_symbol))
		GenFieldLabel (symblab,record_name);
	else
		FPutS (empty_lab.lab_name, OutFile);

	put_arguments__n__b (arity);

	GenFieldLabel (contlab,record_name);

	put_arguments__n_b (offset);

	TreatWaitListAfterFill (offset-1,fkind);
}

void GenFillR (Label symblab,int nr_a_args,int nr_b_args,int rootoffset,int a_offset,int b_offset,FillKind fkind,Bool pop_args)
{
	TreatWaitListBeforeFill (rootoffset, fkind);
	
	put_instruction_ (Ifill_r);
	
	if (! symblab->lab_issymbol || DescriptorNeeded (symblab->lab_symbol))
		GenLabel (symblab);
	else
		FPutS (empty_lab.lab_name, OutFile);

	if (nr_a_args==0)
		a_offset=0;
	if (nr_b_args==0)
		b_offset=0;
	
	FPrintF (OutFile, " %d %d %d %d %d",nr_a_args,nr_b_args,rootoffset,a_offset,b_offset);
	
	if (pop_args){
		GenPopA (nr_a_args);
		GenPopB (nr_b_args);
		TreatWaitListAfterFill (rootoffset-nr_a_args, fkind);		
	} else
		TreatWaitListAfterFill (rootoffset, fkind);
}

void GenFill1R (Label symblab,int n_a_args,int n_b_args,int rootoffset,char bits[])
{	
	put_instruction_ (Ifill1_r);
	
	if (! symblab->lab_issymbol || DescriptorNeeded (symblab->lab_symbol))
		GenLabel (symblab);
	else
		FPutS (empty_lab.lab_name, OutFile);
	
	FPrintF (OutFile, " %d %d %d %s",n_a_args,n_b_args,rootoffset,bits);	
}

void GenFill2R (Label symblab,int n_a_args,int n_b_args,int rootoffset,char bits[])
{	
	put_instruction_ (Ifill2_r);
	
	if (! symblab->lab_issymbol || DescriptorNeeded (symblab->lab_symbol))
		GenLabel (symblab);
	else
		FPutS (empty_lab.lab_name, OutFile);
	
	FPrintF (OutFile, " %d %d %d %s",n_a_args,n_b_args,rootoffset,bits);
}

void GenFill3R (Label symblab,int n_a_args,int n_b_args,int rootoffset,char bits[])
{	
	put_instruction_ (Ifill3_r);
	
	if (! symblab->lab_issymbol || DescriptorNeeded (symblab->lab_symbol))
		GenLabel (symblab);
	else
		FPutS (empty_lab.lab_name, OutFile);
	
	FPrintF (OutFile, " %d %d %d %s",n_a_args,n_b_args,rootoffset,bits);
}

void GenBuildhr (Label symblab,int nr_a_args,int nr_b_args)
{
	put_instruction_ (Ibuildhr);
	
	if (!symblab->lab_issymbol || DescriptorNeeded (symblab->lab_symbol))
		GenLabel (symblab);
	else
		FPutS (empty_lab.lab_name, OutFile);

	FPrintF (OutFile, " %d %d",nr_a_args,nr_b_args);
}

void GenBuildR (Label symblab,int nr_a_args,int nr_b_args,int a_offset,int b_offset)
{
	put_instruction_ (Ibuild_r);
	
	if (!symblab->lab_issymbol || DescriptorNeeded (symblab->lab_symbol))
		GenLabel (symblab);
	else
		FPutS (empty_lab.lab_name, OutFile);

	if (nr_a_args==0)
		a_offset=0;
	if (nr_b_args==0)
		b_offset=0;

	FPrintF (OutFile, " %d %d %d %d",nr_a_args,nr_b_args,a_offset,b_offset);
}

void GenFillFromA (int src, int dst, FillKind fkind)
{
	if (src == dst)
		return;
	
	TreatWaitListBeforeFill (dst, fkind);
	put_instruction_b (fill_a);
	put_arguments_nn_b (src,dst);
	TreatWaitListAfterFill (dst, fkind);
}

void GenFillArrayAndPop (int rootoffset, FillKind fkind)
{
	GenFillh (&BasicDescriptors [ArrayObj], 1, rootoffset, fkind);
}

void GenFillArray (int argoffset, int rootoffset, FillKind fkind)
{
	GenPushA (argoffset);
	GenFillh (&BasicDescriptors [ArrayObj], 1, rootoffset+1, fkind);
}

void GenPushArray (int rootoffset)
{
	put_instruction_ (Ipush_array);
	FPrintF (OutFile, "%d", rootoffset);
}

void GenRtn (int asize, int bsize, StateS resultstate)
{
	GenDStackLayoutOfState (asize, bsize, resultstate);
	put_instructionb (rtn);
}

void GenPushA (int offset)
{
	if (offset<0)
		error_in_function ("GenPushA");

	put_instruction_b (push_a);
	put_arguments_n_b (offset);
}

void GenPushB (int offset)
{
	if (offset<0)
		error_in_function ("GenPushB");

	put_instruction_b (push_b);
	put_arguments_n_b (offset);
}

void GenJsrEval (int offset)
{
	put_instruction_b (jsr_eval);
	put_arguments_n_b (offset);
}

void GenJsrAp (int n_args)
{
	put_instruction_b (jsr_ap);
	put_arguments_n_b (n_args);
}

void GenJsrI (int n_args)
{
	put_instruction_b (jsr_i);
	put_arguments_n_b (n_args);
}

void GenJmpEval (void)
{
	put_instruction (Ijmp_eval);
}

void GenJmpAp (int n_args)
{
	put_instruction_b (jmp_ap);
	put_arguments_n_b (n_args);
}

void GenJmpApUpd (int n_args)
{
	put_instruction_b (jmp_ap_upd);
	put_arguments_n_b (n_args);
}

void GenJmpI (int n_args)
{
	put_instruction_b (jmp_i);
	put_arguments_n_b (n_args);
}

void GenJmpNotEqZ (SymbValue val,Label tolab)
{
	put_instruction_ (Ijmp_not_eqZ);
	FPrintF (OutFile, "%s ", val.val_string);
	GenLabel (tolab);
}

void GenJmpUpd (Label tolab)
{
	put_instruction_b (jmp_upd);
	GenLabel (tolab);
}

void GenPopA (int nr)
{
	if (nr > 0){
		put_instruction_b (pop_a);
		put_arguments_n_b (nr);
	}
}

void GenPopB (int nr)
{
	if (nr > 0){
		put_instruction_b (pop_b);
		put_arguments_n_b (nr);
	}
}

void GenEqDesc (Label symblab,int arity,int offset)
{
	put_instruction_b (eq_desc);
	GenLabel (symblab);
	put_arguments__nn_b (arity,offset);
}

void GenEqD_b (Label symblab,int arity)
{
	put_instruction_b (eqD_b);
	GenLabel (symblab);
	put_arguments__n_b (arity);
}

void GenExitFalse (Label to)
{
	put_instruction_ ("exit_false");
	GenLabel (to);
}

void GenJmpFalse (Label to)
{
	put_instruction_b (jmp_false);
	GenLabel (to);
}

void GenJmpTrue (Label to)
{
	put_instruction_b (jmp_true);
	GenLabel (to);
}

void GenJmp (Label tolab)
{
	put_instruction_b (jmp);
	GenLabel (tolab);
}

void GenJsr (Label tolab)
{
	put_instruction_b (jsr);
	GenLabel (tolab);
}

void GenCreate (int arity)
{
	if (arity == -1)
		put_instruction (Icreate);
	else {
		put_instruction_ (Icreate);
		FPrintF (OutFile, "%d", arity);
	}
}

void GenDumpString (char *str)
{
	put_instruction_ (Iprint);
	FPrintF (OutFile, "\"%s\"", str);
	put_instruction (Ihalt);
}

void GenLabelDefinition (Label lab)
{
	if (lab){
		FPutC ('\n', OutFile);
		GenLabel (lab);
	}
}

void GenNodeEntryLabelDefinition (Label lab)
{
	FPutC ('\n', OutFile);
	GenDescriptorOrNodeEntryLabel (lab);
}

void GenUpdateA (int src, int dst)
{
	if (src != dst){
		put_instruction_b (update_a);
		put_arguments_nn_b (src,dst);
	}
}

void GenUpdatePopA (int src, int dst)
{
	if (src!=dst){
		if (dst!=0){
			put_instruction_b (updatepop_a);
			put_arguments_nn_b (src,dst);
		} else {
			put_instruction_b (update_a);
			put_arguments_nn_b (src,dst);
		}
	} else
		if (dst > 0){
			put_instruction_b (pop_a);
			put_arguments_n_b (dst);
		}
}

void GenUpdateB (int src, int dst)
{
	if (src != dst){
		put_instruction_b (update_b);
		put_arguments_nn_b (src,dst);
	}
}

void GenUpdatePopB (int src, int dst)
{
	if (src!=dst){
		if (dst!=0){
			put_instruction_b (updatepop_b);
			put_arguments_nn_b (src,dst);
		} else {
			put_instruction_b (update_b);
			put_arguments_nn_b (src,dst);
		}
	} else
		if (dst > 0) {
			put_instruction_b (pop_b);
			put_arguments_n_b (dst);
		}
}

void GenHalt (void)
{
	put_instruction (Ihalt);
}

void GenSetRedId (int offset)
{
	FPrintF (OutFile, "\n\tset_red_id %d", offset);
}

void GenNewParallelReducer (int offset, char *reducer_code)
{
	FPrintF (OutFile, "\n\tnew_ext_reducer %s %d", reducer_code, offset);
}

void GenNewContInterleavedReducer (int offset)
{
	FPrintF (OutFile, "\n\tnew_int_reducer _cont_reducer %d", offset);
	FPrintF (OutFile, "\n\tforce_cswitch", offset);
}

void GenNewInterleavedReducer (int offset, char *reducer_code)
{
	FPrintF (OutFile, "\n\tnew_int_reducer %s %d", reducer_code, offset);
}

void GenSendGraph (char *code, int graphoffs, int chanoffs)
{
	FPrintF (OutFile, "\n\tsend_graph %s %d %d", code, graphoffs, chanoffs);
}

void GenCreateChannel (char *code)
{
	FPrintF (OutFile, "\n\tcreate_channel %s", code);
}

void GenNewP (void)
{
	FPutS ("\n\tnewP", OutFile);
}

void SetContinue (int offset)
{
	FPrintF (OutFile, "\n\tset_continue %d", offset);
}

void SetContinueOnReducer (int offset)
{
	FPrintF (OutFile, "\n\tset_continue2 %d", offset);
}

void GenGetNodeArity (int offset)
{
	put_instruction_ (Iget_node_arity);
	FPrintF (OutFile, "%d", offset);
}

static void GenGetDescArity (int offset)
{
	put_instruction_ (Iget_desc_arity);
	FPrintF (OutFile, "%d", offset);
}

void GenPushArgB (int offset)
{
	put_instruction_ (Ipush_arg_b);
	FPrintF (OutFile, "%d", offset);
}

extern char *current_imported_module; /* from statesgen.c */

void GenImpRecordDesc (char *module_name,char *record_name)
{
	if (current_imported_module!=module_name){
		current_imported_module = module_name;
		GenImpMod (module_name);
	}

	put_directive_b (impdesc);
	FPrintF (OutFile, "e_%s_" R_PREFIX "%s",module_name,record_name);
}

void GenImport (SymbDef sdef)
{
	if (DoStackLayout){	
		char *name;
		
		name = sdef->sdef_ident->ident_name;

		switch (sdef->sdef_kind){
			case DEFRULE:
			case SYSRULE:
				if (sdef->sdef_mark & (SDEF_USED_CURRIED_MASK | SDEF_USED_LAZILY_MASK)){
					put_directive_b (impdesc);
					FPrintF (OutFile, "e_%s_" D_PREFIX "%s",sdef->sdef_module,name);
				}
				if (sdef->sdef_mark & SDEF_USED_STRICTLY_MASK && sdef->sdef_arfun==NoArrayFun){
					put_directive_b (implab);
					FPrintF (OutFile,"e_%s_" S_PREFIX "%s",sdef->sdef_module,name);
				}
				break;
			case FIELDSELECTOR:
				if (sdef->sdef_mark & SDEF_USED_LAZILY_MASK){
					char *record_name;
	
					record_name	= sdef->sdef_type->type_lhs->ft_symbol->symb_def->sdef_ident->ident_name;
	
					put_directive_b (impdesc);
					FPrintF (OutFile, "e_%s_" D_PREFIX "%s.%s",sdef->sdef_module,record_name,name);	

					put_directive_b (implab);
				
					FPrintF (OutFile, "e_%s_" N_PREFIX "%s.%s",sdef->sdef_module,record_name,name);
				
					if (sdef->sdef_calledwithrootnode)
						FPrintF (OutFile, " e_%s_" EA_PREFIX "%s.%s",sdef->sdef_module,record_name,name);
					else if (sdef->sdef_returnsnode)
						FPutS (" _",OutFile);
				}
				return;
			case RECORDTYPE:
				if (sdef->sdef_mark & (SDEF_USED_STRICTLY_MASK | SDEF_USED_LAZILY_MASK)){
					GenImpRecordDesc (sdef->sdef_module,name);
					
					sdef->sdef_mark |= SDEF_RECORD_R_LABEL_IMPORTED_MASK;
				}
				
				if (!sdef->sdef_strict_constructor)
					return;

				if (sdef->sdef_mark & SDEF_USED_LAZILY_MASK){
					put_directive_b (impdesc);
					FPrintF (OutFile, "e_%s_" RECORD_D_PREFIX "%s", sdef->sdef_module,name);
					put_directive_b (implab);
					FPrintF (OutFile, "e_%s_" RECORD_N_PREFIX "%s",sdef->sdef_module,name);
				}
				return;
			case CONSTRUCTOR:
				if ((sdef->sdef_mark & (SDEF_USED_STRICTLY_MASK | SDEF_USED_LAZILY_MASK | SDEF_USED_CURRIED_MASK))==0)
					return;
				
				if (!sdef->sdef_strict_constructor){
					put_directive_b (impdesc);
					FPrintF (OutFile, "e_%s_" D_PREFIX "%s", sdef->sdef_module,name);
					return;
				}

				if (sdef->sdef_mark & (SDEF_USED_STRICTLY_MASK | SDEF_USED_LAZILY_MASK)){
					put_directive_b (impdesc);
					FPrintF (OutFile, "e_%s_" CONSTRUCTOR_R_PREFIX "%s",sdef->sdef_module,name);
				}

				if (sdef->sdef_mark & (SDEF_USED_LAZILY_MASK | SDEF_USED_CURRIED_MASK)){
					put_directive_b (impdesc);
					FPrintF (OutFile, "e_%s_" D_PREFIX "%s", sdef->sdef_module,name);
				}
				break;
			default:
				return;
		}

		if (sdef->sdef_mark & SDEF_USED_LAZILY_MASK){
			put_directive_b (implab);
			FPrintF (OutFile, "e_%s_" N_PREFIX "%s",sdef->sdef_module,name);
			if ((sdef->sdef_calledwithrootnode || sdef->sdef_returnsnode) && 
				!(sdef->sdef_kind==CONSTRUCTOR && !sdef->sdef_strict_constructor))
			{
				if (sdef->sdef_calledwithrootnode)
					FPrintF (OutFile, " e_%s_" EA_PREFIX "%s",sdef->sdef_module,name);
				else
					FPutS (" _",OutFile);
			}
		}
	}	
}

void GenExportStrictAndEaEntry (SymbDef sdef)
{
	char *name;
	
	name = sdef->sdef_ident->ident_name;
	
	put_directive_ (Dexport);
	FPrintF (OutFile, "e_%s_" S_PREFIX "%s", CurrentModule,name);

	if (sdef->sdef_calledwithrootnode){
		put_directive_ (Dexport);
		FPrintF (OutFile, "e_%s_%s%s", CurrentModule, ea_pref, name);
	}
}

void GenExportFieldSelector (SymbDef sdef)
{
	char *name;
	char *record_name;
	
	name = sdef->sdef_ident->ident_name;
	
	record_name=sdef->sdef_type->type_lhs->ft_symbol->symb_def->sdef_ident->ident_name;

	put_directive_ (Dexport);
	FPrintF (OutFile,"e_%s_" D_PREFIX "%s.%s",CurrentModule,record_name,name);
	put_directive_ (Dexport);
	FPrintF (OutFile,"e_%s_" N_PREFIX "%s.%s",CurrentModule,record_name,name);

	if (sdef->sdef_calledwithrootnode){
		put_directive_ (Dexport);
		FPrintF (OutFile,"e_%s_" EA_PREFIX "%s.%s",CurrentModule,record_name,name);
	}
}

void GenExportEaEntry (SymbDef sdef)
{
	if (sdef->sdef_calledwithrootnode){
		put_directive_ (Dexport);
		FPrintF (OutFile,"e_%s_" EA_PREFIX "%s",CurrentModule,sdef->sdef_ident->ident_name);
	}
}

void GenExportEuEntry (SymbDef sdef)
{
	if (sdef->sdef_calledwithrootnode){
		put_directive_ (Dexport);
		FPrintF (OutFile,"e_%s_" EU_PREFIX "%s",CurrentModule,sdef->sdef_ident->ident_name);
	}
}

void GenDAStackLayout (int asize)
{
	if (DoStackLayout){
		put_directive_b (d);
		put_arguments_nn_b (asize,0);
	}
}

void GenOAStackLayout (int asize)
{
	if (DoStackLayout){
		put_directive_b (o);
		put_arguments_nn_b (asize,0);
	}
}

void GenDStackLayoutOfStates (int asize,int bsize,int n_states,StateP state_p)
{
	if (DoStackLayout){
		put_directive_b (d);
		if (bsize > 0){
			int i;
	
			put_arguments_nn__b (asize,bsize);

			for (i=0; i<n_states; ++i)
				GenBStackElems (state_p[i]);
		} else
			put_arguments_nn_b (asize,0);
	}
}

void GenOStackLayoutOfStates (int asize,int bsize,int n_states,StateP state_p)
{
	if (DoStackLayout){
		put_directive_b (o);
		if (bsize > 0){
			int i;
			
			put_arguments_nn__b (asize,bsize);

			for (i=0; i<n_states; ++i)
				GenBStackElems (state_p[i]);
		} else
			put_arguments_nn_b (asize,0);
	}
}

void GenDStackLayoutOfState (int asize, int bsize, StateS resultstate)
{
	if (DoStackLayout){
		put_directive_b (d);
		if (bsize > 0){
			put_arguments_nn__b (asize,bsize);
			GenBStackElems (resultstate);
		} else
			put_arguments_nn_b (asize,0);
	}		
}

void GenOStackLayoutOfState (int asize, int bsize, StateS resultstate)
{
	if (DoStackLayout){
		put_directive_b (o);
		if (bsize > 0){
			put_arguments_nn__b (asize,bsize);
			GenBStackElems (resultstate);
		} else
			put_arguments_nn_b (asize,0);
	}		
}

void GenJmpEvalUpdate (void)
{
	put_instruction (Ijmp_eval_upd);
}

void GenNodeEntryDirective (int arity,Label label,Label label2)
{
	if (DoStackLayout){
		put_directive_b (n);
		put_arguments_n__b (arity);

		if (DescriptorNeeded (label->lab_symbol))
			GenDescriptorOrNodeEntryLabel (label);
		else
			FPutS (empty_lab.lab_name, OutFile);

		if (label2){
			FPutC (' ', OutFile);
			GenLabel (label2);
		}
#ifdef MEMORY_PROFILING_WITH_N_STRING
		if (DoProfiling && arity>=0 && !DoParallel){
			put_directive_ (Dn_string);
			FPrintF (OutFile,"\"%s\"",label->lab_symbol->sdef_ident->ident_name);
		}
#endif
	}
}

void GenApplyEntryDirective (int arity,Label label)
{
	put_directive_b (a);
	put_arguments_n__b (arity);
	GenLabel (label);
}

void GenApplyInstanceEntryDirective (int arity,Label label,Label label2)
{
	put_directive_b (ai);
	put_arguments_n__b (arity);
	if (label==NULL)
		FPutS (empty_lab.lab_name, OutFile);
	else
		GenLabel (label);
	FPutC (' ', OutFile);
	GenLabel (label2);
}

void GenLazyRecordNodeEntryDirective (int arity,Label label,Label label2)
{
	if (DoStackLayout){
		put_directive_b (n);
		put_arguments_n__b (arity);

		if (DescriptorNeeded (label->lab_symbol))
			GenLabel (label);
		else
			FPutS (empty_lab.lab_name, OutFile);

		if (label2){
			FPutC (' ', OutFile);
			GenLabel (label2);
		}

#ifdef MEMORY_PROFILING_WITH_N_STRING
		if (DoProfiling && arity>=0 && !DoParallel){
			put_directive_ (Dn_string);
			FPrintF (OutFile,"\"%s\"",label->lab_symbol->sdef_ident->ident_name);
		}
#endif
	}
}

void GenNodeEntryDirectiveForLabelWithoutSymbol (int arity,Label label,Label label2)
{
	if (DoStackLayout){
		put_directive_b (n);
		put_arguments_n__b (arity);

		GenLabel (label);

		if (label2){
			FPutC (' ', OutFile);
			GenLabel (label2);
		}

#ifdef MEMORY_PROFILING_WITH_N_STRING
		if (DoProfiling && arity>=0 && !DoParallel){
			put_directive_ (Dn_string);
			FPrintF (OutFile,"\"%s\"",label->lab_name);
		}
#endif
	}
}

void GenNodeEntryDirectiveUnboxed (int a_size,int b_size,Label label,Label label2)
{
	if (DoStackLayout){
		put_directive_ (Dnu);
		FPrintF (OutFile,"%d %d ",a_size,b_size);

		if (DescriptorNeeded (label->lab_symbol))
			GenDescriptorOrNodeEntryLabel (label);
		else
			FPutS (empty_lab.lab_name, OutFile);

		if (label2){
			FPutC (' ', OutFile);
			GenLabel (label2);
		}

# ifdef MEMORY_PROFILING_WITH_N_STRING
		if (DoProfiling && !DoParallel){
			put_directive_ (Dn_string);
			FPrintF (OutFile,"\"%s\"",label->lab_symbol->sdef_ident->ident_name);
		}
# endif
	}
}

void GenFieldNodeEntryDirective (int arity,Label label,Label label2,char *record_name)
{
	if (DoStackLayout){
		put_directive_b (n);
		put_arguments_n__b (arity);
		
		if (DescriptorNeeded (label->lab_symbol))
			GenFieldLabel (label,record_name);
		else
			FPutS (empty_lab.lab_name, OutFile);
		
		if (label2!=NULL){
			FPutC (' ', OutFile);
			if (label2==&empty_lab)
				FPutS (empty_lab.lab_name, OutFile);				
			else
				GenFieldLabel (label2,record_name);
		}
	}
}

void GenConstructorDescriptorAndExport (SymbDef sdef)
{
	char *name;
	LabDef *add_argument_label;

	name = sdef->sdef_ident->ident_name;

	if (sdef->sdef_arity>0)
		add_argument_label=&add_arg_lab;
	else
		add_argument_label=&hnf_lab;

	if (sdef->sdef_exported || ExportLocalLabels){
		put_directive_ (Dexport);
		FPrintF (OutFile, "e_%s_" D_PREFIX "%s",CurrentModule,name);
		put_directive_ (Ddesc);
		FPrintF (OutFile, "e_%s_" D_PREFIX "%s %s %s %d 0 \"%s\"",
			CurrentModule, name, hnf_lab.lab_name, add_argument_label->lab_name,
			sdef->sdef_arity, name);
	} else if (DoDebug){
		put_directive_ (Ddesc);
		FPrintF (OutFile, D_PREFIX "%s %s %s %d 0 \"%s\"",
			name,hnf_lab.lab_name, add_argument_label->lab_name, sdef->sdef_arity, name);
	} else {
		put_directive_ (Ddesc);
		FPrintF (OutFile, LOCAL_D_PREFIX "%u %s %s %d 0 \"%s\"",
			sdef->sdef_number, hnf_lab.lab_name, add_argument_label->lab_name,
			sdef->sdef_arity, name);
	}
}

void GenConstructor0DescriptorAndExport (SymbDef sdef,int constructor_n)
{
	char *name;

	name = sdef->sdef_ident->ident_name;

	if (sdef->sdef_exported || ExportLocalLabels){
		put_directive_ (Dexport);
		FPrintF (OutFile, "e_%s_" D_PREFIX "%s",CurrentModule,name);
		put_directive_ (Ddesc0);
		FPrintF (OutFile, "e_%s_" D_PREFIX "%s %d \"%s\"",CurrentModule, name, constructor_n , name);
	} else if (DoDebug){
		put_directive_ (Ddesc0);
		FPrintF (OutFile, D_PREFIX "%s %d \"%s\"",name, constructor_n, name);
	} else {
		put_directive_ (Ddesc0);
		FPrintF (OutFile, LOCAL_D_PREFIX "%u %d \"%s\"",sdef->sdef_number, constructor_n, name);
	}
}

void GenRecordDescriptor (SymbDef sdef)
{
	int asize,bsize,has_unboxed_record;
	char *name;
	StateS recstate;
	
	recstate = sdef->sdef_record_state;

	asize=0;
	bsize=0;
	if (recstate.state_type==RecordState)
		has_unboxed_record = AddSizeOfStatesAndImportRecords (recstate.state_arity,recstate.state_record_arguments,&asize,&bsize);
	 else
		has_unboxed_record = AddSizeOfStateAndImportRecords (recstate,&asize,&bsize);

	name = sdef->sdef_ident->ident_name;

	if (sdef->sdef_exported || ExportLocalLabels){
		put_directive_ (Dexport);
		FPrintF (OutFile, "e_%s_" R_PREFIX "%s",CurrentModule,name);
		put_directive_ (Drecord);
		FPrintF (OutFile, "e_%s_" R_PREFIX "%s ",CurrentModule,name);
	} else if (DoDebug){
		put_directive_ (Drecord);
		FPrintF (OutFile, R_PREFIX "%s ",name);
	} else {
		put_directive_ (Drecord);
		FPrintF (OutFile, R_PREFIX "%u ",sdef->sdef_number);
	}

	GenABStackElemsOfRecord (recstate);
	
	if (!has_unboxed_record)
		FPrintF (OutFile, " %d %d \"%s\"",asize,bsize,name);
	else {
		FPrintF (OutFile, " %d %d ",asize,bsize);
		GenUnboxedRecordLabelsReversedForRecord (recstate); 
		FPrintF (OutFile, "\"%s\"",name);	
	}
}

#ifdef STRICT_LISTS
void GenUnboxedConsRecordDescriptor (SymbDef sdef,int tail_strict)
{
	int asize,bsize,has_unboxed_record;
	char *name,*unboxed_record_cons_prefix;
	StateS tuple_arguments_state[2];

	tuple_arguments_state[0] = sdef->sdef_record_state;
	tuple_arguments_state[1] = LazyState;

	DetermineSizeOfState (tuple_arguments_state[1],&asize,&bsize);
	if (tuple_arguments_state[0].state_type==RecordState)
		has_unboxed_record = AddSizeOfStatesAndImportRecords (tuple_arguments_state[0].state_arity,tuple_arguments_state[0].state_record_arguments,&asize,&bsize);
	else
		has_unboxed_record = AddSizeOfStateAndImportRecords (tuple_arguments_state[0],&asize,&bsize);

	name = sdef->sdef_ident->ident_name;
	
	unboxed_record_cons_prefix=tail_strict ? "r_Cons#!" : "r_Cons#";
	
	if (ExportLocalLabels){
		put_directive_ (Dexport);
		FPrintF (OutFile, "e_%s_%s%s",CurrentModule,unboxed_record_cons_prefix,name);
		put_directive_ (Drecord);
		FPrintF (OutFile, "e_%s_%s%s ",CurrentModule,unboxed_record_cons_prefix,name);
	} else {
		put_directive_ (Drecord);
		FPrintF (OutFile, "%s%s ",unboxed_record_cons_prefix,name);
	}

	FPutC ('l', OutFile);
	FPutC ('R', OutFile);

	GenABStackElemsOfRecord (tuple_arguments_state[0]);
	GenABStackElems (tuple_arguments_state[1]);
	
	FPrintF (OutFile," %d %d ",asize,bsize);
	if (has_unboxed_record)
		GenUnboxedRecordLabelsReversedForRecord (tuple_arguments_state[0]);
	
	if (!sdef->sdef_exported && sdef->sdef_module==CurrentModule && !ExportLocalLabels){
		if (DoDebug)
			FPrintF (OutFile, R_PREFIX "%s ",name);
		else
			FPrintF (OutFile, R_PREFIX "%u ",sdef->sdef_number);
	} else
		FPrintF (OutFile, "e_%s_" R_PREFIX "%s ",sdef->sdef_module,name);

	if (ExportLocalLabels)
		FPrintF (OutFile,tail_strict ? "\"_Cons#!%s\"" : "\"_Cons#%s\"",name);
	else
		FPrintF (OutFile,tail_strict ? "\"[#%s!]\"" : "\"[#%s]\"",name);
}
#endif

void GenStrictConstructorDescriptor (SymbDef sdef,StateP constructor_arg_states)
{
	int asize,bsize,state_arity,arg_n,has_unboxed_record;
	StateP constructor_arg_state_p;
	char *name;

	state_arity=sdef->sdef_arity;

	asize = 0;
	bsize = 0;
	has_unboxed_record = 0;
	for (arg_n=0,constructor_arg_state_p=constructor_arg_states; arg_n<state_arity; ++arg_n,++constructor_arg_state_p)
		has_unboxed_record |= AddSizeOfStateAndImportRecords (*constructor_arg_state_p,&asize,&bsize);

	name = sdef->sdef_ident->ident_name;
	
	if (sdef->sdef_exported || ExportLocalLabels){
		put_directive_ (Dexport);
		FPrintF (OutFile, "e_%s_" CONSTRUCTOR_R_PREFIX "%s",CurrentModule,name);
		put_directive_ (Drecord);
		FPrintF (OutFile, "e_%s_" CONSTRUCTOR_R_PREFIX "%s ",CurrentModule,name);
	} else if (DoDebug){
		put_directive_ (Drecord);
		FPrintF (OutFile, CONSTRUCTOR_R_PREFIX "%s ",name);
	} else {
		put_directive_ (Drecord);
		FPrintF (OutFile, CONSTRUCTOR_R_PREFIX "%u ",sdef->sdef_number);
	}

	FPutC ('d', OutFile);

	for (arg_n=0,constructor_arg_state_p=constructor_arg_states; arg_n<state_arity; ++arg_n,++constructor_arg_state_p)
		 GenABStackElemsForRecordDesc (*constructor_arg_state_p);
	
	if (!has_unboxed_record)
		FPrintF (OutFile, " %d %d \"%s\"", asize, bsize, name);
	else {
		FPrintF (OutFile, " %d %d ", asize, bsize);
		for (arg_n=state_arity-1; arg_n>=0; --arg_n)
			GenUnboxedRecordLabelsReversed (constructor_arg_states[arg_n]);
		FPrintF (OutFile, "\"%s\"", name);	
	}
}

void GenArrayFunctionDescriptor (SymbDef arr_fun_def, Label desclab, int arity)
{
	LabDef descriptor_label;
	char *name;
	
	name = arr_fun_def->sdef_ident->ident_name;
	
	if (ExportLocalLabels){
		put_directive_ (Dexport);
		FPrintF (OutFile,"e_%s_" D_PREFIX "%s",CurrentModule,name);
		if (arr_fun_def->sdef_mark & SDEF_USED_LAZILY_MASK){
			put_directive_ (Dexport);
			FPrintF (OutFile,"e_%s_" N_PREFIX "%s",CurrentModule,name);		
		}
	}

	descriptor_label=*desclab;
	descriptor_label.lab_pref=d_pref;
	
	if (arr_fun_def->sdef_mark & SDEF_USED_CURRIED_MASK)
		put_directive_ (Ddesc);
	else
		put_directive_ (Ddescn);
	
	if (ExportLocalLabels)
		FPrintF (OutFile,"e_%s_" D_PREFIX "%s ",CurrentModule,name);
	else
		GenLabel (&descriptor_label);

	FPutC (' ', OutFile);
	GenLabel (&empty_lab);
	FPutC (' ', OutFile);

	if (arr_fun_def->sdef_mark & SDEF_USED_CURRIED_MASK){
		LabDef lazylab;
		
		lazylab = *desclab;
		lazylab.lab_pref = l_pref;
		GenLabel (&lazylab);
	}

	FPrintF (OutFile, " %d 0 \"%s\"", arity, name); 
}

void GenFunctionDescriptorAndExportNodeAndDescriptor (SymbDef sdef)
{
	Ident name_id;
	char *name;

	if (!DescriptorNeeded (sdef))
		return;

	name_id = sdef->sdef_ident;
	name = name_id->ident_name;
	
	if (sdef->sdef_exported){
		put_directive_ (Ddescexp);
		FPrintF (OutFile, "e_%s_" D_PREFIX "%s e_%s_" N_PREFIX "%s e_%s_" L_PREFIX "%s ",
					CurrentModule,name,CurrentModule,name,CurrentModule,name);
	} else {
		if (sdef->sdef_mark & SDEF_USED_CURRIED_MASK){
			int sdef_n;
						
			sdef_n=sdef->sdef_number;

			if (ExportLocalLabels){
				put_directive_ (Dexport);
				FPrintF (OutFile,"e_%s_" D_PREFIX "%s.%u",CurrentModule,name,sdef_n);

				if (sdef->sdef_mark & SDEF_USED_LAZILY_MASK){
					put_directive_ (Dexport);
					FPrintF (OutFile,"e_%s_" N_PREFIX "%s.%u",CurrentModule,name,sdef_n);
				}

				put_directive_ (Ddesc);
				FPrintF (OutFile,"e_%s_" D_PREFIX "%s.%u ",CurrentModule,name,sdef_n);
			} else {
				put_directive_ (Ddesc);
				if (DoDebug)
					FPrintF (OutFile,D_PREFIX "%s.%u ",name,sdef_n);
				else
					FPrintF (OutFile,LOCAL_D_PREFIX "%u ",sdef_n);
			}
			
			if (sdef->sdef_mark & SDEF_USED_LAZILY_MASK){
				if (ExportLocalLabels)
					FPrintF (OutFile,"e_%s_" N_PREFIX "%s.%u ",CurrentModule,name,sdef_n);
				else if (DoDebug)
					FPrintF (OutFile,N_PREFIX "%s.%u ",name,sdef_n);
				else
					FPrintF (OutFile,N_PREFIX "%u ",sdef_n);
			} else
				FPrintF (OutFile, "%s ", hnf_lab.lab_name);
			
			if (DoDebug)
				FPrintF (OutFile,L_PREFIX "%s.%u ",name,sdef_n);
			else
				FPrintF (OutFile,L_PREFIX "%u ",sdef_n);
		} else {
			int sdef_n;

			sdef_n=sdef->sdef_number;

			if (ExportLocalLabels){
				put_directive_ (Dexport);
				FPrintF (OutFile,"e_%s_" D_PREFIX "%s.%u",CurrentModule,name,sdef_n);

				if (sdef->sdef_mark & SDEF_USED_LAZILY_MASK){
					put_directive_ (Dexport);
					FPrintF (OutFile,"e_%s_" N_PREFIX "%s.%u",CurrentModule,name,sdef_n);
				}

				put_directive_ (Ddescn);
				FPrintF (OutFile,"e_%s_" D_PREFIX "%s.%u ",CurrentModule,name,sdef_n);
			} else {
				put_directive_ (Ddescn);
				if (DoDebug)
					FPrintF (OutFile,D_PREFIX "%s.%u ",name,sdef_n);
				else
					FPrintF (OutFile,LOCAL_D_PREFIX "%u ",sdef_n);
			}
			
			if (sdef->sdef_mark & SDEF_USED_LAZILY_MASK){
				if (ExportLocalLabels)
					FPrintF (OutFile,"e_%s_" N_PREFIX "%s.%u ",CurrentModule,name,sdef_n);
				else if (DoDebug)
					FPrintF (OutFile,N_PREFIX "%s.%u ",name,sdef_n);
				else
					FPrintF (OutFile,N_PREFIX "%u ",sdef_n);
			} else
				FPrintF (OutFile, "%s ", hnf_lab.lab_name);				
		}
	}
	
	FPrintF (OutFile, "%d 0 \"", sdef->sdef_arity);
	if (ExportLocalLabels){
		if (sdef->sdef_exported)
			FPrintF (OutFile,"%s",name);
		else
			FPrintF (OutFile,"%s.%u",name,sdef->sdef_number);
	} else
		PrintSymbolOfIdent (name_id, 0, OutFile);
	FPutC ('\"',OutFile);
}

void GenConstructorFunctionDescriptorAndExportNodeAndDescriptor (SymbDef sdef)
{
	Ident name_id;
	char *name;

	if (!DescriptorNeeded (sdef))
		return;

	name_id = sdef->sdef_ident;
	name = name_id->ident_name;
	
	if (sdef->sdef_exported){
		put_directive_ (Ddescexp);
		FPrintF (OutFile, "e_%s_" D_PREFIX "%s e_%s_" N_PREFIX "%s e_%s_" L_PREFIX "%s ",
					CurrentModule,name,CurrentModule,name,CurrentModule,name);
	} else if (ExportLocalLabels && (sdef->sdef_mark & SDEF_USED_CURRIED_MASK)!=0){
		put_directive_ (Ddescexp);
		if (DoDebug)
			FPrintF (OutFile, "e_%s_" D_PREFIX "%s e_%s_" N_PREFIX "%s " L_PREFIX "%s ",
						CurrentModule,name,CurrentModule,name,name);
		else
			FPrintF (OutFile, "e_%s_" D_PREFIX "%s e_%s_" N_PREFIX "%s " L_PREFIX "%u ",
						CurrentModule,name,CurrentModule,name,sdef->sdef_number);
	} else {
		if (sdef->sdef_mark & SDEF_USED_CURRIED_MASK){
			put_directive_ (Ddesc);
			
			if (DoDebug)
				FPrintF (OutFile, D_PREFIX "%s ",name);
			else
				FPrintF (OutFile, LOCAL_D_PREFIX "%u ", sdef->sdef_number);
			
			if (sdef->sdef_mark & SDEF_USED_LAZILY_MASK){
				if (DoDebug)
					FPrintF (OutFile,N_PREFIX "%s ",name);
				else
					FPrintF (OutFile,N_PREFIX "%u ",sdef->sdef_number);
			} else
				FPrintF (OutFile, "%s ", hnf_lab.lab_name);
			
			if (DoDebug)
				FPrintF (OutFile,L_PREFIX "%s ",name);
			else
				FPrintF (OutFile,L_PREFIX "%u ",sdef->sdef_number);
		} else {
			if (ExportLocalLabels){
				put_directive_ (Dexport);
				FPrintF (OutFile,"e_%s_" D_PREFIX "%s",CurrentModule,name);
				put_directive_ (Dexport);
				FPrintF (OutFile,"e_%s_" N_PREFIX "%s",CurrentModule,name);
			}

			put_directive_ (Ddescn);

			if (ExportLocalLabels)
				FPrintF (OutFile,"e_%s_" D_PREFIX "%s ",CurrentModule,name);
			else {
				if (DoDebug)
					FPrintF (OutFile, D_PREFIX "%s ", name);
				else
					FPrintF (OutFile, LOCAL_D_PREFIX "%u ", sdef->sdef_number);
			}

			if (ExportLocalLabels)
				FPrintF (OutFile,"e_%s_" N_PREFIX "%s ",CurrentModule,name);
			else if (sdef->sdef_mark & SDEF_USED_LAZILY_MASK){
				if (DoDebug)
					FPrintF (OutFile,N_PREFIX "%s ", name);
				else
					FPrintF (OutFile,N_PREFIX "%u ",sdef->sdef_number);
			} else
				FPrintF (OutFile, "%s ", hnf_lab.lab_name);				
		}
	}
	
	FPrintF (OutFile, "%d 0 \"", sdef->sdef_arity);
	PrintSymbolOfIdent (name_id, 0, OutFile);
	FPutC ('\"',OutFile);
}

#if OPTIMIZE_LAZY_TUPLE_RECURSION
void GenFunctionDescriptorForLazyTupleRecursion (SymbDef sdef,int tuple_result_arity)
{
	Ident name_id;
	char *name;

	name_id = sdef->sdef_ident;
	name = name_id->ident_name;
	
	put_directive_ (Ddescn);

	if (sdef->sdef_exported){
		FPrintF (OutFile,"e_%s_" D_PREFIX "%s.2 ",CurrentModule,name);
		FPrintF (OutFile,"e_%s_" N_PREFIX "%s.2 ",CurrentModule,name);
	} else if (DoDebug){
		FPrintF (OutFile,D_PREFIX "%s.%u.2 ",name,sdef->sdef_number);
		FPrintF (OutFile,N_PREFIX "%s.%u.2 ",name,sdef->sdef_number);
	} else {
		FPrintF (OutFile,LOCAL_D_PREFIX "%u.2 ",sdef->sdef_number);
		FPrintF (OutFile,N_PREFIX "%u.2 ",sdef->sdef_number);
	}
	
	FPrintF (OutFile, "%d 0 \"",sdef->sdef_arity+tuple_result_arity);
	PrintSymbolOfIdent (name_id,0,OutFile);
	FPutC ('\"',OutFile);

# if 1
	put_directive_ (Ddescn);

	if (sdef->sdef_exported){
		FPrintF (OutFile,"e_%s_" D_PREFIX "%s.3 ",CurrentModule,name);
		FPrintF (OutFile,"e_%s_" N_PREFIX "%s.3 ",CurrentModule,name);
	} else if (DoDebug){
		FPrintF (OutFile,D_PREFIX "%s.%u.3 ",name,sdef->sdef_number);
		FPrintF (OutFile,N_PREFIX "%s.%u.3 ",name,sdef->sdef_number);
	} else {
		FPrintF (OutFile,LOCAL_D_PREFIX "%u.3 ",sdef->sdef_number);
		FPrintF (OutFile,N_PREFIX "%u.3 ",sdef->sdef_number);
	}
	
	FPrintF (OutFile, "%d 0 \"",sdef->sdef_arity+tuple_result_arity);
	PrintSymbolOfIdent (name_id,0,OutFile);
	FPutC ('\"',OutFile);
# endif
}
#endif

void GenLazyRecordDescriptorAndExport (SymbDef sdef)
{
	char *name;
	int arity;

	if (!DescriptorNeeded (sdef))
		return;

	name = sdef->sdef_ident->ident_name;
	arity = sdef->sdef_arity;
	
	if (sdef->sdef_exported){
		put_directive_ (Ddescexp);
		FPrintF (OutFile, "e_%s_" RECORD_D_PREFIX "%s e_%s_" RECORD_N_PREFIX "%s _hnf %d 1 \"%s\"",
							CurrentModule,name,CurrentModule,name,arity,name);
	} else {
		if (ExportLocalLabels){
			put_directive_ (Dexport);
			FPrintF (OutFile,"e_%s_" RECORD_D_PREFIX "%s",CurrentModule,name);
			put_directive_ (Dexport);
			FPrintF (OutFile,"e_%s_" RECORD_N_PREFIX "%s",CurrentModule,name);
		}

		put_directive_ (Ddescn);
		if (DoDebug){
			if (ExportLocalLabels){
				FPrintF (OutFile,"e_%s_" RECORD_D_PREFIX "%s ",CurrentModule,name);
				FPrintF (OutFile,"e_%s_" RECORD_N_PREFIX "%s ",CurrentModule,name);
			} else
				FPrintF (OutFile,RECORD_D_PREFIX "%s " RECORD_N_PREFIX "%s ",name,name);
		} else {
			if (ExportLocalLabels){
				FPrintF (OutFile,"e_%s_" RECORD_D_PREFIX "%s ",CurrentModule,name);
				FPrintF (OutFile,"e_%s_" RECORD_N_PREFIX "%s ",CurrentModule,name);
			} else
				FPrintF (OutFile,RECORD_D_PREFIX "%u " RECORD_N_PREFIX "%u ",sdef->sdef_number,sdef->sdef_number);
		}
		
		FPrintF (OutFile, "%d 1 \"%s\"",arity,name);
	}
}

#ifdef NEW_SELECTOR_DESCRIPTORS

static void print_result_descriptor_and_offsets (StateS field_state,int a_pos,int b_pos,int record_a_size,int record_b_size)
{
	if (field_state.state_kind!=OnB)
		FPrintF (OutFile, "_ %d 0 ",(a_pos<=1 && !(a_pos==1 && record_a_size+record_b_size>2)) ? a_pos+1 : a_pos+2);
	else {
		char *result_descriptor_name;
		int offset1,offset2;
		
		result_descriptor_name=BasicDescriptors[field_state.state_object].lab_name;

		offset1=record_a_size+b_pos;
		offset1=(offset1<=1 && !(offset1==1 && record_a_size+record_b_size>2)) ? offset1+1 : offset1+2;

		if (ObjectSizes[field_state.state_object]>1){
			offset2=record_a_size+b_pos+1;
			offset2=(offset2==1 && record_a_size+record_b_size<=2) ? offset2+1 : offset2+2;
		} else
			offset2=0;
		
		if (field_state.state_object==FileObj){
			/* the code generator stores the fields in a FILE node in reversed order */
			int old_offset1;

			old_offset1=offset1;
			offset1=offset2;
			offset2=old_offset1;
		}		
		FPrintF (OutFile, "%s %d %d ",result_descriptor_name,offset1,offset2);
	}
}

void GenFieldSelectorDescriptor (SymbDef sdef,StateS field_state,int a_pos,int b_pos,int record_a_size,int record_b_size)
{
	char *name,*record_name;
	int gc_updates_selector;

	if (!DescriptorNeeded (sdef))
		return;

	gc_updates_selector=IsSimpleState (field_state);

	name = sdef->sdef_ident->ident_name;
	record_name=sdef->sdef_type->type_lhs->ft_symbol->symb_def->sdef_ident->ident_name;

	put_directive_ (gc_updates_selector ? Ddescs : Ddesc);
	if (sdef->sdef_exported){
		FPrintF (OutFile, "e_%s_" D_PREFIX "%s.%s e_%s_" N_PREFIX "%s.%s ",
			CurrentModule,record_name,name,
			CurrentModule,record_name,name);
		if (gc_updates_selector)
			print_result_descriptor_and_offsets (field_state,a_pos,b_pos,record_a_size,record_b_size);
		else
			FPrintF (OutFile, "_hnf 1 0 ");
	} else if ((sdef->sdef_mark & SDEF_USED_LAZILY_MASK) || gc_updates_selector){
		if (ExportLocalLabels)
			FPrintF (OutFile, "e_%s_" D_PREFIX "%s.%s ",CurrentModule,record_name,name);		
		else if (DoDebug)
			FPrintF (OutFile, D_PREFIX "%s.%s ",record_name,name);				
		else
			FPrintF (OutFile, LOCAL_D_PREFIX "%u ", sdef->sdef_number);

		if (sdef->sdef_mark & SDEF_USED_LAZILY_MASK){
			if (ExportLocalLabels)
				FPrintF (OutFile, "e_%s_" N_PREFIX "%s.%s ",CurrentModule,record_name,name);
			else if (DoDebug)
				FPrintF (OutFile, N_PREFIX "%s.%s ",record_name,name);
			else
				FPrintF (OutFile, N_PREFIX "%u ",sdef->sdef_number);
		} else
			FPrintF (OutFile, "%s ",hnf_lab.lab_name);

		if (gc_updates_selector)
			print_result_descriptor_and_offsets (field_state,a_pos,b_pos,record_a_size,record_b_size);
		else
			FPrintF (OutFile, "%s 1 0 ",hnf_lab.lab_name);
	} else if (DoDebug){
		FPrintF (OutFile, D_PREFIX "%s %s %s 1 0 ", name, hnf_lab.lab_name,hnf_lab.lab_name);
	} else
		FPrintF (OutFile, LOCAL_D_PREFIX "%u %s %s 1 0 ", sdef->sdef_number,hnf_lab.lab_name, hnf_lab.lab_name);

	FPrintF (OutFile, "\"%s.%s\"",record_name,name);
}
#else
void GenFieldSelectorDescriptor (SymbDef sdef,int has_gc_apply_entry)
{
	char *name,*record_name;
	int arity;

	if (!DescriptorNeeded (sdef))
		return;

	name = sdef->sdef_ident->ident_name;
	arity = (sdef->sdef_kind == RECORDTYPE) ? sdef->sdef_cons_arity : sdef->sdef_arity;

	record_name=sdef->sdef_type->type_lhs->ft_symbol->symb_def->sdef_ident->ident_name;
	
	put_directive_ (Ddesc);
	if (sdef->sdef_exported){
		if (has_gc_apply_entry)
			FPrintF (OutFile, "e_%s_" D_PREFIX "%s.%s e_%s_" N_PREFIX "%s.%s e_%s_" L_PREFIX "%s.%s %d 0 \"%s.%s\"",
				CurrentModule,record_name,name,
				CurrentModule,record_name,name,
				CurrentModule,record_name,name,
				arity,record_name,name);	
		else
			FPrintF (OutFile, "e_%s_" D_PREFIX "%s.%s e_%s_" N_PREFIX "%s.%s _hnf %d 0 \"%s.%s\"",
				CurrentModule,record_name,name,
				CurrentModule,record_name,name,
				arity,record_name,name);
	} else if ((sdef->sdef_mark & SDEF_USED_LAZILY_MASK) || has_gc_apply_entry){
		if (ExportLocalLabels)
			FPrintF (OutFile, "e_%s_" D_PREFIX "%s.%s ",CurrentModule,record_name,name);		
		else if (DoDebug)
			FPrintF (OutFile, D_PREFIX "%s.%s ",record_name,name);				
		else
			FPrintF (OutFile, LOCAL_D_PREFIX "%u ", sdef->sdef_number);

		if (sdef->sdef_mark & SDEF_USED_LAZILY_MASK){
			if (ExportLocalLabels)
				FPrintF (OutFile, "e_%s_" N_PREFIX "%s.%s ",CurrentModule,record_name,name);
			else if (DoDebug)
				FPrintF (OutFile, N_PREFIX "%s.%s ",record_name,name);
			else
				FPrintF (OutFile, N_PREFIX "%u ",sdef->sdef_number);
		} else
			FPrintF (OutFile, "%s ",hnf_lab.lab_name);
		
		if (has_gc_apply_entry){
			if (ExportLocalLabels)
				FPrintF (OutFile, "e_%s_" L_PREFIX "%s.%s ",CurrentModule,record_name,name);
			else if (DoDebug)
				FPrintF (OutFile, L_PREFIX "%s.%s ",record_name,name);
			else
				FPrintF (OutFile, L_PREFIX "%u ",sdef->sdef_number);
		} else
			FPrintF (OutFile, "%s ",hnf_lab.lab_name);

		FPrintF (OutFile, "%d 0 \"%s.%s\"",arity,record_name,name);
	} else if (DoDebug){
		FPrintF (OutFile, D_PREFIX "%s %s %s %d 0 \"%s.%s\"", name, hnf_lab.lab_name,
			hnf_lab.lab_name,arity,record_name,name);
	} else
		FPrintF (OutFile, LOCAL_D_PREFIX "%u %s %s %d 0 \"%s.%s\"", sdef->sdef_number,
			hnf_lab.lab_name, hnf_lab.lab_name, arity,record_name,name);
}
#endif

void GenModuleDescriptor (
#if WRITE_DCL_MODIFICATION_TIME
						ModuleFileTime file_time
#else
						void
#endif
	)
{
	put_directive_ (Dmodule);
	FPrintF (OutFile, "m_%s \"%s\"", CurrentModule,CurrentModule);

#if WRITE_DCL_MODIFICATION_TIME
	if (WriteModificationTimes){
		FPutC (' ',OutFile);
		FPutC ('\"',OutFile);
# if CLEAN2
		FPutS (file_time,OutFile);
# else
		FWriteFileTime (file_time,OutFile);
# endif
		FPutC ('\"',OutFile);
	}
#endif
}

void GenDepend (char *modname
#if WRITE_DCL_MODIFICATION_TIME
				,ModuleFileTime file_time
#endif
				)
{
	put_directive_ (Ddepend);
	FPrintF (OutFile, "\"%s\"",modname);

#if WRITE_DCL_MODIFICATION_TIME
	if (WriteModificationTimes){
		FPutC (' ',OutFile);
		FPutC ('\"',OutFile);
# if CLEAN2
		FPutS (file_time,OutFile);
# else
		FWriteFileTime (file_time,OutFile);
# endif
		FPutC ('\"',OutFile);
	}
#endif
}

void GenStart (SymbDef startsymb)
{
	if (startsymb->sdef_module == CurrentModule){
		int arity;
		char *start_function_name;

		arity = startsymb->sdef_arity;
		startsymb->sdef_mark |= SDEF_USED_LAZILY_MASK;

		start_function_name=startsymb->sdef_ident->ident_name;
		
		put_directive_ (Dexport);
		FPrintF (OutFile, "__%s_%s",CurrentModule,start_function_name);
		GenOAStackLayout (0);

		FPrintF (OutFile, "\n__%s_%s", CurrentModule,start_function_name);

		if (arity!=0 || strcmp (start_function_name,"main")==0){
			put_instruction_b (buildI);
			put_arguments_n_b (65536l);
		}
		
		put_instruction_b (build);
		
		if (startsymb->sdef_exported)
			FPrintF (OutFile, "e_%s_" D_PREFIX "%s",CurrentModule,start_function_name);
		else if (ExportLocalLabels)
			if (DoParallel)
				FPrintF (OutFile,"e_%s_" D_PREFIX "%s.%u",CurrentModule,start_function_name,startsymb->sdef_number);
			else
				FPutS (empty_lab.lab_name, OutFile);
		else if (DoDebug){
			if (DoParallel)
				FPrintF (OutFile, D_PREFIX "%s.%u",start_function_name,startsymb->sdef_number);
			else
				FPutS (empty_lab.lab_name, OutFile);
		} else {
			if (DoParallel)
				FPrintF (OutFile, LOCAL_D_PREFIX "%u",startsymb->sdef_number);
			else
				FPutS (empty_lab.lab_name, OutFile);
		}

		put_arguments__n__b (arity);

		if (startsymb->sdef_exported)
			FPrintF (OutFile, "e_%s_" N_PREFIX "%s",CurrentModule,start_function_name);
		else if (ExportLocalLabels)
			FPrintF (OutFile, "e_%s_" N_PREFIX "%s.%u",CurrentModule,start_function_name,startsymb->sdef_number);
		else if (DoDebug)
			FPrintF (OutFile, N_PREFIX "%s.%u",start_function_name,startsymb->sdef_number);
		else
			FPrintF (OutFile, N_PREFIX "%u",startsymb->sdef_number);

		if (arity==0 && strcmp (start_function_name,"main")==0){
			GenJsrEval (0);
			GenJsrAp (1);
		}
		
		GenDAStackLayout (1);
		put_instruction_b (jmp);
		FPutS ("_driver", OutFile);
	}
}

#ifdef NEW_SELECTOR_DESCRIPTORS
void GenSelectorDescriptor (Label sellab,int element_n)
{
	if (sellab->lab_issymbol){
		char *name;
		
		name=sellab->lab_symbol->sdef_ident->ident_name;

		put_directive_ (Dexport);
		FPrintF (OutFile, "e_%s_" D_PREFIX "%s.%d",sellab->lab_mod, name, sellab->lab_post);
		put_directive_ (Dexport);
		FPrintF (OutFile, "e_%s_%s%s.%d",sellab->lab_mod, sellab->lab_pref, name, sellab->lab_post);

		put_directive_ (Ddescs);
		FPrintF (OutFile, "e_%s_" D_PREFIX "%s.%d e_%s_%s%s.%d _ %d 0 \"%s.%d\"",
				sellab->lab_mod, name, sellab->lab_post,
				sellab->lab_mod, sellab->lab_pref, name, sellab->lab_post,
				element_n+1,
				name, sellab->lab_post);
	} else {
		put_directive_ (Ddescs);
		FPrintF (OutFile, D_PREFIX "%s.%d %s%s.%d _ %d 0 \"%s.%d\"",
				sellab->lab_name, sellab->lab_post,
				sellab->lab_pref, sellab->lab_name, sellab->lab_post,
				element_n+1,
				sellab->lab_name, sellab->lab_post);
	}
}
#else
void GenSelectorDescriptor (Label sellab,char *g_pref)
{
	put_directive_ (Ddesc);
	FPrintF (OutFile, D_PREFIX "%s.%d %s%s.%d %s%s.%d 1 0 \"%s.%d\"",
			sellab->lab_name, sellab->lab_post,
			sellab->lab_pref, sellab->lab_name, sellab->lab_post,
			g_pref, sellab->lab_name, sellab->lab_post,
			sellab->lab_name, sellab->lab_post);
}
#endif

void InitFileInfo (ImpMod imod)
{
	char option_string[N_OPTIONS+1];
	SymbDef start_sdef;
	
	start_sdef=imod->im_start;

	ConvertOptionsToString (option_string);

	if (imod->im_def_module!=NULL && imod->im_def_module->dm_system_module)
		option_string[N_System]='1';

	put_first_directive_ (Dcomp);
	FPrintF (OutFile, "%d %s", VERSION,option_string);
	
	put_directive_ (Dstart);
	if (start_sdef!=NULL){
		FPrintF (OutFile, "__%s_%s",start_sdef->sdef_module,start_sdef->sdef_ident->ident_name);
	} else
		FPutS ("_nostart_", OutFile);
}

static int match_error_lab_used = 0;

void GenNoMatchError (SymbDef sdef,int asp,int bsp,int string_already_generated)
{
	Bool desc_needed;
	
	desc_needed = DescriptorNeeded (sdef);

	GenPopA (asp);
	GenPopB (bsp);
	
	put_instruction_b (pushD);
	FPrintF (OutFile, "m_%s", CurrentModule);

	put_instruction_b (pushD);	
	if (!desc_needed)
		FPrintF (OutFile, "x_%u", sdef->sdef_number);
	else if (sdef->sdef_exported)
		FPrintF (OutFile, "e_%s_" D_PREFIX "%s", CurrentModule, sdef->sdef_ident->ident_name);
	else if (ExportLocalLabels){
		if (sdef->sdef_kind==IMPRULE)
			FPrintF (OutFile,"e_%s_" D_PREFIX "%s.%u",CurrentModule,sdef->sdef_ident->ident_name,sdef->sdef_number);
		else
			FPrintF (OutFile,"e_%s_" D_PREFIX "%s",CurrentModule,sdef->sdef_ident->ident_name);
	} else if (DoDebug){
		if (sdef->sdef_kind==IMPRULE)
			FPrintF (OutFile, D_PREFIX "%s.%u", sdef->sdef_ident->ident_name,sdef->sdef_number);
		else
			FPrintF (OutFile, D_PREFIX "%s", sdef->sdef_ident->ident_name);
	} else
		FPrintF (OutFile, LOCAL_D_PREFIX "%u", sdef->sdef_number);
	
	if (DoStackLayout){
		put_directive_b (d);
		put_arguments_nn__b (0,2);
		FPutS ("ii",OutFile);
	}
	
	GenJmp (&match_error_lab);
	match_error_lab_used = 1;
	
	if (!desc_needed && !string_already_generated){
		put_directive_ (Dstring);
		FPrintF (OutFile, "x_%u \"",sdef->sdef_number);		
		PrintSymbolOfIdent (sdef->sdef_ident,0,OutFile);
		FPutS ("\"", OutFile);
	}
}

#if CLEAN2

void GenCaseNoMatchError (SymbDefP case_def,int asp,int bsp)
{

	GenPopA (asp);
	GenPopB (bsp);

	put_instruction_b (pushD);
	FPrintF (OutFile, "m_%s", CurrentModule);

	put_instruction_b (pushD);
	FPrintF (OutFile, "case_fail%u",CaseFailNumber);

	GenJmp (&match_error_lab);
	match_error_lab_used = 1;
	
	put_directive_ (Dstring);
	FPrintF (OutFile, "case_fail%u \"",CaseFailNumber);		
	PrintSymbolOfIdent (case_def->sdef_ident,0,OutFile);
	FPrintF (OutFile, "\"");		

	CaseFailNumber++;
}
#endif

static void GenImpLab (char *label_name)
{
	put_directive_b (implab);
	FPutS (label_name,OutFile);
}

static void GenImpLab_node_entry (char *label_name,char *ea_label_name)
{
	put_directive_b (implab);
	FPrintF (OutFile,"%s %s",label_name,ea_label_name);
}

static void GenImpLab_n_and_ea_label (char *label_name)
{
	put_directive_b (implab);
	FPrintF (OutFile,"n%s ea%s",label_name,label_name);
}

static void GenImpDesc (char *descriptor_name)
{
	put_directive_b (impdesc);
	FPutS (descriptor_name,OutFile);
}

void GenImpMod (char *module_name)
{
	put_directive_b (impmod);
	FPutS (module_name,OutFile);
}

void GenEndInfo (void)
{
	put_directive (Dendinfo);
}

void GenSystemImports (void)
{
	match_error_lab_used = 0;
	selector_m_error_lab_used = 0;

	if (DoStackLayout){
		 /* system module labels and descriptors */

		int selnum;

		GenImpMod ("_system");

		if (DoParallel){
			GenImpLab (channel_code);
			GenImpLab (hnf_reducer_code);
			GenImpDesc (ext_hnf_reducer_code);
			GenImpLab (nf_reducer_code);
			GenImpDesc (ext_nf_reducer_code);
			GenImpLab (reserve_lab.lab_name);
		}
		GenImpLab (cycle_lab.lab_name);
		GenImpLab (type_error_lab.lab_name);
		GenImpLab (hnf_lab.lab_name);

		GenImpDesc (ind_lab.lab_name);
		GenImpLab_node_entry (indirection_lab.lab_name,"e_system_eaind");
		GenImpDesc ("e_system_dif");
		GenImpLab_node_entry ("e_system_nif","e_system_eaif");
		GenImpLab ("e_system_sif");

		GenImpDesc ("e_system_dAP");
		GenImpLab_node_entry ("e_system_nAP","e_system_eaAP");
		GenImpLab ("e_system_sAP");

		GenImpDesc (BasicDescriptors [ArrayObj].lab_name);

		GenImpDesc (nil_lab.lab_name);
		GenImpDesc (cons_lab.lab_name);
#if STRICT_LISTS
		GenImpDesc (conss_lab.lab_name);
		GenImpLab_node_entry ("n_Conss","ea_Conss");
		GenImpDesc (consts_lab.lab_name);
		GenImpLab_node_entry ("n_Consts","ea_Consts");
		GenImpDesc (conssts_lab.lab_name);
		GenImpLab_node_entry ("n_Conssts","ea_Conssts");
#endif

		{
		int i;
		
		for (i=0; i<5; ++i){
			char *descriptor_label_name;

			if (unboxed_cons_mark[i][0]!=0){
				descriptor_label_name=unboxed_cons_labels[i][0].lab_name;
				GenImpDesc (descriptor_label_name);
				if (unboxed_cons_mark[i][0] & SDEF_USED_LAZILY_MASK)
					GenImpLab_n_and_ea_label (descriptor_label_name);
			}
			if (unboxed_cons_mark[i][1]!=0){
				descriptor_label_name=unboxed_cons_labels[i][1].lab_name;
				GenImpDesc (descriptor_label_name);
				if (unboxed_cons_mark[i][1] & SDEF_USED_LAZILY_MASK)
					GenImpLab_n_and_ea_label (descriptor_label_name);
			}
		}
		if (unboxed_cons_array_mark!=0){
			GenImpDesc (unboxed_cons_array_label.lab_name);
			if (unboxed_cons_array_mark & SDEF_USED_LAZILY_MASK)
				GenImpLab_n_and_ea_label (unboxed_cons_array_label.lab_name);
		}
		}		

		GenImpDesc (tuple_lab.lab_name);
		for (selnum=1; selnum<=NrOfGlobalSelectors; ++selnum){
			put_directive_b (impdesc);
			FPrintF (OutFile,D_PREFIX "%s.%d",glob_sel,selnum);
			put_directive_b (implab);
			FPrintF (OutFile,N_PREFIX "%s.%d " EA_PREFIX "%s.%d",glob_sel,selnum,glob_sel,selnum);
		}
#ifdef THUNK_LIFT_SELECTORS
		for (selnum=1; selnum<=NrOfGlobalSelectors; ++selnum){
			put_directive_b (impdesc);
			FPrintF (OutFile,D_PREFIX "%s.%d",glob_selr,selnum);
			put_directive_b (implab);
			FPrintF (OutFile,N_PREFIX "%s.%d " EA_PREFIX "%s.%d",glob_selr,selnum,glob_selr,selnum);
		}
#endif

		if (SeqDef!=NULL && (SeqDef->sdef_mark & (SDEF_USED_LAZILY_MASK | SDEF_USED_CURRIED_MASK))){
			GenImpDesc ("e_system_dseq");
			GenImpLab_node_entry ("e_system_nseq","e_system_easeq");	
		}

		GenImpLab ("_driver");
	}
}

void import_not_yet_imported_system_labels (void)
{
	if (match_error_lab_used ||
		selector_m_error_lab_used)
		GenImpMod ("_system");
	if (match_error_lab_used)
		GenImpLab (match_error_lab.lab_name);
	if (selector_m_error_lab_used)
		GenImpLab (selector_m_error_lab.lab_name);
}

static void print_foreign_export_type (TypeNode type)
{
	if (!type->type_node_is_var){
		Symbol symbol_p;

		symbol_p=type->type_node_symbol;

		if (symbol_p->symb_kind==int_type){
			FPrintF (OutFile,"I");
			return;
		} else if (symbol_p->symb_kind==real_type){
			FPrintF (OutFile,"R");
			return;
		} else if (symbol_p->symb_kind==unboxed_array_type){
			TypeNode type_node_p;

			type_node_p=type->type_node_arguments->type_arg_node;
			if (!type_node_p->type_node_is_var){
				switch (type_node_p->type_node_symbol->symb_kind){
					case char_type:
						FPrintF (OutFile,"S");
						return;
					case int_type:
						FPrintF (OutFile,"Ai");
						return;
					case real_type:
						FPrintF (OutFile,"Ar");
						return;
				}
			}
		} else if (symbol_p->symb_kind==tuple_type){
			TypeArgs type_arg_p;
			
			for_l (type_arg_p,type->type_node_arguments,type_arg_next)
				print_foreign_export_type (type_arg_p->type_arg_node);
			
			return;
		}
	}
	
	error_in_function ("print_foreign_export_type");
}

static void print_foreign_export_result_type (TypeNode type)
{
	if (!type->type_node_is_var && type->type_node_symbol->symb_kind==tuple_type)
		FPrintF (OutFile,"V");

	print_foreign_export_type (type);
}

void GenerateForeignExports (struct foreign_export_list *foreign_export_list)
{
	struct foreign_export_list *foreign_export_p;

	for_l (foreign_export_p,foreign_export_list,fe_next){
		SymbDef function_sdef;
		TypeAlt *rule_type_p;
		TypeArgs type_arg_p;

		function_sdef=foreign_export_p->fe_symbol_p->symb_def;

		FPrintF (OutFile,"\n\tcentry %s e_%s_s%s \"",function_sdef->sdef_ident->ident_name,CurrentModule,function_sdef->sdef_ident->ident_name);
		
		if (foreign_export_list->fe_stdcall)
			FPutC ('P',OutFile);
		
		rule_type_p=function_sdef->sdef_rule->rule_type;
		
		for_l (type_arg_p,rule_type_p->type_alt_lhs->type_node_arguments,type_arg_next)
			print_foreign_export_type (type_arg_p->type_arg_node);
		
		FPrintF (OutFile,":");
		
		print_foreign_export_result_type (rule_type_p->type_alt_rhs);
				
		FPrintF (OutFile,"\"");
	}
}

void GenParameters (Bool input, Parameters params, int asp, int bsp)
{
	int is_first_parameter;

	if (input)
		put_instruction_ (Iin);
	else
		put_instruction_ (Iout);
	
	is_first_parameter=1;
	for (; params!=NULL; params=params->par_next){
		NodeId node_id;
		
		node_id=params->par_node_id;
		if (!is_first_parameter)
			FPutC (' ',OutFile);
		if (IsSimpleState (node_id->nid_state) && node_id->nid_state.state_kind==OnB)
			FPrintF (OutFile, "b%d:%s",bsp-node_id->nid_b_index,params->par_loc->ident_name);
		else
			FPrintF (OutFile, "a%d:%s",asp-node_id->nid_a_index,params->par_loc->ident_name);
		is_first_parameter=0;
	}
}

void GenInstructions (Instructions ilist)
{
	for (; ilist; ilist = ilist->instr_next){
		char *instruction_name;
		
		instruction_name=ilist->instr_this;
		
		FPutC ('\n',OutFile);
		if (instruction_name[0]==':')
			FPutS (&instruction_name[1],OutFile);
		else {
			if (instruction_name[0]!='.')
				FPutC ('\t',OutFile);
			FPutS (instruction_name,OutFile);
		}
	}
	if (!DoDebug)
		FPutC ('\n',OutFile);
}

void GenTestCaf (Label label)
{
	put_instruction_ (Itestcaf);
	GenLabel (label);
}

void GenPushCaf (Label label,int a_stack_size,int b_stack_size)
{
	put_instruction_ (Ipushcaf);
	GenLabel (label);
	FPrintF (OutFile," %d %d",a_stack_size,b_stack_size);
}

void GenFillCaf (Label label,int a_stack_size,int b_stack_size)
{
	put_instruction_ (Ifillcaf);
	GenLabel (label);
	FPrintF (OutFile," %d %d",a_stack_size,b_stack_size);
}

void GenCaf (Label label,int a_stack_size,int b_stack_size)
{
	put_directive_ (Dcaf);
	GenLabel (label);
	FPrintF (OutFile," %d %d",a_stack_size,b_stack_size);
}

void GenPB (char *function_name)
{
	put_directive_ (Dpb);
	FPrintF (OutFile,"\"%s\"",function_name);
}

void GenPB_ident (IdentP ident,unsigned int line_n)
{
	put_directive_ (Dpb);
	PutCOutFile ('\"');
	PrintSymbolOfIdent (ident,line_n,OutFile);
	PutCOutFile ('\"');
}

void GenPB_with_line_number (char *function_name,int line_number)
{
	put_directive_ (Dpb);
	FPrintF (OutFile,"\"%s[line:%d]\"",function_name,line_number);
}

void GenPD (void)
{
	put_directive (Dpd);
}

void GenPN (void)
{
	put_directive (Dpn);
}

void GenPL (void)
{
	put_directive (Dpl);
}

void GenPLD (void)
{
	put_directive (Dpld);
}

void GenPT (void)
{
	put_directive (Dpt);
}

void GenPE (void)
{
	put_directive (Dpe);
}

void GenKeep (int a_offset1,int a_offset2)
{
	put_directive_ (Dkeep);
	FPrintF (OutFile,"%d %d",a_offset1,a_offset2);
}

#if IMPORT_OBJ_AND_LIB
void GenImpObj (char *obj_name)
{
	put_directive_ ("impobj");
	FPrintF (OutFile,"%s",obj_name);
}

void GenImpLib (char *lib_name)
{
	put_directive_ ("implib");
	FPrintF (OutFile,"%s",lib_name);
}
#endif

void InitInstructions (void)
{
#if CLEAN2
	CaseFailNumber = 0;
#endif

    ABCFileName	= NULL;
}
