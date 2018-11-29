implementation module Clean.PrettyPrint.Definition

import StdEnv

import syntax

import Clean.PrettyPrint.Util
import Clean.PrettyPrint.Common
import Clean.PrettyPrint.Expression

instance print ParsedDefinition
where
	print st (PD_Import ips)
		= join st "\n" ips
	print st (PD_Class cd [pd=:PD_TypeSpec _ mem _ _ _]) | cd.class_ident.id_name == mem.id_name
		= print st ("class " :+: pd)
	print st (PD_Class cd mems)
		= print st ("class " :+: cd.class_ident :+: args :+: context :+: if (isEmpty mems) "" "\nwhere" :+: join_start st` ("\n" :+: st`) mems)
	where
		st` = { st & cpp_indent = st.cpp_indent + 1 }
		context = if (isEmpty cd.class_context) "" (" | " +++ join st " & " cd.class_context)
		args = if (isEmpty cd.class_args) "" (join_start st " " cd.class_args)
    print st (PD_Instance pim)
		= print st pim
    print st (PD_Instances pis)
		= join st ("\n" :+: st) pis
	print st (PD_Generic {gen_ident,gen_type,gen_vars})
		= print st ("generic " :+: gen_ident :+: join_start st " " gen_vars :+: " :: " :+: gen_type)
	print st (PD_Derive gencasedefs)
		= print st ("derive " :+: join st ", " gencasedefs)
    print st (PD_TypeSpec pos id prio type funspecs)
		= print st (id` :+: prio` :+: type`)
	where
		id` = case prio of
			NoPrio = id :+: PrintNil
			_      = "(" :+: id :+: ")"
		prio` = case prio of
			(Prio LeftAssoc p)  = " infixl " :+: p
			(Prio RightAssoc p) = " infixr " :+: p
			(Prio NoAssoc p)    = " infix "  :+: p
			NoPrio              = PrintNil
		type` = case type of
			(Yes t) = " :: " :+: t
			No      = PrintNil
	print st (PD_NodeDef _ l r)
		= print {st & cpp_parens=False} (l :+: " = " :+: r)
	print st (PD_Function _ id isinfix args rhs fk)
		= print stnp (id` :+: join_start stp " " args :+: if show_eq eq "" :+: rhs)
	where
		stnp = {st & cpp_parens=False}
		stp = {st & cpp_parens=True}
		id` = if isinfix ("(" :+: id :+: ")") (id :+: PrintNil)
		show_eq = not (compound_rhs rhs.rhs_alts)
		eq = case fk of FK_Macro = " :== "; _ = " = "
	print st (PD_Type {td_ident,td_args,td_attribute,td_rhs})
		= print st (":: " :+: td_attribute :+: td_ident :+: join_start st " " td_args :+: equals :+: td_rhs)
	where
		equals = case td_rhs of
			TypeSpec _         -> " :== "
			EmptyRhs _         -> ""
			NewTypeCons _      -> " =: "
			ConsList _         -> "\n\t= "
			ExtensibleConses _ -> "\n\t= "
			_                  -> " = "
	print st (PD_GenericCase {gc_type,gc_gcf=GCF id {gcf_body=GCB_ParsedBody [desc:args] rhs}} _)
		= print st (id :+: "{|" :+: gc_type :+: desc` :+: "|} " :+: args :+: " = " :+: rhs)
	where
		desc` = case desc of
			PE_WildCard -> PrintNil
			_           -> " of " :+: desc
	print _ _
		= abort "UNKNOWN_PD"

// General types
instance print BasicType
where
	print st BT_Int = "Int"
	print st BT_Char = "Char"
	print st BT_Real = "Real"
	print st BT_Bool = "Bool"
	print st (BT_String t) = print st ("String" :+: t)
	print st BT_File = "File"
	print st BT_World = "World"
	print st BT_Dynamic = "Dynamic"

instance print SymbolType
where
	print st t
		= print st (if (isEmpty t.st_args) PrintNil (args` :+: " -> ")) +++
			print stnp (markVars t.st_result :+: st_context` :+: st_env`)
	where
		stp = {st & cpp_parens=True}
		stnp = {st & cpp_parens=False}
		st_context` = if (isEmpty t.st_context) PrintNil (" | " :+: join stp " & " (markVarsInTC t.st_context))
		st_env` = if (isEmpty t.st_attr_env) PrintNil (", [" :+: join stnp ", " t.st_attr_env :+: "]")
		args` = join stp " " [if s "!" "" :+: markVars a \\ a <- t.st_args & s <- strictnessListToBools t.st_args_strictness]

		markVars :: AType -> AType
		markVars at=:{at_attribute=TA_Var av}
		| isMember av.av_ident.id_name allInequalityVars
			= { at_attribute=TA_Var {av & av_ident.id_name = "_" +++ av.av_ident.id_name}
			  , at_type = markVars` at.at_type
			  }
		markVars at = {at & at_type=markVars` at.at_type}

		markVars` :: Type -> Type
		markVars` (TA tsi ats) = TA tsi (map markVars ats)
		markVars` (TAS tsi ats sl) = TAS tsi (map markVars ats) sl
		markVars` (t1 --> t2) = markVars t1 --> markVars t2
		markVars` (TArrow1 at) = TArrow1 (markVars at)
		markVars` (c :@: ats) = c :@: map markVars ats
		markVars` (TFA atvs t) = TFA (map markVars`` atvs) (markVars` t)
		markVars` (TFAC atvs t tcs) = TFAC (map markVars`` atvs) (markVars` t) (markVarsInTC tcs)
		markVars` (TQualifiedIdent id s ats) = TQualifiedIdent id s (map markVars ats)
		markVars` (TLiftedSubst t) = TLiftedSubst (markVars` t)
		markVars` t = t

		markVars`` :: ATypeVar -> ATypeVar
		markVars`` atv=:{atv_attribute=TA_Var av}
		| isMember av.av_ident.id_name allInequalityVars
			= {atv & atv_attribute=TA_Var {av & av_ident.id_name = "_" +++ av.av_ident.id_name}}
		markVars`` atv = atv

		markVarsInTC :: [TypeContext] -> [TypeContext]
		markVarsInTC tcs = [{tc & tc_types=map markVars` tc.tc_types} \\ tc <- tcs]

		allInequalityVars = flatten [[ineq.ai_demanded.av_ident.id_name, ineq.ai_offered.av_ident.id_name] \\ ineq <- t.st_attr_env]

strictnessListToBools :: !StrictnessList -> [Bool]
strictnessListToBools NotStrict        = repeat False
strictnessListToBools (Strict i)       = [i bitand (1 << e) <> 0 \\ e <- [0..31]]
strictnessListToBools (StrictList i l) = strictnessListToBools (Strict i) ++ strictnessListToBools l

instance print Type
where
	print st (TA tsi ats)
		= print st (tsi, ats, [False \\ _ <- ats])
	print st (TAS tsi ats slist)
		= print st (tsi, ats, strictnessListToBools slist)
	print st (at1 --> at2)
		= printp st (at1 :+: " -> " :+: print {st & cpp_parens=False} at2)
	print st TArrow
		= "(->)"
	print st (TArrow1 at)
		= print {st & cpp_parens=True} ("((->) " :+: at :+: ")")
	print st (cv :@: ats)
		= printp st (cv :+: " " :+: join {st & cpp_parens=True} " " ats)
	print st (TB bt)
		= print st bt
	//print st (TFA atvs type)
	//	= "TFA"
	print st (GTV tv)
		= print st (tv :+: "^")
	print st (TV tv)
		= print st tv
	print st (TFAC atvs t tc)
		= print st ("(A." :+: join st " " atvs :+: ": " :+: t :+: " | " :+: join st " & " tc :+: ")")
	print st (TQualifiedIdent id s [])
		= print st ("'" :+: id :+: "'." :+: s)
	print st (TQualifiedIdent id s ats)
		= printp st ("'" :+: id :+: "'." :+: s :+: join_start st " " ats)
	//|	TGenericFunctionInDictionary !(Global DefinedSymbol) !TypeKind !GlobalIndex /*GenericDict*/
	//|	TE
	print st _
		= abort "UNKNOWN_TYPE"

instance print ConsVariable where print st (CV tv) = print st tv //TODO

instance print TypeVar where print st {tv_ident} = tv_ident.id_name

instance print AType
where
	print st {at_attribute=TA_Var {av_ident},at_type=TV {tv_ident}}
	| av_ident.id_name == tv_ident.id_name = "." +++ tv_ident.id_name
	print st {at_attribute=TA_Var av=:{av_ident},at_type=t}
	| aname.[0] == '_' = print {st & cpp_parens=True} (TA_Var {av & av_ident.id_name=aname % (1,size aname-1)} :+: t)
	where aname = av_ident.id_name
	print st {at_attribute=TA_None,at_type} = print st at_type
	print st at = print {st & cpp_parens=True} (at.at_attribute :+: at.at_type)

instance print ATypeVar
where
	print st v = print st (v.atv_attribute :+: v.atv_variable)

instance print TypeAttribute where print _ a = toString a

instance print (TypeSymbIdent, [AType], /* is_strict */ [Bool])
where
	print st (tsi, ats, strict)
		= print st (case lookup tsi.type_ident.id_name of
			(Yes s) = s
			No      = case ats of
				[]  = tsi :+: PrintNil
				_   = if st.cpp_parens
					("(" :+: tsi :+: " " :+: join stp " " ats :+: ")")
					(        tsi :+: " " :+: join stp " " ats        )
		)
	where
		lookup "_String"           = Yes ("String" :+: PrintNil)
		lookup "_Unit"             = Yes ("()" :+: PrintNil)
		lookup "_List"             = Yes ("["  :+: join stnp " " ats :+:  "]")
		lookup "_!List"            = Yes ("[!" :+: join stnp " " ats :+:  "]")
		lookup "_List!"
		| isEmpty ats              = Yes ("[ !]" :+: PrintNil)
		| otherwise                = Yes ("["  :+: join stnp " " ats :+: "!]")
		lookup "_!List!"           = Yes ("[!" :+: join stnp " " ats :+: "!]")
		lookup "_|List"            = Yes ("[|" :+: join stnp " " ats :+:  "]")
		lookup "_#List"            = Yes ("[#" :+: join stnp " " ats :+:  "]")
		lookup "_#List!"           = Yes ("[#" :+: join stnp " " ats :+: "!]")
		lookup "_Array"            = Yes ("{"  :+: join stnp " " ats :+:  "}")
		lookup "_#Array"           = Yes ("{#" :+: join stnp " " ats :+:  "}")
		lookup "_!Array"           = Yes ("{!" :+: join stnp " " ats :+:  "}")
		lookup name
		| name % (0,5) == "_Tuple"
			| length ats == arity  = Yes ("(" :+: join stnp "," types :+: ")")
			| isEmpty ats          = Yes (tupleString :+: PrintNil)
			| otherwise            = Yes (tupleString :+: " " :+: join stp " " types)
		where
			tupleString = "(" +++ toString (repeatn (arity-1) ',') +++ ")"
			types = [if s "!" "" :+: a \\ a <- ats & s <- strict]
			arity = toInt (name % (6,size name-1))
		lookup _                   = No

		stp  = {st & cpp_parens=True}
		stnp = {st & cpp_parens=False}

// Type contexts
instance print TypeContext
where
	print st tc
		= print st (tc.tc_class :+: " " :+: join st " " tc.tc_types)

// Type definitions
instance print RhsDefsOfType
where
	print st (ConsList conses)
		= join st "\n\t| " conses
	print st (ExtensibleConses conses)
		= join st "\n\t| " conses +++ "\n\t| .."
	print st (SelectorList _ exivars _ fields)
		= print st (exivars` :+: "\n\t{ " :+: join st "\n\t, " (map print_ps fields) :+: "\n\t}")
	where
		exivars` = if (isEmpty exivars) PrintNil ("E." :+: join st " " exivars :+: ": ")

		print_ps ps = print st
			(ps.ps_selector_ident :+:
			{#c \\ c <- repeatn (maxfieldlen - size ps.ps_selector_ident.id_name) ' '} :+:
			" :: " :+: ps.ps_field_annotation :+: ps.ps_field_type)
		maxfieldlen = maxList [size ps.ps_selector_ident.id_name \\ ps <- fields]
	print st (TypeSpec type)
		= print st type
	print st (EmptyRhs _)
		= ""
	print st (AbstractTypeSpec _ at)
		= print st at
	print st (NewTypeCons pc)
		= print st pc
	print _ _
		= abort "UNKNOWN_RHSDEFSOFTYPE"

instance print ParsedSelector
where
	print st ps = print st (ps.ps_selector_ident :+: " :: " :+: ps.ps_field_annotation :+: ps.ps_field_type)

instance print ParsedConstructor
where
	print st cons=:{pc_arg_types=[]} = print st cons.pc_cons_ident
	print st cons = print st
		(cons.pc_cons_ident :+: " " :+:
			[if s "!" "" :+: print {st & cpp_parens=True} t
				\\ t <- cons.pc_arg_types
				 & s <- strictnessListToBools cons.pc_args_strictness])

instance print Annotation
where
	print st AN_Strict = "!"
	print st AN_None   = ""

// Classes
instance print TCClass
where
	print st (TCClass {glob_object={ds_ident}})
		= print st ds_ident
	print st (TCGeneric {gtc_generic,gtc_kind})
		= print st (gtc_generic.glob_object.ds_ident.id_name :+: "{|" :+: gtc_kind :+: "|}")
	print st _
		= abort "UNKNOWN_TCCLASS"

instance print ParsedInstanceAndMembers
where
	print st {pim_pi={pi_pos,pi_ident,pi_types,pi_context},pim_members}
		= print st ("instance " :+: pi_ident :+: " " :+: join st " " pi_types :+: pi_context` :+: members)
	where
		pi_context` = if (isEmpty pi_context) PrintNil (" | " :+: join st " & " pi_context)
		members = if (isEmpty pim_members) PrintNil (" where" :+: join_start st` ("\n" :+: st`) pim_members)
		st` = {st & cpp_indent = st.cpp_indent + 1}
		pos = case pi_pos of
			(FunPos f l n) = "<" :+: f :+: ";" :+: l :+: ";" :+: n :+: ">"
			(LinePos f l) = "<" :+: f :+: ";" :+: l :+: ">"

// Generics
instance print GenericCaseDef
where
	print st {gc_type,gc_gcf=GCF id _}
		= print st (id :+: " " :+: gc_type)
	print st {gc_type,gc_gcf=GCFC id _}
		= print st ("class " :+: id :+: " " :+: gc_type)
	print _ _
		= abort "UNKNOWN_GENERICCASEDEF"

instance print TypeKind
where
	print st KindConst = print st "*"
	print st (KindArrow ks) = print st ("*->" :+: ks)

// Uniqueness
instance print AttrInequality
where
	print st ai = print st (ai.ai_offered.av_ident :+: "<=" :+: ai.ai_demanded.av_ident)

// Miscellaneous
instance print TypeSymbIdent where print st tsi = print st tsi.type_ident
