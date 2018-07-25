implementation module Clean.PrettyPrint.Expression

import StdEnv

import Data.List

import predef
import syntax

import Clean.PrettyPrint.Util
import Clean.PrettyPrint.Common
import Clean.PrettyPrint.Definition

// General expressions
instance print ParsedExpr
where
	print st (PE_List [PE_Ident id=:{id_name},x,xs])
		| id_name == "_Cons"
			= "[" +++ rest
		| id_name == "_cons"
			= "[|" +++ rest
		| id_name.[0] == '_' && id_name % (2,6) == "Cons"
			= "[" +++ {id_name.[1]} +++ rest
	where
		rest = print st x +++ print_in_list st xs

		end_of_list = case id_name of
			"_!Cons!" -> "!]"
			"_#Cons!" -> "!]"
			_         -> "]"

		isConsIdent {id_name} = isMember id_name ["_Cons","_cons","_|Cons","_!Cons","_!Cons!","_Cons!","_#Cons","_#Cons!"]
		isNilIdent  {id_name} = isMember id_name ["_Nil", "_nil", "_|Nil", "_!Nil", "_!Nil!", "_Nil!", "_#Nil", "_#Nil!"]

		print_in_list :: !CPPState !ParsedExpr -> String
		print_in_list st (PE_List [PE_Ident id,x,xs]) | isConsIdent id
			= "," +++ print st x +++ print_in_list st xs
		print_in_list st (PE_Ident id) | isNilIdent id
			= end_of_list
		print_in_list st e
			= ":" +++ print st e +++ end_of_list
	print st (PE_List pes)
		= printp st (print {st & cpp_parens=True} pes)
	print st (PE_Ident id)
		= print st id
	print st (PE_QualifiedIdent id s)
		= print st ("'" :+: id :+: "'." :+: s)
	print st (PE_Basic b)
		= print st b
	print st (PE_Tuple pes)
		= "(" +++ join st "," pes +++ ")"
	print st (PE_ArrayDenot ak elems)
		= print st ("{" :+: ak :+: join st "," elems :+: "}")
	print st (PE_Record init name fields)
		= print st ("{ " :+: name` :+: init` :+: join st ", " fields :+: " }")
	where
		init` = case init of
			PE_Empty = ""
			_        = print st init +++ " & "
		name` = case name of
			NoRecordName         = ""
			(RecordNameIdent id) = print st id +++ " | "
			(RecordNameQualifiedIdent mod s) = print st ("'" :+: mod :+: "'." :+: s) +++ " | "
	print st (PE_ListCompr cons nil pe qs)
		= print st ("[" :+: list_type :+: pe :+: " \\\\ " :+: join st ", " qs :+: tail_strict :+: "]")
	where
		list_type = case cons of
			PD_StrictConsSymbol           -> "!"
			PD_StrictTailStrictConsSymbol -> "!"
			PD_UnboxedConsSymbol          -> "#"
			PD_OverloadedConsSymbol       -> "|"
			PD_cons                       -> "|"
			_                             -> ""
		tail_strict = case cons of
			PD_TailStrictConsSymbol       -> "!"
			PD_StrictTailStrictConsSymbol -> "!"
			_                             -> ""
	print st (PE_If _ c i e)
		= printp st ("if " +++ join { st & cpp_parens=True } " " [c,i,e])
	print st (PE_Case _ pe alts)
		= "case " +++ print {st & cpp_parens=True} pe +++ " of" +++ join_start st` ("\n" :+: st`) alts
	where
		st` = {st & cpp_indent = st.cpp_indent + 1}
	print st (PE_Sequ seq)
		= print st seq
	print st (PE_Lambda _ pes rhs _)
		= printp st ("\\" :+: join st " " pes :+: " -> " :+: rhs)
	print st (PE_Let lds pe)
		= printp st ("let " :+: join st ", " lds :+: " in " :+: pe)
	print st (PE_Bound {bind_src,bind_dst})
		= print {st & cpp_parens=True} (bind_dst :+: "=:" :+: bind_src)
	print st PE_WildCard
		= "_"
	print st (PE_Update e1 sels e2)
		= print {st & cpp_parens=False} ("{" :+: e1 :+: " & " :+: printParsedSelections st sels :+: "=" :+: e2 :+: "}")
	print st (PE_Selection psk pe pes)
		= print st (pe :+: sel :+: printParsedSelections st pes)
	where
		sel = case psk of
			ParsedNormalSelector     = "."
			(ParsedUniqueSelector _) = "!"
	print st (PE_UpdateComprehension base (PE_Update _ sels new) _ qs)
		= print st ("{" :+: base :+: " & " :+: printParsedSelections st sels :+: "=" :+: new :+: " \\\\ " :+: join st ", " qs :+: "}")
	print st (PE_ArrayCompr ak pe qs)
		= print st ("{" :+: ak :+: pe :+: " \\\\ " :+: join st ", " qs :+: "}")
	print st (PE_ABC_Code lines inline)
		= print st ("code " :+: if inline "inline " "" :+: "{" :+:
			join_start st` ("\n" :+: st`) lines :+: "\n" :+: st :+: "}")
	where
		st` = {st & cpp_indent = st.cpp_indent + 1}
	print st (PE_DynamicPattern pe dt)
		= print st ("(" :+: pe :+: " :: " :+: dt :+: ")")
	print st (PE_Dynamic pe (Yes dt))
		= printp st ("dynamic " :+: pe :+: " :: " :+: dt)
	print st (PE_Dynamic pe No)
		= printp st ("dynamic " :+: pe)
	print st (PE_Generic id k)
		= print st (id :+: "{|" :+: k :+: "|}")
	print st (PE_ArrayPattern eas)
		= print st ("{" :+: join st "," eas :+: "}")
	print st (PE_Matches _ e p _)
		= print st (e :+: "=:(" :+: p :+: ")")
	// | PE_Any_Code !(CodeBinding Ident) !(CodeBinding Ident) ![String]
	// | PE_TypeSignature !ArrayKind !ParsedExpr
	// | PE_Empty
	print st pe
		= abort "UNKNOWN_PE"

printParsedSelections :: CPPState [ParsedSelection] -> String
printParsedSelections st [PS_Array pe] = print {st & cpp_parens=False} ("[" :+: pe :+: "]")
printParsedSelections st [PS_Record f NoRecordName] = print st f
printParsedSelections st _             = "UNKNOWN_PARSEDSELECTION"

instance print Rhs
where
	print st {rhs_alts,rhs_locals=LocalParsedDefs []}
		= print st rhs_alts
	print st {rhs_alts,rhs_locals}
		= let st` = {st & cpp_indent = st.cpp_indent + 1} in
			print st (rhs_alts :+: "\n" :+: st :+: "where\n" :+: st` :+: join st` ("\n" :+: st`) rhs_locals)

// Basic values
instance print BasicValue
where
	print _ (BVInt i) = toString i
	print _ (BVC c)   = c
	print _ (BVB b)   = toString b
	print _ (BVR r)   = r
	print _ (BVS s)   = s
	print _ (BVI _)   = "BVI???"

// Lists
instance print Qualifier
where
	print st q=:{qual_filter=Yes filt} = print st ({q & qual_filter=No} :+: " | " :+: filt)
	print st q=:{qual_generators} = join st " & " qual_generators

instance print Generator
where
	print st {gen_pattern,gen_expr,gen_kind}
		= print st (gen_pattern :+: select :+: gen_expr)
	where
		select = case gen_kind of
			IsListGenerator = " <- "
			IsOverloadedListGenerator = " <|- "
			IsArrayGenerator = " <-: "

instance print Sequence
where
	print st (SQ_FromThen i e1 e2)
		= print st ("[" :+: e1 :+: "," :+: e2 :+: "..]")
	print st (SQ_FromThenTo i e1 e2 e3)
		= print st ("[" :+: e1 :+: "," :+: e2 :+: ".." :+: e3 :+: "]")
	print st (SQ_From i e)
		= print st ("[" :+: e :+: "..]")
	print st (SQ_FromTo i e1 e2)
		= print st ("[" :+: e1 :+: ".." :+: e2 :+: "]")

// Arrays
instance print ArrayKind
where
	print _ OverloadedArray = ""
	print _ StrictArray     = "!"
	print _ UnboxedArray    = "#"

instance print ElemAssignment
where
	print st b = print st ("[" :+: join st "," b.bind_dst :+: "]=" :+: b.bind_src)

// Records
instance print FieldAssignment
where
	print st {bind_src=PE_Empty,bind_dst} = print st bind_dst
	print st {bind_src,bind_dst} = print st (bind_dst :+: "=" :+: bind_src)

instance print FieldNameOrQualifiedFieldName
where
	print st (FieldName id)             = print st id
	print st (QualifiedFieldName mod s) = print st ("'" :+: mod :+: "'." :+: s)

// Case .. of
instance print CaseAlt
where
	print st ca = print st (ca.calt_pattern :+: " -> " :+: ca.calt_rhs)

// Local definitions
instance Join LocalDefs
where
	join st glue (LocalParsedDefs lds) = join st glue lds
	join st glue _                     = abort "JOIN: UNKNOWN_LOCALDEFS"

	isNil (LocalParsedDefs []) = True
	isNil (LocalParsedDefs _)  = False
	isNil _                    = abort "JOIN: UNKNOWN_LOCALDEFS"

instance print ExprWithLocalDefs
where
	print st {ewl_expr,ewl_nodes=[],ewl_locals=LocalParsedDefs []}
		= print st ewl_expr
	print st {ewl_expr,ewl_nodes,ewl_locals=LocalParsedDefs []}
		= print st (join_start st` ("\n" :+: st`) ewl_nodes :+: "\n" :+: st` :+: "= " :+: ewl_expr)
	where
		st`  = {st & cpp_indent = st.cpp_indent + 1}
	print st {ewl_expr,ewl_locals}
		= print st (ewl_expr :+: "\n" :+: st` :+: "with" :+: join_start st`` ("\n" :+: st``) ewl_locals)
	where
		st`  = {st & cpp_indent = st.cpp_indent + 1}
		st`` = {st & cpp_indent = st.cpp_indent + 2}

instance print NodeDefWithLocals
where
	print st {ndwl_strict,ndwl_def={bind_src,bind_dst},ndwl_locals}
		= print st (if ndwl_strict "#! " "# " :+: bind_dst :+: " = " :+: bind_src)

// Guards
instance print OptGuardedAlts
where
	print st (GuardedAlts ges (Yes othe))
		= print st (join_start st ("\n" :+: st :+: "| ") ges :+: "\n" :+: st :+: "| otherwise = " :+: othe)
	print st (GuardedAlts ges No)
		= join_start st ("\n" :+: st :+: "| ") ges
	print st (UnGuardedExpr e)
		= print st e

instance print GuardedExpr
where
	print st {alt_guard,alt_expr}
		= print {st & cpp_indent = st.cpp_indent + 1} alt_guard +++ eq +++ print st alt_expr
	where
		eq = if (compound_rhs alt_expr) "" " = "

compound_rhs :: !OptGuardedAlts -> Bool
compound_rhs (GuardedAlts _ _)                 = True
compound_rhs (UnGuardedExpr {ewl_nodes=[_:_]}) = True
compound_rhs _                                 = False

// Generics
instance print TypeKind
where
	print st KindConst = "*"
	print st (KindArrow ks) = printp st (intersperse "->" (map (print {st & cpp_parens=True}) ks))
	print st _ = abort "Unknown TypeKind"

// Dynamics
instance print DynamicType
where
	print st {dt_uni_vars,dt_type,dt_contexts}
		= print st (uni_vars :+: dt_type :+: context)
	where
		uni_vars = case dt_uni_vars of
			[] = PrintNil
			vs = "A." :+: join st " " vs :+: ": "
		context = case dt_contexts of
			[] = PrintNil
			cs = " | " :+: join st " & " cs
