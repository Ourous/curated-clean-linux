implementation module Clean.PrettyPrint.Common

import StdEnv

import syntax

import Clean.PrettyPrint.Util
import Clean.PrettyPrint.Definition

instance print Ident
where
	print _ {id_name} = lookup id_name namemap
	where
		lookup k []         = k
		lookup k [(k`,v):m] = if (k == k`) v (lookup k m)

		namemap =
			[ ("_Nil",  "[]")
			, ("_nil",  "[|]")
			, ("_|Nil", "[|]")
			, ("_#Nil", "[#]")
			, ("_Unit", "()")
			]

instance print Import
where
	print st ip=:{import_symbols=ImportSymbolsOnly ids,import_qualified=NotQualified}
		= print st ("from " :+: ip.import_module :+: " import " :+: join st ", " ids)
	print st ip=:{import_symbols=ImportSymbolsOnly _}
		= abort "UNKNOWN: ImportSymbolsOnly with Qualified\n"
	print st ip=:{import_symbols=ImportSymbolsAll}
		= print st ("import " :+: q :+: ip.import_module :+: as_)
	where
		q = case ip.import_qualified of
			NotQualified = ""
			_            = "qualified "
		as_	= case ip.import_qualified of
			(QualifiedAs name) = " as " :+: name
			_                  = PrintNil
	print st ip=:{import_symbols=ImportSymbolsAllSomeQualified _}
		= abort "UNKNOWN: ImportSymbolsAllSomeQualified\n"

instance print ImportDeclaration
where
	print st (ID_Function f)
		= print st f
	print st (ID_Class c mems)
		= print st ("class " :+: c :+: mems`)
	where
		mems` = case mems of
			(Yes [])   = "(..)"
			(Yes mems) = "(" +++ join st "," mems +++ ")"
			_          = ""
	print st (ID_Type t conses)
		= print st (":: " :+: t :+: conses`)
	where
		conses` = case conses of
			(Yes [])     = "(..)"
			(Yes conses) = "(" +++ join st "," conses +++ ")"
			_            = ""
	print st (ID_Record t fields)
		= print st (":: " :+: t :+: fields`)
	where
		fields` = case fields of
			(Yes [])     = "{..}"
			(Yes fields) = "{" +++ join st "," fields +++ "}"
			_            = ""
	print st (ID_Instance cls _ (ts, tcs))
		= print st (cls :+: join_start st " " ts :+: if (isEmpty tcs) "" (" | " +++ join st " & " tcs))
	print st (ID_Generic id _)
		= print st ("generic " :+: id)
