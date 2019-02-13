implementation module portToNewSyntax

import StdEnv, scanner, checksupport

switch_port_to_new_syntax port dont_port :== dont_port

cTabWidth :== 4

writeExplImportsToFile :: !String ![([Declaration],a)] !{#u:DclModule} !*CheckState 
		-> (!{#u:DclModule},!.CheckState)
writeExplImportsToFile _ _ _ _
	= abort (   "To build a compiler that is able to create ported versions of"
	         +++"1.3 sources you should set your paths in a way that"
	         +++"\"portToNewSyntax/portToNewSyntax\" is used instead of"
	         +++"\"compiler/portToNewSyntax\".\n"
	         +++"Read the readme.txt in folder \"portToNewSyntax\"."
	        )

createPortedFiles :: !String !SearchPaths !*Files -> (!Bool, !*Files)
createPortedFiles _ _ _
	= abort "portToNewSyntax"
	       