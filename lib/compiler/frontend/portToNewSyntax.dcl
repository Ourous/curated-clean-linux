definition module portToNewSyntax
// see the file readme.txt in the portToNewSyntax folder about
// this module

import checksupport

switch_port_to_new_syntax port dont_port :== dont_port

cTabWidth :== 4

writeExplImportsToFile :: !String ![([Declaration],a)] !{#u:DclModule} !*CheckState 
		-> (!{#u:DclModule},!.CheckState)

createPortedFiles :: !String !SearchPaths !*Files -> (!Bool, !*Files)
