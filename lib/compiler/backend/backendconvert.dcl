definition module backendconvert

from backend import ::BackEnd
import frontend

backEndConvertModules :: PredefinedSymbols FrontEndSyntaxTree !Int !*TypeVarHeap !*VarHeap !*AttrVarHeap !*BackEnd
															   -> (!*TypeVarHeap,!*VarHeap,!*AttrVarHeap,!*BackEnd)
