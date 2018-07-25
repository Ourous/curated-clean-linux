definition module Text.Unicode.Encodings.JS

import StdClass, Text.Unicode

// encode Unicode String as a JS String literal

:: JSLit

instance fromUnicode JSLit 
instance toUnicode JSLit 

instance fromString JSLit 
instance toString JSLit 

toJSLiteral :: !UString -> String
