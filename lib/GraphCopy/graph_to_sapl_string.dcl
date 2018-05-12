definition module graph_to_sapl_string

// JMJ 2007
// Convert a graph to a SAPL string that can be evaluated by the SAPL interpreter

// DynamicSapl is only for debugging purposes
:: DynamicSapl = IntS Int | BoolS Bool | CharS Char | RealS Real | StringS String | CstrS String String Int [DynamicSapl] | 
                 FunctionS String String Int [DynamicSapl] | ArrayS Int [DynamicSapl] | ListS [DynamicSapl] |
                 TupleS Int [DynamicSapl] | RecS String String Int [DynamicSapl]

// Conversion to a SAPL string
graph_to_sapl_string :: !a -> String
string_to_graph :: !String -> .a

// Conversion to a SAPL intermediate format, only for debugging
graph_to_sapl_dynamic :: !a -> DynamicSapl

// Testing function for debugging, also gives decoding
graph_to_sapl_dynamic_test :: !a -> (({#Int},Char,{#String},Char,{#String}),Char,DynamicSapl,Char,String)

// Testing function for debugging, also gives decoding
print_graph :: !a -> String