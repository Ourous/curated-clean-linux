definition module graph_to_string_and_descriptors;

// includes unboxed record descriptors

graph_to_string_with_descriptors :: !a -> {#Char};

graph_to_string_with_descriptor_and_module_table :: !a -> (!{#Char},!{#{#Char}},!{#{#Char}});
													   // (graph,descriptor_table,module_table)
