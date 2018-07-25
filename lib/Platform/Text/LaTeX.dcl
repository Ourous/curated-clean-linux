definition module Text.LaTeX

/**
* This module provides data types for construction of very simple LaTeX documents.
*/

:: LaTeX	= Text !String
			| RawText !String

			//Sectioning
			| Chapter !String
		 	| Section !String
		 	| Subsection !String
		 	| Subsubsection !String
		 	| Paragraph !String

		 	//Formatting
		 	| Emph ![LaTeX]
		 	| TextBf ![LaTeX]
		 	| TextIt ![LaTeX]
		 	| CleanCode ![String]
		 	| CleanInline !String
		 	| Itemize ![LaTeX]
		 	| Item ![LaTeX]
		 	
		 	//References
		 	| Label !String
		 	| Cite !String
		 	| Index !String
		 	
		 	//Characters
		 	| EnDash
		 	| EmDash
		 	| NewParagraph
		 	
		 	//Custom
		 	| Environment !String ![LaTeX]

printLaTeX :: ![LaTeX] -> String
