implementation module Text.LaTeX

import StdFunc
import StdList

import Text
from Text.PPrint import class Pretty(..), :: Doc, :: SimpleDoc
from Text.PPrint import renderPretty, text, display, braces, <->, <$>, <+>, hsep, vsep, linebreak

printLaTeX :: ![LaTeX] -> String
printLaTeX l = display (renderPretty 0.8 80 (pretty l))

instance Pretty LaTeX
where
	pretty (Text txt) = text (escapeTeX txt)
	pretty (RawText txt) = text txt
	pretty (Chapter title) = textCommand "chapter" title
	pretty (Section title) = textCommand "section" title
	pretty (Subsection title) = textCommand "subsection" title
	pretty (Subsubsection title) = textCommand "subsubsection" title
	pretty (Paragraph title) = textCommand "paragraph" title
	pretty (Emph latex) = latexCommand "emph" latex
	pretty (TextBf latex) = latexCommand "textbf" latex
	pretty (TextIt latex) = latexCommand "textit" latex
	pretty (CleanCode lines) = latexEnvironment "CleanCode" [RawText (join "\n" lines)]
	pretty (CleanInline line) = latexCommand "CleanInline" [RawText line]
	pretty (Itemize items) = latexEnvironment "itemize" items
	pretty (Item latex) = text "\\item" <+>  hsep (map pretty latex)
	pretty (Label label) = textCommand "label" label
	pretty (Cite ref) = textCommand "cite" ref
	pretty (Index ref) = textCommand "index" ref
	pretty EnDash = text "--"
	pretty EmDash = text "---"
	pretty NewParagraph = linebreak
	pretty (Environment name latex) = latexEnvironment name latex
	
instance Pretty [LaTeX]
where
	pretty items = vsep (map pretty items)

textCommand :: !String !String -> Doc
textCommand name contents = latexCommand name [Text contents]
	
latexCommand :: !String ![LaTeX] -> Doc
latexCommand name latex =
	text "\\" <-> text name <-> braces (pretty latex)
	
latexEnvironment :: !String ![LaTeX] -> Doc
latexEnvironment name latex =
	text "\\begin" <-> braces (text name) <$> pretty latex <$> text "\\end" <-> braces (text name)

escapeTeX :: !String -> String
escapeTeX s = seq (map (\(x,y) -> replaceSubString x y) escapeChars) s
where
	escapeChars = 	[ ("\\", "\\textbackslash{}")
					, ("^", "\\textasciicircum{}")
					, ("~", "\\textasciitilde{}")
					, ("*", "\\textasteriskcentered{}")
					, ("|", "\\textbar{}")
					, ("$", "\\textdollar{}")
					, (">", "\\textgreater{}")
					, ("<", "\\textless{}")
					, ("\"", "\\textquotedblright{}")
					, ("'", "\\textquoteright{}")
					, ("_", "\\textunderscore{}")
					, ("&", "\\&{}")
					]
