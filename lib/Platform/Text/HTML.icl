implementation module Text.HTML

import StdEnv
import Data.Maybe, Data.GenEq
from StdFunc import o
from StdMisc import abort
from Data.List import intersperse

instance toString HtmlTag
where
	toString tag
		#! tagsize   = tagSize tag							//Calculate the size of the string we need
		#! tagstring = createArray tagsize '\0'				//Create an empty buffer
		#! tagstring = fst (serializeTag tag tagstring 0)	//Serialize the html tree
		= tagstring

//Calculate the size (in chars) that the tag will be when
//serialize to a string.

tagSize :: !HtmlTag -> Int
tagSize (Text t)			= escapedSize t
tagSize (Html t)			= size t
tagSize (ATag a t)			=  7 + (attrsSize a) + (tagsSize t) 
tagSize (AbbrTag a t)		= 13 + (attrsSize a) + (tagsSize t) 
tagSize (AcronymTag a t)	= 19 + (attrsSize a) + (tagsSize t) 
tagSize (AddressTag a t)	= 19 + (attrsSize a) + (tagsSize t) 
tagSize (AppletTag a t)		= 17 + (attrsSize a) + (tagsSize t) 
tagSize (AreaTag a)			=  7 + (attrsSize a)
tagSize (BTag a t)			=  7 + (attrsSize a) + (tagsSize t) 
tagSize (BaseTag a t)		= 13 + (attrsSize a) + (tagsSize t) 
tagSize (BasefontTag a)		= 11 + (attrsSize a)
tagSize (BdoTag a t)		= 11 + (attrsSize a) + (tagsSize t) 
tagSize (BigTag a t)		= 11 + (attrsSize a) + (tagsSize t) 
tagSize (BlockquoteTag a t)	= 25 + (attrsSize a) + (tagsSize t) 
tagSize (BodyTag a t)		= 13 + (attrsSize a) + (tagsSize t) 
tagSize (BrTag a)			=  5 + (attrsSize a)
tagSize (ButtonTag a t)		= 17 + (attrsSize a) + (tagsSize t) 
tagSize (CanvasTag a t)		= 17 + (attrsSize a) + (tagsSize t) 
tagSize (CaptionTag a t)	= 19 + (attrsSize a) + (tagsSize t) 
tagSize (CenterTag a t)		= 17 + (attrsSize a) + (tagsSize t) 
tagSize (CircleTag a)		=  9 + (attrsSize a)
tagSize (CiteTag a t)		= 13 + (attrsSize a) + (tagsSize t) 
tagSize (CodeTag a t)		= 13 + (attrsSize a) + (tagsSize t) 
tagSize (ColTag a t)		= 11 + (attrsSize a) + (tagsSize t) 
tagSize (ColgroupTag a t)	= 21 + (attrsSize a) + (tagsSize t)
tagSize (DdTag a t)			=  9 + (attrsSize a) + (tagsSize t) 
tagSize (DelTag a t)		= 11 + (attrsSize a) + (tagsSize t) 
tagSize (DfnTag a t)		= 11 + (attrsSize a) + (tagsSize t) 
tagSize (DirTag a t)		= 11 + (attrsSize a) + (tagsSize t) 
tagSize (DivTag a t)		= 11 + (attrsSize a) + (tagsSize t) 
tagSize (DlTag a t)			=  9 + (attrsSize a) + (tagsSize t) 
tagSize (DtTag a t)			=  9 + (attrsSize a) + (tagsSize t) 
tagSize (EllipseTag a)		= 10 + (attrsSize a)
tagSize (EmTag a t)			=  9 + (attrsSize a) + (tagsSize t) 
tagSize (FieldsetTag a t)	= 21 + (attrsSize a) + (tagsSize t) 
tagSize (FontTag a t)		= 13 + (attrsSize a) + (tagsSize t) 
tagSize (FormTag a t)		= 13 + (attrsSize a) + (tagsSize t) 
tagSize (H1Tag a t)			=  9 + (attrsSize a) + (tagsSize t) 
tagSize (H2Tag a t)			=  9 + (attrsSize a) + (tagsSize t) 
tagSize (H3Tag a t)			=  9 + (attrsSize a) + (tagsSize t) 
tagSize (H4Tag a t)			=  9 + (attrsSize a) + (tagsSize t) 
tagSize (H5Tag a t)			=  9 + (attrsSize a) + (tagsSize t) 
tagSize (H6Tag a t)			=  9 + (attrsSize a) + (tagsSize t) 
tagSize (HeadTag a t)		= 13 + (attrsSize a) + (tagsSize t) 
tagSize (HrTag a)			=  5 + (attrsSize a)
tagSize (HtmlTag a t)		= 13 + (attrsSize a) + (tagsSize t) 
tagSize (ITag a t)			=  7 + (attrsSize a) + (tagsSize t) 
tagSize (IframeTag a t)		= 17 + (attrsSize a) + (tagsSize t) 
tagSize (ImgTag a)			=  6 + (attrsSize a)
tagSize (InputTag a)		=  8 + (attrsSize a)
tagSize (InsTag a t)		= 11 + (attrsSize a) + (tagsSize t) 
tagSize (IsindexTag a)		= 10 + (attrsSize a)
tagSize (KdbTag a t)		= 11 + (attrsSize a) + (tagsSize t) 
tagSize (LabelTag a t)		= 15 + (attrsSize a) + (tagsSize t) 
tagSize (LegendTag a t)		= 17 + (attrsSize a) + (tagsSize t) 
tagSize (LiTag a t)			=  9 + (attrsSize a) + (tagsSize t) 
tagSize (LinkTag a)			=  7 + (attrsSize a)
tagSize (MapTag a t)		= 11 + (attrsSize a) + (tagsSize t) 
tagSize (MenuTag a t)		= 13 + (attrsSize a) + (tagsSize t) 
tagSize (MetaTag a)			=  7 + (attrsSize a)
tagSize (NoframesTag a t)	= 21 + (attrsSize a) + (tagsSize t) 
tagSize (NoscriptTag a t)	= 21 + (attrsSize a) + (tagsSize t) 
tagSize (ObjectTag a t)		= 17 + (attrsSize a) + (tagsSize t) 
tagSize (OlTag a t)			=  9 + (attrsSize a) + (tagsSize t) 
tagSize (OptgroupTag a t)	= 21 + (attrsSize a) + (tagsSize t) 
tagSize (OptionTag a t)		= 17 + (attrsSize a) + (tagsSize t) 
tagSize (PTag a t)			=  7 + (attrsSize a) + (tagsSize t) 
tagSize (ParamTag a t)		= 15 + (attrsSize a) + (tagsSize t) 
tagSize (PreTag a t)		= 11 + (attrsSize a) + (tagsSize t) 
tagSize (QTag a t)			=  7 + (attrsSize a) + (tagsSize t) 
tagSize (RectTag a)			=  7 + (attrsSize a)
tagSize (STag a t)			=  7 + (attrsSize a) + (tagsSize t) 
tagSize (SampTag a t)		= 13 + (attrsSize a) + (tagsSize t) 
tagSize (ScriptTag a t)		= 17 + (attrsSize a) + (tagsSize t) 
tagSize (SelectTag a t)		= 17 + (attrsSize a) + (tagsSize t) 
tagSize (SmallTag a t)		= 15 + (attrsSize a) + (tagsSize t) 
tagSize (SpanTag a t)		= 13 + (attrsSize a) + (tagsSize t) 
tagSize (StrikeTag a t)		= 17 + (attrsSize a) + (tagsSize t) 
tagSize (StrongTag a t)		= 17 + (attrsSize a) + (tagsSize t) 
tagSize (StyleTag a t)		= 15 + (attrsSize a) + (tagsSize t) 
tagSize (SubTag a t)		= 11 + (attrsSize a) + (tagsSize t) 
tagSize (SupTag a t)		= 11 + (attrsSize a) + (tagsSize t) 
tagSize (SvgTag a b t)		= 11 + (attrsSize a) + (svgAttrsSize b) + (svgEltsSize t)
tagSize (TableTag a t)		= 15 + (attrsSize a) + (tagsSize t) 
tagSize (TbodyTag a t)		= 15 + (attrsSize a) + (tagsSize t) 
tagSize (TdTag a t)			=  9 + (attrsSize a) + (tagsSize t) 
tagSize (TextTag a t)		= 13 + (attrsSize a) + (tagsSize t) 
tagSize (TextareaTag a t)	= 21 + (attrsSize a) + (tagsSize t) 
tagSize (TfootTag a t)		= 15 + (attrsSize a) + (tagsSize t) 
tagSize (ThTag a t)			=  9 + (attrsSize a) + (tagsSize t) 
tagSize (TheadTag a t)		= 15 + (attrsSize a) + (tagsSize t) 
tagSize (TitleTag a t)		= 15 + (attrsSize a) + (tagsSize t) 
tagSize (TrTag a t)			=  9 + (attrsSize a) + (tagsSize t) 
tagSize (TspanTag a t)		= 15 + (attrsSize a) + (tagsSize t) 
tagSize (TtTag a t)			=  9 + (attrsSize a) + (tagsSize t) 
tagSize (UTag a t)			=  7 + (attrsSize a) + (tagsSize t) 
tagSize (UlTag a t)			=  9 + (attrsSize a) + (tagsSize t) 
tagSize (VarTag a t)		= 11 + (attrsSize a) + (tagsSize t)
tagSize (DetailsTag a t)    = 19 + (attrsSize a) + (tagsSize t)
tagSize (SummaryTag a t)    = 19 + (attrsSize a) + (tagsSize t)

tagsSize :: ![HtmlTag] -> Int
tagsSize tags = intsum tagSize tags

//Calculates the number of chars this attribute will take once serialized
attrSize :: !HtmlAttr -> Int
attrSize (AbbrAttr a)			=  8 + (escapedSize a)
attrSize (AcceptAttr a)			= 10 + (escapedSize a)
attrSize (AcceptcharsetAttr a)	= 18 + (escapedSize a)
attrSize (AccesskeyAttr a)		= 13 + (escapedSize a)
attrSize (ActionAttr a)			= 10 + (escapedSize a)
attrSize (AlignAttr a)			=  9 + (escapedSize a)
attrSize (AlinkAttr a)			=  9 + (escapedSize a)
attrSize (AltAttr a)			=  7 + (escapedSize a)
attrSize (ArchiveAttr a)		= 11 + (escapedSize a)
attrSize (AxisAttr a)			=  8 + (escapedSize a)
attrSize (BackgroundAttr a)		= 14 + (escapedSize a)
attrSize (BgcolorAttr a)		= 11 + (escapedSize a)
attrSize (BorderAttr a)			= 10 + (escapedSize a)
attrSize (CellspacingAttr a)	= 15 + (escapedSize a)
attrSize (CellpaddingAttr a)	= 15 + (escapedSize a)
attrSize (CharAttr a)			=  8 + (escapedSize a)
attrSize (CharoffAttr a)		= 11 + (escapedSize a)
attrSize (CharsetAttr a)		= 11 + (escapedSize a)
attrSize (CheckedAttr)			= 18
attrSize (CiteAttr a)			=  8 + (escapedSize a)
attrSize (ClassAttr a)			=  9 + (escapedSize a)
attrSize (ClassidAttr a)		= 11 + (escapedSize a)
attrSize (ColorAttr a)			=  9 + (escapedSize a)
attrSize (ColsAttr a)			=  8 + (escapedSize a)
attrSize (ColspanAttr a)		= 11 + (escapedSize a)
attrSize (CodebaseAttr a)		= 12 + (escapedSize a)
attrSize (CodetypeAttr a)		= 12 + (escapedSize a)
attrSize (ContentAttr a)		= 11 + (escapedSize a)
attrSize (CompactAttr)			= 18
attrSize (CoordsAttr a)			= 10 + (escapedSize a)
attrSize (DataAttr a)			=  8 + (escapedSize a)
attrSize (DatetimeAttr a)		= 12 + (escapedSize a)
attrSize (DeclareAttr)			= 18
attrSize (DeferAttr a)			=  9 + (escapedSize a)
attrSize (DirAttr a)			=  7 + (escapedSize a)
attrSize (DisabledAttr)			= 20
attrSize (DXAttr a)				=  6 + (escapedSize a)
attrSize (DYAttr a)				=  6 + (escapedSize a)
attrSize (EnctypeAttr a)		= 11 + (escapedSize a)
attrSize (FaceAttr a)			=  8 + (escapedSize a)
attrSize (ForAttr a)			=  7 + (escapedSize a)
attrSize (FrameAttr a)			=  9 + (escapedSize a)
attrSize (FrameborderAttr a)	= 15 + (escapedSize a)
attrSize (HeadersAttr a)		= 11 + (escapedSize a)
attrSize (HeightAttr a)			= 10 + (escapedSize a)
attrSize (HrefAttr a)			=  8 + (escapedSize a)
attrSize (HreflangAttr a)		= 12 + (escapedSize a)
attrSize (HttpequivAttr a)		= 14 + (escapedSize a)
attrSize (HspaceAttr a)			= 10 + (escapedSize a)
attrSize (IdAttr a)				=  6 + (escapedSize a)
attrSize (IsmapAttr)			= 14
attrSize (LabelAttr a)			=  9 + (escapedSize a)
attrSize (LangAttr a)			=  8 + (escapedSize a)
attrSize (LanguageAttr a)		= 12 + (escapedSize a)
attrSize (LinkAttr a)			=  8 + (escapedSize a)
attrSize (LongdescAttr a)		= 12 + (escapedSize a)
attrSize (MarginheightAttr a)	= 16 + (escapedSize a)
attrSize (MarginwidthAttr a)	= 15 + (escapedSize a)
attrSize (MaxlengthAttr a)		= 13 + (escapedSize a)
attrSize (MediaAttr a)			=  9 + (escapedSize a)
attrSize (MethodAttr a)			= 10 + (escapedSize a)
attrSize (MultipleAttr)			= 20
attrSize (NameAttr a)			=  8 + (escapedSize a)
attrSize (NohrefAttr)			= 16
attrSize (NoshadeAttr)			= 18
attrSize (NowrapAttr)			= 16
attrSize (OnblurAttr a)			= 10 + (escapedSize a)
attrSize (OnchangeAttr a)		= 12 + (escapedSize a)
attrSize (OnclickAttr a)		= 11 + (escapedSize a)
attrSize (OndblclickAttr a)		= 14 + (escapedSize a)
attrSize (OnfocusAttr a)		= 11 + (escapedSize a)
attrSize (OnloadAttr a)			= 10 + (escapedSize a)
attrSize (OnmousedownAttr a)	= 15 + (escapedSize a)
attrSize (OnmousemoveAttr a)	= 15 + (escapedSize a)
attrSize (OnmouseoutAttr a)		= 14 + (escapedSize a)
attrSize (OnmouseoverAttr a)	= 15 + (escapedSize a)
attrSize (OnmouseupAttr a)		= 13 + (escapedSize a)
attrSize (OnkeydownAttr a)		= 13 + (escapedSize a)
attrSize (OnkeypressAttr a)		= 14 + (escapedSize a)
attrSize (OnkeyupAttr a)		= 11 + (escapedSize a)
attrSize (OnresetAttr a)		= 11 + (escapedSize a)
attrSize (OnselectAttr a)		= 12 + (escapedSize a)
attrSize (OnsubmitAttr a)		= 12 + (escapedSize a)
attrSize (OnunloadAttr a)		= 12 + (escapedSize a)
attrSize (OntoggleAttr a)		= 12 + (escapedSize a)
attrSize (OpenAttr a)           =  8 + (escapedSize a)
attrSize (PlaceholderAttr a)	= 15 + (escapedSize a)
attrSize (ProfileAttr a)		= 11 + (escapedSize a)
attrSize (PromptAttr a)			= 10 + (escapedSize a)
attrSize (ReadonlyAttr)			= 20
attrSize (RelAttr a)			=  7 + (escapedSize a)
attrSize (RevAttr a)			=  7 + (escapedSize a)
attrSize (RowsAttr a)			=  8 + (escapedSize a)
attrSize (RowspanAttr a)		= 11 + (escapedSize a)
attrSize (RulesAttr a)			=  9 + (escapedSize a)
attrSize (RXAttr a)				=  6 + (escapedSize a)
attrSize (RYAttr a)				=  6 + (escapedSize a)
attrSize (SchemeAttr a)			= 10 + (escapedSize a)
attrSize (ScopeAttr a)			=  9 + (escapedSize a)
attrSize (ScrollingAttr a)		= 13 + (escapedSize a)
attrSize (SelectedAttr)			= 20
attrSize (ShapeAttr a)			=  9 + (escapedSize a)
attrSize (SizeAttr a)			=  8 + (escapedSize a)
attrSize (SpanAttr a)			=  8 + (escapedSize a)
attrSize (SrcAttr a)			=  7 + (escapedSize a)
attrSize (StandbyAttr a)		= 11 + (escapedSize a)
attrSize (StartAttr a)			=  9 + (escapedSize a)
attrSize (StyleAttr a)			=  9 + (escapedSize a)
attrSize (SummaryAttr a)		= 11 + (escapedSize a)
attrSize (TabindexAttr a)		= 12 + (escapedSize a)
attrSize (TargetAttr a)			= 10 + (escapedSize a)
attrSize (TextAttr a)			=  8 + (escapedSize a)
attrSize (TitleAttr a)			=  9 + (escapedSize a)
attrSize (TypeAttr a)			=  8 + (escapedSize a)
attrSize (UsemapAttr a)			= 10 + (escapedSize a)
attrSize (ValignAttr a)			= 10 + (escapedSize a)
attrSize (ValueAttr a)			=  9 + (escapedSize a)
attrSize (ValuetypeAttr a)		= 13 + (escapedSize a)
attrSize (VlinkAttr a)			=  9 + (escapedSize a)
attrSize (VspaceAttr a)			= 10 + (escapedSize a)
attrSize (WidthAttr a)			=  9 + (escapedSize a)
attrSize (XmllangAttr a)		= 12 + (escapedSize a)
attrSize (XmlnsAttr a)			=  9 + (escapedSize a)
attrSize (XmlnsXlinkAttr a)		= 15 + (escapedSize a)
attrSize (XmlspaceAttr a)		= 13 + (escapedSize a)

attrsSize :: ![HtmlAttr] -> Int
attrsSize attrs = intsum attrSize attrs

//Calculates the number of chars in a string when html special characters are escaped
escapedSize :: !{#Char} -> Int
escapedSize s = escapedSize` s (size s) 0
where
  escapedSize` :: !{#Char} !Int !Int -> Int
  escapedSize` s n i
    | i == n = 0
    | s.[i] == '<' = 4 + escapedSize` s n (i + 1)
    | s.[i] == '>' = 4 + escapedSize` s n (i + 1)
    | s.[i] == '&' = 5 + escapedSize` s n (i + 1)
    | otherwise = 1 + escapedSize` s n (i + 1)

serializeTag :: !HtmlTag !*{#Char} !Int -> (!*{#Char}, !Int)
serializeTag (Text t) s i				= copyChars t 0 True s i
serializeTag (Html t) s i				= copyChars t 0 False s i
serializeTag (ATag a t) s i				= writeTag "a" a t s i
serializeTag (AbbrTag a t) s i			= writeTag "abbr" a t s i
serializeTag (AcronymTag a t) s i		= writeTag "acronym" a t s i
serializeTag (AddressTag a t) s i		= writeTag "address" a t s i
serializeTag (AppletTag a t) s i		= writeTag "applet" a t s i
serializeTag (AreaTag a) s i			= writeEmptyTag "area" a s i
serializeTag (BTag a t) s i				= writeTag "b" a t s i
serializeTag (BaseTag a t) s i			= writeTag "base" a t s i
serializeTag (BasefontTag a) s i		= writeEmptyTag "basefont" a s i
serializeTag (BdoTag a t) s i			= writeTag "bdo" a t s i
serializeTag (BigTag a t) s i			= writeTag "big" a t s i
serializeTag (BlockquoteTag a t) s i	= writeTag "blockquote" a t s i
serializeTag (BodyTag a t) s i			= writeTag "body" a t s i
serializeTag (BrTag a) s i				= writeEmptyTag "br" a s i
serializeTag (ButtonTag a t) s i		= writeTag "button" a t s i
serializeTag (CanvasTag a t) s i		= writeTag "canvas" a t s i
serializeTag (CaptionTag a t) s i		= writeTag "caption" a t s i
serializeTag (CenterTag a t) s i		= writeTag "center" a t s i
serializeTag (CircleTag a) s i			= writeEmptyTag "circle" a s i
serializeTag (CiteTag a t) s i			= writeTag "cite" a t s i
serializeTag (CodeTag a t) s i			= writeTag "code" a t s i
serializeTag (ColTag a t) s i			= writeTag "col" a t s i
serializeTag (ColgroupTag a t) s i		= writeTag "colgroup" a t s i
serializeTag (DdTag a t) s i			= writeTag "dd" a t s i
serializeTag (DelTag a t) s i			= writeTag "del" a t s i
serializeTag (DfnTag a t) s i			= writeTag "dfn" a t s i
serializeTag (DirTag a t) s i			= writeTag "dir" a t s i
serializeTag (DivTag a t) s i			= writeTag "div" a t s i
serializeTag (DlTag a t) s i			= writeTag "dl" a t s i
serializeTag (DtTag a t) s i			= writeTag "dt" a t s i
serializeTag (EllipseTag a) s i			= writeEmptyTag "ellipse" a s i
serializeTag (EmTag a t) s i			= writeTag "em" a t s i
serializeTag (FieldsetTag a t) s i		= writeTag "fieldset" a t s i
serializeTag (FontTag a t) s i			= writeTag "font" a t s i
serializeTag (FormTag a t) s i			= writeTag "form" a t s i
//serializeTag (GTag a t) s i				= writeTag "g" a t s i
serializeTag (H1Tag a t) s i			= writeTag "h1" a t s i
serializeTag (H2Tag a t) s i			= writeTag "h2" a t s i
serializeTag (H3Tag a t) s i			= writeTag "h3" a t s i
serializeTag (H4Tag a t) s i			= writeTag "h4" a t s i
serializeTag (H5Tag a t) s i			= writeTag "h5" a t s i
serializeTag (H6Tag a t) s i			= writeTag "h6" a t s i
serializeTag (HeadTag a t) s i			= writeTag "head" a t s i
serializeTag (HrTag a) s i				= writeEmptyTag "hr" a s i
serializeTag (HtmlTag a t) s i			= writeRootTag a t s i
serializeTag (ITag a t) s i				= writeTag "i" a t s i
serializeTag (IframeTag a t) s i		= writeTag "iframe" a t s i
serializeTag (ImgTag a) s i				= writeEmptyTag "img" a s i
serializeTag (InputTag a) s i			= writeEmptyTag "input" a s i
serializeTag (InsTag a t) s i			= writeTag "ins" a t s i
serializeTag (IsindexTag a) s i			= writeEmptyTag "index" a s i
serializeTag (KdbTag a t) s i			= writeTag "kdb" a t s i
serializeTag (LabelTag a t) s i			= writeTag "label" a t s i
serializeTag (LegendTag a t) s i		= writeTag "legend" a t s i
serializeTag (LiTag a t) s i			= writeTag "li" a t s i
//serializeTag (LineTag a) s i			= writeEmptyTag "line" a s i
serializeTag (LinkTag a) s i			= writeEmptyTag "link" a s i
serializeTag (MapTag a t) s i			= writeTag "map" a t s i
serializeTag (MenuTag a t) s i			= writeTag "menu" a t s i
serializeTag (MetaTag a) s i			= writeEmptyTag "meta" a s i
serializeTag (NoframesTag a t) s i		= writeTag "noframes" a t s i
serializeTag (NoscriptTag a t) s i		= writeTag "noscript" a t s i
serializeTag (ObjectTag a t) s i		= writeTag "object" a t s i
serializeTag (OlTag a t) s i			= writeTag "ol" a t s i
serializeTag (OptgroupTag a t) s i		= writeTag "optgroup" a t s i
serializeTag (OptionTag a t) s i		= writeTag "option" a t s i
serializeTag (PTag a t) s i				= writeTag "p" a t s i
serializeTag (ParamTag a t) s i			= writeTag "param" a t s i
serializeTag (PreTag a t) s i			= writeTag "pre" a t s i
serializeTag (QTag a t) s i				= writeTag "q" a t s i
serializeTag (RectTag a) s i			= writeEmptyTag "rect" a s i
serializeTag (STag a t) s i				= writeTag "s" a t s i
serializeTag (SampTag a t) s i			= writeTag "samp" a t s i
serializeTag (ScriptTag a t) s i		= writeTag "script" a t s i
serializeTag (SelectTag a t) s i		= writeTag "select" a t s i
serializeTag (SmallTag a t) s i			= writeTag "small" a t s i
serializeTag (SpanTag a t) s i			= writeTag "span" a t s i
serializeTag (StrikeTag a t) s i		= writeTag "strike" a t s i
serializeTag (StrongTag a t) s i		= writeTag "strong" a t s i
serializeTag (StyleTag a t) s i			= writeTag "style" a t s i
serializeTag (SubTag a t) s i			= writeTag "sub" a t s i
serializeTag (SupTag a t) s i			= writeTag "sup" a t s i
serializeTag (SvgTag a b t) s i			= writeSVGTag "svg" a b t s i
serializeTag (TableTag a t) s i			= writeTag "table" a t s i
serializeTag (TbodyTag a t) s i			= writeTag "tbody" a t s i
serializeTag (TdTag a t) s i			= writeTag "td" a t s i
serializeTag (TextTag a t) s i			= writeTag "text" a t s i
serializeTag (TextareaTag a t) s i		= writeTag "textarea" a t s i
serializeTag (TfootTag a t) s i			= writeTag "tfoot" a t s i
serializeTag (ThTag a t) s i			= writeTag "th" a t s i
serializeTag (TheadTag a t) s i			= writeTag "thead" a t s i
serializeTag (TitleTag a t) s i			= writeTag "title" a t s i
serializeTag (TrTag a t) s i			= writeTag "tr" a t s i
serializeTag (TspanTag a t) s i			= writeTag "tspan" a t s i
serializeTag (TtTag a t) s i			= writeTag "tt" a t s i
serializeTag (UTag a t) s i				= writeTag "u" a t s i
serializeTag (UlTag a t) s i			= writeTag "ul" a t s i
serializeTag (VarTag a t) s i			= writeTag "var" a t s i
serializeTag (DetailsTag a t) s i       = writeTag "details" a t s i
serializeTag (SummaryTag a t) s i       = writeTag "summary" a t s i

serializeTags :: ![HtmlTag] !*{#Char} !Int -> (!*{#Char}, !Int)
serializeTags [] dest dest_i = (dest,dest_i)
serializeTags [x:xs] dest dest_i
	#! (dest, dest_i) = serializeTag x dest dest_i
	= serializeTags xs dest dest_i

serializeAttr :: !HtmlAttr !*{#Char} !Int -> (!*{#Char}, !Int)
serializeAttr (AbbrAttr a) s i			= writeAttr "abbr" a s i
serializeAttr (AcceptAttr a) s i		= writeAttr "accept" a s i
serializeAttr (AcceptcharsetAttr a) s i	= writeAttr "accept-charset" a s i
serializeAttr (AccesskeyAttr a) s i		= writeAttr "accesskey" a s i
serializeAttr (ActionAttr a) s i		= writeAttr "action" a s i
serializeAttr (AlignAttr a) s i			= writeAttr "align" a s i
serializeAttr (AlinkAttr a) s i			= writeAttr "alink" a s i
serializeAttr (AltAttr a) s i			= writeAttr "alt" a s i
serializeAttr (ArchiveAttr a) s i		= writeAttr "archive" a s i
serializeAttr (AxisAttr a) s i			= writeAttr "axis" a s i
serializeAttr (BackgroundAttr a) s i	= writeAttr "background" a s i
serializeAttr (BgcolorAttr a) s i		= writeAttr "bgcolor" a s i
serializeAttr (BorderAttr a) s i		= writeAttr "border" a s i
serializeAttr (CellspacingAttr a) s i	= writeAttr "cellspacing" a s i
serializeAttr (CellpaddingAttr a) s i	= writeAttr "cellpadding" a s i
serializeAttr (CharAttr a) s i			= writeAttr "char" a s i
serializeAttr (CharoffAttr a) s i		= writeAttr "charoff" a s i
serializeAttr (CharsetAttr a) s i		= writeAttr "charset" a s i
serializeAttr (CheckedAttr) s i			= writeAttr "checked" "checked" s i
serializeAttr (CiteAttr a) s i			= writeAttr "cite" a s i
serializeAttr (ClassAttr a) s i			= writeAttr "class" a s i
serializeAttr (ClassidAttr a) s i		= writeAttr "classid" a s i
serializeAttr (ColorAttr a) s i			= writeAttr "color" a s i
serializeAttr (ColsAttr a) s i			= writeAttr "cols" a s i
serializeAttr (ColspanAttr a) s i		= writeAttr "colspan" a s i
serializeAttr (CodebaseAttr a) s i		= writeAttr "codebase" a s i
serializeAttr (CodetypeAttr a) s i		= writeAttr "codetype" a s i
serializeAttr (ContentAttr a) s i		= writeAttr "content" a s i
serializeAttr (CompactAttr) s i			= writeAttr "compact" "compact" s i
serializeAttr (CoordsAttr a) s i		= writeAttr "coords" a s i
serializeAttr (DataAttr a) s i			= writeAttr "data" a s i
serializeAttr (DatetimeAttr a) s i		= writeAttr "datetime" a s i
serializeAttr (DeclareAttr) s i			= writeAttr "declare" "declare" s i
serializeAttr (DeferAttr a) s i			= writeAttr "defer" a s i
serializeAttr (DirAttr a) s i			= writeAttr "dir" a s i
serializeAttr (DisabledAttr) s i		= writeAttr "disabled" "disabled" s i
serializeAttr (DXAttr a) s i			= writeAttr "dx" a s i
serializeAttr (DYAttr a) s i			= writeAttr "dy" a s i
serializeAttr (EnctypeAttr a) s i		= writeAttr "enctype" a s i
serializeAttr (FaceAttr a) s i			= writeAttr "face" a s i
serializeAttr (ForAttr a) s i			= writeAttr "for" a s i
serializeAttr (FrameAttr a) s i			= writeAttr "frame" a s i
serializeAttr (FrameborderAttr a) s i	= writeAttr "frameborder" a s i
serializeAttr (HeadersAttr a) s i		= writeAttr "headers" a s i
serializeAttr (HeightAttr a) s i		= writeAttr "height" a s i
serializeAttr (HrefAttr a) s i			= writeAttr "href" a s i
serializeAttr (HreflangAttr a) s i		= writeAttr "hreflang" a s i
serializeAttr (HttpequivAttr a) s i		= writeAttr "http-equiv" a s i
serializeAttr (HspaceAttr a) s i		= writeAttr "hspace" a s i
serializeAttr (IdAttr a) s i			= writeAttr "id" a s i
serializeAttr (IsmapAttr) s i			= writeAttr "ismap" "ismap" s i
serializeAttr (LabelAttr a) s i			= writeAttr "label" a s i
serializeAttr (LangAttr a) s i			= writeAttr "lang" a s i
serializeAttr (LanguageAttr a) s i		= writeAttr "language" a s i
serializeAttr (LinkAttr a) s i			= writeAttr "link" a s i
serializeAttr (LongdescAttr a) s i		= writeAttr "longdesc" a s i
serializeAttr (MarginheightAttr a) s i	= writeAttr "marginheight" a s i
serializeAttr (MarginwidthAttr a) s i	= writeAttr "marginwidth" a s i
serializeAttr (MaxlengthAttr a) s i		= writeAttr "maxlength" a s i
serializeAttr (MediaAttr a) s i			= writeAttr "media" a s i
serializeAttr (MethodAttr a) s i		= writeAttr "method" a s i
serializeAttr (MultipleAttr) s i		= writeAttr "multiple" "multiple" s i
serializeAttr (NameAttr a) s i			= writeAttr "name" a s i
serializeAttr (NohrefAttr) s i			= writeAttr "nohref" "nohref" s i
serializeAttr (NoshadeAttr) s i			= writeAttr "noshade" "noshade" s i
serializeAttr (NowrapAttr) s i			= writeAttr "nowrap" "nowrap" s i
serializeAttr (OnblurAttr a) s i		= writeAttr "onblur" a s i
serializeAttr (OnchangeAttr a) s i		= writeAttr "onchange" a s i
serializeAttr (OnclickAttr a) s i		= writeAttr "onclick" a s i
serializeAttr (OndblclickAttr a) s i	= writeAttr "ondblclick" a s i
serializeAttr (OnfocusAttr a) s i		= writeAttr "onfocus" a s i
serializeAttr (OnloadAttr a) s i		= writeAttr "onload" a s i
serializeAttr (OnmousedownAttr a) s i	= writeAttr "onmousedown" a s i
serializeAttr (OnmousemoveAttr a) s i	= writeAttr "onmousemove" a s i
serializeAttr (OnmouseoutAttr a) s i	= writeAttr "onmouseout" a s i
serializeAttr (OnmouseoverAttr a) s i	= writeAttr "onmouseover" a s i
serializeAttr (OnmouseupAttr a) s i		= writeAttr "onmouseup" a s i
serializeAttr (OnkeydownAttr a) s i		= writeAttr "onkeydown" a s i
serializeAttr (OnkeypressAttr a) s i	= writeAttr "onkeypress" a s i
serializeAttr (OnkeyupAttr a) s i		= writeAttr "onkeyup" a s i
serializeAttr (OnresetAttr a) s i		= writeAttr "onreset" a s i
serializeAttr (OnselectAttr a) s i		= writeAttr "onselect" a s i
serializeAttr (OnsubmitAttr a) s i		= writeAttr "onsubmit" a s i
serializeAttr (OnunloadAttr a) s i		= writeAttr "onunload" a s i
serializeAttr (OntoggleAttr a) s i		= writeAttr "ontoggle" a s i
serializeAttr (OpenAttr a) s i          = writeAttr "open" a s i
serializeAttr (PlaceholderAttr a) s i	= writeAttr "placeholder" a s i
serializeAttr (ProfileAttr a) s i		= writeAttr "profile" a s i
serializeAttr (PromptAttr a) s i		= writeAttr "prompt" a s i
serializeAttr (ReadonlyAttr) s i		= writeAttr "readonly" "readonly" s i
serializeAttr (RelAttr a) s i			= writeAttr "rel" a s i
serializeAttr (RevAttr a) s i			= writeAttr "rev" a s i
serializeAttr (RowsAttr a) s i			= writeAttr "rows" a s i
serializeAttr (RowspanAttr a) s i		= writeAttr "rowspan" a s i
serializeAttr (RulesAttr a) s i			= writeAttr "rules" a s i
serializeAttr (RXAttr a) s i			= writeAttr "rx" a s i
serializeAttr (RYAttr a) s i			= writeAttr "ry" a s i
serializeAttr (SchemeAttr a) s i		= writeAttr "scheme" a s i
serializeAttr (ScopeAttr a) s i			= writeAttr "scope" a s i
serializeAttr (ScrollingAttr a) s i		= writeAttr "scrolling" a s i
serializeAttr (SelectedAttr) s i		= writeAttr "selected" "selected" s i
serializeAttr (ShapeAttr a) s i			= writeAttr "shape" a s i
serializeAttr (SizeAttr a) s i			= writeAttr "size" a s i
serializeAttr (SpanAttr a) s i			= writeAttr "span" a s i
serializeAttr (SrcAttr a) s i			= writeAttr "src" a s i
serializeAttr (StandbyAttr a) s i		= writeAttr "standby" a s i
serializeAttr (StartAttr a) s i			= writeAttr "start" a s i
serializeAttr (StyleAttr a) s i			= writeAttr "style" a s i
serializeAttr (SummaryAttr a) s i		= writeAttr "summary" a s i
serializeAttr (TabindexAttr a) s i		= writeAttr "tabindex" a s i
serializeAttr (TargetAttr a) s i		= writeAttr "target" a s i
serializeAttr (TextAttr a) s i			= writeAttr "text" a s i
serializeAttr (TitleAttr a) s i			= writeAttr "title" a s i
serializeAttr (TypeAttr a) s i			= writeAttr "type" a s i
serializeAttr (UsemapAttr a) s i		= writeAttr "usemap" a s i
serializeAttr (ValignAttr a) s i		= writeAttr "valign" a s i
serializeAttr (ValueAttr a) s i			= writeAttr "value" a s i
serializeAttr (ValuetypeAttr a) s i		= writeAttr "valuetype" a s i
serializeAttr (VlinkAttr a) s i			= writeAttr "vlink" a s i
serializeAttr (VspaceAttr a) s i		= writeAttr "vspace" a s i
serializeAttr (WidthAttr a) s i			= writeAttr "width" a s i
serializeAttr (XmllangAttr a) s i		= writeAttr "xml:lang" a s i
serializeAttr (XmlnsAttr a) s i			= writeAttr "xmlns" a s i
serializeAttr (XmlnsXlinkAttr a) s i	= writeAttr "xmlns:xlink" a s i
serializeAttr (XmlspaceAttr a) s i		= writeAttr "xml:space" a s i

serializeAttrs :: ![HtmlAttr] !*{#Char} !Int -> (!*{#Char}, !Int)
serializeAttrs [] dest dest_i = (dest, dest_i)
serializeAttrs [x:xs] dest dest_i
	#! (dest, dest_i) = serializeAttr x dest dest_i
	= serializeAttrs xs dest dest_i

copyChars :: !{#Char} !Int !Bool !*{#Char} !Int -> (!*{#Char},!Int)
copyChars src src_i escape dest dest_i
	| src_i == size src = (dest, dest_i)
	| otherwise	
		#! src_src_i = src.[src_i]
		| not escape
				#! dest = {dest & [dest_i] = src_src_i}
				= copyChars src (src_i + 1) escape dest (dest_i + 1)
		| otherwise	
			| src_src_i == '<'
				#! dest = {dest & [dest_i] = '&', [dest_i + 1] = 'l', [dest_i + 2] = 't', [dest_i + 3] = ';'}
				= copyChars src (src_i + 1) escape dest (dest_i + 4)
			| src_src_i == '>'
				#! dest = {dest & [dest_i] = '&', [dest_i + 1] = 'g', [dest_i + 2] = 't', [dest_i + 3] = ';'}
				= copyChars src (src_i + 1) escape dest (dest_i + 4)
			| src_src_i == '&'
				#! dest = {dest & [dest_i] = '&', [dest_i + 1] = 'a', [dest_i + 2] = 'm', [dest_i + 3] = 'p', [dest_i + 4] = ';'}
				= copyChars src (src_i + 1) escape dest (dest_i + 5)
			| otherwise
				#! dest = {dest & [dest_i] = src_src_i}
				= copyChars src (src_i + 1) escape dest (dest_i + 1)

writeTag :: !{#Char} ![HtmlAttr] ![HtmlTag] !*{#Char} !Int -> (!*{#Char},!Int)
writeTag name attrs tags dest dest_i
	#! dest = {dest & [dest_i] = '<'}
	#! dest_i = dest_i + 1
	#! (dest,dest_i) = copyChars name 0 False dest dest_i
	#! (dest, dest_i) = serializeAttrs attrs dest dest_i
	#! dest = {dest & [dest_i] = '>'}
	#! dest_i = dest_i + 1
	#! (dest, dest_i) = serializeTags tags dest dest_i
	#! dest = {dest & [dest_i] = '<'}
	#! dest = {dest & [dest_i + 1] = '/'}
	#! dest_i = dest_i + 2
	#! (dest,dest_i) = copyChars name 0 False dest dest_i
	#! dest = {dest & [dest_i] = '>'}
	#! dest_i = dest_i + 1
	= (dest,dest_i)

writeEmptyTag :: !{#Char} ![HtmlAttr] !*{#Char} !Int -> (!*{#Char},!Int)
writeEmptyTag name attrs dest dest_i
	#! dest = {dest & [dest_i] = '<'}
	#! dest_i = dest_i + 1
	#! (dest,dest_i) = copyChars name 0 False dest dest_i
	#! (dest, dest_i) = serializeAttrs attrs dest dest_i
	#! dest = {dest & [dest_i] = '/'}
	#! dest_i = dest_i + 1
	#! dest = {dest & [dest_i] = '>'}
	#! dest_i = dest_i + 1
	= (dest,dest_i)

writeRootTag :: ![HtmlAttr] ![HtmlTag] !*{#Char} !Int -> (!*{#Char},!Int)
writeRootTag attrs tags dest dest_i
	#! (dest,dest_i) = copyChars "<html" 0 False dest dest_i
	#! (dest, dest_i) = serializeAttrs attrs dest dest_i
	#! dest = {dest & [dest_i] = '>'}
	#! dest_i = dest_i + 1
	#! (dest, dest_i) = serializeTags tags dest dest_i
	#! (dest, dest_i) = copyChars "</html>" 0 False dest dest_i
	= (dest,dest_i)

writeAttr :: !{#Char} !{#Char} !*{#Char} !Int -> (!*{#Char},!Int)
writeAttr name value dest dest_i
	#! dest = {dest & [dest_i] = ' '}
	#! dest_i = dest_i + 1
	#! (dest,dest_i) = copyChars name 0 False dest dest_i
	#! dest = {dest & [dest_i] = '='}
	#! dest_i = dest_i + 1
	#! dest = {dest & [dest_i] = '"'}
	#! dest_i = dest_i + 1
	#! (dest,dest_i) = copyChars value 0 True dest dest_i
	#! dest = {dest & [dest_i] = '"'}
	#! dest_i = dest_i + 1
	= (dest,dest_i)

class html a 
where
	html :: !a -> HtmlTag
	
instance html String
where
	html s = Text s

instance html HtmlTag
where
	html h = h
	
instance html [a] | html a
where
	html [h]	= html h
	html h		= SpanTag [] (map html h)
		
instance html (Maybe a) | html a
where
	html Nothing	= SpanTag [] []
	html (Just h)	= html h


/* handling SVG elements and attributes:
*/

instance toString SVGElt
where
	toString elt
		#! eltsize	= svgEltSize elt						//Calculate the size of the string we need
		#! eltstring	= createArray eltsize '\0'				//Create an empty buffer
		#! eltstring	= fst (serializeSVGElt elt eltstring 0)	//Serialize the SVG element
		= eltstring

svgEltsSize :: ![SVGElt] -> Int
svgEltsSize elts = intsum svgEltSize elts

svgEltSize :: !SVGElt -> Int
svgEltSize (SVGElt            html_attrs svg_attrs elts) = 11 + attrsSize html_attrs + svgAttrsSize svg_attrs + svgEltsSize elts
svgEltSize (ClipPathElt       html_attrs svg_attrs elts) = 21 + attrsSize html_attrs + svgAttrsSize svg_attrs + svgEltsSize elts
svgEltSize (CircleElt         html_attrs svg_attrs)      =  9 + attrsSize html_attrs + svgAttrsSize svg_attrs
svgEltSize (DefsElt           html_attrs svg_attrs elts) = 13 + attrsSize html_attrs + svgAttrsSize svg_attrs + svgEltsSize elts
svgEltSize (EllipseElt        html_attrs svg_attrs)      = 10 + attrsSize html_attrs + svgAttrsSize svg_attrs
svgEltSize (GElt              html_attrs svg_attrs elts) =  7 + attrsSize html_attrs + svgAttrsSize svg_attrs + svgEltsSize elts
svgEltSize (ImageElt          html_attrs svg_attrs elts) = 15 + attrsSize html_attrs + svgAttrsSize svg_attrs + svgEltsSize elts
svgEltSize (LinearGradientElt html_attrs svg_attrs elts) = 33 + attrsSize html_attrs + svgAttrsSize svg_attrs + svgEltsSize elts
svgEltSize (LineElt           html_attrs svg_attrs)      =  7 + attrsSize html_attrs + svgAttrsSize svg_attrs
svgEltSize (MarkerElt         html_attrs svg_attrs elts) = 17 + attrsSize html_attrs + svgAttrsSize svg_attrs + svgEltsSize elts
svgEltSize (MaskElt           html_attrs svg_attrs elts) = 13 + attrsSize html_attrs + svgAttrsSize svg_attrs + svgEltsSize elts
svgEltSize (PathElt           html_attrs svg_attrs)      =  7 + attrsSize html_attrs + svgAttrsSize svg_attrs
svgEltSize (PolygonElt        html_attrs svg_attrs)      = 10 + attrsSize html_attrs + svgAttrsSize svg_attrs
svgEltSize (PolylineElt       html_attrs svg_attrs)      = 11 + attrsSize html_attrs + svgAttrsSize svg_attrs
svgEltSize (RadialGradientElt html_attrs svg_attrs elts) = 33 + attrsSize html_attrs + svgAttrsSize svg_attrs + svgEltsSize elts
svgEltSize (RectElt           html_attrs svg_attrs)      =  7 + attrsSize html_attrs + svgAttrsSize svg_attrs
svgEltSize (StopElt           html_attrs svg_attrs)      =  7 + attrsSize html_attrs + svgAttrsSize svg_attrs
svgEltSize (TextElt           html_attrs svg_attrs text) = 13 + attrsSize html_attrs + svgAttrsSize svg_attrs + escapedSize text
svgEltSize (RawElt                                 text) = size text
svgEltSize _                                             = abort "Text.HTML: svgEltSize applied to unexpected argument.\n"

svgAttrsSize :: ![SVGAttr] -> Int
svgAttrsSize attrs = intsum svgAttrSize attrs

svgAttrSize :: !SVGAttr -> Int
svgAttrSize (AlignmentBaselineAttr   bl)	    	= 22 + size bl
svgAttrSize (BaseProfileAttr         profile)		= 15 + size profile
svgAttrSize (ContentScriptTypeAttr   type)			= 23 + size type
svgAttrSize (ClipPathAttr            cp)			= 13 + size cp
svgAttrSize (CxAttr                  cx)			=  6 + svgLengthSize cx
svgAttrSize (CyAttr                  cy)			=  6 + svgLengthSize cy
svgAttrSize (DominantBaselineAttr    dbl)			= 21 + size dbl
svgAttrSize (ExternalResourcesRequiredAttr b)		= 29 + svgBoolSize b
svgAttrSize (FillAttr                paint)			=  8 + svgPaintSize paint
svgAttrSize (FillOpacityAttr         opac)			= 16 + svgFillOpacitySize opac
svgAttrSize (FillRuleAttr            rule)			= 13 + svgFillRuleSize rule
svgAttrSize (FontFamilyAttr          family)        = 15 + size family
svgAttrSize (FontSizeAttr            s)             = 13 + size s
svgAttrSize (FontStyleAttr           s)				= 14 + size s
svgAttrSize (FontStretchAttr         s)				= 16 + size s
svgAttrSize (FontVariantAttr         s)				= 16 + size s
svgAttrSize (FontWeightAttr          s)				= 15 + size s
svgAttrSize (LengthAdjustAttr        adjust)		= 16 + svgLengthAdjustSize adjust
svgAttrSize (MarkerStartAttr         m)				= 16 + size m
svgAttrSize (MarkerMidAttr           m)				= 14 + size m
svgAttrSize (MarkerEndAttr           m)				= 14 + size m
svgAttrSize (MarkerHeightAttr        m)				= 16 + svgLengthSize m
svgAttrSize (MarkerWidthAttr         m)				= 15 + svgLengthSize m
svgAttrSize (MaskAttr                m)				=  8 + size m
svgAttrSize (OffsetAttr              offset)		= 10 + size offset
svgAttrSize (OrientAttr              orient)		= 10 + size orient
svgAttrSize (PointsAttr              points)		= 10 + (foldr (\(x, y) acc -> size x + size y + acc + 1) 0 points) + (length points - 1)
svgAttrSize (PreserveAspectRatioAttr md ma mms)		= 23 + case md of
															Just SVGDefer = 5	// "defer"
															nothing       = 0
													     + case ma of
													        Nothing       = 5	// " none"
													        align         = 9	// " x{Min,Mid,Max}Y{Min,Mid,Max}"
													     + case mms of
													     	Just mos      = 1 + svgMeetOrSliceSize mos
													     	nothing       = 0
svgAttrSize (RAttr                   length)		=  5 + svgLengthSize length
svgAttrSize (RefXAttr                length)		=  8 + svgLengthSize length
svgAttrSize (RefYAttr                length)		=  8 + svgLengthSize length
svgAttrSize (RxAttr                  length)		=  6 + svgLengthSize length
svgAttrSize (RyAttr                  length)		=  6 + svgLengthSize length
svgAttrSize (StopColorAttr           color)			= 14 + size color
svgAttrSize (StopOpacityAttr         opacity)		= 16 + size opacity
svgAttrSize (StrokeAttr              paint)			= 10 + svgPaintSize  paint
svgAttrSize (StrokeDashArrayAttr     da)			= 20 + svgStrokeDashArraySize  da
svgAttrSize (StrokeDashOffsetAttr    do)			= 21 + svgStrokeDashOffsetSize do
svgAttrSize (StrokeLineCapAttr       cap)			= 18 + svgLineCapSize cap
svgAttrSize (StrokeLineJoinAttr      join)			= 19 + svgLineJoinSize join
svgAttrSize (StrokeMiterLimitAttr    limit)			= 21 + svgMiterLimitSize limit
svgAttrSize (StrokeOpacityAttr       opac)			= 18 + size opac
svgAttrSize (StrokeWidthAttr         width)			= 16 + svgStrokeWidthSize width
svgAttrSize (TextAnchorAttr          anchor)		= 15 + size anchor
svgAttrSize (TextLengthAttr          length)		= 14 + svgLengthSize length
svgAttrSize (TextRenderingAttr       rendering)		= 18 + size rendering
svgAttrSize (TransformAttr           ts)			= 13 + svgTransformsSize ts
svgAttrSize (VersionAttr             v)				= 11 + size v
svgAttrSize (ViewBoxAttr             x y w h)		= 11 + svgNumbersSize [x,y,w,h]
svgAttrSize (XAttr                   x)				=  5 + svgLengthSize x
svgAttrSize (X1Attr                  x1)			=  6 + svgLengthSize x1
svgAttrSize (X2Attr                  x2)			=  6 + svgLengthSize x2
svgAttrSize (XLinkHRefAttr           iri)			= 16 + size iri
svgAttrSize (YAttr                   y)				=  5 + svgLengthSize y
svgAttrSize (Y1Attr                  y1)			=  6 + svgLengthSize y1
svgAttrSize (Y2Attr                  y2)			=  6 + svgLengthSize y2
svgAttrSize (ZoomAndPanAttr          zp)			= 14 + 7					// "{disable,magnify}"
svgAttrSize _										= abort "Text.HTML: svgAttrSize applied to unexpected argument.\n"

instance toString SVGAlign where
	toString XMinYMin								= "xMinYMin"
	toString XMidYMin								= "xMidYMin"
	toString XMaxYMin								= "xMaxYMin"
	toString XMinYMid								= "xMinYMid"
	toString XMidYMid								= "xMidYMid"
	toString XMaxYMid								= "xMaxYMid"
	toString XMinYMax								= "xMinYMax"
	toString XMidYMax								= "xMidYMax"
	toString XMaxYMax								= "xMaxYMax"

svgBoolSize :: !Bool -> Int
svgBoolSize True									= 4							// "true"
svgBoolSize false									= 5 						// "false"

svgColorSize :: !SVGColor -> Int
svgColorSize (SVGRGB r g b)							= 7 + intsum (size o toString) [r,g,b]
svgColorSize (SVGColorText name)					= size name

instance toString SVGColor where
	toString (SVGRGB r g b)							= "rgb(" +++ glue_list "," (map toString [r,g,b]) +++ ")"
	toString (SVGColorText name)					= name

svgICCColorSize :: !SVGICCColor -> Int
svgICCColorSize (profile,values)					= 12 + size profile + svgNumbersSize values

instance toString SVGICCColor where
	toString (profile,values)						= "icc-color(" +++ profile +++ " " +++ glue_list " " (map toString values) +++ ")"

svgFillOpacitySize :: !SVGFillOpacity -> Int
svgFillOpacitySize (FillOpacity nr)					= size nr					// number
svgFillOpacitySize FillOpacityInherit				= 7							// "inherit"

instance toString SVGFillOpacity where
	toString (FillOpacity nr)						= nr
	toString FillOpacityInherit						= "inherit"

svgFillRuleSize :: !SVGFillRule -> Int
svgFillRuleSize _									= 7							// "{nonzero,evenodd,inherit}"

instance toString SVGFillRule where
	toString FillNonzero							= "nonzero"
	toString FillEvenodd							= "evenodd"
	toString FillInherit							= "inherit"

svgFuncIRISize :: !SVGFuncIRI -> Int
svgFuncIRISize (IRI url)							= 5 + size url				// "url("<url>")"

instance toString SVGFuncIRI where
	toString (IRI url)								= "url(" +++ url +++ ")"

svgLengthSize :: !SVGLength -> Int
svgLengthSize (nr,unit)								= size nr + svgLengthUnitSize unit

instance toString SVGLength where
	toString (nr,unit)								= toString nr +++ toString unit

svgLengthAdjustSize :: !SVGLengthAdjust -> Int
svgLengthAdjustSize Spacing							= 7							// "spacing"
svgLengthAdjustSize spacingAndGlyphs				= 16						// "spacingAndGlyphs"

instance toString SVGLengthAdjust where
	toString Spacing								= "spacing"
	toString spacingAndGlyphs						= "spacingAndGlyphs"

svgLengthUnitSize :: !SVGLengthUnit -> Int
svgLengthUnitSize PERCENT							= 1							// "%"
svgLengthUnitSize other								= 2							// "{em,px,in,cm,mm,pt,pc}"

instance toString SVGLengthUnit where
	toString EM										= "em"
	toString EX										= "ex"
	toString PX										= "px"
	toString IN										= "in"
	toString CM										= "cm"
	toString MM										= "mm"
	toString PT										= "pt"
	toString PC										= "pc"
	toString PERCENT								= "%"

svgLineCapSize :: !SVGLineCap -> Int
svgLineCapSize CapButt								= 4							// "butt"
svgLineCapSize CapRound								= 5							// "round"
svgLineCapSize CapSquare							= 6							// "square"
svgLineCapSize CapInherit							= 7							// "inherit"

instance toString SVGLineCap where
	toString CapButt								= "butt"
	toString CapRound								= "round"
	toString CapSquare								= "square"
	toString CapInherit								= "inherit"

svgLineJoinSize :: !SVGLineJoin -> Int
svgLineJoinSize JoinInherit							= 7							// "inherit"
svgLineJoinSize other								= 5							// "{miter,round,bevel}"

instance toString SVGLineJoin where
	toString JoinMiter								= "miter"
	toString JoinRound								= "round"
	toString JoinBevel								= "bevel"
	toString JoinInherit							= "inherit"

svgMeetOrSliceSize :: !SVGMeetOrSlice -> Int
svgMeetOrSliceSize SVGMeet							= 4							// "meet"
svgMeetOrSliceSize SVGSlice							= 5							// "slice"

instance toString SVGMeetOrSlice where
	toString SVGMeet								= "meet"
	toString SVGSlice								= "slice"

svgMiterLimitSize :: !SVGStrokeMiterLimit -> Int
svgMiterLimitSize (MiterLimit nr)					= size nr					// <miterlimit> >= 1.0
svgMiterLimitSize MiterLimitInherit					= 7							// "inherit"

instance toString SVGStrokeMiterLimit where
	toString (MiterLimit nr)						= nr
	toString MiterLimitInherit						= "inherit"

svgNumbersSize :: ![SVGNumber] -> Int
svgNumbersSize []									= 0
svgNumbersSize nrs									= intsum size nrs + length nrs - 1

svgPaintSize :: !SVGPaint -> Int
svgPaintSize PaintNone								= 4							// "none"
svgPaintSize PaintCurrentColor						= 12						// "currentColor"
svgPaintSize (PaintColor c mc)						= svgColorSize   c   + case mc of
													                         Nothing  = 0
													                         Just sc  = svgICCColorSize sc
svgPaintSize (PaintFuncIRI iri mp)					= svgFuncIRISize iri + case mp of
													                         Nothing  = 0
													                         Just alt = 1 + svgPaintSize alt
svgPaintSize PaintInherit							= 7							// "inherit"

instance toString SVGPaint where
	toString PaintNone								= "none"
	toString PaintCurrentColor						= "currentColor"
	toString (PaintColor c mc)						= toString c +++ case mc of
													                     Nothing  = ""
													                     Just sc  = toString sc
	toString (PaintFuncIRI iri mp)					= toString iri +++ case mp of
													                     Nothing  = ""
													                     Just alt = " " +++ toString alt
	toString PaintInherit							= "inherit"

svgStrokeDashArraySize :: !SVGStrokeDashArray -> Int
svgStrokeDashArraySize NoDash						= 4							// "none"
svgStrokeDashArraySize (DashArray as)				= svgNumbersSize as			// <a_1><space><a_2>...<space><a_n>
svgStrokeDashArraySize InheritDash					= 7							// "inherit"

instance toString SVGStrokeDashArray where
	toString NoDash									= "none"
	toString (DashArray as)							= glue_list " " as
	toString InheritDash							= "inherit"

svgStrokeDashOffsetSize :: !SVGStrokeDashOffset -> Int
svgStrokeDashOffsetSize (DashOffsetLength length)	= svgLengthSize length
svgStrokeDashOffsetSize DashOffsetInherit			= 7							// "inherit"

instance toString SVGStrokeDashOffset where
	toString (DashOffsetLength length)				= toString length
	toString DashOffsetInherit						= "inherit"

svgStrokeWidthSize :: !SVGStrokeWidth -> Int
svgStrokeWidthSize (StrokeWidthLength length)		= svgLengthSize length
svgStrokeWidthSize StrokeWidthInherit				= 7 						// "inherit"

instance toString SVGStrokeWidth where
	toString (StrokeWidthLength length)				= toString length
	toString StrokeWidthInherit						= "inherit"

svgTransformsSize :: ![SVGTransform] -> Int
svgTransformsSize []								= 0
svgTransformsSize ts								= intsum svgTransformSize ts + length ts - 1

svgTransformSize :: !SVGTransform -> Int
svgTransformSize (MatrixTransform a b c d e f)		=  8 + intsum size [a,b,c,d,e,f] + 5
svgTransformSize (TranslateTransform tx ty)			= 11 + size tx + 1 + size ty
svgTransformSize (ScaleTransform     sx sy)			=  7 + size sx + 1 + size sy
svgTransformSize (RotateTransform    a  mc)			=  8 + size a  + case mc of
													                   Just (cx,cy) = 1 + size cx + 1 + size cy
													                   nothing      = 0
svgTransformSize (SkewXTransform     a)				=  7 + size a
svgTransformSize (SkewYTransform     a)				=  7 + size a

instance toString SVGTransform where
	toString (MatrixTransform a b c d e f)			= "matrix(" +++ glue_list " " [a,b,c,d,e,f] +++ ")"
	toString (TranslateTransform tx ty)				= "translate(" +++ tx +++ " " +++ ty +++ ")"
	toString (ScaleTransform     sx sy)				= "scale(" +++ sx +++ " " +++ sy +++")"
	toString (RotateTransform    a  mc)				= "rotate(" +++ a +++ case mc of
													                        Just (cx,cy) = " " +++ cx +++ " " +++ cy +++ ")"
													                        nothing      = ")"
	toString (SkewXTransform     a)					= "skewX(" +++ a +++ ")"
	toString (SkewYTransform     a)					= "skewY(" +++ a +++ ")"

instance toString SVGZoomAndPan where
	toString Disable								= "disable"
	toString Magnify								= "magnify"

serializeSVGElts :: ![SVGElt] !*{#Char} !Int -> (!*{#Char},!Int)
serializeSVGElts [] dest dest_i = (dest, dest_i)
serializeSVGElts [x:xs] dest dest_i
	#! (dest, dest_i) = serializeSVGElt x dest dest_i
	= serializeSVGElts xs dest dest_i

serializeSVGElt :: !SVGElt !*{#Char} !Int -> (!*{#Char}, !Int)
serializeSVGElt (SVGElt             html_attrs svg_attrs elts) s i = writeSVGTag "svg"              html_attrs svg_attrs elts s i
serializeSVGElt (ClipPathElt        html_attrs svg_attrs elts) s i = writeSVGTag "clipPath"         html_attrs svg_attrs elts s i
serializeSVGElt (CircleElt          html_attrs svg_attrs)      s i = writeEmptySVGTag "circle"      html_attrs svg_attrs      s i
serializeSVGElt (EllipseElt         html_attrs svg_attrs)      s i = writeEmptySVGTag "ellipse"     html_attrs svg_attrs      s i
serializeSVGElt (DefsElt            html_attrs svg_attrs elts) s i = writeSVGTag "defs"             html_attrs svg_attrs elts s i
serializeSVGElt (GElt               html_attrs svg_attrs elts) s i = writeSVGTag "g"                html_attrs svg_attrs elts s i
serializeSVGElt (ImageElt           html_attrs svg_attrs elts) s i = writeSVGTag "image"            html_attrs svg_attrs elts s i
serializeSVGElt (LinearGradientElt  html_attrs svg_attrs elts) s i = writeSVGTag "linearGradient"   html_attrs svg_attrs elts s i
serializeSVGElt (LineElt            html_attrs svg_attrs)      s i = writeEmptySVGTag "line"        html_attrs svg_attrs      s i
serializeSVGElt (MarkerElt          html_attrs svg_attrs elts) s i = writeSVGTag "marker"           html_attrs svg_attrs elts s i
serializeSVGElt (MaskElt            html_attrs svg_attrs elts) s i = writeSVGTag "mask"             html_attrs svg_attrs elts s i
serializeSVGElt (PathElt            html_attrs svg_attrs)      s i = writeEmptySVGTag "path"        html_attrs svg_attrs      s i
serializeSVGElt (PolygonElt         html_attrs svg_attrs)      s i = writeEmptySVGTag "polygon"     html_attrs svg_attrs      s i
serializeSVGElt (PolylineElt        html_attrs svg_attrs)      s i = writeEmptySVGTag "polyline"    html_attrs svg_attrs      s i
serializeSVGElt (RadialGradientElt  html_attrs svg_attrs elts) s i = writeSVGTag "radialGradient"   html_attrs svg_attrs elts s i
serializeSVGElt (RectElt            html_attrs svg_attrs)      s i = writeEmptySVGTag "rect"        html_attrs svg_attrs      s i
serializeSVGElt (StopElt            html_attrs svg_attrs)      s i = writeEmptySVGTag "stop"        html_attrs svg_attrs      s i
serializeSVGElt (TextElt            html_attrs svg_attrs text) s i = writeSVGPlainTag "text"        html_attrs svg_attrs text s i
serializeSVGElt (RawElt                                  text) s i = writeRawSVG                                         text s i
serializeSVGElt _ _ _                                              = abort "Text.HTML: serializeSVGElt applied to unexpected argument.\n"

serializeSVGAttrs :: ![SVGAttr] !*{#Char} !Int -> (!*{#Char}, !Int)
serializeSVGAttrs [] dest dest_i = (dest, dest_i)
serializeSVGAttrs [x:xs] dest dest_i
	#! (dest, dest_i) = serializeSVGAttr x dest dest_i
	= serializeSVGAttrs xs dest dest_i

serializeSVGAttr :: !SVGAttr !*{#Char} !Int -> (!*{#Char}, !Int)
serializeSVGAttr (AlignmentBaselineAttr   bl)        s i = writeAttr "alignment-baseline"  bl               s i
serializeSVGAttr (BaseProfileAttr         profile)   s i = writeAttr "baseProfile"         profile          s i
serializeSVGAttr (ContentScriptTypeAttr   type)      s i = writeAttr "contentScriptType"   type             s i
serializeSVGAttr (ClipPathAttr            cp)        s i = writeAttr "clip-path"           cp               s i
serializeSVGAttr (CxAttr                  cx)        s i = writeAttr "cx"                  (toString cx)    s i
serializeSVGAttr (CyAttr                  cy)        s i = writeAttr "cy"                  (toString cy)    s i
serializeSVGAttr (DominantBaselineAttr    dbl)       s i = writeAttr "dominant-baseline"   dbl              s i
serializeSVGAttr (ExternalResourcesRequiredAttr b)   s i = writeAttr "externalResourcesRequired" (if b "true" "false") s i
serializeSVGAttr (FillAttr                paint)     s i = writeAttr "fill"                (toString paint)  s i
serializeSVGAttr (FillOpacityAttr         opac)      s i = writeAttr "fill-opacity"        (toString opac)   s i
serializeSVGAttr (FillRuleAttr            rule)      s i = writeAttr "fill-rule"           (toString rule)   s i
serializeSVGAttr (FontFamilyAttr          family)    s i = writeAttr "font-family"         (toString family)  s i
serializeSVGAttr (FontSizeAttr            size)      s i = writeAttr "font-size"           (toString size)    s i
serializeSVGAttr (FontStyleAttr           style)     s i = writeAttr "font-style"          (toString style)   s i
serializeSVGAttr (FontStretchAttr         stretch)   s i = writeAttr "font-stretch"        (toString stretch) s i
serializeSVGAttr (FontVariantAttr         variant)   s i = writeAttr "font-variant"        (toString variant) s i
serializeSVGAttr (FontWeightAttr          weight)    s i = writeAttr "font-weight"         (toString weight)  s i
serializeSVGAttr (LengthAdjustAttr        adjust)    s i = writeAttr "lengthAdjust"        (toString adjust)  s i
serializeSVGAttr (MarkerStartAttr         m)         s i = writeAttr "marker-start"        m s i
serializeSVGAttr (MarkerMidAttr           m)         s i = writeAttr "marker-mid"          m s i
serializeSVGAttr (MarkerEndAttr           m)         s i = writeAttr "marker-end"          m s i
serializeSVGAttr (MarkerHeightAttr        m)         s i = writeAttr "markerHeight"        (toString m) s i
serializeSVGAttr (MarkerWidthAttr         m)         s i = writeAttr "markerWidth"         (toString m) s i
serializeSVGAttr (MaskAttr                m)         s i = writeAttr "mask"                m s i
serializeSVGAttr (OffsetAttr              offset)    s i = writeAttr "offset"              offset            s i
serializeSVGAttr (OrientAttr              orient)    s i = writeAttr "orient"              orient            s i
serializeSVGAttr (PointsAttr              points)    s i = writeAttr "points"              (foldr (+++) "" (intersperse " " (map (\(x, y) -> x +++ "," +++ y) points))) s i
serializeSVGAttr (PreserveAspectRatioAttr md ma mms) s i = writeAttr "preserveAspectRatio" (   case md of
														                                         Nothing = ""
														                                         Just d  = "defer"
														                                   +++ case ma of
														                                         Nothing = " none"
														                                         Just a  = " " +++ toString a
														                                   +++ case mms of
														                                         Nothing = ""
														                                         Just ms = " " +++ toString ms
														                                   ) s i
serializeSVGAttr (RAttr                   length)    s i = writeAttr "r"                   (toString length) s i
serializeSVGAttr (RefXAttr                length)    s i = writeAttr "refX"                (toString length) s i
serializeSVGAttr (RefYAttr                length)    s i = writeAttr "refY"                (toString length) s i
serializeSVGAttr (RxAttr                  length)    s i = writeAttr "rx"                  (toString length) s i
serializeSVGAttr (RyAttr                  length)    s i = writeAttr "ry"                  (toString length) s i
serializeSVGAttr (StopColorAttr           color)     s i = writeAttr "stop-color"          color             s i
serializeSVGAttr (StopOpacityAttr         opacity)   s i = writeAttr "stop-opacity"        opacity           s i
serializeSVGAttr (StrokeAttr              paint)     s i = writeAttr "stroke"              (toString paint)  s i
serializeSVGAttr (StrokeDashArrayAttr     da)        s i = writeAttr "stroke-dasharray"    (toString da)     s i
serializeSVGAttr (StrokeDashOffsetAttr    do)        s i = writeAttr "stroke-dashoffset"   (toString do)     s i
serializeSVGAttr (StrokeLineCapAttr       cap)       s i = writeAttr "stroke-linecap"      (toString cap)    s i
serializeSVGAttr (StrokeLineJoinAttr      join)      s i = writeAttr "stroke-linejoin"     (toString join)   s i
serializeSVGAttr (StrokeMiterLimitAttr    limit)     s i = writeAttr "stroke-miterlimit"   (toString limit)  s i
serializeSVGAttr (StrokeOpacityAttr       opac)      s i = writeAttr "stroke-opacity"      opac              s i
serializeSVGAttr (StrokeWidthAttr         width)     s i = writeAttr "stroke-width"        (toString width)  s i
serializeSVGAttr (TextAnchorAttr          anchor)    s i = writeAttr "text-anchor"         anchor            s i
serializeSVGAttr (TextLengthAttr          length)    s i = writeAttr "textLength"          (toString length) s i
serializeSVGAttr (TextRenderingAttr       rendering) s i = writeAttr "text-rendering"      rendering         s i
serializeSVGAttr (TransformAttr			  ts)        s i = writeAttr "transform"           (glue_list " " (map toString ts)) s i
serializeSVGAttr (VersionAttr             v)         s i = writeAttr "version"             v                 s i
serializeSVGAttr (ViewBoxAttr             x y w h)   s i = writeAttr "viewBox"             (glue_list " " [x,y,w,h]) s i
serializeSVGAttr (XAttr                   x)         s i = writeAttr "x"                   (toString x)      s i
serializeSVGAttr (X1Attr                  x1)        s i = writeAttr "x1"                  (toString x1)     s i
serializeSVGAttr (X2Attr                  x2)        s i = writeAttr "x2"                  (toString x2)     s i
serializeSVGAttr (XLinkHRefAttr           iri)       s i = writeAttr "xlink:href"          iri               s i
serializeSVGAttr (YAttr                   y)         s i = writeAttr "y"                   (toString y)      s i
serializeSVGAttr (Y1Attr                  y1)        s i = writeAttr "y1"                  (toString y1)     s i
serializeSVGAttr (Y2Attr                  y2)        s i = writeAttr "y2"                  (toString y2)     s i
serializeSVGAttr (ZoomAndPanAttr          zap)       s i = writeAttr "zoomAndPan"          (toString zap)    s i

writeSVGTag :: !{#Char} ![HtmlAttr] ![SVGAttr] ![SVGElt] !*{#Char} !Int -> (!*{#Char},!Int)
writeSVGTag name html_attrs svg_attrs elts dest dest_i
	#! dest = {dest & [dest_i] = '<'}
	#! dest_i = dest_i + 1
	#! (dest,dest_i) = copyChars name 0 False dest dest_i
	#! (dest, dest_i) = serializeAttrs html_attrs dest dest_i
	#! (dest, dest_i) = serializeSVGAttrs svg_attrs dest dest_i
	#! dest = {dest & [dest_i] = '>'}
	#! dest_i = dest_i + 1
	#! (dest, dest_i) = serializeSVGElts elts dest dest_i
	#! dest = {dest & [dest_i] = '<'}
	#! dest = {dest & [dest_i + 1] = '/'}
	#! dest_i = dest_i + 2
	#! (dest,dest_i) = copyChars name 0 False dest dest_i
	#! dest = {dest & [dest_i] = '>'}
	#! dest_i = dest_i + 1
	= (dest,dest_i)

writeEmptySVGTag :: !{#Char} ![HtmlAttr] ![SVGAttr] !*{#Char} !Int -> (!*{#Char},!Int)
writeEmptySVGTag name html_attrs svg_attrs dest dest_i
	#! dest = {dest & [dest_i] = '<'}
	#! dest_i = dest_i + 1
	#! (dest,dest_i) = copyChars name 0 False dest dest_i
	#! (dest, dest_i) = serializeAttrs html_attrs dest dest_i
	#! (dest, dest_i) = serializeSVGAttrs svg_attrs dest dest_i
	#! dest = {dest & [dest_i] = '/'}
	#! dest_i = dest_i + 1
	#! dest = {dest & [dest_i] = '>'}
	#! dest_i = dest_i + 1
	= (dest,dest_i)

writeSVGPlainTag :: !{#Char} ![HtmlAttr] ![SVGAttr] !{#Char} !*{#Char} !Int -> (!*{#Char},!Int)
writeSVGPlainTag name html_attrs svg_attrs plain dest dest_i
	#! dest = {dest & [dest_i] = '<'}
	#! dest_i = dest_i + 1
	#! (dest,dest_i) = copyChars name 0 False dest dest_i
	#! (dest, dest_i) = serializeAttrs html_attrs dest dest_i
	#! (dest, dest_i) = serializeSVGAttrs svg_attrs dest dest_i
	#! dest = {dest & [dest_i] = '>'}
	#! dest_i = dest_i + 1
	#! (dest,dest_i) = copyChars plain 0 True dest dest_i
	#! dest = {dest & [dest_i] = '<'}
	#! dest = {dest & [dest_i + 1] = '/'}
	#! dest_i = dest_i + 2
	#! (dest,dest_i) = copyChars name 0 False dest dest_i
	#! dest = {dest & [dest_i] = '>'}
	#! dest_i = dest_i + 1
	= (dest,dest_i)

writeRawSVG :: !{#Char} !*{#Char} !Int -> (!*{#Char},!Int)
writeRawSVG plain dest dest_i = copyChars plain 0 False dest dest_i

glue_list :: !String ![String] -> String
glue_list sep [str : strs]	= str +++ foldr (\t txt -> sep +++ t +++ txt) "" strs
glue_list _ _				= ""

intsum :: !(a -> Int) ![a] -> Int
intsum f xs				= sum (map f xs)

escapeStr :: !String -> String
escapeStr str
  #! origSz = size str
  #! escdSz = escapedSize str
  | origSz == escdSz = str
  | otherwise
      #! (str, _) = copyChars str 0 True (createArray escdSz '\0') 0
      = str

derive gEq HtmlTag, HtmlAttr
derive gEq SVGElt, SVGAttr, SVGAlign, SVGColor, SVGDefer, SVGFillOpacity, SVGFuncIRI, SVGLengthAdjust, SVGLengthUnit, SVGLineCap, SVGFillRule, SVGLineJoin, SVGMeetOrSlice, SVGStrokeMiterLimit, SVGPaint, SVGStrokeDashArray, SVGStrokeDashOffset, SVGStrokeWidth, SVGTransform, SVGZoomAndPan
