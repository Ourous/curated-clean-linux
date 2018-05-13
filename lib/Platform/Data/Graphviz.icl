// Peter Divianszky, 2007
// Code extended and adapted for using generics by Peter Achten, 2007, Pieter Koopman 2011, Jurriën Stutterheim 2013

implementation module Data.Graphviz

import StdArray, StdOverloaded, StdOrdList, StdTuple, StdString, StdBool, StdMisc
import Data.Maybe, Data.List
import Data.Generics.GenPrint, Data.Generics.GenEq

derive gEq    EdgeStyle, NodeStyle, DirType, NodeShape, Side, ArrowShape, ArrowType, Arrow, Color, Maybe
derive gPrint EdgeStyle, NodeStyle, DirType, NodeShape, Side, ArrowShape,
              Maybe, CompassPoint, StartStyle, ClusterMode, OutputMode,
              PageDir, RankDir, RankType
derive printNameValuePair GraphAttribute, NodeAttribute, EdgeAttribute

//  Almost regular toString instances:
instance toString EdgeStyle      where toString es  = quote (skipName "E" (printToString es))
instance toString NodeStyle      where toString ns  = quote (skipName "NStyle" (printToString ns))
instance toString DirType        where toString dir = quote (skipName "DT" (printToString dir))
instance toString NodeShape      where toString ns  = skipName "NShape" (printToString ns)
instance toString Side           where toString s   = skipName "Side" (printToString s)
instance toString ArrowShape     where toString s   = skipName "AShape" (printToString s)
instance toString CompassPoint   where toString cp  = quote (skipName "CP" (printToString cp))
instance toString ClusterMode    where toString cm  = quote (skipName "CM" (printToString cm))
instance toString OutputMode     where toString om  = quote (skipName "OM" (printToString om))
instance toString PageDir        where toString pd  = quote (skipNameUp "PD" (printToString pd))
instance toString RankDir        where toString rd  = quote (skipNameUp "RD" (printToString rd))
instance toString RankType       where toString rt  = quote (skipName "RT" (printToString rt))
instance toString StartStyle     where toString ss  = quote (skipName "SS" (printToString ss))
instance toString NodeAttribute  where toString na  = printNameValuePair{|*|} "NAtt" na
instance toString EdgeAttribute  where toString ea  = printNameValuePair{|*|} "EAtt" ea
instance toString GraphAttribute where toString ga  = printNameValuePair{|*|} "GAtt" ga
//  Less regular toString instances:
instance toString Arrow where
  toString {open,side,shape}  = if open "o" "" +++ if (isJust side) (toString (fromJust side)) "" +++ toString shape
instance toString ArrowType where
  toString {closest,furthest}  = quote (toString closest +++ if (isJust furthest) (toString (fromJust furthest)) "")
instance toString Color where
  toString (Color name)    = name

  toString (HSV h s v)    = "\"" $> toS h $> " " $> toS s $> " " $> toS v $> "\""
  where
    toS x
      | x<0.0 || x>1.0  = abort "HSV value out of range.\n" 
      | otherwise      = toString (toReal (toInt (1000.0*x)) / 1000.0)

  toString (RGB r g b)    = "\"#" $> toS r $> toS g $> toS b $> "\""
  where
    toS x 
      | x<0 || x>255    = abort "RGB value out of range.\n" 
      | otherwise      = toString [toC (x/16), toC (x rem 16)] 
    toC x 
      | x < 10      = toChar (x + fromChar '0')
      | otherwise      = toChar (x - 10 + fromChar 'A')
instance toString DotPoint where
  toString (DotPoint x y fix)  = x >$ "," >$ y >$ if fix "!" ""
instance toString LayerId where
  toString layerid      = case layerid of
                  LayerAll      = "all"
                  LayerNr   nr  = toString nr
                  LayerName str = str
instance toString LayerList where
  toString (LayerList names)  = foldr (\next before -> before $> layersep $> next) "" names
instance toString LayerRange where
  toString (LayerRange id ids)= foldr (\next before -> before $> layersep $> next) (toString id) ids
instance toString Margin where
  toString margin        = case margin of
                  SingleMargin a   = toString a
                  DoubleMargin a b = a >$ "," $> b
instance toString Pad where
  toString pad        = case pad of
                  SinglePad a   = toString a
                  DoublePad a b = a >$ "," $> b
instance toString Pointf where
  toString (Pointf x y)    = quote (x >$ "," $> y)
instance toString Ratio where
  toString ratio        = case ratio of
                  AspectRatio r = quote (toString r)
                  RFill        = quote "fill"
                  RCompress    = quote "compress"
                  RExpand      = quote "expand"
                  RAuto        = quote "auto"
instance toString Rect where
  toString {llx,lly,urx,ury}  = llx >$ "," $> lly >$ "," $> urx >$ "," $> ury
instance toString Sizef where      // PA++
  toString (Sizef x y True)  = "\"" +++ toString x +++ "," +++ toString y +++ "!\""
  toString (Sizef x y False)  = "\"" +++ toString x +++ "," +++ toString y +++ "\""
instance toString StartType where
  toString {startStyle,startSeed}
                = if (isJust startStyle) (toString (fromJust startStyle)) "" +++ 
                  if (isJust startSeed)  (toString (fromJust startSeed )) ""
instance toString ViewPort where
  toString {vpW,vpH,vpZ,vpXY}
                = (vpW >$ "," $> vpH)                          +++
                  if (isJust vpZ ) ("," $> (fromJust vpZ )) "" +++
                  if (isJust vpXY) ("," $> (fromJust vpXY)) ""

//  Print name=value pairs for algebraic data types with unary data constructors in XXX_name constructor name format.
generic printNameValuePair a :: !String !a -> String
printNameValuePair{|Int|}          pre x      = toString x
printNameValuePair{|Real|}         pre x      = toString x
printNameValuePair{|Char|}         pre x      = toString x
printNameValuePair{|String|}       pre x      = quote    x
printNameValuePair{|Bool|}         pre x      = firstCharLowerCase (toString x)
printNameValuePair{|UNIT|}         pre x      = ""
printNameValuePair{|PAIR|}   px py pre (PAIR x y)  = px pre x +++ " " +++ py "" y
printNameValuePair{|EITHER|} pl pr pre (LEFT   x)  = pl pre x
printNameValuePair{|EITHER|} pl pr pre (RIGHT  y)  = pr pre y
printNameValuePair{|OBJECT|} px    pre (OBJECT x)  = px pre x
printNameValuePair{|CONS of d|} px pre (CONS   x)  = skipName pre d.gcd_name +++ "=" +++ px "" x
// Specializations of printNameValuePair:
printNameValuePair{|ArrowType|}    pre x      = toString x
printNameValuePair{|Color|}        pre x      = toString x
printNameValuePair{|ClusterMode|}  pre x      = toString x
printNameValuePair{|CompassPoint|} pre x      = toString x
printNameValuePair{|DirType|}      pre x      = toString x
printNameValuePair{|DotPoint|}     pre x      = toString x
printNameValuePair{|EdgeStyle|}    pre x      = toString x
printNameValuePair{|LayerList|}    pre x      = toString x
printNameValuePair{|LayerRange|}   pre x      = toString x
printNameValuePair{|Margin|}       pre x      = toString x
printNameValuePair{|NodeShape|}    pre x      = toString x
printNameValuePair{|NodeStyle|}    pre x      = toString x
printNameValuePair{|OutputMode|}   pre x      = toString x
printNameValuePair{|Pad|}          pre x      = toString x
printNameValuePair{|PageDir|}      pre x      = toString x
printNameValuePair{|Pointf|}       pre x      = toString x
printNameValuePair{|RankDir|}      pre x      = toString x
printNameValuePair{|RankType|}     pre x      = toString x
printNameValuePair{|Ratio|}        pre x      = toString x
printNameValuePair{|Rect|}         pre x      = toString x
printNameValuePair{|Sizef|}        pre x      = toString x    // PA++
printNameValuePair{|StartType|}    pre x      = toString x
printNameValuePair{|ViewPort|}     pre x      = toString x

instance == EdgeStyle where (==) a b      = gEq{|*|} a b
instance == NodeStyle where (==) a b      = gEq{|*|} a b
instance == DirType   where (==) a b      = gEq{|*|} a b
instance == NodeShape where (==) a b      = gEq{|*|} a b
instance == ArrowType where (==) a b      = gEq{|*|} a b
instance == Color     where (==) a b      = gEq{|*|} a b


digraphTitle :: !Digraph -> String
digraphTitle (Digraph title _ _ _)        = title

digraphAtts :: !Digraph -> [GraphAttribute]
digraphAtts (Digraph _ atts _ _)        = atts

digraphNodes :: !Digraph -> [NodeDef]
digraphNodes (Digraph _ _ nodes _)        = nodes

digraphSelectedItem :: !Digraph -> Maybe SelectedItem
digraphSelectedItem (Digraph _ _ _ selected)  = selected

pointNode :: [NodeAttribute]
pointNode                    =: [NAttShape NShapePoint]

hiddenNode :: [NodeAttribute]
hiddenNode                    =: [NAttShape NShapePoint,NAttStyle NStyleInvis]

commaseparatedlist :: [String] -> String
commaseparatedlist []              = ""
commaseparatedlist l              = "[" +++ (foldr (+++) "" (intersperse "," l)) +++ "]"

printDigraph :: !Digraph -> [String]
printDigraph (Digraph title atts nodes _)    = map (\x->x+++"\n") (prelude title (graphAtts atts) (contents nodes))

createGraphName :: !String -> String
createGraphName ""                = "G"
createGraphName x                = quote x

prelude :: !String ![String] ![String] -> [String]
prelude title graphAtts contents        = [ "digraph " +++ createGraphName title +++ " {"
                          , "label="   +++ quote title 
                          ]            ++ 
                          graphAtts    ++ 
                          contents     ++ 
                          [ "overlap=false","}" ]

graphAtts :: ![GraphAttribute] -> [String]
graphAtts graphAtts                = map (printNameValuePair{|*|} "GAtt") graphAtts

contents :: ![NodeDef] -> [String]
contents nodeDefs                = map snd (mergeBy (\(x,_) (y,_)= x<y) nodes` edges`)
where
  (nodes,edges)                = unzip (mapSt f nodeDefs 1)
  where
    f (NodeDef id st na edges) num      = (  ((num,id,na)
                            ,[(n,id,id`,ea) \\ (id`,ea)<- edges & n<-[num+1..]]
                          )
                          , num + 1 + length edges
                          )
  
  nodes` = map (\(num, id, atts) = (num, id >$ commaseparatedlist (map toString atts))) nodes
  edges` = map (\(num,source,target,atts) = (num,source >$ "->" $> target >$ commaseparatedlist (map toString atts))) (flatten edges)

/*
<<<<<<< .mine
=======

mkDigraph :: String (KnownAutomaton s i o,s,[s],[s],[s],[SeenTrans s i o],[SeenTrans s i o]) -> Digraph | render, gEq{|*|}, genShow{|*|} s & render, gEq{|*|} i & render, gEq{|*|} o
mkDigraph name (automaton,s_0,init_states,finished,shared,issues,trace)
  = Digraph
    (remove_spaces name)
    graphAttributes
    (if (isEmpty automaton.trans)
      [NodeDef 0                  [NStAllEdgesFound (gisMember s_0 finished)] (nodeAttributes s_0 init_states (gisMember s_0 finished) (gisMember s_0 shared)) []]
      [NodeDef (nrOf automaton n) [NStAllEdgesFound (gisMember n   finished)] (nodeAttributes n   init_states (gisMember n   finished) (gisMember n   shared))
               [ let (s,i,o,t) = trans in
                 (nrOf automaton t  , [ EAtt_label (render i+++"/"+++showList ("[","]",",") o)
                                      , EAtt_fontname "Helvetica"
                                      , EAtt_fontsize fontsize
                                      , EAtt_labelfontname "Helvetica"
                                      , EAtt_labelfontsize fontsize
                                      , EAtt_color
                                             (if (gisMember trans issues)
                                                  (Color "red")
                                             (if (gisMember trans trace)
                                                  (Color "blue")
                                                  (Color "black")))
                                      , EAtt_arrowsize (if (gisMember trans trace) 2.0 1.2)
                                   //   , EAtt_style (if (isMember trans trace) EStyle_bold EStyle_solid)
                                      ])
               \\ trans <- edgesFrom n automaton
               ]
      \\ n <- let nodes = nodesOf automaton in if (gisMember s_0 nodes && hd nodes =!= s_0) [s_0:filter ((=!=) s_0) nodes] nodes
      ]
    ) Nothing
where
//  graphAttributes        = [ GAtt_rankdir  RD_TB // RD_LR
  graphAttributes        = [ GAtt_rankdir  RD_LR // horizontal
                 // , GAtt_size         (Sizef 10.0 6.0 True)
                  , GAtt_size         (Sizef 7.4 15.0 False)
                 // , GAtt_size      (Sizef 5.0 3.0 True)
                  , GAtt_fontsize    9.0 // 12.0
                  , GAtt_bgcolor      (Color "lightsteelblue")
                  , GAtt_ordering     "out"
                  , GAtt_outputorder  OM_nodesfirst  // OM_edgesfirst  //  PK
                  ]
  nodeAttributes n init_states finished shared
                = (if (gisMember n init_states)
                    (if shared  [ NAttFillColor shac_backgr, NAttFontColor shac_txt ]
                          [ NAttFillColor act_backgr, NAttFontColor act_txt ])
                  (if finished [ NAttFillColor done_backgr,NAttFontColor done_txt]
                      (if shared  [ NAttFillColor shar_backgr, NAttFontColor shar_txt ]
                                  [ NAttFillColor def_backgr, NAttFontColor def_txt ])
                  )) ++
                      [ NAttLabel    (render n)
                      , NAttTooltip  (show1 n)
                      , NAttStyle    NStyle_filled
                      , NAttShape    (if (n === s_0) NShapeDoubleCircle NShapeEllipse /*NShapeCircle*/)
                      , NAttFontName  "Helvetica"
                      , NAttFontSize  fontsize
                      , NAttFixedSize  False // True
                      , NAttWidth 1.0,  NAttHeight 1.0
                      , NAttMargin    (SingleMargin 0.003)
                      ]
  where
    ( act_backgr, act_txt)  = active_state_color (length init_states)
    (done_backgr,done_txt)  = finished_state_color
    ( def_backgr, def_txt)  = default_state_color
    (shar_backgr,shar_txt)  = shared_state_color
    (shac_backgr,shac_txt)  = shared_active_state_color

active_state_color :: !Int -> (!Color,!Color)
active_state_color nr  = (RGB 255 dim dim,Color "white")
where
  dim          = min 250 (255 - 255 / (min nr 3))

finished_state_color :: (!Color,!Color)
finished_state_color  = (Color "blue", Color "white")

default_state_color :: (!Color,!Color)
default_state_color    = (Color "grey90",Color "black")

shared_state_color :: (!Color,!Color)
shared_state_color    = (Color "gray",Color "white")

shared_active_state_color :: (!Color,!Color)
shared_active_state_color    = (Color "gray",Color "red")

fontsize = 12.0 // 18.0

>>>>>>> .r1907
*/
//  Utility functions:

mapSt :: (a b -> (c,b)) [a] b -> [c]
mapSt f [] st      = []
mapSt f [h:t] st 
  #! (x, st)      = f h st
  = [x : mapSt f t st]

quote :: !String -> String
quote a          = "\"" $> (flatten (map f (fromString a))) >$ "\""
where
  f '\"'        = ['\\\"']
  f x          = [x]

skipXXX_InConstructorName :: !String -> String
skipXXX_InConstructorName str
  = case dropWhile ((<>) '_') [c \\ c<-:str] of
    []        = str
    underscoreName  = str % (n-length underscoreName+1,n-1)
where
  n          = size str

skipName :: !String !String -> String
skipName pre str = {toLower c \\ c <-: skipNameUp pre str}

skipNameUp :: !String !String -> String
skipNameUp pre str | n >= m && pre == str % (0, m-1)
	= str % (m, n-1)
	= str
where
	n = size str
	m = size pre

firstCharLowerCase :: !String -> String
firstCharLowerCase str
  | size str > 0    = str := (0,toLower str.[0])
  | otherwise      = str

($>) infixr 5 :: !String !a -> String | toString a
($>) str arg      = str +++ toString arg

(>$) infixr 5 :: !a !String -> String | toString a
(>$) arg str      = toString arg +++ str


