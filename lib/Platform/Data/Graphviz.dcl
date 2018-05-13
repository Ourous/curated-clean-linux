// Peter Divianszky, 2007
// Code extended and adapted by Peter Achten, 2007, Pieter Koopman 2010

definition module Data.Graphviz

from StdOverloaded import class toString, class ==
from Data.Maybe import :: Maybe
from Data.Generics.GenEq import generic gEq

// A digraph contains a title and a list of node definitions
:: Digraph
  = Digraph String [GraphAttribute] [NodeDef] (Maybe SelectedItem)
:: SelectedItem
  = SelectedItem Int

digraphTitle        :: !Digraph -> String
digraphAtts         :: !Digraph -> [GraphAttribute]
digraphNodes        :: !Digraph -> [NodeDef]
digraphSelectedItem :: !Digraph -> Maybe SelectedItem

// A node definition contains a unique identifier (an integer), a list of node attributes and a list of edge definitions.
// An edge definition contains an identifier (the id of the end node and edge attributes).
:: NodeDef
   = NodeDef !Int ![NodeState] ![NodeAttribute] [EdgeDef]

:: EdgeDef
  :== (!Int,![EdgeAttribute])

:: NodeState
  = NStAllEdgesFound !Bool // all edges of this node are known

// Convert digraph into list of strings.
// The strings are lines of the graphviz representation of the graph.
printDigraph :: !Digraph -> [String]

:: GraphAttribute
  = GAttDamping        Real
  | GAttK              Real
  | GAttURL            String
  | GAttBB             Rect
  | GAttBGColor        Color
  | GAttCenter         Bool
  | GAttCharset        String
  | GAttClusterRank    ClusterMode
  | GAttColorScheme    String
  | GAttComment        String
  | GAttCompound       Bool
  | GAttConcentrate    Bool
  | GAttDefaultDist    Real
  | GAttDim            Int
//  | GAttDirEdgeConstraints ... PA: ignored, neato only
  | GAttDPI            Real
  | GAttEpsilon        Real
  | GAttESep           Real
  | GAttFontColor      Color
  | GAttFontName       String
  | GAttFontNames      String
  | GAttFontPath       String
  | GAttFontSize       Real
  | GAttLabel          String
  | GAttLabelJust      String
  | GAttLabelLoc       String
  | GAttLandscape      Bool
  | GAttLayers         LayerList
  | GAttLayerSep       String
  | GAttLevelsGap      Real
  | GAttLP             DotPoint
  | GAttMargin         Margin
  | GAttMaxIter        Int
  | GAttMCLimit        Real
  | GAttMinDist        Real
  | GAttMode           String
  | GAttModeL          String
  | GAttMosek          Bool
  | GAttNodeSep        Real
  | GAttNoJustify      Bool
  | GAttNormalize      Bool
  | GAttNSLimit        Real
  | GAttNSLimit1       Real
  | GAttOrdering       String
  | GAttOrientation    String
  | GAttOutputOrder    OutputMode
  | GAttPad            Pad
  | GAttPage           Pointf
  | GAttPageDir        PageDir
  | GAttQuantum        Real
  | GAttRank           RankType
  | GAttRankDir        RankDir
  | GAttRankSep        Real
  | GAttRatio          Ratio
  | GAttRemInCross     Bool
  | GAttResolution     Real
  | GAttRoot           String
  | GAttRotate         Int
  | GAttSearchSize     Int
  | GAttShowBoxes      Int
  | GAttSize           Sizef //Pointf    // PA++
//  | GAttSplines        PA: skipped for the time being
  | GAttStart          StartType
  | GAttStylesheet     String
  | GAttTarget         String
  | GAttTrueColor      Bool
  | GAttViewport       ViewPort
  | GAttVoroMargin     Real

:: NodeAttribute
  = NAttURL            String
  | NAttColor          Color
  | NAttColorScheme    String
  | NAttComment        String
  | NAttDistortion     Real
  | NAttFillColor      Color
  | NAttFixedSize      Bool
  | NAttFontColor      Color
  | NAttFontName       String
  | NAttFontSize       Real
  | NAttGroup          String
  | NAttHeight         Real
  | NAttLabel          String
  | NAttLayer          LayerRange
  | NAttMargin         Margin
  | NAttNoJustify      Bool
  | NAttOrientation    Real
  | NAttPeripheries    Int
  | NAttPin            Bool
//  | NAttPos ...        PA: ignored for the time being
  | NAttRects          Rect
  | NAttRegular        Bool
  | NAttSamplePoints   Int
  | NAttShape          NodeShape
  | NAttShapeFile      String
  | NAttShowBoxes      Int
  | NAttSides          Int
  | NAttSkew           Real
  | NAttStyle          NodeStyle
  | NAttTarget         String
  | NAttTooltip        String
  | NAttWidth          Real
  | NAttZ              Real

:: EdgeAttribute
  = EAttURL            String
  | EAttArrowHead      ArrowType
  | EAttArrowSize      Real
  | EAttArrowTail      ArrowType
  | EAttColor          Color
  | EAttColorScheme    String
  | EAttComment        String
  | EAttConstraint     Bool
  | EAttDecorate       Bool
  | EAttDir            DirType
  | EAttEdgeURL        String
  | EAttEdgeHRef       String
  | EAttEdgeTarget     String
  | EAttEdgeTooltip    String
  | EAttFontColor      Color
  | EAttFontName       String
  | EAttFontSize       Real
  | EAttHeadURL        String
  | EAttHeadClip       Bool
  | EAttHeadHRef       String
  | EAttHeadLabel      String
  | EAttHeadPort       PortPos
  | EAttHeadTarget     String
  | EAttHeadTooltip    String
  | EAttHRef           String
  | EAttLabel          String
  | EAttLabelURL       String
  | EAttLabelAngle     Real
  | EAttLabelDistance  Real
  | EAttLabelFloat     Bool
  | EAttLabelFontColor Color
  | EAttLabelFontName  String
  | EAttLabelFontSize  Real
  | EAttLabelHref      String
  | EAttLabelTarget    String
  | EAttLabelTooltip   String
  | EAttLayer          LayerRange
  | EAttLen            Real
  | EAttLHead          String
  | EAttLP             DotPoint
  | EAttLTail          String
  | EAttMinLen         Int
  | EAttNoJustify      Bool
//  | EAttPos      PA: ignored for the time being
  | EAttSameHead       String
  | EAttSameTail       String
  | EAttShowBoxes      Int
  | EAttStyle          EdgeStyle
  | EAttTailURL        String
  | EAttTailClip       Bool
  | EAttTailHRef       String
  | EAttTailLabel      String
  | EAttTailPort       PortPos
  | EAttTailTarget     String
  | EAttTailTooltip    String
  | EAttTarget         String
  | EAttTooltip        String
  | EAttWeight         Real

:: ClusterMode
  = CMLocal
  | CMGlobal
  | CMNone

:: CompassPoint
  = CPN
  | CPNE
  | CPE
  | CPSE
  | CPS
  | CPSW
  | CPW
  | CPNW

:: DotPoint
  = DotPoint Real Real Bool

:: LayerId
  = LayerAll
  | LayerNr   Int
  | LayerName String

:: LayerList
  = LayerList [String]

:: LayerRange
  = LayerRange LayerId [LayerId]

:: Margin
  = SingleMargin Real
  | DoubleMargin Real Real

:: OutputMode
  = OMBreadthFirst
  | OMNodesFirst
  | OMEdgesFirst

:: Pad
  = SinglePad Real
  | DoublePad Real Real

:: PageDir
  = PDBL
  | PDBR
  | PDTL
  | PDTR
  | PDRB
  | PDRT
  | PDLB
  | PDLT

:: Pointf
  = Pointf Real Real

:: PortPos        // PA: for now only compass points are supported
  :== CompassPoint

:: RankDir
  = RDTB
  | RDLR
  | RDBT
  | RDRL

:: RankType
  = RTSame
  | RTMin
  | RTSource
  | RTMax
  | RTSink

:: Ratio
  = AspectRatio Real
  | RFill
  | RCompress
  | RExpand
  | RAuto

:: Rect
  = { llx :: Int
    , lly :: Int
    , urx :: Int
    , ury :: Int
    }

:: Sizef    // PA++
  = Sizef Real Real Bool

:: StartStyle
  = SSRegular
  | SSSelf
  | SSRandom

:: StartType
  = { startStyle :: Maybe StartStyle
    , startSeed  :: Maybe Int
    }

:: ViewPort
  = { vpW       :: Real
    , vpH       :: Real
    , vpZ       :: Maybe Real
    , vpXY      :: Maybe Pointf
    }

pointNode           :: [NodeAttribute] // attributes of a point-shaped node
hiddenNode          :: [NodeAttribute] // attributes of a hidden node


:: NodeShape
  = NShapeBox
  | NShapeCircle
  | NShapeDiamond
  | NShapeDoubleCircle
  | NShapeDoubleOctagon
  | NShapeEgg
  | NShapeEllipse
  | NShapeHexagon
  | NShapeHouse
  | NShapeInvTriangle
  | NShapeInvTrapezium
  | NShapeInvHouse
  | NShapeOctagon
  | NShapeMDiamond
  | NShapeMSquare
  | NShapeMCircle
  | NShapeParallelogram
  | NShapePentagon
  | NShapePlainText
  | NShapePolygon
  | NShapePoint
  | NShapeRect
  | NShapeRectangle
  | NShapeSeptagon
  | NShapeTrapezium
  | NShapeTriangle
  | NShapeTripleOctagon
  | NShapeNone

instance toString NodeShape
instance ==       NodeShape
derive gEq NodeShape // PK++

:: NodeStyle
  = NStyleFilled
  | NStyleInvis
  | NStyleDiagonals
  | NStyleRounded
  | NStyleDashed
  | NStyleDotted
  | NStyleSolid
  | NStyleBold

instance toString NodeStyle
instance ==       NodeStyle
derive gEq NodeStyle // PK++

:: EdgeStyle
  = EStyleSolid
  | EStyleBold
  | EStyleDashed
  | EStyleDotted
  | EStyleInvis

instance toString EdgeStyle
instance ==       EdgeStyle
derive gEq EdgeStyle // PK++

:: Color
  = RGB   Int  Int  Int
  | HSV   Real Real Real
  | Color String          // X11 1.2 color names; see rgb.txt

CBlack   :== Color "black"
CWhite   :== Color "white"
CGray    :== Color "gray"
CRed     :== Color "red"
CGreen   :== Color "green"
CBlue    :== Color "blue"
CYellow  :== Color "yellow"

instance toString Color
instance ==       Color
derive gEq Color // PK++

:: ArrowType =
  { closest       :: Arrow
  , furthest      :: Maybe Arrow
  }

:: Arrow =
  { open          :: Bool
  , side          :: Maybe Side
  , shape         :: ArrowShape
  }

:: Side
  = SideL
  | SideR

:: ArrowShape
  = AShapeBox
  | AShapeCrow
  | AShapeDiamond
  | AShapeDot
  | AShapeInv
  | AShapeNone
  | AShapeNormal
  | AShapeTee
  | AShapeVee

instance toString ArrowType
instance ==       ArrowType
derive gEq ArrowType // PK++

// direction of the edge
:: DirType
    = DTForward
    | DTBack
    | DTBoth
    | DTNone

instance toString DirType
instance ==       DirType
derive gEq DirType // PK++

layersep :== ":\t"

