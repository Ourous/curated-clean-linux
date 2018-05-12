implementation module ESMVizTool.GraphvizVisualization

//import GenVisualize, GenUpdate, GenPrint, GenParse
//import iTasks.Framework.Generic, iTasks.Framework.UIDefinition, iTasks.Framework.IWorld, iTasks.API.Core.Types
import iTasks._Framework.Generic, iTasks.UI.Definition, iTasks._Framework.IWorld, iTasks.API.Core.Types
//from Data.Error import :: MaybeError (..)
import Data.Error, Data.Void
//import Util, Error, HttpUtil, IWorld
import System.Time
import qualified Data.Map as DM
import StdFile, StdTuple, StdList, StdBool, StdArray, StdString

from Text		import qualified class Text(..), instance Text String
import qualified System.Process as SP
from System.File		import readFile, writeFile, getFileInfo, :: FileError, :: FileInfo(..)

from System.Directory import createDirectory
from System.Directory import :: FilePath(..), :: MaybeOSError, :: OSError, :: OSErrorCode, :: OSErrorMessage

import Data.Graphviz

derive bimap (,), Maybe
derive class iTask	NodeState, Arrow, ArrowShape, ClusterMode, CompassPoint, DotPoint, 
					EdgeAttribute, GraphAttribute, LayerId, LayerList, LayerRange, Margin, 
					NodeAttribute, NodeDef, OutputMode, Pad, PageDir, Pointf, 
					RankDir, RankType, Ratio, Rect, SelectedItem, Side, Sizef, StartStyle, StartType, ViewPort

derive gText			Digraph, ArrowType, Color, DirType, EdgeStyle, NodeShape, NodeStyle
derive gUpdate			Digraph, ArrowType, Color, DirType, EdgeStyle, NodeShape, NodeStyle
derive gVerify			Digraph, ArrowType, Color, DirType, EdgeStyle, NodeShape, NodeStyle
derive JSONEncode		Digraph, ArrowType, Color, DirType, EdgeStyle, NodeShape, NodeStyle
derive JSONDecode		Digraph, ArrowType, Color, DirType, EdgeStyle, NodeShape, NodeStyle
derive gEditor			ArrowType, Color, DirType, EdgeStyle, NodeShape, NodeStyle
derive gEditMeta		Digraph, ArrowType, Color, DirType, EdgeStyle, NodeShape, NodeStyle
derive gDefault			Digraph, ArrowType, Color, DirType, EdgeStyle, NodeShape, NodeStyle
derive gEq				Digraph

config_file_name		:== "Graphviz.config"
commentsymbol			:== '%'
dot_exe_path_name		:== "DOT_PATH"
public 					:== "WebPublic"
target file				= public + "/" + file
toGIF file				= ["-Tgif","-o",gifext file,dotext file]
//toGIF file				= ["-Tgif","-O ",dotext file]
toMAP file name			= ["-Tcmapx","-Glabel=" + name,"-o ","\"" + mapext file + "\"", "\"" + dotext file + "\""]
gifext file				= file + ".gif"
mapext file				= file + ".map"
dotext file				= file + ".dot"

instance + String where (+) a b = a +++ b

undef = undef

gEditor{|Digraph|} _ (digraph, _, _) meta vst = mkControl vst
  where
  mkControl vst=:{VSt|iworld,taskId}
      # (sv, iworld)    = iworld!server
      # filename		= imgname taskId sv.serverName
      # (mbErr, iworld)	= runGraphviz filename (printDigraph (enhanceDigraphWithLinks digraph)) iworld
      = case mbErr of
          Ok _
              = ( NormalEditor [(UIViewHtml defaultSizeOpts {UIViewOpts|value = Just (RawText("<img src=\"/" + (gifext filename) + "\" usemap=\"#" + filename + "\" />"))}, 'DM'.newMap)]
              	, {VSt|vst & iworld = iworld})
          Error msg
              = (NormalEditor [(UIViewHtml defaultSizeOpts {UIViewOpts|value = Just (Text msg)}, 'DM'.newMap)], {VSt|vst & iworld = iworld})

  runGraphviz :: String [String] *IWorld -> (!MaybeErrorString Void,!*IWorld)
  runGraphviz name dotcode iworld=:{IWorld|world}
      # (mbExe,world)		= obtainValueFromConfig dot_exe_path_name world
      | isNothing mbExe	= (Error ("Could not obtain " +++ dot_exe_path_name +++ " from " +++ config_file_name +++ "."), {iworld & world = world})
      # exe				= fromJust mbExe
      # (mbErr,world)		= ensureDirectory public world
      | isError mbErr		= (Error ("Could not create directory " + (target "")), {iworld & world = world})
      # (mbErr,world)		= writeFile (target (dotext name)) ('Text'.join "\n" dotcode) world
      | isError mbErr		= (Error ("Could not write Digraph to " + target (dotext name) + "."), {iworld & world = world})
      # (mbExit,world)	= 'SP'.callProcess exe (toGIF (target name)) Nothing world
      | isError mbExit	= (Error ("Creation of " + gifext (target name) + " failed"), {iworld & world = world})
      # (mbExit,world)	= 'SP'.callProcess exe (toMAP name (target name)) Nothing world
      | isError mbExit	= (Error ("Creation of " + mapext (target name) + " failed"), {iworld & world = world})
      # (mbMap,world)		= readFile (mapext (target name)) world	
      = (Ok Void, {iworld & world = world})

  enhanceDigraphWithLinks (Digraph name graphAtts nodeDefs selected)
      = Digraph name graphAtts
          [  NodeDef nr st [ NAttURL "#" : nodeAtts ] edges 
          \\ NodeDef nr st nodeAtts edges <- nodeDefs
          ] selected

  imgname taskId name	= (toString taskId) + "-" + name

//gVisualizeEditor{|Digraph|} Nothing vst			= noVisualization vst				
//gVisualizeEditor{|Digraph|} (Just digraph) vst	= visualizeCustom mkControl vst
//where
	//mkControl name touched verRes vst=:{VSt|iworld,taskId}
		//# filename			= imgname taskId name
		//# (mbErr, iworld)	= runGraphviz filename (printDigraph (enhanceDigraphWithLinks digraph)) iworld
		//= case mbErr of
			//Ok _
				//= ([(UIViewHtml defaultSizeOpts {UIViewOpts|value = Just (RawText("<img src=\"/" + (gifext filename) + "\" usemap=\"#" + filename + "\" />"))},newMap)], {VSt|vst & iworld = iworld})
			//Error msg
				//= ([(UIViewHtml defaultSizeOpts {UIViewOpts|value = Just (Text msg)},newMap)], {VSt|vst & iworld = iworld})
		
	//runGraphviz :: String [String] *IWorld -> (!MaybeErrorString Void,!*IWorld)
	//runGraphviz name dotcode iworld=:{IWorld|world}
		//# (mbExe,world)		= obtainValueFromConfig dot_exe_path_name world
		//| isNothing mbExe	= (Error ("Could not obtain " +++ dot_exe_path_name +++ " from " +++ config_file_name +++ "."), {iworld & world = world})
		//# exe				= fromJust mbExe
		//# (mbErr,world)		= ensureDirectory public world
		//| isError mbErr		= (Error ("Could not create directory " + (target "")), {iworld & world = world})
		//# (mbErr,world)		= writeFile (target (dotext name)) ('Text'.join "\n" dotcode) world
		//| isError mbErr		= (Error ("Could not write Digraph to " + target (dotext name) + "."), {iworld & world = world})
		//# (mbExit,world)	= 'SP'.callProcess exe (toGIF (target name)) Nothing world
		//| isError mbExit	= (Error ("Creation of " + gifext (target name) + " failed"), {iworld & world = world})
		//# (mbExit,world)	= 'SP'.callProcess exe (toMAP name (target name)) Nothing world
		//| isError mbExit	= (Error ("Creation of " + mapext (target name) + " failed"), {iworld & world = world})
		//# (mbMap,world)		= readFile (mapext (target name)) world	
		//= (Ok Void, {iworld & world = world})

	//enhanceDigraphWithLinks (Digraph name graphAtts nodeDefs selected)
		//= Digraph name graphAtts 
			//[  NodeDef nr st [ NAtt_URL "#" [>("#")<] : nodeAtts ] edges 
			//\\ NodeDef nr st nodeAtts edges <- nodeDefs
			//] selected

	//imgname taskId name	= (toString taskId) + "-" + name

/*
where
	taskfun tst=:{taskNr}
		# (events,tst)		= getEvents tst
		# node = (http_getValue "node" events "")
		| node <> ""
			= (TaskFinished node, tst)
		# (value,tst)		= accWorldTSt (obtainValueFromConfig dot_exe_path_name) tst
		| isNothing value	= error ("Could not obtain " + dot_exe_path_name + " from " + config_file_name + ".") tst
		# exe				= fromJust value
		# tst				= appWorldTSt (ensureDirectory (target "")) tst
		# (ok,tst)			= accWorldTSt (writefile` (target (dotext name)) (printDigraph (enhanceDigraphWithLinks digraph))) tst
		| not ok			= error ("Could not write Digraph to " + target (dotext name) + ".") tst
		# ((ok,exit),tst)	= accWorldTSt (collect3 (launch exe (toGIF (target name)))) tst
		| not ok			= error ("Creation of " + gifext (target name) + " failed. Exit code = " + toString exit + ".") tst
		# ((ok,exit),tst)	= accWorldTSt (collect3 (launch exe (toMAP (target name) name))) tst
		| not ok			= error ("Creation of " + mapext (target name) + " failed. Exit code = " + toString exit + ".") tst
		# ((ok,lines),tst)	= accWorldTSt (collect3 (readfile` (mapext (target name)))) tst
		| not ok			= error ("Reading of " + mapext (target name) + " failed.") tst	
		# lines				= map enhanceMAPlineWithOnClickEvent lines
		# html				= "<img src=\"" + (gifext name) + "\" usemap=\"#" + name + "\" />" + join "" lines + toString legend
		# taskpanel 		= TUIHtmlContainer
								{ TUIHtmlContainer
								| id = "taskform-" + taskid + "-graph"
								, html = html							
								}
		# tst = setTUIDef ([taskpanel],[]) [] tst
		= (TaskBusy, tst)
	where
		name	= iTaskId taskNr "Graphviz"
		taskid	= taskNrToString taskNr
		
		error msg tst
			# tst = setTUIDef ([TUIHtmlContainer
									{ TUIHtmlContainer
									| id = "taskform-" + taskid + "-graph"
									, html = msg
									}],[]) [] tst
			= (TaskBusy, tst)

		enhanceMAPlineWithOnClickEvent :: !String -> String
		enhanceMAPlineWithOnClickEvent line
			| line%(0,5) == "<area "
				| size line <= 6 || isNothing href_bounds 
							= line
				| otherwise	= line%(0,fst href-1) + 
							  "onclick=\"fireTaskEvent('" + taskid + "','node','" + titletext + "');\" " + 
							  line%(snd href+1,size line-1)
			| line%(0,4) == "<map "
							= "<map id=\"" + name + "\" name=\"" + name + "\">\n"
			| otherwise
							= line
		where
			href_bounds		= boundsOfKeyValue "href="  line
			title_bounds	= boundsOfKeyValue "title=" line
			href			= fromJust href_bounds
			title			= fromJust title_bounds
			titletext		= line%(fst title+7,snd title-1)
			
		legend  =	DivTag []
						[ H3Tag [] [Text "Legend:"]
						, Text "Double circled state: intial state.", BrTag []
						, Text "Red state: current state (change state by click).", BrTag []
						, Text "Blue state: all defined inputs are shown.", BrTag []
						, Text "Blue transitions: on current trace.", BrTag []
						, Text "Red transitions: an issue was found on this transition."
						]
*/			
boundsOfKeyValue :: !String !String -> Maybe (!Int,!Int)
boundsOfKeyValue key str
	= case [i \\ i<-[0..size str-size key] | str%(i,i+size key-1) == key] of
		[i : _]			= case [j \\ j<-[i..size str-1] | str.[j]=='\"'] of
							[_,close:_] = Just (i,close)
							otherwise	= Nothing
		otherwise		= Nothing

obtainValueFromConfig :: !String !*env -> (!Maybe String,!*env) | FileSystem env
obtainValueFromConfig name env
	# (ok,file,env)		= fopen config_file_name FReadText env
	| not ok			= (Nothing,env)
	# (value,file)		= obtainValueFromFile name file
	# (ok,env)			= fclose file env
	| not ok			= (Nothing,env)
	| otherwise			= (value,  env)
where
	obtainValueFromFile :: !String !*File -> (!Maybe String,!*File)
	obtainValueFromFile name file
		# (lines,file)	= readlines file
		# value			= case [ skipSpace (line%(name_length,size line-2)) \\ line<-lines
						                                                     | line.[0] <> commentsymbol 
						                                                    && size line > name_length
						                                                    && line%(0,name_length-1) == name
						       ] of [v:_]	= Just v
						            _		= Nothing
		= (value,file)
	where
		name_length		= size name

ensureDirectory :: !String !*World -> (MaybeOSError (), *World)
ensureDirectory pathname world
	# (mbInfo,world)	= getFileInfo pathname world
	= case mbInfo of
		Ok info
			| info.directory	= (Ok (), world)
								= (Error (1,pathname +++ " is not a directory"), world)
		_	= createDirectory pathname world

readlines :: !*File -> (![String],!*File)
readlines file
	# (end,file)		= fend file
	| end				= ([],file)
	# (line, file)		= freadline file
	# (lines,file)		= readlines file
	= ([line:lines],file)

skipSpace :: !String -> String
skipSpace str			= toString (dropWhile isSpace (fromString str))
