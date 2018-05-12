implementation module iTasks.Internal.Client.LinkerSupport

from StdFunc import id
import StdString, StdList, StdFile, StdTuple
import Data.Maybe, System.File
import graph_to_sapl_string

from StdOverloaded import class <
from StdClass import class Ord, class Eq
from Data.Map import :: Map, newMap, get, put, toList, toAscList, foldrWithKey
from Data.Set import :: Set, newSet

from iTasks.UI.JS.Interface import :: JSWorld, :: JSEvent, :: JSObj, :: JSObject, :: JSVal

from iTasks.Internal.Client.RunOnClient import createClientIWorld, getUIUpdates

import iTasks.Internal.IWorld
import Sapl.Target.JS.CodeGeneratorJS, Sapl.Linker.LazyLinker, Sapl.SaplParser

editorLinker :: !f !*IWorld -> *(!MaybeErrorString (!String,!String),!*IWorld)
editorLinker initUIFunc iworld=:{world,current={sessionInstance=Nothing}} = (Error "Could not link editlet javascript: no session instance",iworld)
editorLinker initUIFunc iworld=:{world,current={sessionInstance=Just currentInstance}
									   ,jsCompilerState=Just jsCompilerState=:{loaderState,functionMap,flavour,parserState,skipMap}}
	// Create per sesssion "linker state"
	# linkerstate = (loaderState, functionMap, maybe newSet id Nothing /*(get currentInstance skipMap)*/)
	/* 1. First, we collect all the necessary function definitions to generate ParserState */
	# (linkerstate, lib, sapl_IU, world) = linkByExpr linkerstate newAppender (graph_to_sapl_string initUIFunc) world
	// unwrap linker state
	# (loaderState, functionMap, skipset) = linkerstate
	/* 2. Generate function definitions and ParserState */
	# sapl_lib = toString lib
	# mbInitPs = case sapl_lib of
		"" = Ok (newAppender, parserState)
		   = case generateJS flavour False sapl_lib parserState of
				Ok (script, pst) = Ok (script,Just pst)
				Error e 		 = Error e
	| mbInitPs =:(Error _)
		= (liftError mbInitPs, {iworld & world=world, jsCompilerState = Just jsCompilerState})
	# (js_lib, parserState) = fromOk mbInitPs
	/* 3. Generate expressions by ParserState */
	# mbExprPs = exprGenerateJS flavour False sapl_IU parserState js_lib
	| mbExprPs =:(Error _)
		= (liftError mbExprPs, {iworld & world=world, jsCompilerState = Just jsCompilerState})
	# (js_IU, js_lib, parserstate) = fromOk mbExprPs
	/* Update global compiler state */
	# jsCompilerState 
		= {jsCompilerState & loaderState = loaderState, parserState = parserState
		  , functionMap = functionMap, flavour = flavour, skipMap = put currentInstance skipset skipMap}

	= (Ok (toString js_lib, js_IU),{iworld & world=world, jsCompilerState = Just jsCompilerState})

editletLinker initUIFunc iworld = (Error "Could not link editlet javascript: js compiler not initialized",iworld)
