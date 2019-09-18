implementation module iTasks.Extensions.Editors.DynamicEditor

import StdEnv => qualified foldl
import StdMisc, Data.Tuple, Text, Data.Maybe, Text.GenPrint
from StdFunc import seq, flip
from Data.Tuple import appFst
import iTasks, iTasks.UI.Definition, iTasks.UI.Editor.Common, iTasks.UI.Editor.Modifiers
import qualified Data.Map as Map
from Data.Func import $
from Data.List import zip3, intersperse
import Data.Functor

:: DynamicCons =
    { consId           :: !DynamicConsId
    , label            :: !String
    , builder          :: !DynamicConsBuilder
    , showIfOnlyChoice :: !Bool
    , useAsDefault     :: !Bool
    , uiAttributes     :: !UIAttributes
    }

(<<@@@) infixl 2 :: !DynamicCons !DynamicConsOption -> DynamicCons
(<<@@@) cons opt = tunedDynamicConsEditor opt cons

(@@@>>) infixr 2 :: !DynamicConsOption !DynamicCons -> DynamicCons
(@@@>>) opt cons = cons <<@@@ opt

tunedDynamicConsEditor :: !DynamicConsOption !DynamicCons -> DynamicCons
tunedDynamicConsEditor HideIfOnlyChoice cons = {cons & showIfOnlyChoice = False}
tunedDynamicConsEditor UseAsDefault     cons = {cons & useAsDefault = True}
tunedDynamicConsEditor (ApplyCssClasses classes) cons
	= {cons & uiAttributes = 'Map'.union (classAttr classes) cons.uiAttributes}

functionCons :: !String !String !a -> DynamicCons | TC a
functionCons consId label func = functionConsDyn consId label (dynamic func)

functionConsDyn :: !String !String !Dynamic -> DynamicCons
functionConsDyn consId label func = { consId           = consId
                                    , label            = label
                                    , builder          = FunctionCons func
                                    , showIfOnlyChoice = True
                                    , useAsDefault     = False
                                    , uiAttributes     = 'Map'.newMap
                                    }

listCons :: !String !String !([a] -> b) -> DynamicCons | TC a & TC b
listCons consId label func = listConsDyn consId label (dynamic func)

listConsDyn :: !String !String !Dynamic -> DynamicCons
listConsDyn consId label func = { consId           = consId
                                , label            = label
                                , builder          = ListCons func
                                , showIfOnlyChoice = True
                                , useAsDefault     = False
                                , uiAttributes     = 'Map'.newMap
                                }

customEditorCons :: !String !String !(Editor a) -> DynamicCons
                  | TC, JSONEncode{|*|}, JSONDecode{|*|}, gText{|*|} a
customEditorCons consId label editor = { consId           = consId
                                       , label            = label
                                       , builder          = CustomEditorCons editor
                                       , showIfOnlyChoice = True
                                       , useAsDefault     = False
                                       , uiAttributes     = 'Map'.newMap
                                       }

// TODO: don't use aborts here
toValue :: !(DynamicEditor a) !(DynamicEditorValue a) -> a | TC a
toValue (DynamicEditor elements) (DynamicEditorValue cid val) = case toValue` (cid, val) of
    (v :: a^) = v
    _         = abort "corrupt dynamic editor value"
where
    toValue` :: !(!DynamicConsId, !DEVal) -> Dynamic
    toValue` (cid, val) = case val of
        DEApplication args = case cons.builder of
            FunctionCons fbuilder = toValueFunc fbuilder args
            ListCons     lbuilder = toValueList lbuilder args
            _                     = abort "corrupt dynamic editor value"
        DEJSONValue json = case cons.builder of
            CustomEditorCons editor = toValueGen editor json
            _                       = abort "corrupt dynamic editor value"
    where
        (cons, _) = consWithId cid $ consesOf elements

    toValueFunc :: !Dynamic ![(!DynamicConsId, !DEVal)] -> Dynamic
    toValueFunc v [] = v
    toValueFunc f [x : xs] = case (f, toValue` x) of
        (f :: a -> b, x :: a) = toValueFunc (dynamic (f x)) xs
        _                     = abort "corrupt dynamic editor value"

    toValueGen :: (Editor a) !JSONNode -> Dynamic | JSONDecode{|*|}, TC a
    toValueGen editor json = dynamic (fromJSON` editor json)
    where
        fromJSON` :: (Editor a) !JSONNode -> a | JSONDecode{|*|} a
        fromJSON` _ json = fromMaybe (abort "corrupt dynamic editor value") $ fromJSON json

    toValueList :: !Dynamic ![(!DynamicConsId, !DEVal)] -> Dynamic
    toValueList (f :: [a] -> b) [] = dynamic (f [])
    toValueList f args=:[fst : _] = case (f, toValue` fst) of
        (g :: [a] -> b, _ :: a) -> dynamic (g $ fromDynList [toValue` val \\ val <- args])
        _                       -> abort "corrupt dynamic editor value"
    toValueList _ _ = abort "corrupt dynamic editor value"

    fromDynList :: ![Dynamic] -> [a] | TC a
    fromDynList dyns = fromDynList` dyns []
    where
        fromDynList` [] acc = reverse acc
        fromDynList` [(a :: a^) : dyns] acc = fromDynList` dyns [a:acc]
        fromDynList` _ _ = abort "corrupt dynamic editor value"

dynEditorValToString :: !(DynamicEditor a) !(DynamicEditorValue a) -> String
dynEditorValToString (DynamicEditor elements) (DynamicEditorValue cid val) =
	concat $ withCapitalisedFirstLetter $
		dropWhile (\s -> textSize (trim s) == 0) $ reverse [".": dynEditorValToString` (cid, val) []]
where
	withCapitalisedFirstLetter [firstString: rest] = [upperCaseFirst firstString: rest]

    dynEditorValToString` :: !(!DynamicConsId, !DEVal) ![String] -> [String]
    dynEditorValToString` (cid, val) accum = case val of
        DEApplication args = case cons.builder of
            FunctionCons fbuilder = 'StdEnv'.foldl (flip dynEditorValToString`)
                                                   [" ", cons.DynamicCons.label : accum]
                                                   args
            ListCons lbuilder
                # listElStrs = flatten $ intersperse [" ", cons.DynamicCons.label] $
                                                     (\arg -> dynEditorValToString` arg []) <$> reverse args
                = listElStrs ++ [" "] ++ accum
            _ = abort "corrupt dynamic editor value"
        DEJSONValue json = case cons.builder of
            CustomEditorCons editor = [ " ", toStringGen editor json
                                      , " ", cons.DynamicCons.label
                                      : accum
                                      ]
            _ = abort "corrupt dynamic editor value"
    where
        (cons, _) = consWithId cid $ consesOf elements

    toStringGen :: (Editor a) !JSONNode -> String | gText{|*|}, JSONDecode{|*|}  a
    toStringGen editor json = toSingleLineText $ fromJSON` editor json
    where
        fromJSON` :: (Editor a) !JSONNode -> a | JSONDecode{|*|} a
        fromJSON` _ json = fromMaybe (abort "corrupt dynamic editor value") $ fromJSON json

derive class iTask DynamicEditorValue, DEVal

:: E = E.a: E (Editor (DynamicEditorValue a))
:: ConsType = Function | List | CustomEditor

derive JSONEncode ConsType
derive JSONDecode ConsType

parametrisedDynamicEditor
	:: !(p -> DynamicEditor a) -> Editor (!p, !DynamicEditorValue a)
	| TC a & gEq{|*|}, JSONEncode{|*|}, JSONDecode{|*|} p
parametrisedDynamicEditor editor
	= compoundEditorToEditor
		{CompoundEditor| genUI = genUI, onEdit = onEdit, onRefresh = onRefresh, valueFromState = valueFromState}
where
	genUI attr dp mode vst
		= case editModeValue mode of
			Nothing
				= abort "Enter mode not supported by parametrisedDynamicEditor.\n"
			Just (p, _)
				= appFst
					(fmap $ appSnd3 \st -> (p, st))
					((dynamicCompoundEditor $ editor p).CompoundEditor.genUI attr dp (mapEditMode snd mode) vst)

	onEdit dp event (p, mbSt) childSts vst
		= appFst
			(fmap $ appSnd3 \st -> (p, st))
			((dynamicCompoundEditor $ editor p).CompoundEditor.onEdit dp event mbSt childSts vst)

	onRefresh dp (p, new) st=:(p`, mbSt) childSts vst
		| p === p` =
			appFst
				(fmap $ appSnd3 \st -> (p, st))
				((dynamicCompoundEditor $ editor p).CompoundEditor.onRefresh dp new mbSt childSts vst)
		| otherwise =
			appFst
				(fmap $ \(ui, st, childSts) -> (ReplaceUI ui, (p, st), childSts))
				((dynamicCompoundEditor $ editor p).CompoundEditor.genUI 'Map'.newMap dp (Update new) vst)

	valueFromState (p, st) childSts
		= (\val -> (p, val)) <$> (dynamicCompoundEditor $ editor p).CompoundEditor.valueFromState st childSts

dynamicEditor :: !(DynamicEditor a) -> Editor (DynamicEditorValue a) | TC a
dynamicEditor dynEditor = compoundEditorToEditor $ dynamicCompoundEditor dynEditor

dynamicCompoundEditor
	:: !(DynamicEditor a) -> CompoundEditor (Maybe (!DynamicConsId, !ConsType)) (DynamicEditorValue a) | TC a
dynamicCompoundEditor dynEditor=:(DynamicEditor elements)
	| not $ isEmpty duplicateIds
		= abort $ concat ["duplicate cons IDs in dynamic editor: ", printToString duplicateIds, "\n"]
	= {CompoundEditor| genUI = genUI, onEdit = onEdit, onRefresh = onRefresh, valueFromState = valueFromState}
where
    // conses with optional group labels
    conses :: [(!DynamicCons, !Maybe String)]
    conses = consesOf elements

    duplicateIds = duplicateIds` $ (\(b, _) -> b.consId) <$> conses
    where
		duplicateIds` [] = []
		duplicateIds` [x: xs]
			| isMember x xs = [x: duplicateIds` xs]
			| otherwise     = duplicateIds` xs

    genUI :: !UIAttributes !DataPath !(EditMode (DynamicEditorValue a)) !*VSt
          -> *(!MaybeErrorString (!UI, !Maybe (!DynamicConsId, !ConsType), ![EditState]), !*VSt)
    genUI attr dp mode vst=:{VSt|taskId} = case mode of
        Enter = case matchingConses of
            [(onlyChoice, _)] | hideCons
                # (mbUis, _, type, _, vst) = genChildEditors dp onlyChoice.consId Enter vst
                # mbUis = ( \(uis, childSts) -> (uiContainer attr uis, Just (onlyChoice.consId, type), [nullState: childSts])
                          ) <$>
                          mbUis
                = (mbUis, vst)
            _ = case filter (\(cons, _) -> cons.useAsDefault) matchingConses of
                [(defaultChoice, _): _]
                    # (mbUis, idx, type, label, vst) = genChildEditors dp defaultChoice.consId Enter vst
                    = case mbUis of
                        Ok (uis, childSts)
                            | hideCons
                                = (Ok (uiContainer attr uis, Just (defaultChoice.consId, type), [nullState: childSts]), vst)
                            | otherwise
                                # (consChooseUI, chooseSt) = genConsChooseUI taskId dp (Just idx)
                                = ( Ok ( uiContainer attr [consChooseUI: uis]
                                       , Just (defaultChoice.consId, type)
                                       , [chooseSt: childSts]
                                       )
                                  , vst
                                  )
                        Error e = (Error e, vst)
                _
                    # (consChooseUI, chooseSt) = genConsChooseUI taskId dp Nothing
                    = (Ok (uiContainer attr [consChooseUI], Nothing, [chooseSt]), vst)
		Update Undefined = genUI attr dp Enter vst
        Update (DynamicEditorValue cid val)
            # (mbUis, idx, type, label, vst) = genChildEditors dp cid (Update val) vst
            = case mbUis of
                Ok (uis, childSts)
                    | hideCons
                        = (Ok (uiContainer attr uis, Just (cid, type), [nullState: childSts]), vst)
                    | otherwise
                        # (consChooseUI, chooseSt) = genConsChooseUI taskId dp (Just idx)
                        = (Ok (uiContainer attr [consChooseUI: uis], Just (cid, type), [chooseSt: childSts]), vst)
                Error e = (Error e, vst)

        View (DynamicEditorValue cid val)
            # (mbUis, _, type, label, vst) = genChildEditors dp cid (View val) vst
            = case mbUis of
                Ok (uis, childSts)
                    | hideCons
                        = (Ok (uiContainer attr uis, Just (cid, type), [nullState: childSts]), vst)
                    | otherwise
                        # consChooseUI = uia UITextView $ valueAttr $ JSONString label
                        = (Ok (uiContainer attr [consChooseUI: uis], Just (cid, type), [nullState: childSts]), vst)
                Error e = (Error e, vst)

    genConsChooseUI taskId dp mbSelectedCons = (consChooseUI, consChooseSt)
    where
        consOptions = [ JSONObject $ [("id",JSONInt i),("text",JSONString cons.DynamicCons.label)] ++
                                     maybe [] (\label -> [("grouplabel", JSONString label)]) mbGroupLabel
                      \\ (cons, mbGroupLabel) <- matchingConses & i <- [0..]
                      ]
        consChooseUI = uia UIDropdown
                           ( 'Map'.put "width" JSONNull $
                             choiceAttrs taskId (editorId dp) (maybe [] (\x -> [x]) mbSelectedCons) consOptions
                           )
        consChooseSt = LeafState {touched=False,state=maybe JSONNull (\x -> JSONInt x) mbSelectedCons}

    onEdit :: !DataPath
              !(!DataPath, !JSONNode)
              !(Maybe (!DynamicConsId, !ConsType))
              ![EditState]
              !*VSt
           -> *( !MaybeErrorString (!UIChange, !Maybe (!DynamicConsId, !ConsType), ![EditState])
               , !*VSt
               )
    // new builder is selected: create a UI for the new builder
    onEdit dp ([], JSONArray [JSONInt builderIdx]) _ [_: childrenSts] vst
        | builderIdx < 0 || builderIdx >= length matchingConses
            = (Error "Dynamic editor selection out of bounds", vst)
        # (cons, _) = matchingConses !! builderIdx
        # (mbRes, _, type, _, vst) = genChildEditors dp cons.consId Enter vst
        = case mbRes of
            Ok (uis, childSts)
                // insert new UIs for arguments
                # inserts = [(i, InsertChild ui) \\ ui <- uis & i <- [1..]]
                # removals = removeNChildren $ length childrenSts
                # change = ChangeUI [] (removals ++ inserts)
                # builderChooseState = LeafState {touched = True, state = JSONInt $ length uis}
                = (Ok (change, Just (cons.consId, type), [builderChooseState: childSts]), vst)
            Error e = (Error e, vst)

    // other events targeted directly at this building cons
    onEdit dp ([],e) _ [_: childSts] vst
        | e =: JSONNull || e =: (JSONArray []) // A null or an empty array are accepted as a reset events
            //If necessary remove the fields of the previously selected cons
            # change = ChangeUI [] $ removeNChildren $ length childSts
            = (Ok (change, Nothing, [nullState: childSts]), vst)
        | otherwise
            = (Error $ concat ["Unknown dynamic editor select event: '", toString e, "'"], vst)

    // update is targeted somewhere inside this value    
    onEdit dp ([argIdx: tp], e) (Just (cid, type)) childSts vst
        # (cons, _) = consWithId cid matchingConses
        # (res, vst) = case cons.builder of
            FunctionCons fbuilder
                # children = childrenEditors fbuilder
                | argIdx < 0 || argIdx >= length children
                    = (Error "Edit event for dynamic editor has invalid path", vst)
                # (E editor) = children !! argIdx
                = editor.Editor.onEdit (dp ++ [argIdx]) (tp, e) (childSts !! (argIdx + 1)) vst
            ListCons lbuilder
                = (listBuilderEditor lbuilder cons.uiAttributes).Editor.onEdit (dp ++ [0]) (tp, e) (childSts !! 1) vst
            CustomEditorCons editor
                = editor.Editor.onEdit (dp ++ [0]) (tp, e) (childSts !! 1) vst
        = case res of
            Ok (change, childSt)
                # change = ChangeUI [] [(argIdx + if hideCons 0 1, ChangeChild change)]
                // replace state for this child
                = (Ok (change, Just (cid, type), updateAt (argIdx + 1) childSt childSts), vst)
            Error e = (Error e, vst)

    onEdit _ _ _ _ vst = (Error "Invalid edit event for dynamic editor.", vst)

    removeNChildren :: !Int -> [(!Int, !UIChildChange)]
    removeNChildren nrArgs = repeatn nrArgs (1, RemoveChild)

    childrenEditors :: !Dynamic -> [E]
    childrenEditors (f :: a -> b) = [E $ dynamicEditorFstArg f : childrenEditors (dynamic (f undef))]
    where
        // first argument only used for type
        dynamicEditorFstArg :: (a -> b) -> Editor (DynamicEditorValue a) | TC a
        dynamicEditorFstArg _ = dynamicEditor $ DynamicEditor elements
    childrenEditors _         = []

    onRefresh :: !DataPath
                 !(DynamicEditorValue a)
                 !(Maybe (!DynamicConsId, !ConsType))
                 ![EditState]
                 !*VSt
              -> *( !MaybeErrorString ( !UIChange
                                      , !Maybe (!DynamicConsId, !ConsType)
                                      , ![EditState]
                                      )
                  , !*VSt
                  )
	// TODO: how to get UI attributes?
	// TODO: fine-grained replacement
    onRefresh dp new st childSts vst
		| isNotChanged (valueFromState st childSts) new = (Ok (NoChange, st, childSts), vst)
		= appFst (fmap $ appFst3 ReplaceUI) $ genUI 'Map'.newMap dp (Update new) vst
	where
		isNotChanged (Just (DynamicEditorValue consId val)) (DynamicEditorValue consId` val`) =
			consId == consId` && val === val`
		isNotChanged _ _ = False

    // TODO: accept ID or index
    genChildEditors :: !DataPath !DynamicConsId !(EditMode DEVal) !*VSt
                    -> *(!MaybeErrorString (![UI], ![EditState]), Int, ConsType, String, !*VSt)
    genChildEditors dp cid mode vst= case cons.builder of
        FunctionCons fbuilder
            # (mbUis, vst) = genChildEditors` (reverse $ zip3 vals (childrenEditors fbuilder) [0..]) [] [] vst
            = (mbUis, idx, type, cons.DynamicCons.label, vst)
        where
            genChildEditors` [] accUi accSt vst = (Ok (accUi, accSt), vst)
            genChildEditors` [(mbVal, E editor, i): children] accUi accSt vst =
                case editor.Editor.genUI cons.uiAttributes (dp ++ [i]) (maybe Enter (if viewMode View Update) mbVal) vst of
                    (Ok (ui, st), vst) = genChildEditors` children [ui: accUi] [st: accSt] vst
                    (Error e,     vst) = (Error e, vst)

            vals :: [Maybe (DynamicEditorValue a)]
            vals = case editModeValue mode of
                // update or view mode
                Just (DEApplication children) = [Just $ DynamicEditorValue cid val \\ (cid, val) <- children]
                // enter mode
                _                             = repeat Nothing
        ListCons lbuilder
            # listEditorMode = mapEditMode (\(DEApplication listElems) -> listElems) mode
            # (mbUi, vst) = (listBuilderEditor lbuilder cons.uiAttributes).Editor.genUI 'Map'.newMap (dp ++ [0]) listEditorMode vst
            = ((\(ui, st) -> ([ui], [st])) <$> mbUi, idx, type, cons.DynamicCons.label, vst)
        CustomEditorCons editor
            # editorMode = mapEditMode
                (\(DEJSONValue json) -> fromMaybe (abort "Invalid dynamic editor state") $ fromJSON json)
                mode
            # (mbUi, vst) = editor.Editor.genUI cons.uiAttributes (dp ++ [0]) editorMode vst
            = ((\(ui, st) -> ([ui], [st])) <$> mbUi, idx, type, cons.DynamicCons.label, vst)
    where
        (cons, idx) = consWithId cid matchingConses
        type = case cons.builder of
            FunctionCons     _ = Function
            ListCons         _ = List
            CustomEditorCons _ = CustomEditor
        viewMode = mode =: View _
 
    hideCons = case matchingConses of
        [(onlyChoice, _)] | not onlyChoice.showIfOnlyChoice = True
        _                                                   = False

    matchingConses :: [(!DynamicCons, !Maybe String)]
    matchingConses = catMaybes $
        (\(cons, mbGroupLabel) -> (\cons` -> (cons`, mbGroupLabel)) <$> matchingCons dynEditor cons) <$> conses

    // first arg only used for type
    // packs matching conses, with possibly updated (= more specific) type
    matchingCons :: !(DynamicEditor a) !DynamicCons -> Maybe DynamicCons | TC a
    matchingCons dynEd cons=:{builder} = (\b -> {cons & builder = b}) <$> mbBuilder`
    where
        mbBuilder` = case builder of
            FunctionCons     fbuilder = matchf fbuilder
            CustomEditorCons editor   = matchc editor
            ListCons         lbuilder = matchl lbuilder

        // works for functions with upto 10 args
        // the type of the dynamic is updated by unifying the function result with the type produced by the editor
        matchf :: !Dynamic -> Maybe DynamicConsBuilder
        matchf b = case (b, dynamic dynEd) of
            (b :: a b c d e f g h i j -> z, _ :: DynamicEditor z) = Just $ FunctionCons (dynamic b)
            (b :: a b c d e f g h i   -> z, _ :: DynamicEditor z) = Just $ FunctionCons (dynamic b)
            (b :: a b c d e f g h     -> z, _ :: DynamicEditor z) = Just $ FunctionCons (dynamic b)
            (b :: a b c d e f g       -> z, _ :: DynamicEditor z) = Just $ FunctionCons (dynamic b)
            (b :: a b c d e f         -> z, _ :: DynamicEditor z) = Just $ FunctionCons (dynamic b)
            (b :: a b c d e           -> z, _ :: DynamicEditor z) = Just $ FunctionCons (dynamic b)
            (b :: a b c d             -> z, _ :: DynamicEditor z) = Just $ FunctionCons (dynamic b)
            (b :: a b c               -> z, _ :: DynamicEditor z) = Just $ FunctionCons (dynamic b)
            (b :: a b                 -> z, _ :: DynamicEditor z) = Just $ FunctionCons (dynamic b)
            (b :: a                   -> z, _ :: DynamicEditor z) = Just $ FunctionCons (dynamic b)
            (b ::                        z, _ :: DynamicEditor z) = Just $ FunctionCons (dynamic b)
            _                                                     = Nothing

        // custom editors do not allow for quantified variables, so no type update is required
        matchc e = case (dynamic e, dynamic dynEd) of
            (_ :: Editor a, _ :: DynamicEditor a) = Just $ CustomEditorCons e
            _                                     = Nothing

        matchl f = case (f, dynamic dynEd) of
            (f :: [a] -> b, _ :: DynamicEditor b) = Just $ ListCons (dynamic f)
            _                                     = Nothing

    listBuilderEditor :: !Dynamic !UIAttributes -> Editor [(!DynamicConsId, !DEVal)]
    listBuilderEditor (lbuilder :: [a] -> b) attrs = listEditor (Just $ const Nothing) True True Nothing childrenEd`
    where
        childrenEd  = childrenEditorList lbuilder
        childrenEd` = bijectEditorValue (\(cid, val)                   -> DynamicEditorValue cid val)
                                        (\(DynamicEditorValue cid val) -> (cid, val))
                                        childrenEd

        // first argument only used for type
        childrenEditorList :: ([a] -> b) -> Editor (DynamicEditorValue a) | TC a
        childrenEditorList _ = dynamicEditor (DynamicEditor elements) <<@ attrs
    listBuilderEditor _ _ = abort "dynamic editors: invalid list builder value"

    uiContainer :: !UIAttributes ![UI] -> UI
    uiContainer attr uis = UI UIContainer attr uis

    valueFromState :: !(Maybe (!DynamicConsId, !ConsType)) ![EditState] -> *Maybe (DynamicEditorValue a)
    valueFromState (Just (cid, CustomEditor)) [_: [editorSt]] =
        mapMaybe (DynamicEditorValue cid o DEJSONValue o toJSON`) $ editor.Editor.valueFromState editorSt
    where
        ({builder}, _) = consWithId cid conses

        // toJSON` is used to solve overloading, JSONEncode{|*|} is attached to CustomEditorCons
        (editor, toJSON`) = case builder of
            CustomEditorCons editor = (editor, toJSON)
            _                       = abort "corrupt dynamic editor state"

    valueFromState (Just (cid, type)) [_: childSts] =
        mapMaybe (\childVals -> DynamicEditorValue cid $ DEApplication childVals) $ childValuesFor childSts` []
    where
        childSts` = case (type, childSts) of
            (List, [CompoundState _ childSts]) = childSts
            (_,    childSts)                   = childSts

        childValuesFor :: ![EditState] ![(!DynamicConsId, !DEVal)]
                       -> Maybe [(!DynamicConsId, !DEVal)]
        childValuesFor [] acc = Just $ reverse acc
        childValuesFor [childSt: childSts] acc = case (dynamicEditor dynEditor).Editor.valueFromState childSt of
            Just (DynamicEditorValue childCid childVal) = childValuesFor childSts [(childCid, childVal): acc]
            _                                           = Nothing
    valueFromState _ _ = Nothing

consWithId :: !DynamicConsId ![(!DynamicCons, !Maybe String)] -> (!DynamicCons, !Int)
consWithId cid conses = case filter (\(({consId}, _), _) -> consId == cid) $ zip2 conses [0..] of
    [((cons, _), idx)] = (cons, idx)
    []                 = abort $ concat ["dynamic editor: cons not found: '",   cid, "'\n"]
    _                  = abort $ concat ["dynamic editor: duplicate conses: '", cid, "'\n"]

nullState :: EditState
nullState = LeafState {touched = True, state = JSONNull}

consesOf :: ![DynamicEditorElement] -> [(!DynamicCons, !Maybe String)]
consesOf elements = flatten $ consesOf <$> elements
where
    consesOf :: !DynamicEditorElement -> [(!DynamicCons, !Maybe String)]
    consesOf (DynamicCons cons)              = [(cons, Nothing)]
    consesOf (DynamicConsGroup label conses) = (\cons -> (cons, Just label)) <$> conses
