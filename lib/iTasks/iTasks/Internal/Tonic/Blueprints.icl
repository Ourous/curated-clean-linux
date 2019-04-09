implementation module iTasks.Internal.Tonic.Blueprints

import iTasks
import StdMisc, Data.Tuple, Text, Data.Either, Data.Functor
import iTasks.Internal.SDS
import iTasks.Internal.Tonic.Blueprints
import iTasks.Engine
import iTasks.Internal.SDS
import qualified iTasks.Internal.SDS as DSDS
import iTasks.Internal.IWorld
import iTasks.Internal.Tonic.AbsSyn
import iTasks.Internal.Tonic.Blueprints
import iTasks.Internal.Tonic.Images
import iTasks.Internal.Tonic.Types
import iTasks.Internal.Tonic.Pretty
import iTasks.Internal.Tonic.Shares
import iTasks.Internal.TaskState
import iTasks.Internal.TaskStore
import iTasks.Internal.TaskEval
import iTasks.Internal.Task
from StdFunc import seq
import qualified Data.Map as DM
import Data.Map.GenJSON
import System.File, StdFile
import StdArray
import System.Directory, System.FilePath

getTonicFunc :: !TonicModule !String -> Maybe TonicFunc
getTonicFunc tm tn = 'DM'.get tn tm.tm_funcs

getTonicModules` :: !*IWorld -> *(!MaybeError (Dynamic, String) [String], !*IWorld)
getTonicModules` iworld
  # (dir, iworld) = getTonicDir iworld
  # (mfs, world)  = readDirectory dir iworld.world
  # iworld        = {iworld & world = world}
  = case mfs of
      Ok fs
        = (Ok (map dropExtension (filter (\x -> noDots x && onlyTonic x) fs)), iworld)
      Error _
        # msg = "Failed to read Tonic directory"
        = (Error (dynamic msg, msg), iworld)
  where
  onlyTonic :: !String -> Bool
  onlyTonic str = endsWith ".tonic" str

  noDots :: !String -> Bool
  noDots str = not (str.[0] == '.')

getModule` :: !String !*IWorld -> *(!MaybeError (Dynamic, String) TonicModule, !*IWorld)
getModule` moduleName iworld
  # (dir, iworld)  = getTonicDir iworld
  # (mjson, world) = readFile (dir </> (moduleName +++ ".tonic")) iworld.world
  # iworld         = {iworld & world = world}
  = case mjson of
      Ok json   -> case fromJSON (fromString json) of
                     Just gg  -> (Ok gg, iworld)
                     _        -> err ("Failed to deserialize JSON: " +++ json) iworld
      Error msg -> err (toString msg) iworld
  where
  err msg iworld
    # msg = "Failed to load Tonic file for module " +++ moduleName +++ ": " +++ msg
    = (Error (dynamic msg, msg), iworld)

allBlueprints :: Task AllBlueprints
allBlueprints
  =           getTonicModules >>-
  \modnms  -> allTasks (map getModule modnms) >>-
  \modules -> return (foldr f 'DM'.newMap modules)
  where
  f mod acc
    = case 'DM'.get mod.tm_name acc of
        Just _ -> acc
        _      -> 'DM'.put mod.tm_name mod.tm_funcs acc

getModule :: !String -> Task TonicModule
getModule moduleName = mkInstantTask (const (getModule` moduleName))

getTonicModules :: Task [String]
getTonicModules = mkInstantTask (const getTonicModules`)

getTonicDir :: !*IWorld -> *(!String, !*IWorld)
getTonicDir iworld=:{options}
  = (takeDirectory options.appPath </> "tonic", iworld)

getTasks :: !TonicModule -> [String]
getTasks tm = 'DM'.keys tm.tm_funcs
