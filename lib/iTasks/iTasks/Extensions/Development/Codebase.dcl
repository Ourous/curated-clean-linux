definition module iTasks.Extensions.Development.Codebase

import iTasks
import iTasks.UI.Editor.Controls
import System.FilePath

:: CodeBase 		:== [SourceTree]

:: SourceTree =
    { name          :: String                               //Unique name
    , rootPath      :: FilePath                             //Root directory of the modules
    , subPaths      :: [FilePath]                           //Optional list of subpaths (for example: OS-Indepent,OS-Windows etc)
    , readOnly      :: Bool
    , modules       :: [(ModuleName,ModuleType,FilePath)]   //Modules found in the paths locations
    }

:: SourceTreeSelection
    = SelSourceTree SourceTreeName FilePath
    | SelMainModule ModuleName FilePath
    | SelAuxModule  ModuleName FilePath

:: SourceTreeName   :== String								// Identifier
:: FileName			:== String								// Name of file, with extension
:: ModuleName 		:== String								// Name of module, without extension
:: ModuleType       =   MainModule | AuxModule              // main module: (only icl) auxilary module (icl + dcl)

:: Extension		= 	Icl | Dcl
:: CleanFile        :== (FilePath,ModuleName,Extension)     // A reference to a clean file on disk (either an icl or dcl)

:: CleanModuleName	:== (FilePath,ModuleName)				// Clean Module Name

:: CleanPath		:== FilePath                            // Directory where clean application / batchbuild is located
:: Identifier		:== String								// Clean identifier 

derive class iTask SourceTree, SourceTreeSelection, ModuleType, Extension
instance toString Extension
instance == Extension

//List all modules in a directory on disk
moduleList :: SDSSource FilePath [(ModuleName,ModuleType)] ()

//Access the code and documentation for a module on disk
moduleDefinition     :: SDSLens (FilePath,ModuleName) [String] [String]
moduleImplementation :: SDSLens (FilePath,ModuleName) [String] [String]
moduleDocumentation  :: SDSLens (FilePath,ModuleName) [String] [String]

//Convert a list of modules to a tree for choice tasks
toModuleSelectTree :: [(ModuleName,ModuleType)] -> [(ChoiceNode)]

// Scan filesystem to find all the modules on disk
rescanCodeBase :: CodeBase -> Task CodeBase

// Browse the modules in a code base and select a Clean module
navigateCodebase :: CodeBase -> Task SourceTreeSelection

// Search for the type and location of a module in the codebase
lookupModule :: ModuleName CodeBase -> Maybe (ModuleName,ModuleType,FilePath)

//List all clean files in a codebase
listFilesInCodeBase :: CodeBase -> [CleanFile]

//Convert a CleanFile triple to an absolute file path
cleanFilePath :: CleanFile -> FilePath

//Lookup type of a module
getModuleType :: ModuleName CodeBase -> Maybe ModuleType

// Convert CodeBase Tree to a list
codeBaseToCleanModuleNames :: CodeBase -> [CleanModuleName]
