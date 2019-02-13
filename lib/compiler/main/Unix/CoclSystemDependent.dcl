// this is for Windows
definition module CoclSystemDependent

from StdFile import ::Files

// RWS split
// from deltaIOSystem import DeviceSystem
// from deltaEventIO import InitialIO, IOState

PathSeparator
	:==	':'
DirectorySeparator
	:== '/'

SystemDependentDevices :: [a]
SystemDependentInitialIO :: [a]

ensureCleanSystemFilesExists :: !String !*Files -> (!Bool, !*Files)
set_compiler_id :: Int -> Int

compiler_loop :: ([{#Char}] *st -> *(Bool, *st)) *st -> (!Bool, !*st)

