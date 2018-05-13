definition module Data.Generics

import StdGeneric

import Data.Generics.GenEq
import Data.Generics.GenLexOrd
import Data.Generics.GenMap
import Data.Generics.GenMapSt
import Data.Generics.GenReduce
import Data.Generics.GenZip 
import Data.Generics.GenPrint
import Data.Generics.GenParse
import Data.Generics.GenCompress
import Data.Generics.GenMonad
import Data.Generics.GenHylo
import Data.Generics.GenFMap
import Data.Generics.GenBimap
import Data.Generics.GenFDomain

fromOBJECT :: !(OBJECT x) -> x
fromCONS   :: !(CONS x)   -> x
fromRECORD :: !(RECORD x) -> x
fromFIELD  :: !(FIELD x)  -> x
fromPAIRX  :: !(PAIR x y) -> x
fromPAIRY  :: !(PAIR x y) -> y
