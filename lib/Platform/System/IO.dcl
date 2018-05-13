definition module System.IO

from Control.Applicative import class Applicative
from Data.Functor import class Functor
from Control.Monad import class Monad
from StdOverloaded import class toString

:: IO a = IO .(*World -> *(a, !*World))

execIO :: !(IO a) !*World -> *World

evalIO :: !(IO a) !*World -> *(a, !*World)

withWorld :: (*World -> *(.a, !*World)) -> IO .a

putStr :: String -> IO ()

putStrLn :: String -> IO ()

print :: a -> IO () | toString a

getChar :: IO Char

getLine :: IO String

readFileM :: !String -> IO String

writeFileM :: !String !String -> IO ()

instance Applicative IO
instance Functor IO
instance Monad IO

unsafePerformIO :: !(*World -> *(.a, !*World)) -> .a
unsafePerformIOTrue :: !(*World -> *(a, !*World)) -> Bool
