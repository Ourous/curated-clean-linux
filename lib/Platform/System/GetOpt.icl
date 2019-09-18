implementation module System.GetOpt

import StdEnv, Text, Data.Maybe, Data.Either
from Data.Foldable import maximum
from Data.List import isnull, find, concatMap, unzip3, zipWith3, instance Foldable []

:: OptKind a                    // kind of cmd line arg (internal use only):
   = Opt       a                //    an option
   | UnreqOpt  String           //    an un-recognized option
   | NonOpt    String           //    a non-option
   | EndOfOpts                  //    end-of-options marker (i.e. "--")
   | OptErr    String           //    something went wrong...

usageInfo :: String [OptDescr a] -> String   
usageInfo header optDescr = join "\n" [header:table]
   where (ss,ls,ds)     = (unzip3 o concatMap fmtOpt) optDescr
         table          = zipWith3 paste (sameLen ss) (sameLen ls) ds
         paste x y z    = "  " +++ x +++ "  " +++ y +++ "  " +++ z
         sameLen xs     = flushLeft ((maximum o map textSize) xs) xs
         flushLeft n xs = map (\s -> rpad s n ' ') xs

fmtOpt :: (OptDescr a) -> [(String,String,String)]
fmtOpt (Option sos los ad descr) =
   case split "\n" descr of
     []     -> [(sosFmt,losFmt,"")]
     [d:ds] -> [(sosFmt,losFmt,d) : [("","",d`) \\ d` <- ds]]
   where sosFmt = join ", " (map (fmtShort ad) sos)
         losFmt = join ", " (map (fmtLong  ad) los)

fmtShort :: (ArgDescr a) Char -> String
fmtShort (NoArg  _   ) so = "-" +++ toString so
fmtShort (ReqArg _ ad) so = "-" +++ toString so +++ " " +++ ad
fmtShort (OptArg _ ad) so = "-" +++ toString so +++ "[" +++ ad +++ "]"

fmtLong :: (ArgDescr a) String -> String
fmtLong (NoArg  _   ) lo = "--" +++ lo
fmtLong (ReqArg _ ad) lo = "--" +++ lo +++ "=" +++ ad
fmtLong (OptArg _ ad) lo = "--" +++ lo +++ "[=" +++ ad +++ "]"

getOpt :: (ArgOrder a) [OptDescr a] [String] -> ([a],[String],[String])
getOpt ordering optDescr args = (os,xs,es ++ map errUnrec us)
   where (os,xs,us,es) = getOpt` ordering optDescr args

getOpt` :: (ArgOrder a) [OptDescr a] ![String] -> ([a],[String],[String],[String])
getOpt` _        _        []         =  ([],[],[],[])
getOpt` ordering optDescr [arg:args] = procNextOpt opt ordering
   where procNextOpt (Opt o)      _                 = ([o:os],     xs,       us,     es)
         procNextOpt (UnreqOpt u) _                 = (os,         xs,       [u:us], es)
         procNextOpt (NonOpt x)   RequireOrder      = ([],         [x:rest], [],     [])
         procNextOpt (NonOpt x)   Permute           = (os,         [x:xs],   us,     es)
         procNextOpt (NonOpt x)   (ReturnInOrder f) = ([f x:os],   xs,       us,     es)
         procNextOpt EndOfOpts    RequireOrder      = ([],         rest,     [],     [])
         procNextOpt EndOfOpts    Permute           = ([],         rest,     [],     [])
         procNextOpt EndOfOpts    (ReturnInOrder f) = (map f rest, [],       [],     [])
         procNextOpt (OptErr e)   _                 = (os,         xs,       us,     [e:es])

         (opt,rest) = getNext arg args optDescr
         (os,xs,us,es) = getOpt` ordering optDescr rest

// take a look at the next cmd line arg and decide what to do with it
getNext :: String [String] [OptDescr a] -> (OptKind a,[String])
getNext "--" rest _        = (EndOfOpts,rest)
getNext xs rest optDescr | startsWith "--" xs = longOpt (dropChars 2 xs) rest optDescr
getNext xs rest optDescr | startsWith "-" xs  = shortOpt xs.[1] (dropChars 2 xs) rest optDescr
getNext a            rest _        = (NonOpt a,rest)

breakAt -1 s = (s, "")	
breakAt n s = (s % (0,n-1), dropChars n s)

// handle long option
longOpt :: String [String] [OptDescr a] -> (OptKind a,[String])
longOpt ls rs optDescr = long ads arg rs
   where (opt,arg) = breakAt (indexOf "=" ls) ls
         getWith p = [o \\ o=:(Option _ xs _ _) <- optDescr | isJust (find (p opt) xs)]
         exact     = getWith (==)
         options   = if (isnull exact) (getWith startsWith) exact
         ads       = [ ad \\ Option _ _ ad _ <- options ]
         optStr    = ("--"+++opt)

         long [_,_,_]      _        rest     = (errAmbig options optStr,rest)
         long [NoArg  a  ] ""       rest     = (Opt a,rest)
         long [NoArg  _  ] xs       rest | startsWith "=" xs = (errNoArg optStr,rest)
         long [ReqArg _ d] ""       []       = (errReq d optStr,[])
         long [ReqArg f _] ""       [r:rest] = (Opt (f r),rest)
         long [ReqArg f _] xs       rest | startsWith "=" xs = (Opt (f (dropChars 1 xs)),rest)
         long [OptArg f _] ""       rest = (Opt (f Nothing),rest)
         long [OptArg f _] xs       rest | startsWith "=" xs = (Opt (f (Just (dropChars 1 xs))),rest)
         long _            _        rest     = (UnreqOpt ("--"+++ls),rest)

// handle short option
shortOpt :: Char String [String] [OptDescr a] -> (OptKind a,[String])
shortOpt y ys rs optDescr = short ads ys rs
  where options = [ o  \\ o=:(Option ss _ _ _) <- optDescr, s <- ss | y == s ]
        ads     = [ ad \\ Option _ _ ad _ <- options ]
        optStr  = toString ['-',y]

        short [_,_,_]        _  rest     = (errAmbig options optStr,rest)
        short [NoArg  a  :_] "" rest     = (Opt a,rest)
        short [NoArg  a  :_] xs rest     = (Opt a,["-"+++xs:rest])
        short [ReqArg _ d:_] "" []       = (errReq d optStr,[])
        short [ReqArg f _:_] "" [r:rest] = (Opt (f r),rest)
        short [ReqArg f _:_] xs rest     = (Opt (f xs),rest)
        short [OptArg f _:_] "" rest     = (Opt (f Nothing),rest)
        short [OptArg f _:_] xs rest     = (Opt (f (Just xs)),rest)
        short []             "" rest     = (UnreqOpt optStr,rest)
        short []             xs rest     = (UnreqOpt optStr,["-"+++xs:rest])

// miscellaneous error formatting

errAmbig :: [OptDescr a] String -> OptKind a
errAmbig ods optStr = OptErr (usageInfo header ods)
   where header = "option `" +++ optStr +++ "' is ambiguous; could be one of:"

errReq :: String String -> OptKind a
errReq d optStr = OptErr ("option `" +++ optStr +++ "' requires an argument " +++ d +++ "\n")

errUnrec :: String -> String
errUnrec optStr = "unrecognized option `" +++ optStr +++ "'\n"

errNoArg :: String -> OptKind a
errNoArg optStr = OptErr ("option `" +++ optStr +++ "' doesn't allow an argument\n")

//-----------------------------------------------------------------------------------------
// and here a small and hopefully enlightening example:

:: Flag = Verbose | Version | Name String | Output String | Arg String

options :: [OptDescr Flag]
options =
   [Option ['v']     ["verbose"]           (NoArg Verbose)      "verbosely list files",
    Option ['V','?'] ["version","release"] (NoArg Version)      "show version info",
    Option ['o']     ["output"]            (OptArg out "FILE")  "use FILE for dump",
    Option ['n']     ["name"]              (ReqArg Name "USER") "only dump USER's files"]

out :: (Maybe String) -> Flag
out Nothing  = Output "stdout"
out (Just o) = Output o

test order cmdline = case getOpt order options cmdline of
                        (o,n,[]  ) -> Left ("options=",o,"args=",n)
                        (_,_,errs) -> Right (concat errs +++ usageInfo header options)
   where header = "Usage: foobar [OPTION...] files..."

//Start = test Permute ["-?o","--name","bar","--na=baz"]

/*
-- example runs:
-- putStr (test RequireOrder ["foo","-v"])
--    ==> options=[]  args=["foo", "-v"]
-- putStr (test Permute ["foo","-v"])
--    ==> options=[Verbose]  args=["foo"]
-- putStr (test (ReturnInOrder Arg) ["foo","-v"])
--    ==> options=[Arg "foo", Verbose]  args=[]
-- putStr (test Permute ["foo","--","-v"])
--    ==> options=[]  args=["foo", "-v"]
-- putStr (test Permute ["-?o","--name","bar","--na=baz"])
--    ==> options=[Version, Output "stdout", Name "bar", Name "baz"]  args=[]
-- putStr (test Permute ["--ver","foo"])
--    ==> option `--ver' is ambiguous; could be one of:
--          -v      --verbose             verbosely list files
--          -V, -?  --version, --release  show version info   
--        Usage: foobar [OPTION...] files...
--          -v        --verbose             verbosely list files  
--          -V, -?    --version, --release  show version info     
--          -o[FILE]  --output[=FILE]       use FILE for dump     
--          -n USER   --name=USER           only dump USER's files
-----------------------------------------------------------------------------------------
*/



