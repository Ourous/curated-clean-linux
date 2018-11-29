implementation module System.Time

import StdString, StdArray, StdClass, StdOverloaded, StdInt, StdMisc, StdBool
import Data.GenEq
import System.OS
import System._Pointer, System._Posix
import Text

//String buffer size
MAXBUF :== 256

derive gEq Timestamp

instance == Timestamp
where
	(==) (Timestamp t1) (Timestamp t2) = t1 == t2

instance < Timestamp
where
	(<) (Timestamp t1) (Timestamp t2) = t1 < t2

instance toString Tm
where
	toString tm = trim (derefString (toStringTmC (packTm tm)))
	where
		toStringTmC :: !{#Int} -> Pointer
		toStringTmC a0 = code {
			ccall asctime "A:p"
		}
instance toString Timestamp
where
	toString (Timestamp t) = trim (derefString (toStringTimeC (packInt t)))
	where	
		toStringTimeC :: !{#Int} -> Pointer
		toStringTimeC a0 = code {
			ccall ctime "A:p"
		}
instance toString Clock
where
	toString (Clock c) = toString c
instance toInt Timestamp
where
	toInt (Timestamp i) = i

clock :: !*World -> (!Clock, !*World)
clock world
	# (c, world) = clockC world
	= (Clock c, world)
	where
	clockC :: !*World -> (!Int, !*World)
	clockC world = code {
		ccall clock ":I:A"
	}

time :: !*World -> (!Timestamp, !*World)
time world
	# (t, world)	= timeC 0 world
	= (Timestamp t, world)
	where
	timeC :: !Int !*World -> (!Int,!*World)
	timeC a0 world = code {
		ccall time "I:I:A"
	}

gmTime :: !*World -> (!Tm, !*World)
gmTime world
	# ((Timestamp t),world)	= time world
	# (tm, world)			= gmTimeC (packInt t) world
	= (derefTm tm, world)
	where
	gmTimeC :: !{#Int} !*World -> (!Int, !*World)
	gmTimeC tm world = code {
		ccall gmtime "A:p:A"
	}

localTime :: !*World -> (!Tm, !*World)
localTime world
	# ((Timestamp t),world)	= time world
	# (tm,world)			= localTimeC (packInt t) world
	= (derefTm tm, world)
	where
	localTimeC :: !{#Int} !*World -> (!Int, !*World)
	localTimeC tm world = code {
		ccall localtime "A:p:A"
	}

mkTime :: !Tm !*World-> (!Timestamp, !*World)
mkTime tm world
	# (t, world) = mkTimeC (packTm tm) world
	= (Timestamp t, world)
where
	mkTimeC :: !{#Int} !*World -> (!Int, !*World)
	mkTimeC tm world = code {
		ccall mktime "A:I:A"
	}

timeGm :: !Tm -> Timestamp
timeGm tm = Timestamp (timegm (packTm tm))

diffTime :: !Timestamp !Timestamp -> Int
diffTime (Timestamp t1) (Timestamp t2) = t1 - t2

strfTime :: !String !Tm -> String
strfTime format tm 
	# buf		= createArray MAXBUF 'X'
	# (len,buf)	= strfTimeC buf MAXBUF (packString format) (packTm tm) buf
	= buf % (0, len - 1)
	where
		strfTimeC :: !{#Char} !Int !{#Char} !{#Int} !{#Char} -> (!Int,!{#Char})
		strfTimeC a0 a1 a2 a3 a4 = code {
			ccall strftime "sIsA:I:A"
		}

toLocalTime :: !Timestamp !*World -> (!Tm,!*World)
toLocalTime (Timestamp t) world
    # (tm,world) = localTimeC (packInt t) world
    = (derefTm tm, world)

toGmTime :: !Timestamp -> Tm
toGmTime (Timestamp t) = derefTm (gmTimeC (packInt t))

gmTimeC :: !{#Int} -> Pointer
gmTimeC tm = code {
    ccall gmtime "A:p"
}

localTimeC :: !{#Int} !*World -> (!Pointer, !*World)
localTimeC tm world = code {
	ccall localtime "A:p:A"
}

derefTm :: !Pointer-> Tm
derefTm ptr = unpackTm (derefCharArray ptr sizeOfTm) 0

packTm :: !Tm -> {#Int}
packTm tm = (IF_INT_64_OR_32 packTm64 packTm32) tm

packTm64 :: !Tm -> {#Int}
packTm64 tm =   { tm.sec  + tm.min  << 32
                , tm.hour + tm.mday << 32
                , tm.mon  + tm.year << 32
                , tm.wday + tm.yday << 32
                , tm.isdst
                }

packTm32 :: !Tm -> {#Int}
packTm32 tm =   { tm.sec
                , tm.min
                , tm.hour
                , tm.mday
                , tm.mon
                , tm.year
                , tm.wday
                , tm.yday
                , tm.isdst
                }

unpackTm :: !{#Char} !Int -> Tm
unpackTm buf off =
	{ sec   = unpackInt4S buf (off + 0)
    , min   = unpackInt4S buf (off + 4)
	, hour  = unpackInt4S buf (off + 8)
	, mday  = unpackInt4S buf (off + 12)
	, mon   = unpackInt4S buf (off + 16)
	, year  = unpackInt4S buf (off + 20)
	, wday  = unpackInt4S buf (off + 24)
	, yday  = unpackInt4S buf (off + 28)
	, isdst = unpackInt4S buf (off + 32)
	}

sizeOfTm :: Int
sizeOfTm = IF_ANDROID 44 36 

nsTime :: !*World -> (!Timespec, !*World)
nsTime w
# (p, w) = mallocSt 16 w
# (r, w) = clock_gettime 0 p w
//For completeness sake
| r <> 0 = abort "clock_gettime error: everyone should have permission to open CLOCK_REALTIME?"
# (tv_sec, p) = readIntP p 0
# (tv_nsec, p) = readIntP p 8
= ({Timespec | tv_sec = tv_sec, tv_nsec = tv_nsec}, freeSt p w)

timespecToStamp :: !Timespec -> Timestamp
timespecToStamp t = Timestamp t.tv_sec

timestampToSpec :: !Timestamp -> Timespec
timestampToSpec (Timestamp t) = {tv_sec=t,tv_nsec=0}

instance < Timespec
where
	(<) t1 t2
		| t1.tv_sec == t2.tv_sec = t1.tv_nsec < t2.tv_nsec
		= t1.tv_sec < t2.tv_sec

instance + Timespec
where
	(+) t1 t2 = let tv_nsec = t1.tv_nsec + t2.tv_nsec in
		{ tv_sec  = t1.tv_sec + t2.tv_sec + tv_nsec / 1000000000
		, tv_nsec = tv_nsec rem 1000000000
		}

instance - Timespec
where
	(-) t1 t2
		# tv_nsec = t1.tv_nsec - t2.tv_nsec
		| tv_nsec < 0
			= {tv_sec = t1.tv_sec - t2.tv_sec - 1, tv_nsec = 1000000000 - tv_nsec}
			= {tv_sec = t1.tv_sec - t2.tv_sec,     tv_nsec = tv_nsec}

instance zero Timespec
where zero = {tv_sec=0, tv_nsec=0}

instance == Timespec
where
	(==) t1 t2 = t1.tv_sec == t2.tv_sec && t1.tv_nsec == t2.tv_nsec
