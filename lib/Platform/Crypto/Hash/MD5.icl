implementation module Crypto.Hash.MD5
/**
* Pure Clean MD5 implementation.
* Ported from the MD5 implemenation used by the Clean Dynamics system
*/
import StdEnv
import Data.Tuple, Text, System.FilePath, System.File, Data.Error, Data.Functor

md5 :: !String -> String
md5 msg = toString (md5StringDigest msg)

/* "the values of the integer part of 4294967296 times abs(sin(i))" */
T1:== 0xd76aa478 /* 1 */
T2:== 0xe8c7b756 /* 2 */
T3:== 0x242070db /* 3 */
T4:== 0xc1bdceee/* 4 */
T5:== 0xf57c0faf /* 5 */
T6:== 0x4787c62a /* 6 */
T7:== 0xa8304613 /* 7 */
T8:== 0xfd469501/* 8 */
T9:== 0x698098d8/* 9 */
T10:== 0x8b44f7af /* 10 */
T11:== 0xffff5bb1 /* 11 */
T12:== 0x895cd7be /* 12 */
T13:== 0x6b901122 /* 13 */
T14:== 0xfd987193 /* 14 */
T15:== 0xa679438e /* 15 */
T16:== 0x49b40821 /* 16 */

T17:== 0xf61e2562 /* 17 */
T18:== 0xc040b340 /* 18 */
T19:== 0x265e5a51 /* 19 */
T20:== 0xe9b6c7aa /* 20 */
T21:== 0xd62f105d /* 21 */
T22:== 0x2441453/* 22 */
T23:== 0xd8a1e681 /* 23 */
T24:== 0xe7d3fbc8 /* 24 */
T25:== 0x21e1cde6 /* 25 */
T26:== 0xc33707d6 /* 26 */
T27:== 0xf4d50d87 /* 27 */
T28:== 0x455a14ed /* 28 */
T29:== 0xa9e3e905 /* 29 */
T30:== 0xfcefa3f8 /* 30 */
T31:== 0x676f02d9 /* 31 */
T32:== 0x8d2a4c8a /* 32 */

T33:== 0xfffa3942 /* 33 */
T34:== 0x8771f681 /* 34 */
T35:== 0x6d9d6122 /* 35 */
T36:== 0xfde5380c /* 36 */
T37:== 0xa4beea44 /* 37 */
T38:== 0x4bdecfa9 /* 38 */
T39:== 0xf6bb4b60 /* 39 */
T40:== 0xbebfbc70 /* 40 */
T41:== 0x289b7ec6 /* 41 */
T42:== 0xeaa127fa /* 42 */
T43:== 0xd4ef3085 /* 43 */
T44:== 0x4881d05 /* 44 */
T45:== 0xd9d4d039 /* 45 */
T46:== 0xe6db99e5 /* 46 */
T47:== 0x1fa27cf8 /* 47 */
T48:== 0xc4ac5665 /* 48 */

T49:== 0xf4292244 /* 49 */
T50:== 0x432aff97 /* 50 */
T51:== 0xab9423a7 /* 51 */
T52:== 0xfc93a039 /* 52 */
T53:== 0x655b59c3 /* 53 */
T54:== 0x8f0ccc92 /* 54 */
T55:== 0xffeff47d /* 55 */
T56:== 0x85845dd1 /* 56 */
T57:== 0x6fa87e4f /* 57 */
T58:== 0xfe2ce6e0 /* 58 */
T59:== 0xa3014314 /* 59 */
T60:== 0x4e0811a1 /* 60 */
T61:== 0xf7537e82 /* 61 */
T62:== 0xbd3af235 /* 62 */
T63:== 0x2ad7d2bb /* 63 */
T64:== 0xeb86d391 /* 64 */

ROTATE_LEFT x n :== (x << n) bitor (((x>>1) bitand 0x7fffffff) >> (31-n));
ROTATE_RIGHT x n:== ROTATE_LEFT x (~n)

F x y z:== z bitxor (x bitand (y bitxor z))
G x y z:== (x bitand z) bitor (y bitand (bitnot z))
H x y z:== x bitxor y bitxor z
I x y z:== y bitxor (x bitor (bitnot z))

process :: *(Int,Int,Int,Int) *{#Int} -> (Int,Int,Int,Int,*{#Int})
process (a,b,c,d) x
// Saving old values
	#!aa= a
	#!bb= b
	#!cc= c
	#!dd= d
/*
  ROUND 1
  [abcd k s i] means a = b + ((a + F(b,c,d) + X[k] + T[i]) <<< s)

  [ABCD  0  7  1]  [DABC  1 12  2]  [CDAB  2 17  3]  [BCDA  3 22  4]
  [ABCD  4  7  5]  [DABC  5 12  6]  [CDAB  6 17  7]  [BCDA  7 22  8]
  [ABCD  8  7  9]  [DABC  9 12 10]  [CDAB 10 17 11]  [BCDA 11 22 12]
  [ABCD 12  7 13]  [DABC 13 12 14]  [CDAB 14 17 15]  [BCDA 15 22 16]
*/
	#!a= b + (ROTATE_LEFT (a + (F b c d) + x.[0] + T1) 7)
	#!d= a + (ROTATE_LEFT (d + (F a b c) + x.[1] + T2) 12)
	#!c= d + (ROTATE_LEFT (c + (F d a b) + x.[2] + T3) 17)
	#!b= c + (ROTATE_LEFT (b + (F c d a) + x.[3] + T4) 22)	
	#!a= b + (ROTATE_LEFT (a + (F b c d) + x.[4] + T5) 7)	
	#!d= a + (ROTATE_LEFT (d + (F a b c) + x.[5] + T6) 12)	
	#!c= d + (ROTATE_LEFT (c + (F d a b) + x.[6] + T7) 17)	
	#!b= c + (ROTATE_LEFT (b + (F c d a) + x.[7] + T8) 22)	
	#!a= b + (ROTATE_LEFT (a + (F b c d) + x.[8] + T9) 7)	
	#!d= a + (ROTATE_LEFT (d + (F a b c) + x.[9] + T10) 12)	
	#!c= d + (ROTATE_LEFT (c + (F d a b) + x.[10] + T11) 17)	
	#!b= c + (ROTATE_LEFT (b + (F c d a) + x.[11] + T12) 22)	
	#!a= b + (ROTATE_LEFT (a + (F b c d) + x.[12] + T13) 7)	
	#!d= a + (ROTATE_LEFT (d + (F a b c) + x.[13] + T14)  12)	
	#!c= d + (ROTATE_LEFT (c + (F d a b) + x.[14] + T15)  17)	
	#!b= c + (ROTATE_LEFT (b + (F c d a) + x.[15] + T16)  22)	
/*
  ROUND 2
  [abcd k s i] means a = b + ((a + G(b,c,d) + X[k] + T[i]) <<< s)

  [ABCD  1  5 17]  [DABC  6  9 18]  [CDAB 11 14 19]  [BCDA  0 20 20]
  [ABCD  5  5 21]  [DABC 10  9 22]  [CDAB 15 14 23]  [BCDA  4 20 24]
  [ABCD  9  5 25]  [DABC 14  9 26]  [CDAB  3 14 27]  [BCDA  8 20 28]
  [ABCD 13  5 29]  [DABC  2  9 30]  [CDAB  7 14 31]  [BCDA 12 20 32]
*/
	#!a= b + (ROTATE_LEFT (a + (G b c d) + x.[1] + T17)  5)
	#!d= a + (ROTATE_LEFT (d + (G a b c) + x.[6] + T18)  9)	
	#!c= d + (ROTATE_LEFT (c + (G d a b) + x.[11] + T19)  14)	
	#!b= c + (ROTATE_LEFT (b + (G c d a) + x.[0] + T20)  20)	
	#!a= b + (ROTATE_LEFT (a + (G b c d) + x.[5] + T21)  5)	
	#!d= a + (ROTATE_LEFT (d + (G a b c) + x.[10] + T22)  9)	
	#!c= d + (ROTATE_LEFT (c + (G d a b) + x.[15] + T23)  14)	
	#!b= c + (ROTATE_LEFT (b + (G c d a) + x.[4] + T24)  20)	
	#!a= b + (ROTATE_LEFT (a + (G b c d) + x.[9] + T25)  5)	
	#!d= a + (ROTATE_LEFT (d + (G a b c) + x.[14] + T26)  9)	
	#!c= d + (ROTATE_LEFT (c + (G d a b) + x.[3] + T27)  14)	
	#!b= c + (ROTATE_LEFT (b + (G c d a) + x.[8] + T28)  20)	
	#!a= b + (ROTATE_LEFT (a + (G b c d) + x.[13] + T29)  5)	
	#!d= a + (ROTATE_LEFT (d + (G a b c) + x.[2] + T30)  9)	
	#!c= d + (ROTATE_LEFT (c + (G d a b) + x.[7] + T31)  14)	
	#!b= c + (ROTATE_LEFT (b + (G c d a) + x.[12] + T32)  20)	
/*
  ROUND 3
  [abcd k s i] means a = b + ((a + H(b,c,d) + X[k] + T[i]) <<< s)

  [ABCD  5  4 33]  [DABC  8 11 34]  [CDAB 11 16 35]  [BCDA 14 23 36]
  [ABCD  1  4 37]  [DABC  4 11 38]  [CDAB  7 16 39]  [BCDA 10 23 40]
  [ABCD 13  4 41]  [DABC  0 11 42]  [CDAB  3 16 43]  [BCDA  6 23 44]
  [ABCD  9  4 45]  [DABC 12 11 46]  [CDAB 15 16 47]  [BCDA  2 23 48]
*/
	#!a= b + (ROTATE_LEFT (a + (H b c d) + x.[5] + T33)  4)
	#!d= a + (ROTATE_LEFT (d + (H a b c) + x.[8] + T34)  11)	
	#!c= d + (ROTATE_LEFT (c + (H d a b) + x.[11] + T35)  16)	
	#!b= c + (ROTATE_LEFT (b + (H c d a) + x.[14] + T36)  23)	
	#!a= b + (ROTATE_LEFT (a + (H b c d) + x.[1] + T37)  4)	
	#!d= a + (ROTATE_LEFT (d + (H a b c) + x.[4] + T38)  11)	
	#!c= d + (ROTATE_LEFT (c + (H d a b) + x.[7] + T39)  16)	
	#!b= c + (ROTATE_LEFT (b + (H c d a) + x.[10] + T40)  23)	
	#!a= b + (ROTATE_LEFT (a + (H b c d) + x.[13] + T41)  4)	
	#!d= a + (ROTATE_LEFT (d + (H a b c) + x.[0] + T42)  11)	
	#!c= d + (ROTATE_LEFT (c + (H d a b) + x.[3] + T43)  16)	
	#!b= c + (ROTATE_LEFT (b + (H c d a) + x.[6] + T44)  23)	
	#!a= b + (ROTATE_LEFT (a + (H b c d) + x.[9] + T45)  4)	
	#!d= a + (ROTATE_LEFT (d + (H a b c) + x.[12] + T46)  11)	
	#!c= d + (ROTATE_LEFT (c + (H d a b) + x.[15] + T47)  16)	
	#!b= c + (ROTATE_LEFT (b + (H c d a) + x.[2] + T48)  23)	
/*
  ROUND 4
  [abcd k s i] means a = b + ((a + I(b,c,d) + X[k] + T[i]) <<< s)

  [ABCD  0  6 49]  [DABC  7 10 50]  [CDAB 14 15 51]  [BCDA  5 21 52]
  [ABCD 12  6 53]  [DABC  3 10 54]  [CDAB 10 15 55]  [BCDA  1 21 56]
  [ABCD  8  6 57]  [DABC 15 10 58]  [CDAB  6 15 59]  [BCDA 13 21 60]
  [ABCD  4  6 61]  [DABC 11 10 62]  [CDAB  2 15 63]  [BCDA  9 21 64]
*/
	#!a= b + (ROTATE_LEFT (a + (I b c d) + x.[0] + T49)  6)
	#!d= a + (ROTATE_LEFT (d + (I a b c) + x.[7] + T50)  10)	
	#!c= d + (ROTATE_LEFT (c + (I d a b) + x.[14] + T51)  15)	
	#!b= c + (ROTATE_LEFT (b + (I c d a) + x.[5] + T52)  21)	
	#!a= b + (ROTATE_LEFT (a + (I b c d) + x.[12] + T53)  6)	
	#!d= a + (ROTATE_LEFT (d + (I a b c) + x.[3] + T54)  10)	
	#!c= d + (ROTATE_LEFT (c + (I d a b) + x.[10] + T55)  15)	
	#!b= c + (ROTATE_LEFT (b + (I c d a) + x.[1] + T56)  21)	
	#!a= b + (ROTATE_LEFT (a + (I b c d) + x.[8] + T57)  6)	
	#!d= a + (ROTATE_LEFT (d + (I a b c) + x.[15] + T58)  10)	
	#!c= d + (ROTATE_LEFT (c + (I d a b) + x.[6] + T59)  15)	
	#!b= c + (ROTATE_LEFT (b + (I c d a) + x.[13] + T60)  21)	
	#!a= b + (ROTATE_LEFT (a + (I b c d) + x.[4] + T61)  6)	
	#!d= a + (ROTATE_LEFT (d + (I a b c) + x.[11] + T62)  10)	
	#!c= d + (ROTATE_LEFT (c + (I d a b) + x.[2] + T63)  15)	
	#!b= c + (ROTATE_LEFT (b + (I c d a) + x.[9] + T64)  21)	
	= (a+aa,b+bb,c+cc,d+dd,x)

reverse :: Int-> Int
reverse szam
	#first= szam bitand 0x000000FF
	#second= szam bitand 0x0000FF00
	#third=	szam bitand 0x00FF0000
	#fourth= szam bitand 0xFF000000
	= ((first << 24)bitor(second << 8)bitor(ROTATE_RIGHT third 8)bitor(ROTATE_RIGHT fourth 24))

readResult :: (Int,Int,Int,Int) -> MD5Digest
readResult (x,y,z,zs)
	= MD5Digest (join "" [{toChar (i >> (j * 8)) \\ j <- [0..3]} \\ i <- [x,y,z,zs]])

md5StringDigest :: !String -> MD5Digest
md5StringDigest msg
	# (result,buffer) = computeForString msg (0x67452301,0xefcdab89,0x98badcfe,0x10325476) ((size msg)*8) (createArray 16 0)
	= result

computeForString :: String *(Int,Int,Int,Int) Int *{#Int} -> (MD5Digest,*{#Int})
computeForString szoveg (a,b,c,d) numbOfBits buffer
	#!(buffer,numbOfBits,szoveg,eof)	= readS szoveg numbOfBits buffer
	#!(a,b,c,d,buffer)					= process (a,b,c,d) buffer
	| eof	= ((readResult (a,b,c,d)),buffer)
	= computeForString szoveg (a,b,c,d) numbOfBits buffer

readS :: !String !Int *{#Int} -> (!*{#Int}, !Int, !String, !Bool)
readS szoveg numberOfBits buffer
	|szoveg=="" && (numberOfBits rem 512) <> 0
		#buffer= {0,0,0,0,0,0,0,0,0,0,0,0,0,0,numberOfBits,0}
		=(buffer, numberOfBits, "", True)
	|szoveg==""	
		#buffer= {reverse (ROTATE_RIGHT 1 1),0,0,0,0,0,0,0,0,0,0,0,0,0,numberOfBits,0}
		= (buffer,numberOfBits,"",True)	
	|(size szoveg)==64
		#buffer= convertIntoProcessInput szoveg buffer 0
		= (buffer, numberOfBits, "", False)
	|(size szoveg)>64
		#szoveg1= szoveg % (0,63)
		#restOfSzoveg= szoveg % (64,(size szoveg)-1)
		#buffer= convertIntoProcessInput szoveg1 buffer 0
		= (buffer, numberOfBits, restOfSzoveg, False)
	//from this point we handle the last part of the String
	//which is not correctly 512 bits long.
	#extension= createArray (64-(size szoveg)) (toChar 0)
	#extension= update extension 0 (toChar 128)
	#buffer= convertIntoProcessInput (szoveg+++extension) buffer 0
	|(size szoveg)<56
		#buffer= update buffer 14 numberOfBits
		= (buffer, numberOfBits, "", True)
	= (buffer, numberOfBits, "", False)
	where
		convertIntoProcessInput :: String *{#Int} Int -> *{#Int}
		convertIntoProcessInput text temporaryState index
			|15 < index = temporaryState
			#element= (((toInt fourth) << 24) bitor ((toInt third) << 16) bitor ((toInt second) << 8) bitor (toInt first))
			= convertIntoProcessInput text (update temporaryState index element) (index+1)
			where
				first	= select text (index*4)
				second	= select text (index*4+1)
				third	= select text (index*4+2)
				fourth	= select text (index*4+3)

md5FileDigest :: !FilePath !*env -> (!MaybeError FileError MD5Digest,!*env) | FileSystem env
md5FileDigest path world
	#!(ok,f,world)= fopen path FReadData world
	|not ok
		= (Error CannotOpen,world)
	#!buffer= createArray 16 0
	#!fileBuffer= createArray 64 ' '
	#!(result,f,buffer,fileBuffer) = computeForFile f (0x67452301,0xefcdab89,0x98badcfe,0x10325476) 0 buffer fileBuffer
	#!(ok,world)= fclose f world
	= (Ok result,world)
	
computeForFile :: *File *(Int,Int,Int,Int) Int *{#Int} *String -> (MD5Digest,*File,*{#Int},*String)
computeForFile f (a,b,c,d) numbOfBits buffer fileBuffer
	#!(buffer,numbOfBits,f,fileBuffer,eof)	= readF f numbOfBits buffer fileBuffer
	#!(a,b,c,d,buffer)						= process (a,b,c,d) buffer
	|eof	= (readResult (a,b,c,d),f,buffer,fileBuffer)
	= computeForFile f (a,b,c,d) numbOfBits buffer fileBuffer
/*
The size of the file is counted in an ordinary Int (32 bits)
-> the size of the file mustn't be greater than 2^32bits = 4 GB
If the file is greater, we will have a checksum, but it's not good.
*/
readF :: !*File !Int *{#Int} *String -> (!*{#Int}, !Int, !*File, *String, !Bool)
readF f numberOfBits buffer characterBlock
	|numberOfBits rem 512 <> 0	
		#buffer= {0,0,0,0,0,0,0,0,0,0,0,0,0,0,numberOfBits,0}
		= (buffer, numberOfBits, f, characterBlock, True)	
	#!(size,characterBlock,f)= freadsubstring 0 64 characterBlock f
	|size==64
		#!(buffer,characterBlock)= convertIntoProcessInput characterBlock buffer 0
		= (buffer, (numberOfBits+512), f, characterBlock, False)
	|size==0
		#!buffer= {reverse (ROTATE_RIGHT 1 1),0,0,0,0,0,0,0,0,0,0,0,0,0,numberOfBits,0}
		= (buffer,numberOfBits,f,characterBlock,True)	
	//from this point we handle the last "characterBlock" of 
	//the file, which is not correctly 512 bits long.
	#characterBlock= update characterBlock size (toChar 128)
	#!characterBlock= updateWithZeroToEnd characterBlock (size+1)
	#!(buffer,characterBlock)= convertIntoProcessInput characterBlock buffer 0
	#!numberOfBits= numberOfBits + size*8
	|size<56
		#!buffer= update buffer 14 numberOfBits
		= (buffer, numberOfBits, f, characterBlock, True)
	= (buffer, numberOfBits, f, characterBlock, False)
 	
	where
		convertIntoProcessInput :: *String *{#Int} Int -> (*{#Int},*String)
		convertIntoProcessInput text temporaryState index
			|15 < index = (temporaryState,text)
			#!first	= select text (index*4)
			#!second	= select text (index*4+1)
			#!third	= select text (index*4+2)
			#!fourth	= select text (index*4+3)
			#element= (((toInt fourth) << 24) bitor ((toInt third) << 16) bitor ((toInt second) << 8) bitor (toInt first))
			= convertIntoProcessInput text (update temporaryState index element) (index+1)

		updateWithZeroToEnd :: !*String Int -> *String
		updateWithZeroToEnd text index
			|index > (size text)-1
				= text
			= updateWithZeroToEnd (update text index (toChar 0)) (index+1)

instance toString MD5Digest
where
	toString (MD5Digest d)	= join "" [char c \\ c<-:d]
	where
		char c = {hex.[((toInt c >> 4) bitand 15)],hex.[(toInt c) bitand 15]}
		hex = "0123456789abcdef"
