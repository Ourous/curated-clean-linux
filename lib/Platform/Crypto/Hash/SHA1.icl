implementation module Crypto.Hash.SHA1
import StdEnv
import Data.Tuple, Text, System.FilePath, System.File, Data.Error, Data.Functor
/**
* Simple pure Clean SHA1 implementation.
* Probably not the fastest, but without any dependencies
* Relatively straightforward implementation of FIPS 180-1
* 
* Because Clean has no explicit 32-bit word type, we use Clean Ints.
* These are 64-bit or 32-bit depending on the processor.
* Therefore some addiditional bitmask are necessary to make sure we only use 32 bits
*/
:: SHA1Digest = SHA1Digest {#Char} // 160 bit (20byte) message digest

sha1 :: !String -> String
sha1 msg = toString (sha1StringDigest msg)

sha1StringDigest :: !String -> SHA1Digest
sha1StringDigest msg = SHA1Digest (toBytes (foldl processChunk initState (chunk (pad msg))))
where
	//Pre-processing:
	//Pad the message to a multiple of 512-bit blocks
	pad :: String -> String 
	pad s = s +++ onebyte +++ createArray numzerobytes '\0' +++ sizeAs64bit byteSize
	where
		byteSize = size s //In bytes!
		onebyte = {toChar 128} // 0x10000000
		rembytes = byteSize rem 64 //Number of bytes used by the message in the last block containing the message
		//Determine the number of full zero bytes we need to end up with a multiple of 64 bytes
		numzerobytes = if (rembytes + 9 > 64) (119 - rembytes) (55 - rembytes)
		//Encode size IN BITS as 64-bit big-endian in 8 bytes (size in bits == size in bytes times 8 (or << 3))
        sizeAs64bit n = IF_INT_64_OR_32
            {toChar (if (b == 0) (n << 3) (n >> ((b * 8) - 3))) \\ b <- [7,6,5,4,3,2,1,0]}
            {toChar (if (b > 3) 0 (if (b == 0) (n << 3) (n >> ((b * 8) - 3)))) \\ b <- [7,6,5,4,3,2,1,0]}

	//Split the message into a list of 512-bit blocks (assumes a padded input)
	chunk :: String -> [String]
	chunk s = [let o = i*64 in {s.[o+j] \\ j <- [0..63]} \\ i <- [0..(size s / 64) - 1]] 

	//Initial values of h0,h1,h2,h3,h4	
	initState :: {#Int}
	initState = {0x67452301,0xEFCDAB89,0x98BADCFE,0x10325476,0xC3D2E1F0}

	//Main function that modifies the digest for each block
	processChunk :: {#Int} {#Char} -> {#Int}
	processChunk h m 
		= let (a,b,c,d,e) = foldl iteration (h.[0],h.[1],h.[2],h.[3],h.[4]) (zip ([0..79],W m)) in
			{h.[0] + a, h.[1] + b, h.[2] + c, h.[3] + d, h.[4] + e} 
	where
		//Generate the 80 word sequence from the block data
		W :: {#Char} -> [Int]
		W m = gen 0 []
		where
			gen t acc
				| t == 80	= acc
				| t < 16	= gen (t + 1) (acc ++ [wordAt t m])
				# w3  = acc !! (t - 3)
				# w8  = acc !! (t - 8)
				# w14 = acc !! (t - 14)
				# w16 = acc !! (t - 16)
				= gen (t + 1) (acc ++ [S 1 (w3 bitxor w8 bitxor w14 bitxor w16)])

			wordAt t m = (toInt m.[t*4] << 24) + (toInt m.[t*4 + 1] << 16) + (toInt m.[t*4 + 2] << 8) + (toInt m.[t*4 + 3])

		//Iterate on the digest
		iteration :: (Int,Int,Int,Int,Int) (Int,Int) -> (Int,Int,Int,Int,Int)
		iteration (a,b,c,d,e) (t,w) = (S 5 a + f t b c d + e + w + K t, a, S 30 b, c, d)

		//K values are constants based defined for ranges of t
		K :: Int -> Int
		K t 
			| t <= 19	= 0x5a827999
			| t <= 39	= 0x6ed9eba1
			| t <= 59	= 0x8f1bbcdc
						= 0xca62c1d6
		//S is a 32-bit circular left shift operation
		S :: Int Int -> Int
		S n x = ((x << n) bitor ((x >> (32 - n)) bitand ((1 << n) - 1))) bitand 0xffffffff

		//f is a logical function that merges three of the buffer words into one
		f :: Int Int Int Int -> Int
		f t b c d 
			| t <= 19	= (b bitand c) bitor ((bitnot b) bitand d)
			| t <= 39	= b bitxor c bitxor d
			| t <= 59	= (b bitand c) bitor (b bitand d) bitor (c bitand d)
						= b bitxor c bitxor d						
	//Post-processing:
	//Convert the 5-integer representation of the digest to a 20 byte array
	toBytes :: {#Int} -> {#Char}
	toBytes h = join "" [{toChar (h.[i] >> (j * 8)) \\ j <- [3,2,1,0]} \\ i <- [0..4]]

//The file digest is computed very dumb. Reading the full file and then hashing
//This can be optimized in the future because the digest can also be computed streaming
sha1FileDigest :: !FilePath !*env -> (!MaybeError FileError SHA1Digest,!*env) | FileSystem env
sha1FileDigest path env	
	= appFst (fmap sha1StringDigest) (readFile path env)	

instance toString SHA1Digest
where
	toString (SHA1Digest d)	= join "" [char c \\ c <-:d]
    where
        char c = {hex.[((toInt c >> 4) bitand 15)],hex.[(toInt c) bitand 15]}
        hex = "0123456789abcdef"
