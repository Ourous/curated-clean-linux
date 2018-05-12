implementation module Network.IP
/**
* Small module which provides basic IP functionality
*/
import StdString, StdInt
import Data.Maybe, Text, System._Pointer

/**
* Type which represents an IP (v4) address
* We can fit an IP address into a 32 bit integer by putting the four quads in big endian order (network byte order)
*/
:: IPAddress = IPAddress Int

/**
* Convert an IP address to and from its 'dotted decimal' string representation
*/
instance toString IPAddress
where
	toString (IPAddress ip)	= toString b1 +++ "." +++ toString b2 +++ "." +++ toString b3 +++ "." +++ toString b4
	where
		b1	=  ip        bitand 255
		b2	= (ip >> 8)  bitand 255
		b3	= (ip >> 16) bitand 255
		b4	= (ip >> 24) bitand 255

instance fromString IPAddress
where
	fromString s = case (split "." s) of 
		[b1,b2,b3,b4]	= IPAddress ((toInt b1) + ((toInt b2) << 8) + ((toInt b3) << 16) + ((toInt b4) << 24))
		_				= IPAddress 0

instance toInt IPAddress
where
	toInt (IPAddress ip)	= ip

instance fromInt IPAddress
where
	fromInt i				= IPAddress i

/**
* Looks up a DNS name (e.g www.example.com) and returns an IP address on success
*/
lookupIPAddress :: !String !*World -> (!Maybe IPAddress, !*World)
lookupIPAddress name world
	# (ptrhe,world) = gethostbynameC (packString name) world
	| ptrhe == 0	= (Nothing, world)	//Fail
	# ptrli			= readInt ptrhe (IF_INT_64_OR_32 24 16) 
	# ptrad			= readInt ptrli 0
	# addr			= readInt4Z ptrad 0
	= (Just (IPAddress addr), world)	
	where
		gethostbynameC :: !{#Char} !*World -> (!Pointer, !*World)
		gethostbynameC a0 a1 = code {
			ccall gethostbyname "s:p:p"
		}
