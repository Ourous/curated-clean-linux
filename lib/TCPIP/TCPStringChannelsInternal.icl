implementation module TCPStringChannelsInternal

import	StdEnv
from StdFunc import seq
import	TCPChannelClass, TCPDef, TCPChannels, TCPEvent

//from tcp import ::Tick,class ChannelEnv (channel_env_get_current_tick)

addString	::	!(!String, !Int) !ReadPhase !Int -> (![String], !ReadPhase)
/*	addString (s,i) rp maxSize
		performs a state transition on the ReadPhase which reflects the receiving of (s % (i,(size s)-1))
*/
addString _ readPhase=:EndOfMessages _
	= ([], readPhase)
addString (string, stringBegin) readPhase=:(ReadingLength l) maxSize
	|	stringBegin>=size string
		= ([], readPhase)
	#!	chList	= [ string.[i] \\ i<-[stringBegin..(size string)-1] ]
		digits	= takeWhile isDigit chList
		newL	= seq (map transition digits) l
				// converts character list into an integer
	|	newL>maxSize && maxSize>0
		= ([], EndOfMessages)
	|	length digits < size string - stringBegin
		// the whole number has been read
		= addString (string, stringBegin+(length digits)+1) (ReadingString (createArray newL ' ') newL newL) maxSize
	// there are still some digits unread
	= ([], ReadingLength newL)
  where
	transition digit int
		= 10*int + (digitToInt digit)
addString (string, stringBegin) readPhase=:(ReadingString wholeString stillToReceive total) maxSize
	|	stringBegin>=size string
		= ([], readPhase)
	#!	readyBytes	= (size string) - stringBegin
	|	stillToReceive<=readyBytes
		// wholeString can be filled totally
		#!	wholeString	= strcpy 	wholeString	(total-stillToReceive)
									string 		stringBegin
									stillToReceive
			newBegin	= stringBegin + stillToReceive + 1
			(otherStrings, readPhase)	= addString (string, newBegin) (ReadingLength 0) maxSize
		= ([wholeString:otherStrings], readPhase)
	// wholeString can not be filled totally
	#!	wholeString	= strcpy 	wholeString	(total-stillToReceive)
								string		stringBegin
								readyBytes
	= ([], ReadingString wholeString (stillToReceive-readyBytes) total)

strcpy	::	!*{#Char} !Int !{#Char} !Int !Int -> *{#Char}
strcpy dest destBegin src srcBegin nrOfBytes
	|	nrOfBytes<=0
		= dest
	= strcpy { dest & [destBegin] = src.[srcBegin] } (inc destBegin) src (inc srcBegin) (dec nrOfBytes)
