implementation module TCPEvent

import StdEnv
import TCPChannelClass, TCPDef, TCPChannels

class accSChannel ch :: (TCP_SChannel -> (x, TCP_SChannel)) *(ch .a) -> (x, *(ch .a))

instance accSChannel TCP_SChannel_ where
	accSChannel f tcpSChanPolymorph
		#!	(x, tcpSChanByteSeq)	= f (Cast tcpSChanPolymorph)
		= (x, Cast tcpSChanByteSeq)

instance accSChannel TCP_SCharStream_ where
	accSChannel f sChannel=:{sbs_schan}
		#!	(x, sbs_schan)	= f sbs_schan
		= (x, { sChannel & sbs_schan=sbs_schan })

Cast :: !.a -> .b
Cast a
	= code
		{
			pop_a 0
		}
