definition module ostcp

//	********************************************************************************
//	Clean Standard TCP library, version 1.2.2
//	
//	Author: Martin Wierich
//	Modified: 15 October 2001 for Clean 2.0 (Peter Achten)
//	********************************************************************************

import	StdMaybe
import	TCPDef
from	TCPChannelClass import :: Timeout
import	tcp

os_eom						:: !EndpointRef !*env -> (!Bool, !*env)
os_disconnected				:: !EndpointRef !*env -> (!Bool, !*env)
os_connectrequestavailable	:: !EndpointRef !*env -> (!Bool, !*env)
os_connectTCP_sync			:: !Int !(!Bool,!Int) !(!Int,!Int) !*env
							-> (!(!InetErrCode,!Bool,!EndpointRef),  !*env)

//	os_connectTCPC isIOProg block (doTimeout, stopTime) (ipAddr, portnum) env
//	gets ip address and port number of destination in host order, returns new endpoint, and whether the timeout expired
//	iff block==True, the function blocks, otherwise an event will be generated later.
//	errCode:	0 ok;	1:error (e.g. can't connect)
//	also adds a new dictionary item whose values are
//		block: 		(referencecount=2, hasSNotif=False, hasRNotif=False)
//		not block:	(referencecount=1, hasSNotif=False, hasRNotif=True)
// if timeout or error occurs, then resources (endpoint and dictionary entry) will be removed

getMbStopTime				:: !(Maybe Timeout) !*env -> (!(!Bool, !Int), !*env) | ChannelEnv env
tcp_getcurrenttick :: !*World -> (!Int, !*World)
