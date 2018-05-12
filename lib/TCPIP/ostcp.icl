implementation module ostcp

import	StdInt, StdTuple
import StdMaybe

import	TCPDef,TCPChannelClass
import	tcp
import	code from "cTCP_121."

os_eom :: !EndpointRef !*env -> (!Bool, !*env)
os_eom er e
// check for eom
	= IF_INT_64_OR_32 (os_eom64 er e) (os_eom32 er e);

os_eom64 :: !EndpointRef !*env -> (!Bool, !*env)
os_eom64 er e
	= code inline {
		ccall os_eom "p:I:A"
	}

os_eom32 :: !EndpointRef !*env -> (!Bool, !*env)
os_eom32 er e
	= code inline {
		ccall os_eom "I:I:A"
	}
		
os_disconnected :: !EndpointRef !*env -> (!Bool, !*env)
os_disconnected er e
// check for disconnected
	= IF_INT_64_OR_32 (os_disconnected64 er e) (os_disconnected32 er e);

os_disconnected64 :: !EndpointRef !*env -> (!Bool, !*env)
os_disconnected64 er e
	= code inline {
		ccall os_disconnected "p:I:A"
	}

os_disconnected32 :: !EndpointRef !*env -> (!Bool, !*env)
os_disconnected32 er e
	= code inline {
		ccall os_disconnected "I:I:A"
	}

os_connectrequestavailable :: !EndpointRef !*env -> (!Bool, !*env)
os_connectrequestavailable er e
	= IF_INT_64_OR_32 (os_connectrequestavailable64 er e) (os_connectrequestavailable32 er e);

os_connectrequestavailable64 :: !EndpointRef !*env -> (!Bool, !*env)
os_connectrequestavailable64 er e
	= code inline {
		ccall os_connectrequestavailable "p:I:A"
	}

os_connectrequestavailable32 :: !EndpointRef !*env -> (!Bool, !*env)
os_connectrequestavailable32 er e
	= code inline {
		ccall os_connectrequestavailable "I:I:A"
	}

/*
os_connectTCP :: !Int !Bool !(!Bool, !Int) !(!Int,!Int) !*env -> (!(!InetErrCode,!Bool,!EndpointRef), !*env)
os_connectTCP onlyForMac block time addr e
	= IF_INT_64_OR_32 (os_connectTCP64 onlyForMac block time addr e) (os_connectTCP32 onlyForMac block time addr e);

os_connectTCP64 :: !Int !Bool !(!Bool, !Int) !(!Int,!Int) !*env -> (!(!InetErrCode,!Bool,!EndpointRef), !*env)
os_connectTCP64 onlyForMac block time addr e
	= code inline {
		ccall os_connectTCPC "IIIIII:VIIp:A"
	}

os_connectTCP32 :: !Int !Bool !(!Bool, !Int) !(!Int,!Int) !*env -> (!(!InetErrCode,!Bool,!EndpointRef), !*env)
os_connectTCP32 onlyForMac block time addr e
	= code inline {
		ccall os_connectTCPC "IIIIII:VIII:A"
	}
*/

os_connectTCP_sync :: !Int !(!Bool, !Int) !(!Int,!Int) !*env -> (!(!InetErrCode,!Bool,!EndpointRef), !*env)
os_connectTCP_sync onlyForMac time addr e
	= IF_INT_64_OR_32 (os_connectTCP_sync64 onlyForMac time addr e) (os_connectTCP_sync32 onlyForMac time addr e);

os_connectTCP_sync64 :: !Int !(!Bool, !Int) !(!Int,!Int) !*env -> (!(!InetErrCode,!Bool,!EndpointRef), !*env)
os_connectTCP_sync64 onlyForMac time addr e
	= code inline {
		ccall os_connectTCP_syncC "IIIII:VIIp:A"
	}

os_connectTCP_sync32 :: !Int !(!Bool, !Int) !(!Int,!Int) !*env -> (!(!InetErrCode,!Bool,!EndpointRef), !*env)
os_connectTCP_sync32 onlyForMac time addr e
	= code inline {
		ccall os_connectTCP_syncC "IIIII:VIII:A"
	}

getMbStopTime :: !(Maybe Timeout) !*env -> (!(!Bool, !Int), !*env) | ChannelEnv env
getMbStopTime Nothing env
	=((False,0), env)
getMbStopTime (Just timeout) env
	# (now, env) = channel_env_get_current_tick env
	= ((True, timeout + now), env)

tcp_getcurrenttick :: !*World -> (!Int, !*World)
tcp_getcurrenttick world
	= (GetTickCount, world)

GetTickCount :: Int
GetTickCount
	= code inline {
		ccall GetTickCount ":I"
	}
