#pragma export on

Clean (:: *DoCommandEnvironment :== Int)
/* Clean (DoCommand command :== DoCommandNullTerminated (command +++ "\0")) */

Clean (DoCommandNullTerminated :: String DoCommandEnvironment -> (Int, DoCommandEnvironment))

/* functions */
int DoCommandNullTerminated (CleanString command);
Clean (DoCommandNullTerminated :: String DoCommandEnvironment -> (Int, DoCommandEnvironment))

#pragma export off
