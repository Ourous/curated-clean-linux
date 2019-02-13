module CleanCocl;

import StdEnv;

import cache_variable;

import CoclSystemDependent,Clean2AppleEventHandler;
//1.3
from events import KeyDownEvent,HighLevelEvent,GetNextEvent,WaitNextEvent,Toolbox,RgnHandle;
//3.1
/*2.0
from events import KeyDownEvent,HighLevelEvent,GetNextEvent,WaitNextEvent,::Toolbox,::RgnHandle;
0.2*/
from predef import init_identifiers;

DeviceMask :== -31361;		// HighLevelEventMask+UpdateMask+ActivMask+KeyboardMask+MouseMask+OsMask+1

Start world
	# (symbol_table,world)	= init_identifiers newHeap world;
	| install_apple_event_handlers==0 && store_state (empty_cache symbol_table)<>0
		= event_loop world;
		= world;
{}{
	event_loop world
//		# (b,what,message,when,position1,position2,modifiers,tb) = GetNextEvent DeviceMask 0;
		# (b,what,message,when,position1,position2,modifiers,tb) = WaitNextEvent (-1)/*DeviceMask*/ (-1) 0 0;
		| what==KeyDownEvent
			= world;
		| what==HighLevelEvent
			# event = (b,what,message,when,position1,position2,modifiers);
			# ((ok,quit),world) = accFiles handle_apple_event world;
			with {
				handle_apple_event :: !*Files -> (!(!Bool,!Bool),!*Files);
				handle_apple_event files
					# (ok,quit,files) = HandleAppleEvent event script_handler files;
					= ((ok,quit),files);
			}
			| quit
				= world;
				= event_loop world;
			= event_loop world;
}

/*
import deltaEventIO, deltaIOState
import CoclSystemDependent

Don`tCareId
	:==	0

Start :: !*World -> *World
Start world
	# (_, world)
		=	StartIO [menus : SystemDependentDevices] 0 SystemDependentInitialIO world
		with
			menus
				=	MenuSystem [file]
			file
				=	PullDownMenu Don`tCareId "File" Able
						[MenuItem Don`tCareId "Quit" (Key 'Q') Able Quit]
	=	world

Quit :: *s (IOState *s) -> (*s, IOState *s)
Quit s io
	=	(s, QuitIO io)
*/
