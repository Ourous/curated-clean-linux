implementation module set_return_code;

import code from "set_return_code_c.o";

import StdString;
import StdDebug;

:: *UniqueWorld :== World;

set_return_code :: !Int !UniqueWorld -> UniqueWorld;
set_return_code a0 a1 = code
{
	ccall set_return_code "I:V:A"
	fill_a 0 1
	pop_a 1
}

// void set_return_code (int return_code);
