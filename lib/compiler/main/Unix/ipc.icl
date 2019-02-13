implementation module ipc;

open_pipes :: !String !String -> Int;
open_pipes a0 a1 = code {
	ccall open_pipes "SS:I"
}
// int open_pipes (CleanString commands_name,CleanString results_name);

get_command_length :: Int;
get_command_length  = code {
	ccall get_command_length ":I"
}
// int get_command_length ();

get_command :: !String -> Int;
get_command a0 = code {
	ccall get_command "S:I"
}
// int get_command (CleanString cleanString);

send_result :: !Int -> Int;
send_result a0 = code {
	ccall send_result "I:I"
}
// int send_result (int result);
