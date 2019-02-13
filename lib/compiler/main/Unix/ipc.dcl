definition module ipc;

open_pipes :: !String !String -> Int;
// int open_pipes (CleanString commands_name,CleanString results_name);
get_command_length :: Int;
// int get_command_length ();
get_command :: !String -> Int;
// int get_command (CleanString cleanString);
send_result :: !Int -> Int;
// int send_result (int result);
