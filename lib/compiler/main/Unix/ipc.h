int open_pipes (CleanString commands_name, CleanString results_name);
Clean (open_pipes :: String String -> Int)
int get_command_length (void);
Clean (get_command_length :: Int)
int get_command (CleanString cleanString);
Clean (get_command :: String -> Int)
int send_result (int result);
Clean (send_result :: Int -> Int)
