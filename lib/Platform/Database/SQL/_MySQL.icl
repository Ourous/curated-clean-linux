implementation module Database.SQL._MySQL
import System._Pointer

mysql_affected_rows :: !Pointer -> Int
mysql_affected_rows a0 = code {
	ccall mysql_affected_rows "p:I"
}
mysql_close :: !Pointer -> Int
mysql_close a0 = code {
	ccall mysql_close "p:I"
}
mysql_errno :: !Pointer -> Int
mysql_errno a0 = code {
	ccall mysql_errno "p:I"
}
mysql_error :: !Pointer -> Pointer
mysql_error a0 = code {
	ccall mysql_error "p:p"
}
mysql_fetch_fields :: !Pointer -> Pointer
mysql_fetch_fields a0 = code {
	ccall mysql_fetch_fields "p:p"
}
mysql_fetch_lengths :: !Pointer -> Pointer
mysql_fetch_lengths a0 = code {
	ccall mysql_fetch_lengths "p:p"
}
mysql_fetch_row :: !Pointer -> Pointer
mysql_fetch_row a0 = code {
	ccall mysql_fetch_row "p:p"
}
mysql_free_result :: !Pointer -> Int
mysql_free_result a0 = code {
	ccall mysql_free_result "p:I"
}
mysql_init :: !Int -> Pointer
mysql_init a0 = code {
	ccall mysql_init "p:p"
}
mysql_insert_id :: !Pointer -> Int
mysql_insert_id a0 = code {
	ccall mysql_insert_id "p:I"
}
mysql_num_fields :: !Pointer -> Int
mysql_num_fields a0 = code {
	ccall mysql_num_fields "p:I"
}
mysql_num_rows :: !Pointer -> Int
mysql_num_rows a0 = code {
	ccall mysql_num_rows "p:I"
}
mysql_real_connect :: !Pointer !{#Char} !{#Char} !{#Char} !{#Char} !Int !Int !Int -> Pointer
mysql_real_connect a0 a1 a2 a3 a4 a5 a6 a7 = code {
	ccall mysql_real_connect "pssssIII:p"
}
mysql_real_escape_string :: !Pointer !{#Char} !{#Char} !Int -> Int
mysql_real_escape_string a0 a1 a2 a3 = code {
	ccall mysql_real_escape_string "pssI:I"
}
mysql_real_query :: !Pointer !{#Char} !Int -> Int
mysql_real_query a0 a1 a2 = code {
	ccall mysql_real_query "psI:I"
}
mysql_store_result :: !Pointer -> Pointer
mysql_store_result a0 = code {
	ccall mysql_store_result "p:p"
}
