definition module Database.SQL._MySQL
import System._Pointer

//MySQL C-API Constants and Offsets
ENUM_FLAG				:== 256
CLIENT_FOUND_ROWS		:== 2

MYSQL_TYPE_TINY			:== 1
MYSQL_TYPE_SHORT		:== 2
MYSQL_TYPE_LONG			:== 3
MYSQL_TYPE_INT24		:== 9
MYSQL_TYPE_LONGLONG		:== 8
MYSQL_TYPE_DECIMAL		:== 0
MYSQL_TYPE_NEWDECIMAL	:== 246
MYSQL_TYPE_FLOAT		:== 4
MYSQL_TYPE_DOUBLE		:== 5
MYSQL_TYPE_TIMESTAMP	:== 7
MYSQL_TYPE_DATE			:== 10
MYSQL_TYPE_TIME			:== 11
MYSQL_TYPE_DATETIME		:== 12
MYSQL_TYPE_STRING		:== 254
MYSQL_TYPE_VAR_STRING	:== 253
MYSQL_TYPE_BLOB			:== 252
MYSQL_TYPE_ENUM			:== 247

SIZEOF_MYSQL_FIELD			:== 128
MYSQL_FIELD_TYPE_OFFSET 	:== 112
MYSQL_FIELD_FLAGS_OFFSET	:== 100

//MySQL C-API foreign functions
// libmysqlclient access
mysql_affected_rows :: !Pointer -> Int
mysql_close :: !Pointer -> Int
mysql_errno :: !Pointer -> Int
mysql_error :: !Pointer -> Pointer
mysql_fetch_fields :: !Pointer -> Pointer
mysql_fetch_lengths :: !Pointer -> Pointer
mysql_fetch_row :: !Pointer -> Pointer
mysql_free_result :: !Pointer -> Int
mysql_init :: !Int -> Pointer
mysql_insert_id :: !Pointer -> Int
mysql_num_fields :: !Pointer -> Int
mysql_num_rows :: !Pointer -> Int
mysql_real_connect :: !Pointer !{#Char} !{#Char} !{#Char} !{#Char} !Int !Int !Int -> Pointer
mysql_real_escape_string :: !Pointer !{#Char} !{#Char} !Int -> Int
mysql_real_query :: !Pointer !{#Char} !Int -> Int
mysql_store_result :: !Pointer -> Pointer
