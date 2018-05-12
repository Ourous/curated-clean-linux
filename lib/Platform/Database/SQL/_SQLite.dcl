definition module Database.SQL._SQLite
import System._Pointer

//SQLite C-API Constants and Offsets
SQLITE_OK           :== 0
SQLITE_ERROR        :== 1
SQLITE_ROW          :== 100
SQLITE_DONE         :== 101
SQLITE_INTEGER      :== 1
SQLITE_FLOAT        :== 2
SQLITE_TEXT         :== 3
SQLITE_BLOB         :== 4
SQLITE_NULL         :== 5
SQLITE_STATIC       :== 0
SQLITE_TRANSIENT    :== -1

//SQLite C-API foreign functions
sqlite3_open :: !{#Char} -> (!Int,!Pointer)
sqlite3_close :: !Pointer -> Int
sqlite3_errcode :: !Pointer -> Int
sqlite3_errmsg :: !Pointer -> Pointer
sqlite3_prepare :: !Pointer !{#Char} !Int -> (!Int,!Pointer,!Pointer)
sqlite3_bind_null :: !Pointer !Int -> Int
sqlite3_bind_text :: !Pointer !Int !{#Char} !Int !Pointer -> Int
sqlite3_bind_blob :: !Pointer !Int !{#Char} !Int !Pointer -> Int
sqlite3_bind_int64 :: !Pointer !Int !Int -> Int
sqlite3_bind_double :: !Pointer !Int !Real -> Int
sqlite3_step :: !Pointer -> Int
sqlite3_finalize :: !Pointer -> Int
sqlite3_column_count :: !Pointer -> Int
sqlite3_column_type :: !Pointer !Int -> Int
sqlite3_column_int :: !Pointer !Int -> Int
sqlite3_column_text :: !Pointer !Int -> Pointer
sqlite3_last_insert_rowid :: !Pointer -> Int
sqlite3_changes :: !Pointer -> Int
