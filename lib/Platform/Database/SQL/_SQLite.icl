implementation module Database.SQL._SQLite
import System._Pointer

sqlite3_open :: !{#Char} -> (!Int,!Pointer)
sqlite3_open a0 = code {
	ccall sqlite3_open "s:Ip"
}
sqlite3_close :: !Pointer -> Int
sqlite3_close a0 = code {
    ccall sqlite3_close "p:I"
}
sqlite3_errcode :: !Pointer -> Int
sqlite3_errcode a0 = code {
    ccall sqlite3_errcode "p:I"
}
sqlite3_errmsg :: !Pointer -> Pointer
sqlite3_errmsg a0 = code {
    ccall sqlite3_errmsg "p:p"
}
sqlite3_prepare :: !Pointer !{#Char} !Int -> (!Int,!Pointer,!Pointer)
sqlite3_prepare a0 a1 a2 = code {
    ccall sqlite3_prepare "psI:Ipp"
}
sqlite3_bind_null :: !Pointer !Int -> Int
sqlite3_bind_null a0 a1 = code {
    ccall sqlite3_bind_null "pI:I"
}
sqlite3_bind_text :: !Pointer !Int !{#Char} !Int !Pointer -> Int
sqlite3_bind_text a0 a1 a2 a3 a4 = code {
    ccall sqlite3_bind_text "pIsIp:I"
}
sqlite3_bind_blob :: !Pointer !Int !{#Char} !Int !Pointer -> Int
sqlite3_bind_blob a0 a1 a2 a3 a4 = code {
    ccall sqlite3_bind_blob "pIsIp:I"
}
sqlite3_bind_int64 :: !Pointer !Int !Int -> Int
sqlite3_bind_int64 a0 a1 a2 = code {
    ccall sqlite3_bind_int64 "pII:I"
}
sqlite3_bind_double :: !Pointer !Int !Real -> Int
sqlite3_bind_double a0 a1 a2 = code {
    ccall sqlite3_bind_double "pIr:I"
}
sqlite3_step :: !Pointer -> Int
sqlite3_step a0 = code {
    ccall sqlite3_step "p:I"
}
sqlite3_finalize :: !Pointer -> Int
sqlite3_finalize a0 = code {
    ccall sqlite3_finalize "p:I"
}
sqlite3_column_count :: !Pointer -> Int
sqlite3_column_count a0 = code {
    ccall sqlite3_column_count "p:I"
}
sqlite3_column_type :: !Pointer !Int -> Int
sqlite3_column_type a0 a1 = code {
    ccall sqlite3_column_type "pI:I"
}
sqlite3_column_int :: !Pointer !Int -> Int
sqlite3_column_int a0 a1 = code {
    ccall sqlite3_column_int "pI:I"
}
sqlite3_column_text :: !Pointer !Int -> Pointer
sqlite3_column_text a0 a1 = code {
    ccall sqlite3_column_text "pI:p"
}
sqlite3_last_insert_rowid :: !Pointer -> Int
sqlite3_last_insert_rowid a0 = code {
    ccall sqlite3_last_insert_rowid "p:I"
}
sqlite3_changes :: !Pointer -> Int
sqlite3_changes a0 = code {
    ccall sqlite3_changes "p:I"
}
