definition module Database.SQL.SQLite
//This module defines implements the interface for relatational databases
//of SQL.dcl for the SQLite database engine

import Database.SQL
import Data.Maybe, StdString

:: SQLiteContext
:: SQLiteConnection
:: SQLiteCursor

instance SQLEnvironment		World			    SQLiteContext
instance SQLContext			SQLiteContext	    SQLiteConnection
instance SQLConnection		SQLiteConnection	SQLiteCursor
instance SQLCursor			SQLiteCursor
instance SQLSchemaCursor    SQLiteCursor
