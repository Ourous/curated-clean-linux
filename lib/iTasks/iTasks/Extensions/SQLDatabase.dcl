definition module iTasks.Extensions.SQLDatabase
/**
* This experimental extension provides tasks and shares
* for interacting with a relational database.
*
* To run this, link the code with sqlite3 and a mysqlclient
* To find out what the linkerflags on debian you can run:
* $ pkg-config --libs mariadb sqlite3
*
* Note that on debian mariadb is the mysql standard, it is fully compatible.
*
* It provides only mimimal functionality and currently only works with MySQL and SQLite
*/
import iTasks, Database.SQL, Data.Error, System.FilePath

//Type for describing database configurations
:: SQLDatabaseDef
    = MySQLDatabase SQLDatabase
    | SQLiteDatabase FilePath

derive class iTask SQLDatabaseDef, SQLDatabase, SQLValue, SQLTime, SQLDate, SQLTable, SQLColumn, SQLColumnType

//Core access functions

/**
* Generic SQL Database share
* You need to supply the read/write operations as functions using an SQL cursor
*
* @param The database connection details
* @param A unique identifier for the share
* @param The read access function
* @param The write access function
*
* @return The shared data source
*/
sqlShare :: String (A.*cur: p *cur -> *(MaybeErrorString r,*cur) | SQLCursor cur)
								(A.*cur: p w *cur -> *(MaybeErrorString (), *cur) | SQLCursor cur) -> RWShared (SQLDatabaseDef,p) r w



/**
* Perform one or multiple queries on an SQL database
*/
sqlExecute	:: SQLDatabaseDef [String] (A.*cur: *cur -> *(MaybeErrorString a,*cur) | SQLCursor cur) -> Task a | iTask a

//Common helper functions for sqlExecute
execSelect :: SQLStatement [SQLValue] *cur -> *(MaybeErrorString [SQLRow],*cur) | SQLCursor cur
execInsert :: SQLStatement [SQLValue] *cur -> *(MaybeErrorString Int,*cur) | SQLCursor cur
execDelete :: SQLStatement [SQLValue] *cur -> *(MaybeErrorString (),*cur) | SQLCursor cur

/**
* Run a single query and fetch all results
*/
sqlExecuteSelect :: SQLDatabaseDef SQLStatement ![SQLValue] -> Task [SQLRow]

/**
* Read only query that is run each time the share is read.
*
* Note: Although it is possible to do other queries than just selects,
* this is a bad idea. You never know how many times the query will be executed
*/
sqlSelectShare	:: String SQLStatement ![SQLValue] -> ROShared SQLDatabaseDef [SQLRow]

/*
* View the list of tables in a database
*/
sqlTables :: ROShared SQLDatabaseDef [SQLTableName]
/**
* The structure of database table
*/
sqlTableDefinition :: ROShared (SQLDatabaseDef,SQLTableName) SQLTable

sqlExecuteCreateTable :: SQLDatabaseDef SQLTable -> Task ()
sqlExecuteDropTable :: SQLDatabaseDef SQLTableName -> Task ()
