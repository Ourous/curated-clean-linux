definition module Database.SQL

// *********************************************************************************************************************
// Clean Relational Database (SQL) API. v 0.2
// This module defines a common API for working with relational databases.
// *********************************************************************************************************************

import StdString, Data.Maybe

// *********************************************************************************************************************
// Basic types
// *********************************************************************************************************************

:: SQLDatabase =
	{ database	:: !String
	, host		:: !Maybe String
	, username	:: !Maybe String
	, password	:: !Maybe String
	}

// SQL Statements and queries are just strings
:: SQLStatement		:== String

// This type represents the possible values that elements of a row in a 
// result set can have. The constructors map directly to the constructors of
// the SQLType type.
:: SQLValue			=	SQLVChar		!String
					|	SQLVVarchar 	!String
					|	SQLVText		!String
					|	SQLVInteger		!Int
					|	SQLVReal		!Real
					|	SQLVFloat		!Real
					|	SQLVDouble		!Real
					|	SQLVDate		!SQLDate
					|	SQLVTime		!SQLTime
					|	SQLVTimestamp	!Int
					|	SQLVDatetime	!SQLDate !SQLTime
					|	SQLVEnum		!String
					|	SQLVNull
					|	SQLVBlob		!String
					|	SQLVUnknown		!String

// Date and time structures
:: SQLDate =
	{ year		:: !Int
	, month		:: !Int
	, day		:: !Int
	}
:: SQLTime =
	{ hour		:: !Int
	, minute	:: !Int
	, second	:: !Int
	}
	
// A row is just a list of values
:: SQLRow			:== [SQLValue]


// *********************************************************************************************************************
// Errors
// *********************************************************************************************************************

// Warning:				Non fatal errors, you can still continue
// InterfaceError:		Error related to the interface, not the database itself
// DatabaseError:		Error related to the database that can not be classified
//						as Operational error or Internal error
// DataError:			Error due to problems with the data
// OperationalError:	Error due to operational problems with the database.
//						E.g. disconnects, memory full etc.
// IntegrityError:		Errors related to data integrity, e.g. key constraint 
//						violations
// InternalError:		Errors related to internal problems in the database 
//						library
// ProgrammingError:	Errors of the end user, e.g. syntax errors in SQL
//						statements
// NotSupportedError:	An operation is not supported by the database library

:: SQLError	= SQLWarning			!Int !String	
			| SQLInterfaceError		!Int !String	
			| SQLDatabaseError		!Int !String	
			| SQLDataError			!Int !String	
			| SQLOperationalError	!Int !String
			| SQLIntegrityError		!Int !String
			| SQLInternalError		!Int !String
			| SQLProgrammingError 	!Int !String
			| SQLNotSupportedError

// *********************************************************************************************************************
// Database Schema Definitions
// *********************************************************************************************************************
:: SQLSchema :== [SQLTable]
:: SQLTable =
	{ name			:: SQLTableName
	, columns		:: [SQLColumn]
	, primaryKey	:: [SQLColumnName]
	, foreignKeys	:: [([SQLColumnName],SQLTableName,[SQLColumnName])]
	}

:: SQLTableName		:== String
:: SQLColumnName	:== String

:: SQLColumn =
	{ name			:: SQLColumnName
	, type			:: SQLColumnType
	, null			:: Bool
	, autoIncrement	:: Bool 
	}

::	SQLColumnType	=	SQLTChar !Int
					|	SQLTVarchar !Int
					|	SQLTText
					|	SQLTInteger	
					|	SQLTReal
					|	SQLTFloat
					|	SQLTDouble
					|	SQLTDate
					|	SQLTTime
					|	SQLTTimestamp
					|	SQLTDatetime
					|	SQLTEnum ![String]
					|	SQLTBlob
					|	SQLTUnknown

// *********************************************************************************************************************
// Database Interaction API
// *********************************************************************************************************************

class SQLEnvironment env ctx
where
	openContext		:: !*env									-> (!(Maybe SQLError), !(Maybe *ctx), !*env)
	closeContext	:: !*ctx !*env								-> (!(Maybe SQLError), !*env)					

class SQLContext ctx con
where
	openConnection	:: !SQLDatabase !*ctx						-> (!(Maybe SQLError), !(Maybe *con), !*ctx)	
	closeConnection	:: !*con !*ctx								-> (!(Maybe SQLError), !*ctx)
	
class SQLConnection con cur
where
	openCursor		:: !*con									-> (!(Maybe SQLError), !(Maybe *cur), !*con)
	closeCursor		:: !*cur !*con								-> (!(Maybe SQLError), !*con)

class SQLCursor cur
where
	execute			:: !SQLStatement ![SQLValue] !*cur			-> (!(Maybe SQLError), !*cur)
	executeMany		:: !SQLStatement ![[SQLValue]] !*cur		-> (!(Maybe SQLError), !*cur)
	numRows			:: !*cur									-> (!(Maybe SQLError), !Int, !*cur)
	numFields		:: !*cur									-> (!(Maybe SQLError), !Int, !*cur)
	insertId		:: !*cur									-> (!(Maybe SQLError), !Int, !*cur)
	fetchOne		:: !*cur									-> (!(Maybe SQLError), !(Maybe SQLRow), !*cur)
	fetchMany		:: !Int !*cur 								-> (!(Maybe SQLError), ![SQLRow], !*cur)
	fetchAll		:: !*cur									-> (!(Maybe SQLError), ![SQLRow], !*cur)
	commit			:: !*cur									-> (!(Maybe SQLError), !*cur)
	rollback		:: !*cur									-> (!(Maybe SQLError), !*cur)

class SQLSchemaCursor cur
where
    listTables      :: !*cur                                    -> (!(Maybe SQLError), ![SQLTableName], !*cur)
    describeTable   :: !SQLTableName !*cur                      -> (!(Maybe SQLError), !(Maybe SQLTable), !*cur)
    createTable     :: !SQLTable !*cur                          -> (!(Maybe SQLError), !*cur)
    deleteTable     :: !SQLTableName !*cur                      -> (!(Maybe SQLError), !*cur)

// *********************************************************************************************************************
// Common class instances
// *********************************************************************************************************************

instance toString SQLValue, SQLDate, SQLTime, SQLError
instance == SQLValue, SQLDate, SQLTime
