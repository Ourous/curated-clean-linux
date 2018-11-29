implementation module Database.SQL.MySQL
//MySQL implementation of the Clean SQL database API
// 
import Database.SQL
import StdEnv, Data.Maybe, System._Pointer, Text
import Database.SQL._MySQL

//MySQL Does not really need a context
:: MySQLContext		:== Int
	
//A wrapper for access to MySQL's MYSQL* structs
:: MySQLConnection =
	{ conn_ptr		:: !Pointer
	}
//A wrapper to MySQL's result sets
:: MySQLCursor = 
	{ conn_ptr		:: !Pointer
	, result_ptr	:: !Pointer
	, result_size	:: !Int
	, fields_ptr	:: !Pointer
	, row_ptr		:: !Pointer
	, row_lengths	:: !Pointer
	}

instance SQLEnvironment World MySQLContext
where
	//Dummy environment
	openContext :: !*World -> (!(Maybe SQLError),!(Maybe *MySQLContext),!*World)
	openContext world = (Nothing, Just 42, world)

	closeContext :: !*MySQLContext !*World -> (!(Maybe SQLError), !*World)
	closeContext context world = (Nothing, world)

instance SQLContext MySQLContext MySQLConnection
where
	openConnection	:: !SQLDatabase !*MySQLContext	-> (!(Maybe SQLError),!(Maybe *MySQLConnection),!*MySQLContext)
	openConnection {SQLDatabase|host,username,password,database} context
		//Initialize a handle
		# conn_ptr	= mysql_init 0
		| conn_ptr == 0		= (Just (SQLInterfaceError 1 "Could not initialize a connection"), Nothing, context)
		//Connect
		# ok_ptr	= mysql_real_connect conn_ptr (packString (fromMaybe "" host)) (packString (fromMaybe "" username)) (packString (fromMaybe "" password)) (packString database) 0 0 CLIENT_FOUND_ROWS
		| ok_ptr == 0
			# errno 	= mysql_errno conn_ptr
			# errmsg	= derefString (mysql_error conn_ptr)
			| errno <> errno || errmsg <> errmsg	= undef //Force execution
			
			= (Just (SQLDatabaseError errno errmsg), Nothing, context) 
		//Success
		= (Nothing, Just {MySQLConnection|conn_ptr = conn_ptr}, context)
		
	closeConnection	:: !*MySQLConnection !*MySQLContext -> (!(Maybe SQLError), !*MySQLContext)
	closeConnection connection=:{MySQLConnection|conn_ptr} context 
		# dummy = mysql_close conn_ptr
		| dummy <> dummy = undef	// Force execution of void function
		= (Nothing, context)

instance SQLConnection MySQLConnection MySQLCursor
where
	openCursor :: !*MySQLConnection -> (!(Maybe SQLError), !(Maybe *MySQLCursor), !*MySQLConnection)
	openCursor connection=:{MySQLConnection|conn_ptr}
		# cursor = {MySQLCursor
					| conn_ptr		= conn_ptr
					, result_ptr	= 0
					, result_size	= 0
					, fields_ptr	= 0
					, row_ptr		= 0
					, row_lengths	= 0
					}
		= (Nothing, Just cursor, connection)
	
	closeCursor	:: !*MySQLCursor !*MySQLConnection -> (!(Maybe SQLError), !*MySQLConnection)
	closeCursor cursor=:{MySQLCursor|result_ptr} connection
		| result_ptr == 0
			= (Nothing, connection)
		# dummy = mysql_free_result result_ptr
		| dummy <> dummy = undef	// Force execution of void function
		= (Nothing,connection)

instance SQLCursor MySQLCursor
where
	execute	:: !SQLStatement ![SQLValue] !*MySQLCursor -> (!(Maybe SQLError), !*MySQLCursor)
	execute statement values cursor=:{MySQLCursor|conn_ptr,result_ptr}
		# (error, stmt, cursor)		= mkStatement statement values cursor
		| isJust error				= (error, cursor)
		# dummy						= if (result_ptr <> 0) (mysql_free_result result_ptr) 0
		| dummy <> dummy			= undef // Force execution of void function
		# ok						= mysql_real_query conn_ptr stmt (size stmt)
		| ok <> 0
			# errno 	= mysql_errno conn_ptr
			# errmsg	= derefString (mysql_error conn_ptr)
			| errno <> errno || errmsg <> errmsg	= undef //Force execution
			= (Just (SQLDatabaseError errno errmsg),{MySQLCursor|cursor & result_ptr = 0})
		# result_ptr				= mysql_store_result conn_ptr
		| result_ptr == 0
			= (Nothing, {MySQLCursor|cursor & result_ptr = 0})
		# result_size				= mysql_num_fields result_ptr
		# fields_ptr				= mysql_fetch_fields result_ptr
		= (Nothing, {MySQLCursor|cursor & result_ptr = result_ptr, result_size = result_size, fields_ptr = fields_ptr})
	where
		mkStatement :: !SQLStatement ![SQLValue] !*MySQLCursor -> (!(Maybe SQLError), !SQLStatement, !*MySQLCursor)
		mkStatement statement [] cursor
			# index			= markerIndex statement 0
			| index <> -1	= (Just (SQLProgrammingError 1 "Too many markers in SQL statement"), "", cursor)
							= (Nothing, statement, cursor)
		mkStatement statement [x:xs] cursor
			# index			= markerIndex statement 0
			| index == -1	= (Just (SQLProgrammingError 1 "Not enough markers in SQL statement"), "", cursor)
			# premarker							= statement % (0,index - 1)
			# postmarker						= statement % (index + 1, size statement - 1)
			# (x, cursor)						= formatSQLValue x cursor
			# (xs_error,xs_statement, cursor)	= mkStatement postmarker xs cursor
			= (xs_error, premarker +++ x +++ xs_statement, cursor)

		//Find the index of the first '?' in the string (-1 if not found)
		markerIndex :: String Int -> Int
		markerIndex s i
			| i == size s		= -1
			| select s i == '?'	= i
			| otherwise			= markerIndex s (i + 1)

		//Convert an SQLValue to a string which is properly escaped for inclusion in an SQL statement
		formatSQLValue :: !SQLValue !*MySQLCursor -> (!String, !*MySQLCursor)
		formatSQLValue (SQLVChar s) cursor 
			# (s, cursor) = escapeString s cursor
			= ("'" +++ s +++ "'", cursor)
		formatSQLValue (SQLVVarchar s) cursor 
			# (s, cursor) = escapeString s cursor
			= ("'" +++ s +++ "'", cursor)
		formatSQLValue (SQLVText s) cursor 
			# (s, cursor) = escapeString s cursor
			= ("'" +++ s +++ "'", cursor)
		formatSQLValue (SQLVInteger i) cursor = (toString i, cursor)
		formatSQLValue (SQLVReal r) cursor = (toString r, cursor)
		formatSQLValue (SQLVFloat f) cursor = (toString f, cursor)
		formatSQLValue (SQLVDouble d) cursor = (toString d, cursor)
		formatSQLValue (SQLVDate d) cursor = ("'" +++ toString d +++  "'", cursor) 
		formatSQLValue (SQLVTime t) cursor = ("'" +++ toString t +++ "'", cursor)
		formatSQLValue (SQLVTimestamp t) cursor = (toString t, cursor)
		formatSQLValue (SQLVDatetime d t) cursor = ("'" +++ toString d +++ " " +++ toString t +++ "'", cursor) 
		formatSQLValue (SQLVEnum s) cursor
			# (s, cursor) = escapeString s cursor
			= ("'" +++ s +++ "'", cursor)
		formatSQLValue SQLVNull cursor = ("NULL", cursor)
		formatSQLValue (SQLVBlob b) _ = abort "formatSQLValue for SQLVBlob not implemented\n"
		formatSQLValue (SQLVUnknown s) cursor 
			# (s, cursor) = escapeString s cursor
			= ("'" +++ s +++ "'", cursor)
		
		//The cursor argument is required because it allows the escape function to take
		//the character set of the database into account.
		escapeString :: !String !*MySQLCursor -> (!String, !*MySQLCursor)
		escapeString s cursor=:{MySQLCursor|conn_ptr}
			# buffer = createArray (2 * (size s) + 1 ) '\0'			//Create a buffer that is large enough to hold the escaped string
			# escaped_size = mysql_real_escape_string conn_ptr buffer s (size s)
			# escaped = buffer % (0, escaped_size - 1)				//Trim the buffer to the right size
			= (escaped, cursor)
		
	executeMany :: !SQLStatement ![[SQLValue]] !*MySQLCursor -> (!(Maybe SQLError), !*MySQLCursor)
	executeMany statement [] 	cursor = (Nothing, cursor)
	executeMany statement [x:xs] cursor
		# (error, cursor)	= execute statement x cursor
		| isJust error	= (error, cursor)
						= executeMany statement xs cursor

	insertId :: !*MySQLCursor -> (!(Maybe SQLError), !Int, !*MySQLCursor)
	insertId cursor=:{MySQLCursor|conn_ptr}
		= (Nothing, mysql_insert_id conn_ptr, cursor)
	
	numRows	:: !*MySQLCursor -> (!(Maybe SQLError), !Int, !*MySQLCursor)
	numRows	cursor=:{MySQLCursor|conn_ptr}
		= (Nothing, mysql_affected_rows conn_ptr, cursor) 
	
	numFields :: !*MySQLCursor -> (!(Maybe SQLError), !Int, !*MySQLCursor)
	numFields cursor=:{MySQLCursor|result_ptr}
		= (Nothing, mysql_num_fields result_ptr, cursor)

	fetchOne :: !*MySQLCursor -> (!(Maybe SQLError), !(Maybe SQLRow), !*MySQLCursor)
	fetchOne cursor=:{MySQLCursor|conn_ptr,result_ptr,result_size}
		| result_ptr == 0
			= (Just (SQLProgrammingError 1 "You cannot fetch a row when there is no result set") ,Nothing, cursor)
		# row_ptr	= mysql_fetch_row result_ptr
		| row_ptr == 0
			# errno = mysql_errno conn_ptr
			| errno == 0
				= (Nothing, Nothing, {MySQLCursor|cursor & row_ptr = 0})
			| otherwise
				# errmsg	= derefString (mysql_error conn_ptr)
				| errmsg <> errmsg	= undef //Force execution
				= (Just (SQLDatabaseError errno errmsg), Nothing, {MySQLCursor|cursor & row_ptr = 0})
		| otherwise
			# row_lengths		= mysql_fetch_lengths result_ptr	
			# (row, cursor)		= mapst readField [0..(result_size - 1)] {MySQLCursor|cursor & row_ptr = row_ptr, row_lengths = row_lengths}
			= (Nothing, Just row, cursor)
	where
		mapst :: (.a *st -> (.b, *st)) [.a] *st -> ([.b], *st)
		mapst f [] st = ([],st)
		mapst f [x:xs] st
			# (fx,st)	= f x st
			# (fxs,st)	= mapst f xs st
			= ([fx:fxs],st)
	
		readField :: !Int !*MySQLCursor -> (!SQLValue, !*MySQLCursor)
		readField n cursor=:{MySQLCursor|fields_ptr,row_ptr,row_lengths}
			# cell_ptr			= readInt row_ptr ((IF_INT_64_OR_32 8 4) * n)
			| cell_ptr	== 0
				= (SQLVNull, cursor)
			# cell_size			= readInt row_lengths ((IF_INT_64_OR_32 8 4) * n)
			# data				= {readChar cell_ptr i \\ i <- [0.. cell_size - 1]}
			# type				= readInt fields_ptr ((SIZEOF_MYSQL_FIELD * n) + MYSQL_FIELD_TYPE_OFFSET)
			# flags				= readInt fields_ptr ((SIZEOF_MYSQL_FIELD * n) + MYSQL_FIELD_FLAGS_OFFSET)
			| (flags bitand ENUM_FLAG) <> 0 
				= (SQLVEnum data, cursor)
			| otherwise
				= case type of
					MYSQL_TYPE_TINY			= (SQLVInteger (toInt data),cursor)
					MYSQL_TYPE_SHORT		= (SQLVInteger (toInt data),cursor)
					MYSQL_TYPE_LONG			= (SQLVInteger (toInt data),cursor)
					MYSQL_TYPE_INT24		= (SQLVInteger (toInt data),cursor)
					MYSQL_TYPE_LONGLONG		= (SQLVInteger (toInt data),cursor)
					MYSQL_TYPE_DECIMAL		= (SQLVReal (toReal data),cursor)
					MYSQL_TYPE_NEWDECIMAL	= (SQLVReal (toReal data),cursor)
					MYSQL_TYPE_FLOAT		= (SQLVFloat (toReal data),cursor)
					MYSQL_TYPE_DOUBLE		= (SQLVDouble (toReal data),cursor)
					MYSQL_TYPE_TIMESTAMP	= (SQLVTimestamp (toInt data),cursor)
					MYSQL_TYPE_DATE			= (SQLVDate {year = toInt (data % (0,3)), month = toInt (data % (5,6)), day = toInt (data % (8,9))},cursor)
					MYSQL_TYPE_TIME			= (SQLVTime {hour = toInt (data % (0,1)), minute = toInt (data % (3,4)), second = toInt (data % (6,7))},cursor)
					MYSQL_TYPE_DATETIME		= (SQLVDatetime {year = toInt (data % (0,3)), month = toInt (data % (5,6)), day = toInt (data % (8,9))}
															{hour = toInt (data % (11,12)), minute = toInt (data % (14,15)), second = toInt (data % (17,18))},cursor)
					MYSQL_TYPE_STRING		= (SQLVChar data,cursor)
					MYSQL_TYPE_VAR_STRING	= (SQLVVarchar data,cursor)
					MYSQL_TYPE_BLOB			= (SQLVText data,cursor)
					MYSQL_TYPE_ENUM			= (SQLVEnum data,cursor)
					_						= (SQLVUnknown data,cursor)
	
	fetchMany :: !Int !*MySQLCursor -> (!(Maybe SQLError), ![SQLRow], !*MySQLCursor)
	fetchMany 0 cursor = (Nothing, [], cursor)
	fetchMany n cursor 
		# (error, row, cursor) = fetchOne cursor
		= case row of	Nothing		= (Nothing, [], cursor)
						(Just x)
							# (error, xs, cursor) = fetchMany (n - 1) cursor
							= (error, [x:xs], cursor)

	fetchAll :: !*MySQLCursor -> (!(Maybe SQLError), ![SQLRow], !*MySQLCursor)
	fetchAll cursor 
		# (error, row, cursor) = fetchOne cursor
		= case row of	Nothing 	= (Nothing, [], cursor)
						(Just x)
							# (error, xs, cursor) = fetchAll cursor
							= (error, [x:xs], cursor)

	commit :: !*MySQLCursor -> (!(Maybe SQLError), !*MySQLCursor)
	commit cursor = (Just SQLNotSupportedError, cursor)

	rollback :: !*MySQLCursor -> (!(Maybe SQLError), !*MySQLCursor)
	rollback cursor = (Just SQLNotSupportedError, cursor)

instance SQLSchemaCursor MySQLCursor
where
    listTables :: !*MySQLCursor -> (!(Maybe SQLError), ![SQLTableName], !*MySQLCursor)
    listTables cur
        # (error, cur)      = execute listTablesStatement [] cur
        | isJust error      = (error,[],cur)
        # (error, res, cur) = fetchAll cur
        | isJust error      = (error,[],cur)
        = (Nothing, [table \\ [SQLVVarchar table:_] <- res],cur)

    describeTable :: !SQLTableName !*MySQLCursor -> (!(Maybe SQLError), !(Maybe SQLTable), !*MySQLCursor)
    describeTable name cur
        # (error, cur)      = execute (describeTableStatement name) [] cur
        | isJust error      = (error,Nothing,cur)
        # (error, res, cur) = fetchAll cur
        | isJust error      = (error,Nothing,cur)
        # (columns,primaryKey,foreignKeys) = foldr addColumn ([],[],[]) res
        = (Nothing,Just {SQLTable|name=name,columns=columns,primaryKey=primaryKey,foreignKeys=foreignKeys},cur)
    where
        addColumn [SQLVVarchar colName,SQLVText colType,SQLVVarchar nullYesNo,SQLVVarchar key,def,SQLVVarchar extra] (cols,pk,fks)
            # null = nullYesNo == "YES"
            # autoIncrement = indexOf "auto_increment" extra >= 0
            # cols  = [{SQLColumn|name=colName,type=columnTypeFromString colType,null=null,autoIncrement = autoIncrement}:cols]
            # pk    = if (key == "PRI") [colName:pk] pk
            = (cols,pk,fks)
        addColumn row (cols,pk,fks) = (cols,pk,fks)

    createTable :: !SQLTable !*MySQLCursor -> (!(Maybe SQLError), !*MySQLCursor)
    createTable table cur
        # (error, cur)      = execute (createTableStatement table) [] cur
        | isJust error      = (error,cur)
        = (Nothing,cur)

    deleteTable :: !SQLTableName !*MySQLCursor -> (!(Maybe SQLError), !*MySQLCursor)
    deleteTable name cur
        # (error, cur)      = execute (deleteTableStatement name) [] cur
        | isJust error      = (error,cur)
        = (Nothing,cur)

listTablesStatement :: SQLStatement
listTablesStatement = "SHOW TABLES"

describeTableStatement :: SQLTableName -> SQLStatement
describeTableStatement tablename = "DESCRIBE `" +++tablename +++ "`"

createTableStatement :: SQLTable -> SQLStatement
createTableStatement {SQLTable|name,columns,primaryKey,foreignKeys}
    = "CREATE TABLE `" +++ name +++ "` (" +++ join "," (colSQL ++ pkSQL ++ fkSQL) +++ ")"
where
    colSQL = [concat (["`",name,"` ",columnTypeToString type]
                ++ if autoIncrement [" auto_increment"] []
                ++ if null [] [" NOT NULL"])
              \\{SQLColumn|name,type,null,autoIncrement}<- columns]
    pkSQL = case primaryKey of
        [] = []
        pk = ["PRIMARY KEY ("+++ join "," ["`"+++col+++"`" \\col <- pk] +++")"]

    fkSQL = case foreignKeys of
        [] = []
        fk = ["FOREIGN KEY ("+++ join "," ["`"+++col+++"`" \\col <- fk_cols] +++
              ") REFERENCES `" +++ fk_table +++ "` ("+++ join "," ["`"+++col+++"`" \\col <- fk_refs] +++ ")"
             \\ (fk_cols,fk_table,fk_refs) <- fk]

deleteTableStatement :: SQLTableName -> SQLStatement
deleteTableStatement tablename = "DROP TABLE `" +++ tablename +++ "`"

columnTypeToString :: SQLColumnType -> String
columnTypeToString (SQLTText) = "TEXT"
columnTypeToString (SQLTInteger) = "INT"
columnTypeToString (SQLTReal) = "REAL"
columnTypeToString (SQLTDate) = "DATE"
columnTypeToString (SQLTTime) = "TIME"
columnTypeToString (SQLTDatetime) = "DATETIME"
columnTypeToString (SQLTEnum options) = "ENUM (" +++ join "," ["'"+++option+++"'" \\ option <- options] +++ ")"
columnTypeToString _ = "TEXT" //TODO Add all types

columnTypeFromString :: String -> SQLColumnType //TODO Add more column types
columnTypeFromString "text" = SQLTText
columnTypeFromString "date" = SQLTDate
columnTypeFromString "time" = SQLTTime
columnTypeFromString "datetime" = SQLTDatetime
columnTypeFromString s
    | startsWith "int(" s   = SQLTInteger
                            = SQLTUnknown

