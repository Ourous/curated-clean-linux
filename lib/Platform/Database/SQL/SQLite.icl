implementation module Database.SQL.SQLite
//SQLite implementation of the Clean SQL database API
//
import Database.SQL
import StdEnv, Data.Maybe, System._Pointer, Text
import Database.SQL._SQLite

//SQLite Does not really need a context
:: SQLiteContext :== Int
	
//A wrapper for access to sqlite3 structs
:: SQLiteConnection =
	{ conn_ptr		:: !Pointer
	}
//A wrapper to sqlite3 result sets
:: SQLiteCursor =
	{ conn_ptr		:: !Pointer
	, stmt_ptr	    :: !Pointer
    , step_res      :: !Int
    , num_cols      :: !Int
	}

instance SQLEnvironment World SQLiteContext
where
	//Dummy environment
	openContext :: !*World -> (!(Maybe SQLError),!(Maybe *SQLiteContext),!*World)
	openContext world = (Nothing, Just 42, world)

	closeContext :: !*SQLiteContext !*World -> (!(Maybe SQLError), !*World)
	closeContext context world = (Nothing, world)

instance SQLContext SQLiteContext SQLiteConnection
where
	openConnection	:: !SQLDatabase !*SQLiteContext -> (!(Maybe SQLError),!(Maybe *SQLiteConnection),!*SQLiteContext)
	openConnection {SQLDatabase|host,username,password,database} context
		//Initialize a handle
		# (rc,conn_ptr) = sqlite3_open (packString database)
        | rc <> SQLITE_OK
			# errno 	= sqlite3_errcode conn_ptr
			# errmsg	= derefString (sqlite3_errmsg conn_ptr)
			| errno <> errno || errmsg <> errmsg	= undef //Force eval
            = (Just (SQLDatabaseError errno errmsg), Nothing, context)
		= (Nothing, Just {SQLiteConnection|conn_ptr = conn_ptr}, context)
		
	closeConnection	:: !*SQLiteConnection !*SQLiteContext -> (!(Maybe SQLError), !*SQLiteContext)
	closeConnection connection=:{SQLiteConnection|conn_ptr} context
        # rc = sqlite3_close conn_ptr
        | rc <> SQLITE_OK
			# errno 	= sqlite3_errcode conn_ptr
			# errmsg	= derefString (sqlite3_errmsg conn_ptr)
			| errno <> errno || errmsg <> errmsg	= undef //Force eval
            = (Just (SQLDatabaseError errno errmsg), context)
		= (Nothing, context)

instance SQLConnection SQLiteConnection SQLiteCursor
where
	openCursor :: !*SQLiteConnection -> (!(Maybe SQLError), !(Maybe *SQLiteCursor), !*SQLiteConnection)
	openCursor connection=:{SQLiteConnection|conn_ptr}
		# cursor = {SQLiteCursor
					| conn_ptr		= conn_ptr
					, stmt_ptr	    = 0
                    , step_res      = 0
                    , num_cols      = 0
					}
		= (Nothing, Just cursor, connection)
	
	closeCursor	:: !*SQLiteCursor !*SQLiteConnection -> (!(Maybe SQLError), !*SQLiteConnection)
	closeCursor cursor=:{SQLiteCursor|stmt_ptr} connection
		| stmt_ptr == 0
			= (Nothing, connection)
		# rc = sqlite3_finalize stmt_ptr
		| rc <> rc = undef	// Force eval
		= (Nothing,connection)

instance SQLCursor SQLiteCursor
where
	execute	:: !SQLStatement ![SQLValue] !*SQLiteCursor -> (!(Maybe SQLError), !*SQLiteCursor)
	execute statement values cursor=:{SQLiteCursor|conn_ptr,stmt_ptr}
        //Free finalize previous query
		# rc                        = if (stmt_ptr <> 0) (sqlite3_finalize stmt_ptr) 0
		| rc <> rc = undef	// Force eval
        //Create statement
        # (rc,stmt_ptr,_)           = sqlite3_prepare conn_ptr statement (size statement)
        | rc <> SQLITE_OK
			# errno 	= sqlite3_errcode conn_ptr
			# errmsg	= derefString (sqlite3_errmsg conn_ptr)
			| errno <> errno || errmsg <> errmsg	= undef //Force eval
            = (Just (SQLDatabaseError errno errmsg), {SQLiteCursor|cursor & stmt_ptr = 0})
        //Get column count
        # num_cols = sqlite3_column_count stmt_ptr
		| num_cols <> num_cols = undef	// Force eval
        //Bind parameters
        # rc = bind_parameters stmt_ptr 1 values
        | rc <> SQLITE_OK
			# errno 	= sqlite3_errcode conn_ptr
			# errmsg	= derefString (sqlite3_errmsg conn_ptr)
			| errno <> errno || errmsg <> errmsg	= undef //Force eval
            = (Just (SQLDatabaseError errno errmsg), {SQLiteCursor|cursor & stmt_ptr = 0})
        //Step once to actually start executing the query
        # rc                        = sqlite3_step stmt_ptr
        | rc == SQLITE_ERROR
			# errno 	= sqlite3_errcode conn_ptr
			# errmsg	= derefString (sqlite3_errmsg conn_ptr)
			| errno <> errno || errmsg <> errmsg	= undef //Force eval
            = (Just (SQLDatabaseError errno errmsg), {SQLiteCursor|cursor & stmt_ptr = 0})
        = (Nothing, {SQLiteCursor|cursor & stmt_ptr = stmt_ptr, step_res = rc, num_cols = num_cols})
    where
        bind_parameters stmt_ptr i [] = SQLITE_OK
        bind_parameters stmt_ptr i [v:vs]
            # rc = case v of
                (SQLVChar x)        = sqlite3_bind_text stmt_ptr i x (size x) SQLITE_TRANSIENT
                (SQLVVarchar x)     = sqlite3_bind_text stmt_ptr i x (size x) SQLITE_TRANSIENT
                (SQLVText x)        = sqlite3_bind_text stmt_ptr i x (size x) SQLITE_TRANSIENT
                (SQLVInteger x)     = sqlite3_bind_int64 stmt_ptr i x
                (SQLVReal x)        = sqlite3_bind_double stmt_ptr i x
                (SQLVFloat x)       = sqlite3_bind_double stmt_ptr i x
                (SQLVDouble x)      = sqlite3_bind_double stmt_ptr i x
                (SQLVDate d)
                    # x = toString d
                    = sqlite3_bind_text stmt_ptr i x (size x) SQLITE_TRANSIENT
                (SQLVTime t)
                    # x = toString t
                    = sqlite3_bind_text stmt_ptr i x (size x) SQLITE_TRANSIENT
                (SQLVTimestamp x)   = sqlite3_bind_int64 stmt_ptr i x
                (SQLVDatetime d t)
                    # x = toString d +++ " " +++ toString t
                    = sqlite3_bind_text stmt_ptr i x (size x) SQLITE_TRANSIENT
                (SQLVEnum x)        = sqlite3_bind_text stmt_ptr i x (size x) SQLITE_TRANSIENT
                (SQLVNull)          = sqlite3_bind_null stmt_ptr i
                (SQLVBlob x)        = sqlite3_bind_blob stmt_ptr i x (size x) SQLITE_TRANSIENT
                (SQLVUnknown x)     = sqlite3_bind_blob stmt_ptr i x (size x) SQLITE_TRANSIENT
            | rc == SQLITE_OK   = bind_parameters stmt_ptr (i + 1) vs
                                = rc

	executeMany :: !SQLStatement ![[SQLValue]] !*SQLiteCursor -> (!(Maybe SQLError), !*SQLiteCursor)
	executeMany statement [] 	cursor = (Nothing, cursor)
	executeMany statement [x:xs] cursor
		# (error, cursor)	= execute statement x cursor
		| isJust error	= (error, cursor)
						= executeMany statement xs cursor

	insertId :: !*SQLiteCursor -> (!(Maybe SQLError), !Int, !*SQLiteCursor)
	insertId cursor=:{SQLiteCursor|conn_ptr}
        # insertId = sqlite3_last_insert_rowid conn_ptr
        | insertId <> insertId = undef //Force eval
        = (Nothing, insertId, cursor)
	
	numRows	:: !*SQLiteCursor -> (!(Maybe SQLError), !Int, !*SQLiteCursor)
	numRows	cursor=:{SQLiteCursor|conn_ptr} //TODO: This now does not count for select queries
        # num = sqlite3_changes conn_ptr
        | num <> num = undef //Force eval
        = (Nothing, num, cursor)
	
	numFields :: !*SQLiteCursor -> (!(Maybe SQLError), !Int, !*SQLiteCursor)
	numFields cursor=:{SQLiteCursor|stmt_ptr,num_cols}
        = (Nothing, num_cols, cursor)

	fetchOne :: !*SQLiteCursor -> (!(Maybe SQLError), !(Maybe SQLRow), !*SQLiteCursor)
	fetchOne cursor=:{SQLiteCursor|step_res=SQLITE_DONE}
        = (Nothing, Nothing, cursor)
    fetchOne cursor=:{SQLiteCursor|conn_ptr,stmt_ptr,step_res=SQLITE_ROW,num_cols}
        //Fetch rows
        # (row,cursor)  = foldr readField ([],cursor) [0..(num_cols - 1)]
        | length row < 0 = undef //Force eval
        //Step to next row
        # rc            = sqlite3_step stmt_ptr
        | rc == SQLITE_ERROR
			# errno 	= sqlite3_errcode conn_ptr
			# errmsg	= derefString (sqlite3_errmsg conn_ptr)
			| errno <> errno || errmsg <> errmsg	= undef //Force eval
            = (Just (SQLDatabaseError errno errmsg),Nothing, cursor)
        = (Nothing, Just row, {cursor & step_res = rc})
    where
		readField :: !Int (![SQLValue],!*SQLiteCursor) -> (![SQLValue], !*SQLiteCursor)
        readField i (row,cursor=:{SQLiteCursor|stmt_ptr})
            # type = sqlite3_column_type stmt_ptr i
            | type == SQLITE_INTEGER
                # val = sqlite3_column_int stmt_ptr i
                | val <> val = undef //Force eval
                = ([SQLVInteger val:row],cursor)
            | type == SQLITE_FLOAT
                # val = toReal (derefString (sqlite3_column_text stmt_ptr i))
                | val <> val = undef //Force eval
                = ([SQLVFloat val:row],cursor)
            | type == SQLITE_TEXT
                # val = derefString (sqlite3_column_text stmt_ptr i)
                | size val < 0 = undef //Force eval
                = ([SQLVText val:row],cursor)
            | type == SQLITE_BLOB
                # val = derefString (sqlite3_column_text stmt_ptr i)
                | size val < 0 = undef //Force eval
                = ([SQLVText val:row],cursor)
            | type == SQLITE_NULL
                = ([SQLVNull:row],cursor)
			| otherwise
				= abort ("unknown type " +++ toString type +++ " in readField\n")
    fetchOne cursor
	    = (Just (SQLProgrammingError 1 "You cannot fetch a row when there is no result set") ,Nothing, cursor)

	fetchMany :: !Int !*SQLiteCursor -> (!(Maybe SQLError), ![SQLRow], !*SQLiteCursor)
	fetchMany 0 cursor = (Nothing, [], cursor)
	fetchMany n cursor
		# (error, row, cursor) = fetchOne cursor
		= case row of	Nothing		= (Nothing, [], cursor)
						(Just x)
							# (error, xs, cursor) = fetchMany (n - 1) cursor
							= (error, [x:xs], cursor)

	fetchAll :: !*SQLiteCursor -> (!(Maybe SQLError), ![SQLRow], !*SQLiteCursor)
	fetchAll cursor
		# (error, row, cursor) = fetchOne cursor
		= case row of	Nothing 	= (Nothing, [], cursor)
						(Just x)
							# (error, xs, cursor) = fetchAll cursor
							= (error, [x:xs], cursor)

	commit :: !*SQLiteCursor -> (!(Maybe SQLError), !*SQLiteCursor)
	commit cursor = (Just SQLNotSupportedError, cursor)

	rollback :: !*SQLiteCursor -> (!(Maybe SQLError), !*SQLiteCursor)
	rollback cursor = (Just SQLNotSupportedError, cursor)

instance SQLSchemaCursor SQLiteCursor
where
    listTables :: !*SQLiteCursor -> (!(Maybe SQLError), ![SQLTableName], !*SQLiteCursor)
    listTables cur
        # (error, cur)      = execute listTablesStatement [] cur
        | isJust error      = (error,[],cur)
        # (error, res, cur) = fetchAll cur
        | isJust error      = (error,[],cur)
        = (Nothing, [table \\ [SQLVText table:_] <- res],cur)

    describeTable :: !SQLTableName !*SQLiteCursor -> (!(Maybe SQLError), !(Maybe SQLTable), !*SQLiteCursor)
    describeTable name cur
        # (error, cur)      = execute (describeTableStatement name) [] cur
        | isJust error      = (error,Nothing,cur)
        # (error, res, cur) = fetchAll cur
        | isJust error      = (error,Nothing,cur)
        # (columns,primaryKey,foreignKeys) = foldr addColumn ([],[],[]) res
        = (Nothing,Just {SQLTable|name=name,columns=columns,primaryKey=primaryKey,foreignKeys=foreignKeys},cur)
    where
        addColumn [_,SQLVText colName,SQLVText colType,SQLVInteger nullYesNo,_,SQLVInteger keyYesNo] (cols,pk,fks)
            # null = nullYesNo == 1
            # cols  = [{SQLColumn|name=colName,type=columnTypeFromString colType,null=null,autoIncrement = False}:cols]
            # pk    = if (keyYesNo == 1) [colName:pk] pk
            = (cols,pk,fks)
        addColumn row (cols,pk,fks) = (cols,pk,fks)

    createTable :: !SQLTable !*SQLiteCursor -> (!(Maybe SQLError), !*SQLiteCursor)
    createTable table cur
        # (error, cur)      = execute (createTableStatement table) [] cur
        | isJust error      = (error,cur)
        = (Nothing,cur)

    deleteTable :: !SQLTableName !*SQLiteCursor -> (!(Maybe SQLError), !*SQLiteCursor)
    deleteTable name cur
        # (error, cur)      = execute (deleteTableStatement name) [] cur
        | isJust error      = (error,cur)
        = (Nothing,cur)

listTablesStatement :: SQLStatement
listTablesStatement = "SELECT name FROM sqlite_master WHERE type = 'table'"

describeTableStatement :: SQLTableName -> SQLStatement
describeTableStatement tablename = "PRAGMA table_info(" +++tablename +++ ")"

createTableStatement :: SQLTable -> SQLStatement
createTableStatement {SQLTable|name,columns,primaryKey,foreignKeys}
    = "CREATE TABLE `" +++ name +++ "` (" +++ join "," (colSQL  ++ pkSQL ++ fkSQL) +++ ")"
where
    colSQL = [concat (["`",name,"` ",columnTypeToString type] ++ if null [] [" NOT NULL"])
             \\ {SQLColumn|name,type,null} <- columns]
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
columnTypeToString (SQLTChar _) = "TEXT"
columnTypeToString (SQLTVarchar _) = "TEXT"
columnTypeToString (SQLTText) = "TEXT"
columnTypeToString (SQLTInteger) = "INTEGER"
columnTypeToString (SQLTReal) = "REAL"
columnTypeToString (SQLTFloat) = "REAL"
columnTypeToString (SQLTDouble) = "REAL"
columnTypeToString (SQLTDate) = "NUMERIC"
columnTypeToString (SQLTTime) = "NUMERIC"
columnTypeToString (SQLTTimestamp) = "INTEGER"
columnTypeToString (SQLTDatetime) = "NUMERIC"
columnTypeToString (SQLTEnum _) = "TEXT"
columnTypeToString (SQLTBlob) = "NONE"
columnTypeToString (SQLTUnknown) = "TEXT"

columnTypeFromString :: String -> SQLColumnType //TODO Add more column types
columnTypeFromString "TEXT"     = SQLTText
columnTypeFromString "INTEGER"  = SQLTInteger
columnTypeFromString "REAL"     = SQLTReal
columnTypeFromString _          = SQLTUnknown
