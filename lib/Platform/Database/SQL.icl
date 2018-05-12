implementation module Database.SQL

import StdString, StdList, StdBool, Data.Maybe, Text

//Utility functions
instance toString SQLValue					
where
	toString (SQLVChar s)		= "SQLVChar " +++ s
	toString (SQLVVarchar s)	= "SQLVVarchar " +++ s
	toString (SQLVText s)		= "SQLVText " +++ s
	toString (SQLVInteger i)	= "SQLVInteger " +++ (toString i)
	toString (SQLVReal r)		= "SQLVReal " +++ (toString r)
	toString (SQLVFloat	f)		= "SQLVFloat " +++ (toString f)
	toString (SQLVDouble d)		= "SQLVDouble " +++ (toString d)
	toString (SQLVDate d)		= "SQLVDate " +++ (toString d)
	toString (SQLVTime t)		= "SQLVTime " +++ (toString t)
	toString (SQLVTimestamp	i)	= "SQLVTimestamp " +++ (toString i)
	toString (SQLVDatetime d t)	= "SQLVDatetime " +++ (toString d) +++ " " +++ (toString t)
	toString (SQLVEnum s)		= "SQLVEnum " +++ s
	toString (SQLVNull)			= "SQLVNull"
	toString (SQLVBlob s)		= "SQLVBlob " +++ s
	toString (SQLVUnknown s)	= "SQLVUnknown " +++ s

instance toString SQLDate
where
	toString {SQLDate|year,month,day} = lpad (toString year) 4 '0' +++ "-" +++ lpad (toString month) 2 '0' +++ "-" +++ lpad (toString day) 2 '0'

instance toString SQLTime
where
	toString {SQLTime|hour,minute,second} = lpad (toString hour) 2 '0' +++ ":" +++ lpad (toString minute) 2 '0' +++ ":" +++ lpad (toString second) 2 '0'

instance toString SQLError
where
	toString	(SQLWarning errno errmsg)			= "SQLWarning " +++ toString errno +++ ": " +++ errmsg
	toString	(SQLInterfaceError errno errmsg)	= "SQLInterfaceError " +++ toString errno +++ ": " +++ errmsg
	toString	(SQLDatabaseError errno errmsg)		= "SQLDatabaseError " +++ toString errno +++ ": " +++ errmsg
	toString	(SQLDataError errno errmsg)			= "SQLDataError " +++ toString errno +++ ": " +++ errmsg
	toString	(SQLOperationalError errno errmsg)	= "SQLOperationalError " +++ toString errno +++ ": " +++ errmsg
	toString	(SQLIntegrityError errno errmsg)	= "SQLIntegrityError " +++ toString errno +++ ": " +++ errmsg
	toString	(SQLInternalError errno errmsg)		= "SQLInternalError " +++ toString errno +++ ": " +++ errmsg
	toString	(SQLProgrammingError errno errmsg)	= "SQLProgrammingError " +++ toString errno +++ ": " +++ errmsg
	toString	(SQLNotSupportedError)				= "SQLNotSupportedError"

instance == SQLValue
where
	(==) (SQLVChar x)			(SQLVChar y)			= x == y
	(==) (SQLVVarchar x)		(SQLVVarchar y)			= x == y
	(==) (SQLVText x)			(SQLVText y)			= x == y
	(==) (SQLVInteger x)		(SQLVInteger y)			= x == y
	(==) (SQLVReal x)			(SQLVReal y)			= x == y
	(==) (SQLVFloat x)			(SQLVFloat y)			= x == y
	(==) (SQLVDouble x)			(SQLVDouble y)			= x == y
	(==) (SQLVDate x)			(SQLVDate y)			= x == y
	(==) (SQLVTime x)			(SQLVTime y)			= x == y
	(==) (SQLVTimestamp x)		(SQLVTimestamp y)		= x == y
	(==) (SQLVDatetime xd xt)	(SQLVDatetime yd yt)	= xd == yd && xt == yt
	(==) (SQLVEnum x)			(SQLVEnum y)			= x == y
	(==) (SQLVNull)				(SQLVNull)				= True
	(==) (SQLVBlob x)			(SQLVBlob y)			= x == y
	(==) (SQLVUnknown x)		(SQLVUnknown y)			= x == y
	(==) _						_						= False
	
instance == SQLDate
where
	(==) {SQLDate|year=xyear,month=xmonth,day=xday} {SQLDate|year=yyear,month=ymonth,day=yday}
		= xyear == yyear && xmonth == ymonth && xday == yday

instance == SQLTime
where
	(==) {SQLTime|hour=xhour,minute=xminute,second=xsecond} {SQLTime|hour=yhour,minute=yminute,second=ysecond}
		= xhour == yhour && xminute == yminute && xsecond == ysecond
