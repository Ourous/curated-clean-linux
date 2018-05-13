definition module Database.SQL.RelationalMapping
/**
* This module provides type-generic functions that map Clean values to a relational database.
* It provides mapping functions for the basic C(reate)R(ead)U(pdate)D(elete) operations.
*
* Although it uses a type-generic function it is defined only for a subset of all Clean types.
* It only has meaning for types that are specifically designed to represent parts of a relational database.
*
* For more information about defining such "representation types" see:
* http://www.baslijnse.nl/projects/between-types-and-tables/
*/

import StdGeneric, Data.Maybe
import Database.SQL 

// Wrapper functions which provide the basic mapping
mapRead		:: !ref !*cur -> (!(Maybe MappingError), !(Maybe val), !*cur)	| relMap{|*|} ref & relMap{|*|} val & SQLCursor cur & bimap{|*|} cur
mapCreate	:: !val !*cur -> (!(Maybe MappingError), !(Maybe ref), !*cur)	| relMap{|*|} ref & relMap{|*|} val & SQLCursor cur & bimap{|*|} cur
mapUpdate	:: !val !*cur -> (!(Maybe MappingError), !(Maybe ref), !*cur)	| relMap{|*|} ref & relMap{|*|} val & SQLCursor cur & bimap{|*|} cur
mapDelete	:: !ref !*cur -> (!(Maybe MappingError), !(Maybe val), !*cur)	| relMap{|*|} ref & relMap{|*|} val & SQLCursor cur & bimap{|*|} cur

// Errors
:: MappingError	= DatabaseError SQLError	//Something went wrong during interaction with the database
				| TypeError String			//The structure is not conform the constraints of "representation types"
				
instance toString MappingError

//----------------------------------------------------------------------------------------------------------------------------------------------
/*
* Everything below this line is only relevant if you want to define specializations of the generic mapping function.
* for normal use of the library you may ignore this machinery
*/


// The generic mapper can operate in six different modes
:: RelMapMode	= RelMapCreate									// Create a new representation in the database for a value
				| RelMapRead									// Read a representation from the database to the token stream
				| RelMapUpdate									// Update an existing representation in the database
				| RelMapDelete									// Remove a representation from the database
				| RelMapInfo									// Determine type structure information 
				| RelMapInit									// Serialize a value to the tokenstream

// Some operations have to be performed in multiple passes
:: RelMapPass :== Int

// The different types of tokens in the token stream that is
// used as buffer for serializing and deserializing values
:: RelMapToken	= RelMapValue SQLValue							//Plain value, index in the stream determines to which field it is mapped
				| RelMapTerminator								//Terminator token, indicates the end of a list of values
				| RelMapOverride String SQLValue				//Field override, if there is such a token in the stream for a given field,
																//its value is used instead of the 'normal' value in the stream
// Information about the structure of types
:: RelMapFieldInfo =	{ fld_table		:: String				//The database table in which this value is stored
						, fld_select	:: Maybe String			//The database field in which this value is stored
						, fld_match		:: Maybe String			//The database field on which can be matched to find the right database record
						, rec_table		:: String				//The database table of the key field of the parent record
						, rec_key		:: String				//The database field of the key field of the parent record
						, val_list		:: Bool					//Are dealing with one, or with a set of values
						, val_maybe		:: Bool					//Is the value optional
						, val_fields	:: [RelMapFieldInfo]	//Information about the fields if this value is a record
						, val_id		:: Bool					//Is the field an ID type or an entity record
						}

// *The* core generic function
// This function does all operations on a database. It can both read and write information
generic	relMap t ::
		!RelMapMode !RelMapPass !(Maybe t) ![RelMapFieldInfo] ![RelMapToken] !*cur	->
		(!(Maybe MappingError), !(Maybe t),![RelMapFieldInfo],![RelMapToken],!*cur)	| SQLCursor cur

// Instances for the standard data types
derive 		relMap Int, Real, Bool, Char, String, UNIT, PAIR, EITHER, CONS of d, FIELD of d, OBJECT, {}, {!}, Maybe, []
