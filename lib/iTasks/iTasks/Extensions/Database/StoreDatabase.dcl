definition module iTasks.Extensions.Database.StoreDatabase
/**
* This module provides a simple database on top of the iTasks store
*
*/

import iTasks

//Convenience wrapper functions for databases with multiple values of type a 
class DB a where
	/*
	* Retrieve the database handle
	*/
	databaseId	:: Shared [a]
	/*
	* Retrieve the reference to a stored instance
	*/
	getItemId	:: !a -> DBRef a
	/*
	* Set the a reference on a specific instance
	*/
	setItemId	:: !(DBRef a) !a -> a
	
//Database identifier to a value of type a in a database with multiple values
:: DBRef a = DBRef !Int

instance == (DBRef a)
instance <  (DBRef a)
derive class iTask DBRef

/*
* Checks whether two instances have equal database-references
*
* @param The first instance
* @param The second instance
*
* @return Whether both items have the same database-handle
*/
eqItemId 		:: a a -> Bool | DB a

/*
* Reads all instances from a database
*
* @return All instances in the database
*/
dbReadAll		::                 Task [a]       | iTask, DB a
/*
* Replaces an entire database with new data
*
* @param The instances which need to be stored
*
* @return The instances which were stored
*/
dbWriteAll		:: ![a]         -> Task [a]       | iTask, DB a

//	C(reate)R(ead)U(pdate)D(elete) operations:

/*
* Stores an item in the database and creates a new reference for this
* specific instance.
*
* @param The item which needs to be stored
*
* @return The stored instance
*/
dbCreateItem	:: a            -> Task a         | iTask, DB a

/*
* Retrieve an instance give a reference, if it exists
*
* @param The reference to the stored instance
*
* @return The stored instance if it exists
*/
dbReadItem		:: !(DBRef a)	-> Task (Maybe a) | iTask, DB a

/*
* Update the value of a specific instance in the database
*
* @param The new value
*
* @return The stored instance
*/
dbUpdateItem	:: a			-> Task a         | iTask, DB a

/*
* Delete an instance from the database
*
* @param The reference to the stored instance
*
* @return The removed instance, if it existed
*/
dbDeleteItem	:: !(DBRef a)	-> Task (Maybe a) | iTask, DB a
