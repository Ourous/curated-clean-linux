definition module iTasks.Extensions.Admin.UserAdmin
/**
* This extension provides workflows for managing the users of an iTask system.
*/
import iTasks
import iTasks.Extensions.User

:: UserAccount			=
	{ credentials	:: !Credentials
	, title			:: !Maybe UserTitle
	, roles			:: ![Role]
	}

derive class iTask UserAccount

// Shares

//* All user accounts
userAccounts			::				Shared [UserAccount]

//* All users
users					:: 				ReadOnlyShared [User]
//* Users with a specific role
usersWithRole			:: !Role ->		ReadOnlyShared [User]

/**
* Authenticates a user by username and password
*
* @param Username: The username
* @param Password: The password
*
* @return A single user who matches the given credentials, or nothing of none or more than one exists.

* @gin-icon key
*/
authenticateUser	:: !Username !Password	-> Task (Maybe User)

/**
* Wraps a task with an authentication task
*
* @param	the task to wrap
*
* @gin-icon key
*/
doAuthenticated :: (Task a) -> Task a | iTask a


doAuthenticatedWith :: !(Credentials -> Task (Maybe User)) (Task a) -> Task a | iTask a

/**
* Add a new user
*
* @param User details: The user-information which needs to be stored
*
* @return The stored user
* 
* @gin-icon user_add
*/
createUser			:: !UserAccount -> Task UserAccount
/**
* Delete an existing user
*
* @param User: The user who needs to be deleted
*
* @return The deleted user
* 
* @gin-icon user_delete
*/
deleteUser			:: !UserId -> Task ()
/**
* Browse and manage the existing users
*/
manageUsers			:: Task ()
/**
* Create set of user names handy for giving demo's: alice, bob, carol, ...
*/
importDemoUsersFlow :: Task [UserAccount]





