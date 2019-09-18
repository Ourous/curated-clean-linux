definition module iTasks.Extensions.Admin.UserAdmin
/**
* This extension provides workflows for managing the users of an iTask system.
*/
import iTasks
import iTasks.Extensions.User
from iTasks.Extensions.Document import :: Document

:: UserAccount			=
	{ credentials	:: !Credentials
	, title			:: !Maybe UserTitle
	, roles			:: ![Role]
	}

/**
 * A user account which can safely be stored, as it does not contains the password in cleartext.
 */
:: StoredUserAccount =
	{ credentials :: !StoredCredentials
	, title       :: !Maybe UserTitle
	, roles       :: ![Role]
	}

/**
 * Stored user credentials not containing the password in cleartext.
 */
:: StoredCredentials = { username           :: !Username //* The username.
                       , saltedPasswordHash :: !String   //* The salted SHA1 password hash.
                       , salt               :: !String   //* The 32-byte random salt.
                       }

derive class iTask UserAccount, StoredUserAccount, StoredCredentials

// Shares

//* All user accounts
userAccounts			::				SDSLens () [StoredUserAccount] [StoredUserAccount]

//* All users
users					:: 				SDSLens () [User] ()
//* Users with a specific role
usersWithRole			:: !Role ->		SDSLens () [User] ()

/**
* Authenticates a user by username and password
*
* @param Username: The username
* @param Password: The password
*
* @return A single user who matches the given credentials, or nothing of none or more than one exists.
*/
authenticateUser	:: !Username !Password	-> Task (Maybe User)

/**
* Wraps a task with an authentication task
*
* @param	the task to wrap
*/
doAuthenticated :: (Task a) -> Task a | iTask a


doAuthenticatedWith :: !(Credentials -> Task (Maybe User)) (Task a) -> Task a | iTask a

/**
* Add a new user
*
* @param User details: The user-information which needs to be stored
*
* @return The stored user
*/
createUser			:: !UserAccount -> Task StoredUserAccount
/**
* Delete an existing user
*
* @param User: The user who needs to be deleted
*
* @return The deleted user
*/
deleteUser			:: !UserId -> Task ()
/**
* Browse and manage the existing users
*/
manageUsers			:: Task ()

createUserFlow :: Task ()
updateUserFlow :: UserId -> Task StoredUserAccount
changePasswordFlow :: !UserId -> Task StoredUserAccount
deleteUserFlow :: UserId -> Task StoredUserAccount
importUserFileFlow :: Task ()
exportUserFileFlow :: Task Document

/**
* Create set of user names handy for giving demo's: alice, bob, carol, ...
*/
importDemoUsersFlow :: Task [UserAccount]
