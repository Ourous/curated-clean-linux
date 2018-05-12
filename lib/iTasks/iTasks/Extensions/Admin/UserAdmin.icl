implementation module iTasks.Extensions.Admin.UserAdmin

import iTasks
import iTasks.Extensions.CSVFile
import iTasks.UI.Editor
import Text, Data.Tuple, StdArray, Data.Maybe

derive class iTask UserAccount

//Initial root user
ROOT_USER :== {credentials={Credentials|username=Username "root",password = Password "root"},title = Just "Root user", roles = ["admin","manager"]}

userAccounts :: Shared [UserAccount]
userAccounts = sharedStore "UserAccounts" [ROOT_USER]

users :: ReadOnlyShared [User]
users = mapReadWrite (\accounts -> [AuthenticatedUser (toString a.UserAccount.credentials.Credentials.username) a.UserAccount.roles a.UserAccount.title
									\\ a <- accounts]
					 , \() accounts -> Nothing) userAccounts

usersWithRole :: !Role -> ReadOnlyShared [User]
usersWithRole role = mapRead (filter (hasRole role)) users
where
	hasRole role (AuthenticatedUser _ roles _) = isMember role roles
	hasRole _ _ = False

userAccount :: UserId -> Shared (Maybe UserAccount)
userAccount userId = mapReadWrite (getAccount userId, \w r -> Just (setAccount w r)) userAccounts
where
	getAccount :: UserId [UserAccount] -> Maybe UserAccount
	getAccount userId accounts = case [a \\ a <- accounts | identifyUserAccount a == userId] of
		[a] = Just a
		_	= Nothing
		
	setAccount :: (Maybe UserAccount) [UserAccount] -> [UserAccount]
	setAccount Nothing accounts = accounts
	setAccount (Just updated) accounts = [if (identifyUserAccount a == identifyUserAccount updated) updated a \\ a <- accounts]

identifyUserAccount :: UserAccount -> UserId
identifyUserAccount {UserAccount|credentials={Credentials|username}} = toString username

accountToUser :: UserAccount -> User
accountToUser {UserAccount|credentials={Credentials|username},title,roles} = AuthenticatedUser (toString username) roles title

accountTitle :: UserAccount -> String
accountTitle {UserAccount|credentials={Credentials|username},title=Just title} = title  
accountTitle {UserAccount|credentials={Credentials|username}} = "Untitled (" +++ toString username +++ ")" 

authenticateUser :: !Username !Password	-> Task (Maybe User)
authenticateUser (Username username) password
	=	get (userAccount username)
	@	(maybe Nothing (\a -> if (a.UserAccount.credentials.Credentials.password == password) (Just (accountToUser a)) Nothing))
	
doAuthenticated :: (Task a) -> Task a | iTask a
doAuthenticated task = doAuthenticatedWith verify task
where
	verify {Credentials|username,password} = authenticateUser username password
	
doAuthenticatedWith :: !(Credentials -> Task (Maybe User)) (Task a) -> Task a | iTask a
doAuthenticatedWith verifyCredentials task
	=	enterInformation ("Log in","Please enter your credentials") []
	>>!	verifyCredentials
	>>= \mbUser -> case mbUser of
		Nothing		= throw "Authentication failed"
		Just user	= workAs user task
	
createUser :: !UserAccount -> Task UserAccount
createUser account
	=	get (userAccount (identifyUserAccount account))
	>>= \mbExisting -> case mbExisting of
		Nothing
			= upd (\accounts -> accounts ++ [account]) userAccounts @ const account
		_	
			= throw ("A user with username '" +++ toString account.UserAccount.credentials.Credentials.username +++ "' already exists.")

deleteUser :: !UserId -> Task ()
deleteUser userId = upd (filter (\acc -> identifyUserAccount acc <> userId)) userAccounts @! ()


manageUsers :: Task ()
manageUsers =
	(		enterChoiceWithSharedAs ("Users","The following users are available") [ChooseFromGrid id] userAccounts identifyUserAccount
		>>*	[ OnAction		(Action "New")									(always (createUserFlow	@ const False))
			, OnAction 	    (ActionEdit) 						                (hasValue (\u -> updateUserFlow u @ const False))
			, OnAction      (ActionDelete) 		            					(hasValue (\u -> deleteUserFlow u @ const False))
			, OnAction      (Action "Import & export/Import CSV file...")	(always (importUserFileFlow @ const False))
			, OnAction      (Action "Import & export/Export CSV file...")	(always (exportUserFileFlow @ const False))
			, OnAction      (Action "Import & export/Import demo users")		(always (importDemoUsersFlow @ const False))
			]
	) <! id @! ()

createUserFlow :: Task ()
createUserFlow =
		enterInformation ("Create user","Enter user information") []
	>>*	[ OnAction		ActionCancel	(always (return ()))
		, OnAction	    ActionOk 		(hasValue (\user ->
											createUser user
										>>|	viewInformation "User created" [] "Successfully added new user"
										>>| return ()
									    ))
		]
		
updateUserFlow :: UserId -> Task UserAccount
updateUserFlow userId
	=	get (userAccount userId)
	>>= \mbAccount -> case mbAccount of 
		(Just account)
			=	(updateInformation ("Editing " +++ fromMaybe "Untitled" account.UserAccount.title ,"Please make your changes") [] account
			>>*	[ OnAction ActionCancel (always (return account))
				, OnAction ActionOk (hasValue (\newAccount ->
												set (Just newAccount) (userAccount userId)
											>>=	viewInformation "User updated" [ViewAs (\(Just {UserAccount|title}) -> "Successfully updated " +++ fromMaybe "Untitled" title)]
											>>| return newAccount
											))
				])
		Nothing
			=	(throw "Could not find user details")
				
deleteUserFlow :: UserId -> Task UserAccount
deleteUserFlow userId
	=	get (userAccount userId)
	>>= \mbAccount -> case mbAccount of 
		(Just account)
			=	viewInformation "Delete user" [] ("Are you sure you want to delete " +++ accountTitle account +++ "? This cannot be undone.")
			>>*	[ OnAction ActionNo	(always (return account))
				, OnAction ActionYes (always (deleteUser userId
									>>|	viewInformation "User deleted" [ViewAs (\account -> "Successfully deleted " +++ accountTitle account +++ ".")] account
									>>| return account
									))
				]
				
importUserFileFlow :: Task ()
importUserFileFlow = viewInformation "Not implemented" [] ()

exportUserFileFlow :: Task Document
exportUserFileFlow
	=	get userAccounts -&&- get applicationName
	>>= \(list,app) ->
		createCSVFile (app +++ "-users.csv") (map toRow list)
	>>=	viewInformation ("Export users file","A CSV file containing the users of this application has been created for you to download.") []
where
	toRow {credentials = {Credentials|username =(Username username), password = (Password password)}, title, roles}
		= [fromMaybe "" title,username,password:roles]
	
importDemoUsersFlow :: Task [UserAccount]
importDemoUsersFlow =
	allTasks [catchAll (createUser (demoUser n)) (\_ -> return (demoUser n)) \\ n <- names]
where
	demoUser name
		= {UserAccount
		  | credentials = {Credentials| username = Username (toLowerCase name), password = Password (toLowerCase name)}
		  , title = Just name
		  , roles = ["manager"]
		  }
	names = ["Alice","Bob","Carol","Dave","Eve","Fred"]
