definition module iTasks.Extensions.Admin.WorkflowAdmin
/**
* This extension provides a framework for managing a collection of tasks
* in 'workflow management' style with multiple users, persistent re-assignable tasks
* and a dedicated work list for each user
*/
import iTasks

// A workflow specification
:: Workflow	=
	{ path				:: String					//* a unique name of this workflow
	, roles				:: [String]					//* the roles that are allowed to initate this workflow
	, description		:: String					//* a description of the workflow
	, transient			:: Bool						//* this workflow is intended only as part of a session, it is not persistent
	, managerProperties	:: TaskAttributes           //* the initial manager properties of the main task
	, task				:: WorkflowTaskContainer	//* the thread of the main task of the workflow
	}						
:: WorkflowTaskContainer
	= E.a:		WorkflowTask		(Task a)		& iTask a
	| E.a b:	ParamWorkflowTask	(a -> (Task b))	& iTask a & iTask b

:: WorklistRow =
    { taskNr	 :: Maybe String
    , title		 :: Maybe String
	, priority	 :: Maybe String
	, createdBy	 :: Maybe String
	, date		 :: Maybe String
	, deadline	 :: Maybe String
	, createdFor :: Maybe String
	, parentTask :: Maybe String
	}

// Make the management framework startable 
:: WorkflowCollection = 
	{ name :: !String
	, workflows :: ![Workflow]
	}

instance Startable WorkflowCollection

derive class iTask Workflow, WorklistRow
		
derive gText	        WorkflowTaskContainer
derive gEditor			WorkflowTaskContainer
derive JSONEncode		WorkflowTaskContainer
derive JSONDecode		WorkflowTaskContainer
derive gDefault			WorkflowTaskContainer
derive gEq				WorkflowTaskContainer

// Available workflows
:: WorkflowFolderLabel :== String

workflows :: SDSLens () [Workflow] [Workflow]
workflowByPath :: !String -> SDSLens () Workflow Workflow
/**
* Wraps any task as a workflow with no access restrictions
*
* @param A label for the workflow. This may contain slashes to group workflows
* @param A description of the workflow
* @param The task(container) (with or without parameter)
*/
workflow :: String String w -> Workflow | toWorkflow w
/*
* Wraps any task as a transient workflow with no access restrictions
* users will be able to do this flow embedded in a session, but can't add it
* as a persisent workflow.
*/
transientWorkflow :: String String w -> Workflow | toWorkflow w
/**
*
* Wraps any task as a workflow that is only available to specified roles
*
* @param A label for the workflow. This may contain slashes to group workflows
* @param A description of the workflow
* @param A list of roles. The workflow will be available to users with any of the specified roles
* @param The task(container) (with or without parameter)
*/
restrictedWorkflow :: String String [Role] w -> Workflow | toWorkflow w
restrictedTransientWorkflow :: String String [Role] w -> Workflow | toWorkflow w

class toWorkflow w :: String String [Role] Bool !w -> Workflow

instance toWorkflow (Task a)						| iTask a
instance toWorkflow (WorkflowContainer a)			| iTask a
instance toWorkflow (a -> Task b)					| iTask a & iTask b
instance toWorkflow (ParamWorkflowContainer a b)	| iTask a & iTask b

:: WorkflowContainer a			= Workflow		TaskAttributes (Task a)
:: ParamWorkflowContainer a b	= ParamWorkflow	TaskAttributes (a -> Task b)

/**
* Default workflow management tasks.
* These task allows users to manage a catalogue of task definitions
* and let's them create instances of these tasks and work on instances.
*/
installWorkflows :: ![Workflow] -> Task ()
loginAndManageWork :: !String -> Task ()
manageWorkOfCurrentUser :: Task ()

/**
* Dynamically adds a workflow to the system.
*
* @param Workflow: The workflow to add
* @return The description of the added workflow
* 
* @gin False
*/
addWorkflows :: ![Workflow] -> Task [Workflow]

isAllowedWorkflow :: !User !Workflow -> Bool

appendOnce :: TaskId (Task a) (SharedTaskList a) -> Task () | iTask a
