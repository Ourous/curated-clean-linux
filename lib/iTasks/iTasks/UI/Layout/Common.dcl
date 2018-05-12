definition module iTasks.UI.Layout.Common
/**
* This module provides common alternative layout annotations
* that you can apply at strategic points in your task specifications
* to optimize the user experience for specific tasks
*/
import iTasks.UI.Layout
from iTasks.UI.Definition import :: UISide(..), :: UIDirection(..), :: UIWindowType(..), :: UIHAlign(..), :: UIVAlign(..)
from iTasks.UI.Prompt import :: Title, :: Label, :: Icon
from iTasks.UI.Tune import class tune
from iTasks.WF.Definition import :: Task

/**
* Create a tabset with all child items as separate tabs
* The flag denotes whether close buttons should be lifted to the tabs
*/
arrangeWithTabs :: Bool -> Layout

/**
* Extract one child item and put it in a separate panel at the side of the screen
*
* @param Index of the task in the set that should be put in the sidebar
* @param Location of the sidebar
* @param Initial size of the sidebar
* @param Enable resize?
*/
arrangeWithSideBar :: !Int !UISide !Int !Bool -> Layout

/**
 * Lift actions starting with / to the menu
 * @param The list of paths to menu separators
 */
arrangeAsMenu :: [[Int]] -> Layout

/**
* Divide the available screen space
*
* @param Direction to split the available space in
* @param Enable resize?
*/
arrangeSplit :: !UIDirection !Bool -> Layout

/**
*  Turn current UI into a panel and set direction to vertical.
*/
arrangeVertical :: Layout

/**
*  Turn current UI into a panel and set direction to vertical.
*/
arrangeHorizontal :: Layout

/**
* Turn the UI into a wrapping framed container inside a general container
* 
* Use this is if you don't want to use the entire viewport
*/
frameCompact :: Layout

/**
* Apply a layout only before a step has been made
*/
beforeStep :: Layout -> Layout


/**
* Add a tool bar and move selected actions to it
*/ 
insertToolBar :: [String] -> Layout

//Convenient annotatation types
:: ArrangeWithTabs = ArrangeWithTabs Bool
instance tune ArrangeWithTabs Task

:: ArrangeWithSideBar = ArrangeWithSideBar !Int !UISide !Int !Bool
instance tune ArrangeWithSideBar Task

:: ArrangeAsMenu = ArrangeAsMenu [[Int]]
instance tune ArrangeAsMenu Task

:: ArrangeSplit = ArrangeSplit !UIDirection !Bool
instance tune ArrangeSplit Task

:: ArrangeVertical = ArrangeVertical
instance tune ArrangeVertical Task

:: ArrangeHorizontal = ArrangeHorizontal
instance tune ArrangeHorizontal Task

//Changing container types

toContainer ::                                   Layout
toPanel     ::                                   Layout
toWindow    :: UIWindowType UIVAlign UIHAlign -> Layout
toEmpty     ::                                   Layout

:: ToWindow = ToWindow UIWindowType UIVAlign UIHAlign
InWindow                :== InFloatingWindow
InFloatingWindow        :== ToWindow FloatingWindow AlignMiddle AlignCenter
InNotificationBubble    :== ToWindow NotificationBubble AlignTop AlignRight
instance tune ToWindow Task

:: InPanel          = InPanel           //Indicate that a task should be wrapped in a panel
instance tune InPanel Task

:: InContainer      = InContainer       //Indicate that a task should be wrapped in a panel
instance tune InContainer Task

:: NoUserInterface  = NoUserInterface   //Replace the UI by an empty UI
instance tune NoUserInterface Task

actionToButton :: Layout

setActionIcon :: (Map String String) -> Layout

//Setting attributes 
instance tune Title Task
instance tune Label Task
instance tune Icon Task

/*
 * Format a basic editor as if it was a generic labelled iconized edtior
 */
toFormItem :: Layout
