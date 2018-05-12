definition module iTasks.UI.Prompt
/**
* This module provides different ways to create prompts
*/
from iTasks.UI.Definition import :: UI

class toPrompt d :: !d -> UI

instance toPrompt ()                  //No prompt
instance toPrompt String              //Simple instruction
instance toPrompt (!String, !String)  //Title attribute + instruction

//Additional instances to create more complex prompts
:: Att				= E.a: Att !a & toPrompt a

:: Title			= Title !String
:: Label            = Label !String
:: Hint				= Hint !String
:: Icon				= Icon !String
					| IconView
					| IconEdit

instance toPrompt (!Icon, !String, !String)	//Icon attribute, title attribute, and instruction
instance toPrompt Title
instance toPrompt Label
instance toPrompt Hint
instance toPrompt Icon

instance toPrompt Att
instance toPrompt [d] | toPrompt d
