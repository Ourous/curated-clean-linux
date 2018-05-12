implementation module iTasks.UI.Prompt

import StdOverloaded, StdString
import qualified Data.Map as DM
import Data.Maybe
import Text.GenJSON

import iTasks.UI.Definition
from StdFunc import o

instance toPrompt ()
where toPrompt _ = ui UIEmpty

instance toPrompt String
where toPrompt hint = createPrompt hint
	
instance toPrompt (!String,!String)
where toPrompt (title,hint) = let (UI type attr items) = createPrompt hint in
		(UI type ('DM'.union (titleAttr title) attr) items)

createPrompt :: String -> UI
createPrompt hint = (uiac UIContainer attr [stringDisplay hint])
where
	attr = 'DM'.unions [marginsAttr 5 5 10 5, widthAttr FlexSize, minWidthAttr WrapBound, heightAttr WrapSize]

instance toString Icon
where
	toString (Icon icon) = icon
	toString (IconView)	= "view"
	toString (IconEdit) = "edit"
	
instance toPrompt (!Icon,!String,!String)
where
	toPrompt (icon,title,hint) 
		# (UI type attr items) = stringDisplay hint
		# attr = 'DM'.put ICON_ATTRIBUTE (JSONString (toString icon)) attr
		# attr = 'DM'.put TITLE_ATTRIBUTE (JSONString title) attr
		= UI type attr items

instance toPrompt Title
where
	toPrompt (Title title) = uia UIEmpty ('DM'.fromList [(TITLE_ATTRIBUTE,JSONString title)])
	
instance toPrompt Label
where
	toPrompt (Label label) = uia UIEmpty ('DM'.fromList [(LABEL_ATTRIBUTE,JSONString label)])

instance toPrompt Hint
where
	toPrompt (Hint hint) = uia UIEmpty ('DM'.fromList [(HINT_ATTRIBUTE,JSONString hint)])
	
instance toPrompt Icon
where
	toPrompt icon = uia UIEmpty ('DM'.fromList [(ICON_ATTRIBUTE,JSONString (toString icon))])

instance toPrompt Att
where
	toPrompt (Att a) = toPrompt a
	
instance toPrompt [d] | toPrompt d
where
	toPrompt list = ui UIEmpty


