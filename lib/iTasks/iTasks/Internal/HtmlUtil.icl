implementation module iTasks.Internal.HtmlUtil

from Data.Map import :: Map
import qualified Data.Map as DM
import Text.HTML, Text.GenJSON, Text, Internet.HTTP, System.OS
import StdList, StdBool

embeddedStyle :: HtmlTag
embeddedStyle = StyleTag [TypeAttr "text/css"] [Html css] 
where
 css = 	"body { background: #fff; font-family: Verdana, Arial, sans-serif; font-size: 12px;} th { text-align: left; } "
	+++ ".field-error em {color: #f00; font-weight: bold} .field-error input {border-color: #f00;} "
	+++ "#main {margin: 20px; background: #d1dded; border: solid 2px #3a81ad; -moz-border-radius: 5px; background: -moz-linear-gradient(bottom,  #d1dded,  #fff);} "
	+++ "#content { padding: 10px; } "
	+++ ".buttons { padding: 5px; background-color: #3a81ad; } "
	+++ ".section { margin: 10px; padding: 5px; overflow: auto;} "
	+++ ".description { margin: 0px 15px 0px 15px; } "
	+++ ".parameters th, { width: 150px; } "
	+++ ".json { font-family: Courier, monotype; font-size: 12px;} "
	+++ ".json ul { padding-left: 15px;} "
	+++ "h1 { margin: 10px 15px 10px 15px; font-weight: normal; font-size: 24px;} "
	+++ "h2 { margin: 5px 5px 5px 0px; font-weight: bold; font-size: 14px;  border: solid #999; border-width: 0px 0px 1px 0px;} "
	+++ "p { margin: 0px 0px 10px 0px; } "
	+++ "button {-moz-border-radius: 3px; }"
	
pageLayout :: !String !String ![HtmlTag] -> HtmlTag
pageLayout title description content = HtmlTag [] [head,body]
where
	head = HeadTag [] [TitleTag [] [Text title], embeddedStyle]
	body = BodyTag [] [DivTag [IdAttr "main"] (header ++ content)]
	
	header = [H1Tag [] [Text title],PTag [] [DivTag [ClassAttr "description"] [Html description]]]

notFoundPage :: !HTTPRequest -> HtmlTag
notFoundPage req = pageLayout "404 - Not Found" "" message
where
	message = [DivTag [IdAttr "content"] [Text "The resource you tried to access ",StrongTag [] [Text req.HTTPRequest.req_path], Text " could not be found."]] 

notFoundResponse :: !HTTPRequest -> HTTPResponse
notFoundResponse req
	= {notfoundResponse & rsp_data = toString (notFoundPage req)}

paramValue :: !String !HTTPRequest -> String
paramValue name req
	= case 'DM'.get name req.arg_post of
		Just val	= val
		Nothing		= case 'DM'.get name req.arg_get of
			Just val	= val
			Nothing		= ""

hasParam :: !String !HTTPRequest -> Bool
hasParam name req = isJust ('DM'.get name req.arg_post) || isJust ('DM'.get name req.arg_get)

nl2br :: !String -> HtmlTag
nl2br str = html [[Text line,BrTag []] \\ line <- split OS_NEWLINE str]

html2text :: !String -> String
html2text s
	# s	= replaceSubString "<br>" OS_NEWLINE s
	# s	= replaceSubString "<BR>" OS_NEWLINE s
	# s	= replaceSubString "<br/>" OS_NEWLINE s
	# s	= replaceSubString "<BR/>" OS_NEWLINE s
	# s	= replaceSubString "</li>" OS_NEWLINE s
	# s	= stripHtmlTags s
	# s = replaceSubString "&nbsp;" " " s
	# s = replaceSubString "&lt;" "<" s
	# s = replaceSubString "&gt;" ">" s
	# s = replaceSubString "&amp;" "&" s
	= s
where
	stripHtmlTags s
		# fstOpen	= indexOf "<" s
		# fstClose	= indexOf ">" s
		| fstOpen <> -1 && fstClose <> -1 && fstOpen < fstClose
			= stripHtmlTags (subString 0 fstOpen s +++ subString (fstClose + 1) (textSize s - fstClose) s)
		| otherwise
			= s

