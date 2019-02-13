implementation module iTasks.Extensions.Picture.Interaction

import iTasks
import iTasks.API.Core.Client.Tasklet
import iTasks.Extensions.Picture.JPEG

showJPEGPicture :: JPEGPicture -> Task (Maybe JPEGPicture)
showJPEGPicture photo
	= mkTask (showJPEGPictureTasklet photo)

showJPEGPictureTasklet :: JPEGPicture -> Tasklet (Maybe JPEGPicture) (Maybe JPEGPicture)
showJPEGPictureTasklet picture
	=
	{ genUI                         = showJPEGPictureGUI picture
	, resultFunc                    = \_ -> Value (Just picture) True
	, tweakUI                       = setTitle "Picture"
	}
where
	showJPEGPictureGUI picture _ _ iworld
		# gui = { width                 = WrapSize
		        , height                = WrapSize
		        , html                  = RawText htmlText
		        , eventHandlers = []
		        }
		= (TaskletHTML gui, Nothing, iworld)
		
	htmlText :: String
	htmlText = "<img style='max-width: 300px;' src='data:image/jpg;base64," +++ picture +++ "' alt='no photo' />"
