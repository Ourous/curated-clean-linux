module TestMetarParser

//	**************************************************************************************************
//
//	A program that lets one edit a METAR (Aeronautical Weather Message) and it displays parse results.
//
//	The program has been written in Clean 2.0 and uses the Clean Standard Object I/O library 1.2.4
//	
//	**************************************************************************************************

import StdEnv, StdIO, Text.Parsers.MetarDemo.MetarParser

Start :: *World -> *World
Start world
	# (ids,world)	= openIds 3 world
	= startIO NDI () (initialise ids) [] world

::	ResultSt =	{offset :: !Int, text :: [String],nrLines :: Int}

initialise ids pst
	# (error,pst)	= openDialog () METAR_Dialog pst
	| error<>NoError= closeProcess pst
	| otherwise		= pst
where
	[viewid,showid,setid:_] = ids

	METAR_Dialog = Dialog "METAR Tester"
			{ newLS = {offset=0,text=[],nrLines=0}
			, newDef = (editMETAR :+: parseButton :+: scrollUpButton :+: scrollDownButton :+: showResult)
			} [WindowId viewid, WindowClose (noLS closeProcess)]

	editMETAR
		# tip = "Type METAR without the leading airport code, such as EHAM for Amsterdam"
		= EditControl "" width 1
			[ControlId setid, ControlPos (Left,NoOffset), ControlTip tip]
				
	parseButton
		# tip = "Parse METAR to see if its syntax is correct"
		= ButtonControl "Parse"
			[ControlFunction startParse, ControlPos (Left,NoOffset), ControlTip tip]
				
	scrollUpButton
		# tip = "Scroll up"
		= ButtonControl "/\\"
			[ControlFunction (scroll (~scrollStep)), ControlPos (RightToPrev,NoOffset),	ControlTip tip]
			
	scrollDownButton
		# tip =  "Scroll down"
		= ButtonControl "\\/"
			[ControlFunction (scroll scrollStep), ControlPos (RightToPrev,NoOffset), ControlTip tip]

	showResult
		= EditControl "" width nrLines
			[ControlSelectState Unable, ControlId showid, ControlPos (Left,NoOffset)]
	
	startParse :: (ResultSt,PSt .l) -> (ResultSt,PSt .l)
	startParse (_,pst)
		# (Just dialog,pst)	= accPIO (getWindow viewid) pst
		  (_,Just text)		= getControlText setid dialog
		  result			= metarParse (fromString text)
		  show				= sep (take nrLines result)
		= ({offset=0,text=result,nrLines = length result},appPIO (setControlText showid show) pst)
					// put the parse-result in the protected text block

	scroll :: Int (ResultSt,PSt .l) -> (ResultSt,PSt .l)
	scroll step (result=:{offset,text,nrLines},pst)
		# offset	= min nrLines (max 0 (offset+step))
		# partText	= sep (take nrLines (drop offset text))
		= ({result & offset=offset},appPIO (setControlText showid partText) pst)

metarParse :: [Char] -> [String]
metarParse metar = case (testMetar (wordify metar)) of
	(Succ [m:ms])		= msgOK (report m)
	(Err st rose sp)	= (errorToStrings st rose sp)
	
width		= PixelWidth (hmm 300.0)
nrLines		= 30
scrollStep	= 5
sep			= flattenSep "\r\n"
	
/*	The \r\n above is a work-around supplied on 4 January 2005 by Diederik van Arkel for a bug in EditControl. 
	We would expect \n to suffice to insert a newline.
*/
msgOK result	=	["METAR OK. Partial contents decoded:"
					,""
					,"* Bound 1 of Wind Direction (Degrees)"
					,"* Bound 2 of Wind Direction (Degrees)"
					,"* Wind Speed (Knots)"
					,"* Horizontal Visibility (Meters)"
					,"* Vertical Visibility = Ceiling (Feet)"
					,"* Temperature (Centigrade)"
					,"* Air Pressure (Milimeters Mercury)"
					,""
					,result]
