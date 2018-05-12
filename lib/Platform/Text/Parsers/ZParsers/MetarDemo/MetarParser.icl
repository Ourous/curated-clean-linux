implementation module Text.Parsers.MetarDemo.MetarParser
import StdEnv, Text.Parsers
from StdIO import :: Maybe (..)

wordify :: ![a] -> [[a]]	| space a	
wordify cs = splitUp (dropWhile space cs)
where	splitUp :: ![a] -> [[a]]	| space a	
		splitUp [] = []
		splitUp cs
			# (first,rest)	= span (not o space) cs
			= [first:wordify rest]

//===========================

testMetar :: [[Char]] -> Result MetarData
testMetar myText = parse metar myText "METAR" "word"

:: SurfaceWind = {direction :: WindDirection, speed :: Int, gust :: Gust}

:: WindDirection = StraightDir Int | BoundedDir Int Int | VariableWindDirection

:: Gust :== Maybe Int

surfaceWind :: Parser [Char] SurfaceWind t
surfaceWind = "Surface Wind" :> p
where	p	= (word constantWind) <&> \surfaceWind -> <!?> (word directionVariation)
							(\dir2->{surfaceWind & direction=dir2})	// if found
							surfaceWind								// if nothing found

constantWind :: Parser Char SurfaceWind a
constantWind =	direction	<&> \dir	 ->
				speed		<&> \(u,spd) ->
				gust u		<@  \g		 -> {direction=dir,speed=spd,gust=g}
				
directionVariation :: Parser Char WindDirection t
directionVariation = "Wind Direction Variation (such as 120V150)" :> p 
where p = BoundedDir @> degrees <++> variableDir

direction :: Parser Char WindDirection t
direction = "Wind Direction" :> p
where	p	= ("Variable Designator (VRB)" :> token ['VRB'] <@ const VariableWindDirection)
				<!> degrees <&> \firstDir -> <!?> variableDir
							(\s->BoundedDir firstDir s)	// if found
							(StraightDir firstDir)		// if nothing found

degrees :: Parser Char Int a
degrees = "Degrees Rounded to Multiple of 10 (three digits)" :> p
where	p = twoDigits <& symbol '0' <@ \tenDeg -> norm (tenDeg * 10) 360

variableDir :: Parser Char Int a
variableDir = "Symbol V" :> symbol 'V' &> degrees

gust :: SpeedUnit -> Parser Char Gust t
gust u = "Gust" :> <!?> ("Symbol G" :> symbol 'G' &> twoDigits)
						(\sp->Just (snd (convSpeedJ sp u)))	// if found
						Nothing								// if not found
		
speed :: Parser Char (SpeedUnit,Int) t
speed = "Wind Speed (two digits)" :> twoDigits
		<&> \sp -> (<!?> speedUnit (convSpeedJ sp) (convSpeedN sp))
		
metersPerNauticalMile	:== 1852
metersPerStatuteMile	:== 1609
metersPerKilometer		:== 1000

convSpeedJ :: Int SpeedUnit -> (SpeedUnit,Int)
convSpeedJ sp u
	# (u,i)	= case u of
		Knots	-> (Knots,	sp)
		KmH		-> (KmH,	sp *: (metersPerKilometer,metersPerNauticalMile))
		StMH	-> (StMH,	sp *: (metersPerStatuteMile,metersPerNauticalMile))
	= (u, norm i 100)

(*:) infix :: Int (Int,Int) -> Int
(*:) var (num,denom) = (var*num + denom/2) / denom

convSpeedN :: Int -> (SpeedUnit,Int)
convSpeedN sp = (Knots,	norm sp 100)

speedUnit :: Parser Char SpeedUnit t
speedUnit = "Wind Speed Unit" :> p
where	p = 	("Knots, i.e. Nautical Miles per Hour (KT)"	:> token ['KT']		<@ const Knots)
			<!> ("Kilometers per Hour (KMH)"				:> token ['KMH']	<@ const KmH)
			<!>	("Statute Miles per Hour (MPH)"				:> token ['MPH']	<@ const StMH)
			
:: SpeedUnit = Knots | KmH | StMH

twoDigits :: Parser Char Int a
twoDigits = (\d1 d2 -> d1*10 + d2) @> digit` <++> digit`

digit` :: Parser Char Int t
digit` = digit <@ digitToInt

//==============================

:: VisibilityDir = {range :: Int, compass :: Int}
:: Visibility = StraightVis Int | MinVis VisibilityDir | MinMax VisibilityDir VisibilityDir

visibility :: Parser Char Visibility t
visibility = "Horizontal Visibility" :> p
where	p =	fourDigits "Visibility in Meters" <&> \r1 -> (<!?> minMaxVis (conv r1) (StraightVis r1))
		conv r1 (c1,Nothing)	  = MinVis {range=r1,compass=c1}
		conv r1 (c1,Just (r2,c2)) = MinMax {range=r1,compass=c1} {range=r2,compass=c2}
		
minMaxVis :: Parser Char (Int,Maybe (Int,Int)) t
minMaxVis = compass <&&> (<!?> maxVis Just Nothing)

maxVis :: Parser Char (Int,Int) t
maxVis = "Maximum Visibility" :> (fourDigits "Visibility in Meters" <&&> compass)

fourDigits :: String -> Parser Char Int t
fourDigits str = (str+++" (four digits)") :> ((\i1 i2 -> i1*100 + i2) @> twoDigits <++> twoDigits)

compass :: Parser Char Int t
compass = "Compass Heading for min/max Windspeed" :> p
where	p =		"North West (NW)"	:>	(token ['NW']	<@ const 45)
			<!> "North East (NE)"	:>	(token ['NE']	<@ const 315)	
			<!> "South West (SW)"	:>	(token ['SW']	<@ const 135)	
			<!> "South East (SE)"	:>	(token ['SE']	<@ const 225)	
			<!> "North (N)"			:>	(symbol 'N'		<@ const 0)	
			<!> "West (W)"			:>	(symbol 'W'		<@ const 90)
			<!> "South (S)"			:>	(symbol 'S'		<@ const 180)
			<!> "East (E)"			:>	(symbol 'E'		<@ const 270)
			
// runway visibility

:: RunwayVisGroup :== [RunwayVisibility]
:: RunwayVisibility = {id :: Int, range :: Int, tendency :: Maybe Tendency}
:: Tendency = Increasing | Decreasing | NoChange

runwayVisibility :: Parser Char RunwayVisibility t
runwayVisibility =	"Runway Visibility" :> p
where	p =	(\rw rg t -> {id=rw,range=rg,tendency=t}) @>
			runwayID <++> fourDigits "visibility in meters" <++> (<!?> tendency Just Nothing)

tendency :: Parser Char Tendency t
tendency = "Change in Runway Visibility" :> p
where	p =		"Increasing (U)"	:>	(symbol 'U'	<@ const Increasing)
			<!> "Decreasing (D)"	:>	(symbol 'D'	<@ const Decreasing)
			<!>	"No Change (N)"		:>	(symbol 'N' <@ const NoChange)
			
runwayID :: Parser Char Int t
runwayID = "Runway ID (R followed by two digits and /)" :> symbol 'R' &> twoDigits <& symbol '/'

//presentWeather

:: PresentWeather :== [WeatherGroup] 
:: WeatherGroup = {intensity :: Intensity, descriptor :: Maybe Descriptor,
					phenomenon :: WeatherPhenomenon}
:: Intensity = Light | Moderate | Heavy | Vicinity

:: Descriptor		= Shallow | Patches | Partial | Drifting | Blowing | Showers | Thunderstorm
					| SuperCooled
:: WeatherPhenomenon= Precip [Precipitation] | OO ObscurationOther
:: Precipitation	= Drizzle | Rain | Snow | SnowGrains | IceCrystals | IcePellets | Hail
					| SmallHailOrSnowPellets
:: ObscurationOther	= Mist | Fog | Smoke | VolcanicAsh | WidespreadDust | Sand | Haze
					| WellDevDustSandWhirls | Squalls | FunnelClouds | SandStorm | DustStorm

weatherGroup :: Parser Char WeatherGroup t
weatherGroup	= (\intens (d,ph) -> {intensity=intens,descriptor=d,phenomenon=ph}) @>
				  (<!?> intensity id Moderate) <++> descrPhenom
where	descrPhenom = ((<!?> descriptor Just Nothing) <&&> phenomenon)		<!>
					  "Showers (SH)" :> token ['SH'] <@ const (Just Showers,Precip [Rain])

intensity :: Parser Char Intensity t
intensity	= "Weather Intensity/Proximity" :> p
where	p =		"Light (-)" 	:> (symbol '-'		<@ const Light)		
			<!>	"Heavy (+)"		:> (symbol '+'		<@ const Heavy)		
			<!>	"Vicinity (VC)" :> (token ['VC']	<@ const Vicinity)
			
descriptor :: Parser Char Descriptor t
descriptor	= "Weather Phenomenon Descriptor" :> p
where	p =		"Shallow (MI)"		:> (token ['MI']	<@ const Shallow)
			<!>	"Patches (BC)"		:> (token ['BC']	<@ const Patches)
			<!>	"Partial (PR)"		:> (token ['PR']	<@ const Partial)
			<!>	"Drifting (DR)"		:> (token ['DR']	<@ const Drifting)
			<!>	"Blowing (BL)"		:> (token ['BL']	<@ const Blowing)
			<!>	"Showers (SH)"		:> (token ['SH']	<@ const Showers)
			<!>	"Thunderstorm (TS)"	:> (token ['TS']	<@ const Thunderstorm)
			<!>	"Super Cooled (FZ)"	:> (token ['FZ']	<@ const SuperCooled)
			
phenomenon :: Parser Char WeatherPhenomenon t
phenomenon =	<!+> precipitation		<@ Precip
				<!> obscurationOther	<@ OO
				
precipitation :: Parser Char Precipitation t
precipitation = "Precipitation" :> p
where	p =		"Drizzle (DZ)"		:>	(token ['DZ']	<@ const Drizzle)
			<!>	"Rain (RA)"			:>	(token ['RA']	<@ const Rain)
			<!>	"Snow (SN)"			:>	(token ['SN']	<@ const Snow)
			<!>	"Snow grains (SG)"	:>	(token ['SG']	<@ const SnowGrains)
			<!>	"Ice Crystals (IC)"	:>	(token ['IC']	<@ const IceCrystals)
			<!>	"Ice Pellets (PL)"	:>	(token ['PL']	<@ const IcePellets)
			<!>	"Hail(GR)"			:>	(token ['GR']	<@ const Hail)
			<!>	"Small Hail or Snow Pellets (GS)"
									:>	(token ['GS']	<@ const SmallHailOrSnowPellets)

obscurationOther :: Parser Char ObscurationOther t
obscurationOther	= "Obscuration etc." :> p
where	p =		"Mist (BR)"			:>	(token ['BR']	<@ const Mist)
			<!> "Fog (FG)"			:>	(token ['FG']	<@ const Fog)
			<!> "Smoke (FU)"		:>	(token ['FU']	<@ const Smoke)
			<!> "Volcanic Ash (VA)"	:>	(token ['VA']	<@ const VolcanicAsh)
			<!> "Widespread Dust (DU)"
									:>	(token ['DU']	<@ const WidespreadDust)
			<!> "Sand (SA)"			:>	(token ['SA']	<@ const Sand)
			<!> "Haze (HZ)"			:>	(token ['HZ']	<@ const Haze)
			<!> "Well Developped Dust or Sand Whirls (PO)"
									:>	(token ['PO']	<@ const WellDevDustSandWhirls)
			<!> "Squalls (SQ)"		:>	(token ['SQ']	<@ const Squalls)
			<!> "Funnel Clouds (FC)":>	(token ['FC']	<@ const FunnelClouds)
			<!> "Sand Storm (SS)"	:>	(token ['SS']	<@ const SandStorm)
			<!> "Dust Storm (DS)"	:>	(token ['DS']	<@ const DustStorm)
					
// clouds and vertical visibility

:: CloudsVertVisibility = L [CloudsVV] | SkyClear

:: CloudsVV = {amount :: CloudAmount,visibility :: Int}

:: CloudAmount = Few | Scattered | Broken | Overcast

cloudsVV :: Parser Char CloudsVV t
cloudsVV = "Clouds" :> p
where	p = (\ca vv -> {amount=ca,visibility=vv}) @> cloudAmount <++> verticalVisibility

skyClear :: Parser Char CloudsVertVisibility t
skyClear = "Sky Clear (SKC)" :> token ['SKC'] <@ const SkyClear

cloudAmount :: Parser Char CloudAmount t
cloudAmount = "Cloud Amount" :> p
where	p =		"Few (FEW)"			:>	(token ['FEW']	<@ const Few)
			<!>	"Scattered (SCT)"	:>	(token ['SCT']	<@ const Scattered)
			<!>	"Broken (BKN)"		:>	(token ['BKN']	<@ const Broken)
			<!>	"Overcast (OVC)"	:>	(token ['OVC']	<@ const Overcast)
				
verticalVisibility :: Parser Char Int t
verticalVisibility = "Vertical Visibility in Hundreds of Feet (three digits)" :> p
where	p = (\kFeet hFeet -> (1000*kFeet+100*hFeet)) @> twoDigits <++> digit`

// temperature and dew point

:: TempAndDewPoint = {temperature::Int,dewPoint::Int}

temperature :: String -> Parser Char Int t
temperature str = str :> p
where	p = (<!?> ("Minus Sign (M)" :> symbol 'M') (\min temp -> ~temp) id) <++>
			"Centigrade (two digits)" :> twoDigits
		
tempAndDewPoint :: Parser [Char] TempAndDewPoint t
tempAndDewPoint	= "Temperature and Dew Point" :> (word p)
where	p = (\t d -> {temperature=t,dewPoint=d}) @>
			(temperature "Temperature" <& ("Slash" :> symbol '/')) <++> (temperature "Dew Point")		

// pressure

pressure :: Parser [Char] Int t
pressure = "pressure" :> (word p)
where	p = ("Symbol Q" :> symbol 'Q') &> fourDigits "Pressure in mm Mercury"

// visibility, weather and cloud group

:: VisibilityWeatherCloud	= CloudsVisibilityOK
							| VWCGroup Visibility RunwayVisGroup PresentWeather CloudsVertVisibility

word p	:== drill p "position"

/*	The following might be wrong: when you type in a horizontal visibility, that is enough. You are not
	prompted to type in SKC (sky clear). It just seems to be assumed. */

visibilityWeatherCloud :: Parser [Char] VisibilityWeatherCloud t
visibilityWeatherCloud = p
where	p = word cloudsVisibilityOK <!>
			VWCGroup @>	
			 word visibility <++> <!*> (word runwayVisibility)	<++> <!*> (word weatherGroup) <++>
			 (word skyClear <!> <!*>  (word cloudsVV) <@ L)

cloudsVisibilityOK :: Parser Char VisibilityWeatherCloud t
cloudsVisibilityOK = "Clouds and Visibility OK (CAVOK)" :> token ['CAVOK']	<@ const CloudsVisibilityOK
							 
// metar data

:: MetarData = {	surfaceWind				:: SurfaceWind,
					visibilityWeatherCloud	:: VisibilityWeatherCloud,
					tempAndDewPoint			:: TempAndDewPoint,
					pressure				:: Int}
					
metarData :: Parser [Char] MetarData t
metarData	=	(\sw vwc td p -> {surfaceWind = sw, visibilityWeatherCloud	= vwc,
					 			  tempAndDewPoint = td, pressure = p})				@>
				surfaceWind <++> visibilityWeatherCloud	<++> tempAndDewPoint <++> pressure				

// metar prefix

metarPrefix :: Parser [Char] Char t
metarPrefix = "Date and Time Prefix (four or six digits followed by Z)" :> (word p)
where	p =  twoDigits &> twoDigits &> (<!?> twoDigits undef undef) &> (symbol 'Z')

// metar

metar :: Parser [Char] MetarData t
metar = (<!?> metarPrefix undef undef) &> metarData

// reporting

class report a :: a -> String

instance report MetarData
where	report {surfaceWind,visibilityWeatherCloud,tempAndDewPoint,pressure} =
			report surfaceWind +++ " " +++
			report visibilityWeatherCloud +++ " " +++
			report tempAndDewPoint +++ " " +++
			toDigits pressure 4

instance report WindDirection
where	report VariableWindDirection = "VRB VRB"
		report d = sixDigits (decode d)
		where	decode :: WindDirection -> (Int,Int)
				decode (StraightDir i)	= (i,i)
				decode (BoundedDir a b)	= (a,b)
				sixDigits (i,j) = toDigits i 3 +++ " " +++ toDigits j 3

toDigits :: !Int !Int -> String
toDigits x n = toString (p 10 n + abs x) % (1,n)
where	p :: !Int !Int -> Int
		p cum n
			| n < 2		= cum
			| otherwise	= p (cum*10) (n-1)

instance report SurfaceWind
where	report {direction,speed} = report direction +++ " " +++ toDigits speed 2

instance report Visibility
where	report vis
			# i = case vis of
					StraightVis i					-> i
					MinVis {VisibilityDir|range}	-> range
					MinMax {VisibilityDir|range} _	-> range
			= toDigits i 4
		
instance report TempAndDewPoint
where	report {temperature}
			# n= norm (abs temperature) 100
			= sign temperature +++ toDigits n 2
		where	sign t	| t < 0 = "-"
						| t > 0 = "+"
						= " "  

instance report VisibilityWeatherCloud
where	report CloudsVisibilityOK					= "9999 >05000"
		report (VWCGroup visibility _ _ cloudsVV)	= report visibility +++ " " +++ report cloudsVV
		
instance report CloudsVertVisibility
where	report SkyClear	=  ">05000"
		report (L cs) = case getCeilings cs of
			[]	-> ">05000"
			cs	# visibility = minList cs
				-> " " +++ toDigits visibility 5

getCeilings [{amount,visibility}:rest] = case amount of
	Broken	-> [visibility:getCeilings rest]
	Overcast-> [visibility:getCeilings rest]
	_		-> getCeilings rest
getCeilings [] = []

// utilities

norm :: Int Int -> Int
norm a bound
	# t = a / bound
	| a >= 0	= a - t*bound
				= a - (t-1)*bound
