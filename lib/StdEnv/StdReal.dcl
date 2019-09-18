system module StdReal

// ****************************************************************************************
//	Concurrent Clean Standard Library Module Version 3.0
//	Copyright 2019 University of Nijmegen
// ****************************************************************************************

import	StdOverloaded

instance +			Real	:: !Real !Real -> Real		:== code { addR }
instance -			Real	:: !Real !Real -> Real		:== code { subR }
instance zero		Real	:: Real						:== code { pushR 0.0 }

instance *			Real	:: !Real !Real -> Real		:== code { mulR }
instance /			Real	:: !Real !Real -> Real		:== code { divR }
instance one		Real	:: Real						:== code { pushR 1.0 }

instance ^			Real	:: !Real !Real -> Real		:== code { powR }
instance abs		Real	:: !Real -> Real			:== code { absR }
instance sign		Real
instance ~			Real	:: !Real -> Real			:== code { negR}

instance ==			Real	:: !Real !Real -> Bool		:== code { eqR }

instance <  		Real	:: !Real !Real -> Bool		:== code { ltR }

instance toReal		Int		:: !Int -> Real				:== code { ItoR }
instance toReal		Real	:: !Real -> Real			:== code { no_op }
instance toReal		{#Char}

instance fromReal	Int		:: !Real -> Int				:== code { RtoI }
instance fromReal	Real	:: !Real -> Real			:== code { no_op }
instance fromReal	{#Char}	:: !Real -> {#Char}			:== code { .d 0 1 r ; jsr RtoAC ; .o 1 0 }

//	Logarithmical Functions:

instance ln			Real	:: !Real -> Real			:== code { lnR }
							//	Logarithm base e
instance log10		Real	:: !Real -> Real			:== code { log10R }
							//	Logarithm base 10
instance exp		Real	:: !Real -> Real			:== code { expR }
							//	e to the power	
instance sqrt		Real	:: !Real -> Real			:== code { sqrtR }
							//	Square root

//	Trigonometrical Functions:

instance sin		Real	:: !Real -> Real			:== code { sinR }
							//	Sinus
instance cos		Real	:: !Real -> Real			:== code { cosR }
							//	Cosinus
instance tan		Real	:: !Real -> Real			:== code { tanR }
							//	Tangens
instance asin		Real	:: !Real -> Real			:== code { asinR }
							//	Arc Sinus
instance acos		Real	:: !Real -> Real			:== code { acosR }
							//	Arc Cosinus
instance atan		Real	:: !Real -> Real			:== code { atanR }
							//	Arc Tangent
instance sinh		Real	//	Hyperbolic Sine
instance cosh		Real	//	Hyperbolic Cosine
instance tanh		Real	//	Hyperbolic Tangent
instance asinh		Real	//	Arc Hyperbolic Sine
instance acosh		Real	//	Arc Hyperbolic Cosine, partial function, only defined if arg > 1.0
instance atanh		Real	//	Arc Hyperbolic Tangent, partial function, only defined if -1.0 < arg < 1.0

//	Additional conversion:

entier			:: !Real -> Int									:== code { entierR }
							//	Convert Real into Int by taking entier

Infinity :== 1E9999
NaN :== 1E9999+(-1E9999)

isNaN x :== if (x==x) False True
isInfinity x :== if (abs x==1E9999) True False
isFinite x :== if (x-x==0.0) True False
