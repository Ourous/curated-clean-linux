system module StdReal

// ****************************************************************************************
//	Concurrent Clean Standard Library Module Version 2.0
//	Copyright 1998 University of Nijmegen
// ****************************************************************************************

import	StdOverloaded

instance +			Real
instance -			Real
instance zero		Real

instance *			Real
instance /			Real
instance one		Real

instance ^			Real
instance abs		Real
instance sign		Real
instance ~			Real

instance ==			Real

instance <  		Real

instance toReal		Int
instance toReal		Real
instance toReal		{#Char}

instance fromReal	Int
instance fromReal	Real
instance fromReal	{#Char}

//	Logarithmical Functions:

instance ln			Real	//	Logarithm base e
instance log10		Real	//	Logarithm base 10
instance exp		Real	//	e to to the power	
instance sqrt		Real	//	Square root

//	Trigonometrical Functions:

instance sin		Real	//	Sinus
instance cos		Real	//	Cosinus
instance tan		Real	//	Tangens
instance asin		Real	//	Arc Sinus
instance acos		Real	//	Arc Cosinus
instance atan		Real	//	Arc Tangent
instance sinh		Real	//	Hyperbolic Sine
instance cosh		Real	//	Hyperbolic Cosine
instance tanh		Real	//	Hyperbolic Tangent
instance asinh		Real	//	Arc Hyperbolic Sine
instance acosh		Real	//	Arc Hyperbolic Cosine, partial function, only defined if arg > 1.0
instance atanh		Real	//	Arc Hyperbolic Tangent, partial function, only defined if -1.0 < arg < 1.0

//	Additional conversion:

entier			:: !Real		->	Int		//	Convert Real into Int by taking entier

Infinity :== 1E9999
NaN :== 1E9999+(-1E9999)

isNaN x :== if (x==x) False True
isInfinity x :== if (abs x==1E9999) True False
isFinite x :== if (x-x==0.0) True False
