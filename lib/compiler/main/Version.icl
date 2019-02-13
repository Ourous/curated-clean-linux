/*
	module owner: Ronny Wichers Schreur
*/
implementation module Version

import StdInt, StdClass

:: VersionInfo =
	{	versionCurrent
			:: Int
	,	versionOldestDefinition
			:: Int
	,	versionOldestImplementation
			:: Int
	}

:: VersionsCompatability
	=	VersionsAreCompatible
	|	VersionObservedIsTooOld
	|	VersionObservedIsTooNew

versionCompare :: VersionInfo VersionInfo -> VersionsCompatability
versionCompare expected observed
	| expected.versionCurrent < observed.versionCurrent
		| expected.versionCurrent >= observed.versionOldestDefinition
			=	VersionsAreCompatible
		// otherwise
			=	VersionObservedIsTooNew
	| expected.versionCurrent == observed.versionCurrent
		=	VersionsAreCompatible
	// expected.versionCurrent > observed.versionCurrent
		| expected.versionOldestImplementation <= observed.versionCurrent
			=	VersionsAreCompatible
		// otherwise
			=	VersionObservedIsTooOld
