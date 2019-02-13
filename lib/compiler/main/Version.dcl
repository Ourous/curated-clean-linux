/*
	module owner: Ronny Wichers Schreur
*/
definition module Version

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
// expected observed
