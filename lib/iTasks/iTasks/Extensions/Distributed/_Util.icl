implementation module iTasks.Extensions.Distributed._Util

import iTasks
from iTasks.Internal.Store import memoryStore, :: StoreName, :: StoreNamespace
from System.Time import :: Timestamp(..)
from iTasks.Extensions.DateTime import :: DateTime, instance < DateTime, instance toString DateTime, timestampToGmDateTime, localDateTimeToTimestamp
from Data.Maybe import fromMaybe, isNothing, fromJust, maybe, instance Functor Maybe, isJust

memoryShare_ :: String a -> SimpleSDSLens a | iTask a
memoryShare_ name default = sdsFocus name (memoryStore name (Just default))

repeatClient :: (Task (Maybe a)) -> Task (Maybe a) | iTask a
repeatClient task
	= (try task) <! isJust
where
	try :: (Task (Maybe a)) -> Task (Maybe a) | iTask a
	try task
		= catchAll task (\_ -> return Nothing)
		>>- \result -> if (isNothing result) tryAgain (return result)

	tryAgain :: Task (Maybe a) | iTask a
	tryAgain
		= waitForTimer` timeout @! Nothing
	where
		timeout = 60

waitForTimer` :: !Int -> Task DateTime
waitForTimer` interval
	= get currentDateTime
	>>- \now -> endTime interval now
	>>- \later -> waitForDateTime` later
where
	endTime interval now = localDateTimeToTimestamp now >>- \(Timestamp ts) -> return (timestampToGmDateTime (Timestamp (ts + interval)))

waitForDateTime` :: !DateTime -> Task DateTime
waitForDateTime` datetime
	= Title "Connection interrupted" @>>
	  Hint ("The connection with the other controller is interrupted, next attempt: " +++ toString datetime) @>>
		viewSharedInformation [] currentUTCDateTime >>* [OnValue (ifValue (\now -> datetime < now) return)]
	>>* [ OnValue (ifValue (\now -> datetime < now) return)
	    , OnAction (Action "Reconnect") (always (return datetime))
	    ]
