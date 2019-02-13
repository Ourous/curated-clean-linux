definition module iTasks.Extensions.Distributed._Util

import iTasks

memoryShare_ :: String a -> SimpleSDSLens a | iTask a

repeatClient :: (Task (Maybe a)) -> Task (Maybe a) | iTask a
