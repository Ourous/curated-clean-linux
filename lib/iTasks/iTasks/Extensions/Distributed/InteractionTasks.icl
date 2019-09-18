implementation module iTasks.Extensions.Distributed.InteractionTasks

import iTasks

viewSharedInformation :: [ViewOption r] !(sds () r w) -> Task r | iTask r & iTask w & RWShared sds
viewSharedInformation options share
        = watch share
        >>* [OnValue (hasValue return)]
        >>- \v -> loop v options share
where
        loop :: r [ViewOption r] (sds () r w) -> Task r | iTask r & iTask w & RWShared sds
        loop v options share
                = (viewInformation options v)
                ||- (watch share >>* [OnValue (ifValue ((=!=) v) return)])
                >>- \v -> loop v options share
