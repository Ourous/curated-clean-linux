implementation module iTasks.Extensions.Distributed.InteractionTasks

import iTasks

viewSharedInformation :: String [ViewOption r] !(sds () r w) -> Task r | iTask r & iTask w & RWShared sds
viewSharedInformation title options share
        = watch share
        >>* [OnValue (hasValue return)]
        >>- \v -> loop v title options share
where
        loop :: r String [ViewOption r] (sds () r w) -> Task r | iTask r & iTask w & RWShared sds
        loop v title options share
                = (viewInformation title options v)
                ||- (watch share >>* [OnValue (ifValue ((=!=) v) return)])
                >>- \v -> loop v title options share
