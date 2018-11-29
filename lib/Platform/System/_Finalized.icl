implementation module System._Finalized

import StdEnv
import Data.Func
import StdOverloadedList
import System._Pointer

finalize :: a !Pointer !Int -> Finalized a
finalize val ptr arg = Finalized val $ make_finalizer ptr arg

withFinalizedValue :: !(a -> b) !(Finalized a) -> (!b, !Finalized a)
withFinalizedValue func fin=:(Finalized x _) = (func x, fin)

finalizeInt :: !Int !Pointer -> Finalizer
finalizeInt val ptr = make_finalizer ptr val

withFinalizedInt :: !(Int -> a) !Finalizer -> (!a, !Finalizer)
withFinalizedInt func fin=:{finalizer_implementation=DummyFinalizer _ _ val} = (func val, fin)

make_finalizer :: !Pointer !Int -> Finalizer
make_finalizer f v = {finalizer_implementation = fst $ make_finalizer_c f v}
where
    make_finalizer_c :: !Int !Int -> (!FinalizerT, !Int)
    make_finalizer_c f v = code {
        push_finalizers
        push_a_b 0
        pop_a 1
        build_r e__system_kFinalizer 0 3 0 0
        pop_b 3
        set_finalizers
        pushI 0
    }
