implementation module iTasks.Internal.Client.Override

import iTasks.WF.Definition

cast :: a -> b | TC a & TC b
cast a = case make_dynamic a of (a::b^) -> a

cast_to_TaskValue :: a -> TaskValue b | TC a & TC b
cast_to_TaskValue a = case make_dynamic a of (a::TaskValue b^) -> a

make_dynamic tva = dynamic tva

unwrapTask :: Dynamic -> Task a | TC a
unwrapTask (task :: Task a^) = task

