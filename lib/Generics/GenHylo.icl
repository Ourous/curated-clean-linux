implementation module GenHylo 

import StdGeneric, GenMap, StdFunc

:: Fix f = In (f .(Fix f))

Out :: !u:(Fix v:a) -> v:(a w:(Fix v:a)), [u <= w]
Out (In x) = x

hylo :: ((.f .b) -> .b) (.a -> (.f .a)) -> (.a -> .b) | gMap{|*->*|} f
hylo consume produce = consume o gMap{|*->*|} (hylo consume produce) o produce

cata :: (u:(f .a) -> .a) -> (Fix u:f) -> .a | gMap{|*->*|} f
cata f = hylo f Out 

ana :: (.a -> u:(f .a)) -> .a -> (Fix u:f) | gMap{|*->*|} f
ana f = hylo In f

