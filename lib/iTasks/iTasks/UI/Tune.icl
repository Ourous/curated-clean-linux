implementation module iTasks.UI.Tune

import iTasks.WF.Definition 

class tune b f :: !b !(f a) -> f a
class tunev b a f | iTask a :: !(b a) !(f a) -> f a

(<<@) infixl 2 :: !(f a) !b	-> f a | tune b f
(<<@) t a = tune a t

(@>>) infixr 2 :: !b !(f a)	-> f a | tune b f
(@>>) a t = tune a t

(<@@) infixl 2 :: !(f a) !(b a) -> f a | tunev b a f & iTask a
(<@@) t a = tunev a t

(@@>) infixr 2 :: !(b a) !(f a) -> f a | tunev b a f & iTask a
(@@>) a t = tunev a t

