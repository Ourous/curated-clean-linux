definition module Sapl.Transform.AddSelectors

from Sapl.SaplStruct import :: SaplTerm

class addSelectors t :: !t -> t

instance addSelectors SaplTerm
