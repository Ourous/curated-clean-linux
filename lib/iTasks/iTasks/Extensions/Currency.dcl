definition module iTasks.Extensions.Currency
import iTasks

//* Money (ISO4217 currency codes are used)
:: EUR 			= EUR !Int		//Euros (amount in cents)
:: USD 			= USD !Int		//Dollars (amount in cents)

instance toString	EUR, USD
instance + 			EUR, USD
instance - 			EUR, USD
instance == 		EUR, USD
instance < 			EUR, USD
instance toInt		EUR, USD
instance zero		EUR, USD

derive JSONEncode	EUR, USD
derive JSONDecode	EUR, USD
derive gDefault		EUR, USD
derive gEq			EUR, USD
derive gText	    EUR, USD
derive gEditor 		EUR, USD
