implementation module iTasks.Extensions.Contact
import iTasks
import Text.HTML

//* EmailAddress
derive JSONEncode		EmailAddress
derive JSONDecode		EmailAddress
derive gDefault			EmailAddress
derive gEq				EmailAddress
derive gText	        EmailAddress
derive gEditor			EmailAddress

instance toString EmailAddress
where
	toString (EmailAddress email) = email

instance html EmailAddress
where
	html (EmailAddress email) = ATag [HrefAttr ("mailto:" +++ email)] [Text email]

//* PhoneNumber
derive JSONEncode		PhoneNumber
derive JSONDecode		PhoneNumber
derive gDefault			PhoneNumber
derive gEq				PhoneNumber
derive gText	        PhoneNumber
derive gEditor			PhoneNumber

instance toString PhoneNumber
where
	toString (PhoneNumber num) = num

instance html PhoneNumber
where
	html (PhoneNumber num) = Text num


