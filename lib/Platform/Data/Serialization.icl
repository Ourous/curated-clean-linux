implementation module Data.Serialization

import StdEnv
import StdDynamicLowLevelInterface
import DynamicGraphConversion
from DynamicUtilities import WriteLong, NF
import md5
import DynamicLinkerInterface
import StdDynamic

import Error
import File

serialize :: !a -> String | TC a
serialize value = serializeDynamic (dynamic value)

deserialize :: !String -> MaybeErrorString a | TC a
deserialize str = 
	case deserializeDynamic str of
		Ok dyn = unpack dyn
		error  = liftError error
	where
		unpack :: Dynamic -> MaybeErrorString a | TC a
		unpack (value :: a^) = Ok value
		unpack dyn = Error ("Data.Serialization, deserialize: unpacking dynamic of type " +++ (toString (typeCodeOfDynamic dyn))+++ " failed")

serializeDynamic :: !Dynamic -> String
serializeDynamic dynamic_value
	// this forces evaluation of the type part of the dynamic
	| typeCodeOfDynamic dynamic_value == TypeVar (-1)
		= abort "Data.Serialize, dynamicToString: malformed dynamic type"
	# (_,encoded_dynamic)
		= dynamic_to_string dynamic_value
	= write_encoded_dynamic encoded_dynamic
where
	write_encoded_dynamic {ed_encoded_graph,ed_dynamic_rts_info}
		// Offset/Size
		# (s_ed_encoded_graph,ed_encoded_graph)
			= usize ed_encoded_graph
		# ed_encoded_graph
			= WriteLong ed_encoded_graph (DYNAMIC_RTS_INFO_OFFSET - HEADER_SIZE_OFFSET) s_ed_encoded_graph
		
		# (s_ed_dynamic_rts_info,ed_dynamic_rts_info)
			= usize ed_dynamic_rts_info
		# ed_encoded_graph
			= WriteLong ed_encoded_graph (DYNAMIC_RTS_INFO_SIZE - HEADER_SIZE_OFFSET) s_ed_dynamic_rts_info
		= ed_encoded_graph +++ ed_dynamic_rts_info

deserializeDynamic :: !String -> MaybeErrorString Dynamic
deserializeDynamic dynamic_string = 
	case string_to_dynamic dynamic_string of
		(True , dyn) = Ok dyn
		(False, _  ) = Error "Data.Serialization, deserializeDynamic: failed to deserialize"
