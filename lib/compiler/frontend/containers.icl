implementation module containers
// compile using "reuse unique nodes" option

import StdEnv, utilities, syntax

:: NumberSet = Numbers !Int !NumberSet | EndNumbers

inNumberSet :: !Int !NumberSet -> Bool
inNumberSet n EndNumbers
	= False;
inNumberSet n (Numbers module_numbers rest_module_numbers)
	| n<32
		= (module_numbers bitand (1<<n))<>0
		= inNumberSet (n-32) rest_module_numbers

nsFromTo :: !Int -> NumberSet
	// all numbers from 0 to (i-1)
nsFromTo i
	| i<=0
		= EndNumbers
	| i<=31
	 	= Numbers (bitnot ((-1)<<i)) EndNumbers
	= Numbers (-1) (nsFromTo (i-32))

addNr :: !Int !NumberSet -> NumberSet
addNr n EndNumbers
	| n<32
		= Numbers (1<<n) EndNumbers
		= Numbers 0 (addNr (n-32) EndNumbers)
addNr n (Numbers module_numbers rest_module_numbers)
	| n<32
		= Numbers (module_numbers bitor (1<<n)) rest_module_numbers
		= Numbers module_numbers (addNr (n-32) rest_module_numbers)

numberSetUnion :: !NumberSet !NumberSet -> NumberSet
numberSetUnion EndNumbers x
	= x
numberSetUnion x EndNumbers
	= x
numberSetUnion (Numbers i1 tail1) (Numbers i2 tail2)
	= Numbers (i1 bitor i2) (numberSetUnion tail1 tail2)

is_empty_module_n_set EndNumbers
	= True;
is_empty_module_n_set (Numbers 0 module_numbers)
	= is_empty_module_n_set module_numbers
is_empty_module_n_set _
	= False;

remove_first_module_number (Numbers 0 rest_module_numbers)
	# (bit_n,rest_module_numbers) = remove_first_module_number rest_module_numbers
	= (bit_n+32,Numbers 0 rest_module_numbers)
remove_first_module_number (Numbers module_numbers rest_module_numbers)
	# bit_n = first_one_bit module_numbers
	= (bit_n,Numbers (module_numbers bitand (bitnot (1<<bit_n))) rest_module_numbers)

numberSetToList :: !NumberSet -> [Int]
numberSetToList ns
	= numberset_to_list ns 0
  where
	numberset_to_list :: !NumberSet !Int -> [Int]
	numberset_to_list EndNumbers i
		= []
	numberset_to_list (Numbers n rest_ns) i
		# rest_l
				= numberset_to_list rest_ns (i+32)
		= add_numbers_in_word n i rest_l
	
	add_numbers_in_word :: !Int !Int [Int] -> [Int]
	add_numbers_in_word n i rest_l
		| n==0
			= rest_l
		# (last_i, mask)
				= last_one_bit n
		= add_numbers_in_word (n bitand (bitnot mask)) i [last_i+i:rest_l]
	
	last_one_bit :: !.Int -> (!Int, !Int)
	last_one_bit n
		| n bitand 0xff000000<>0
			= last_one_bit_in_byte 31 n
		| n bitand 0xff0000<>0
			= last_one_bit_in_byte 23 n
		| n bitand 0xff00<>0
			= last_one_bit_in_byte 15 n
		= last_one_bit_in_byte 7 n
	
	last_one_bit_in_byte :: !Int !Int -> (!Int, !Int)
	last_one_bit_in_byte i n
		# mask
				= 1<<i
		| n bitand mask<>0
			= (i, mask)
		= last_one_bit_in_byte (i-1) n

first_one_bit module_numbers
	| module_numbers bitand 0xff<>0
		= first_one_bit_in_byte 0 module_numbers
	| module_numbers bitand 0xff00<>0
		= first_one_bit_in_byte 8 module_numbers
	| module_numbers bitand 0xff0000<>0
		= first_one_bit_in_byte 16 module_numbers
		= first_one_bit_in_byte 24 module_numbers

first_one_bit_in_byte n module_numbers
	| module_numbers bitand (1<<n)<>0
		= n
		= first_one_bit_in_byte (n+1) module_numbers

bitvectToNumberSet :: !LargeBitvect -> .NumberSet
bitvectToNumberSet a
	= loop a (size a - 1)
  where
	loop a (-1)
		= EndNumbers
	loop a i
		| a.[i]==0
			= loop a (i-1) 
		= loop2 a i EndNumbers
		
	loop2 a (-1) accu
		= accu
	loop2 a i accu
		= loop2 a (i-1) (Numbers a.[i] accu)

BITINDEX index :== index >> 5
BITNUMBER index :== index bitand 31

:: LargeBitvect :== {#Int}

bitvectCreate :: !Int -> .LargeBitvect 
bitvectCreate 0 = {}
bitvectCreate n_elements = createArray ((BITINDEX (n_elements-1)+1)) 0

bitvectSelect :: !Int !LargeBitvect -> Bool
bitvectSelect index a
	= a.[BITINDEX index] bitand (1 << BITNUMBER index) <> 0

bitvectTestAndSet :: !Int !*LargeBitvect -> (!Bool,!.LargeBitvect)
bitvectTestAndSet index a
	#  bit_index = BITINDEX index
	#! a_bit_index = a.[bit_index]
	#  mask = 1 << BITNUMBER index
	#  new_a_bit_index = a_bit_index bitor mask
	= (new_a_bit_index==a_bit_index,{ a & [bit_index] = new_a_bit_index})

bitvectSet :: !Int !*LargeBitvect -> .LargeBitvect 
bitvectSet index a
	#! bit_index = BITINDEX index
	   a_bit_index = a.[bit_index]
	= { a & [bit_index] = a_bit_index bitor (1 << BITNUMBER index)}

bitvectReset :: !Int !*LargeBitvect -> .LargeBitvect
bitvectReset index a
	#! bit_index = BITINDEX index
	   a_bit_index = a.[bit_index]
	= { a & [bit_index] = a_bit_index bitand (bitnot (1 << BITNUMBER index))}

bitvectSetFirstN :: !Int !*LargeBitvect -> .LargeBitvect 
bitvectSetFirstN n_bits a
		= set_bits 0 n_bits a
	where
		set_bits index n_bits a
			| n_bits<=0
				= a
			| n_bits<32
				# (a_index,a) = a![index]
			 	= {a & [index]=a_index bitor (bitnot ((-1)<<n_bits))}
			 	= set_bits (index+1) (n_bits-32) {a & [index]= -1}

bitvectResetAll :: !*LargeBitvect -> .LargeBitvect 
bitvectResetAll arr
	#! size = size arr
	= { arr & [i] = 0 \\ i<-[0..size-1] } // list should be optimized away

bitvectOr :: !u:LargeBitvect !*LargeBitvect -> (!Bool, !u:LargeBitvect, !*LargeBitvect)
// Boolean result: whether the unique bitvect has changed
bitvectOr op1 op2
	#! size
		= size op1
	= iFoldSt word_or 0 size (False, op1, op2)
  where
	word_or i (has_changed, op1=:{[i]=op1_i}, op2=:{[i]=op2_i})
		# or = op1_i bitor op2_i
		| or==op2_i
			= (has_changed, op1, op2)
		= (True, op1, { op2 & [i] = or })

add_strictness :: !Int !StrictnessList -> StrictnessList
add_strictness index NotStrict
	| index<32
		= Strict (1<<index);
		= StrictList 0 (add_strictness (index-32) NotStrict)
add_strictness index (Strict s)
	| index<32
		= Strict (s bitor (1<<index));
		= StrictList s (add_strictness (index-32) NotStrict)
add_strictness index (StrictList s l)
	| index<32
		= StrictList (s bitor (1<<index)) l;
		= StrictList s (add_strictness (index-32) l)

first_n_strict :: !Int -> StrictnessList
first_n_strict 0
	= NotStrict
first_n_strict n
	| n<32
		= Strict (bitnot ((-1)<<n))
		= StrictList (-1) (first_n_strict (n-32))

insert_n_strictness_values_at_beginning :: !Int !StrictnessList -> StrictnessList
insert_n_strictness_values_at_beginning 0 s
	= s
insert_n_strictness_values_at_beginning n NotStrict
	| n<32
		= Strict (bitnot ((-1)<<n))
		= StrictList (-1) (first_n_strict (n-32))
insert_n_strictness_values_at_beginning n (Strict s)
	| n<32
		# s2=((s>>1) bitand 0x7fffffff)>>(31-n)
		# s=(bitnot ((-1)<<n)) bitor (s<<n)
		| s2==0
			= Strict s
			= StrictList s (Strict s2)
		= StrictList (-1) (first_n_strict (n-32))
insert_n_strictness_values_at_beginning n (StrictList s l)
	| n<32
		# s2=((s>>1) bitand 0x7fffffff)>>(31-n)
		# s=(bitnot ((-1)<<n)) bitor (s<<n)
		= StrictList s (shift_or l n s2)
		= StrictList (-1) (insert_n_strictness_values_at_beginning (n-32) l)

insert_n_lazy_values_at_beginning :: !Int !StrictnessList -> StrictnessList
insert_n_lazy_values_at_beginning 0 s
	= s
insert_n_lazy_values_at_beginning n NotStrict
	= NotStrict
insert_n_lazy_values_at_beginning n (Strict s)
	| n<32
		# s2=((s>>1) bitand 0x7fffffff)>>(31-n)
		# s=s<<n
		| s2==0
			= Strict s
			= StrictList s (Strict s2)
		= StrictList (-1) (first_n_strict (n-32))
insert_n_lazy_values_at_beginning n (StrictList s l)
	| n<32
		# s2=((s>>1) bitand 0x7fffffff)>>(31-n)
		# s=s<<n
		= StrictList s (shift_or l n s2)
		= StrictList (-1) (insert_n_lazy_values_at_beginning (n-32) l)

shift_or NotStrict n s2
	| s2==0	
		= NotStrict
		= Strict s2
shift_or (Strict s) n s2
	# new_s=(s<<n) bitor s2
	# new_s2=((s>>1) bitand 0x7fffffff)>>(31-n)
	| new_s2==0
		= Strict new_s
		= StrictList new_s (Strict new_s2)
shift_or (StrictList s l) n s2
	# new_s=(s<<n) bitor s2
	# new_s2=((s>>1) bitand 0x7fffffff)>>(31-n)
	= StrictList new_s (shift_or l n new_s2)

arg_strictness_annotation :: !Int !StrictnessList -> Annotation;
arg_strictness_annotation _ NotStrict
	= AN_None
arg_strictness_annotation i (Strict s)
	| i<32 && (s bitand (1<<i))<>0
		= AN_Strict
		= AN_None
arg_strictness_annotation i (StrictList s l)
	| i<32
		| (s bitand (1<<i))<>0
			= AN_Strict
			= AN_None
		= arg_strictness_annotation (i-32) l

arg_is_strict :: !Int !StrictnessList -> Bool;
arg_is_strict _ NotStrict
	= False
arg_is_strict i (Strict s)
	= i<32 && (s bitand (1<<i))<>0
arg_is_strict i (StrictList s l)
	| i<32
		= (s bitand (1<<i))<>0
		= arg_is_strict (i-32) l

is_not_strict :: !StrictnessList -> Bool
is_not_strict NotStrict = True
is_not_strict (Strict s) = s==0
is_not_strict (StrictList s l) = s==0 && is_not_strict l

equal_strictness_lists :: !StrictnessList !StrictnessList -> Bool
equal_strictness_lists NotStrict NotStrict
	= True
equal_strictness_lists NotStrict (Strict s)
	= s==0
equal_strictness_lists NotStrict (StrictList s l)
	= s==0 && is_not_strict l
equal_strictness_lists (Strict s) NotStrict
	= s==0
equal_strictness_lists (Strict s1) (Strict s2)
	= s1==s2
equal_strictness_lists (Strict s1) (StrictList s2 l)
	= s1==s2 && is_not_strict l
equal_strictness_lists (StrictList s l) NotStrict
	= s==0 && is_not_strict l
equal_strictness_lists (StrictList s1 l) (Strict s2)
	= s1==s2 && is_not_strict l
equal_strictness_lists (StrictList s1 l1) (StrictList s2 l2)
	= s1==s2 && equal_strictness_lists l1 l2

more_or_equal_strictness_lists :: !StrictnessList !StrictnessList -> Bool
more_or_equal_strictness_lists NotStrict s2
	= is_not_strict s2
more_or_equal_strictness_lists (Strict s) NotStrict
	= True
more_or_equal_strictness_lists (Strict s1) (Strict s2)
	= (bitnot s1) bitand s2==0
more_or_equal_strictness_lists (Strict s1) (StrictList s2 l)
	= (bitnot s1) bitand s2==0 && is_not_strict l
more_or_equal_strictness_lists (StrictList s l) NotStrict
	= True
more_or_equal_strictness_lists (StrictList s1 l) (Strict s2)
	= (bitnot s1) bitand s2==0
more_or_equal_strictness_lists (StrictList s1 l1) (StrictList s2 l2)
	= (bitnot s1) bitand s2==0 && more_or_equal_strictness_lists l1 l2

add_next_strict :: !Int !Int !StrictnessList -> (!Int,!Int,!StrictnessList)
add_next_strict strictness_index strictness strictness_list
	| strictness_index<32
		= (strictness_index+1,strictness bitor (1<<strictness_index),strictness_list)
		= (0,0x80000000,append_strictness strictness strictness_list)

add_next_not_strict :: !Int !Int !StrictnessList -> (!Int,!Int,!StrictnessList)
add_next_not_strict strictness_index strictness strictness_list
	| strictness_index<32
		= (strictness_index+1,strictness,strictness_list)
		= (0,0,append_strictness strictness strictness_list)

append_strictness :: !Int !StrictnessList -> StrictnessList
append_strictness strictness NotStrict
	= Strict strictness
append_strictness strictness (Strict s)
	= StrictList s (Strict strictness)
append_strictness strictness (StrictList s l)
	= StrictList s (append_strictness strictness l)

first_n_are_strict :: !Int !StrictnessList -> Bool
first_n_are_strict 0 _
	= True
first_n_are_strict n NotStrict
	= False
first_n_are_strict n (Strict s)
	| n>32
		= False
	| n==32
		= s==0xffffffff
		# m=(1<<n)-1
		= s bitand m==m
first_n_are_strict n (StrictList s l)
	| n>=32
		= s==0xffffffff && first_n_are_strict (n-32) l
		# m=(1<<n)-1
		= s bitand m==m

remove_first_n :: !Int !StrictnessList -> StrictnessList
remove_first_n 0 s
	= s
remove_first_n _ NotStrict
	= NotStrict
remove_first_n n (Strict s)
	| n<32
		= Strict (((s>>1) bitand 0x7fffffff)>>(n-1))
		= NotStrict
remove_first_n n (StrictList s l)
	| n<32
		# s2=case l of
				Strict s -> s
				StrictList s _ -> s
				NotStrict -> 0
		# s=(((s>>1) bitand 0x7fffffff)>>(n-1)) bitor (s2<<(32-n))
		= StrictList s (remove_first_n n l)
		= remove_first_n (n-32) l

screw :== 80

:: IntKey :== Int

:: IntKeyHashtable a = IntKeyHashtable !Int !Int !Int !.{!.IntKeyTree a}
//						ikh_rehash_threshold ikh_nr_of_entries ikh_bitmask ikh_entries
// it's not a record type to prevent it from being unboxed
	
:: IntKeyTree a = IKT_Leaf | IKT_Node !IntKey a !.(IntKeyTree a) !.(IntKeyTree a)

ikhEmpty :: .(IntKeyHashtable a)
ikhEmpty = IntKeyHashtable 0 0 0 {}

ikhInsert :: !Bool !IntKey !a !*(IntKeyHashtable a) -> (!Bool, !.IntKeyHashtable a)
ikhInsert overide int_key value (IntKeyHashtable ikh_rehash_threshold ikh_nr_of_entries ikh_bitmask ikh_entries)
	| ikh_rehash_threshold<=ikh_nr_of_entries
		= ikhInsert overide int_key value (grow ikh_entries)
	#! hash_value = int_key bitand ikh_bitmask
	   (tree, ikh_entries) = ikh_entries![hash_value]
	   (is_new, tree)
	   		= iktUInsert overide int_key value tree 
	   ikh_entries = {ikh_entries & [hash_value] = tree}
	| is_new
		= (is_new, (IntKeyHashtable ikh_rehash_threshold (ikh_nr_of_entries+1) ikh_bitmask ikh_entries))
	= (is_new, (IntKeyHashtable ikh_rehash_threshold ikh_nr_of_entries ikh_bitmask ikh_entries))

grow :: !{!*(IntKeyTree a)} -> .(IntKeyHashtable a)
grow old_entries
	#! size
			= size old_entries
	   new_size
	   		= if (size==0) 2 (2*size)
	   new_entries
	   		= { IKT_Leaf \\ i<-[1..new_size] }
	   ikh
	   		= (IntKeyHashtable ((new_size*screw)/100) 0 (new_size-1) new_entries)
	   (_, ikh)
	   		= iFoldSt rehashTree 0 size (old_entries, ikh)
	= ikh
  where
	rehashTree :: !Int (!{!*IntKeyTree a}, !*IntKeyHashtable a)
				-> (!{!*IntKeyTree a}, !*IntKeyHashtable a)
	rehashTree index (old_entries, ikh)
		#! (tree, old_entries)
				= replace old_entries index IKT_Leaf
		   list
		   		= iktFlatten tree
		   ikh
		   		= foldSt (\(key, value) ikh -> snd (ikhInsert False key value ikh)) list ikh
		= (old_entries, ikh)

ikhInsert` :: !Bool !IntKey !a !*(IntKeyHashtable a) -> .IntKeyHashtable a
ikhInsert` overide int_key value ikh
	= snd (ikhInsert overide int_key value ikh)

ikhSearch :: !IntKey !(IntKeyHashtable a) -> .Optional a
ikhSearch int_key (IntKeyHashtable _ _ ikh_bitmask ikh_entries)
	| size ikh_entries==0
		= No
	= iktSearch int_key ikh_entries.[int_key bitand ikh_bitmask]

ikhSearch` :: !IntKey !(IntKeyHashtable a) -> a
ikhSearch` int_key (IntKeyHashtable _ _ ikh_bitmask ikh_entries)
	| size ikh_entries==0
		= abort "ikhSearch`: key not found"
	= iktSearch` int_key ikh_entries.[int_key bitand ikh_bitmask]

ikhUSearch :: !IntKey !*(IntKeyHashtable a) -> (!.Optional a, !*IntKeyHashtable a)
ikhUSearch int_key (IntKeyHashtable ikh_rehash_threshold ikh_nr_of_entries ikh_bitmask ikh_entries)
	| size ikh_entries==0
		= (No, IntKeyHashtable ikh_rehash_threshold ikh_nr_of_entries ikh_bitmask ikh_entries)
	# hash_value = int_key bitand ikh_bitmask
	  (ikt, ikh_entries) = ikh_entries![hash_value]
	  (opt_result, ikt)
			= iktUSearch int_key ikt
	  ikh_entries = {ikh_entries & [hash_value] = ikt}
	= (opt_result, (IntKeyHashtable ikh_rehash_threshold ikh_nr_of_entries ikh_bitmask ikh_entries))

iktUInsert :: !Bool !IntKey !a !*(IntKeyTree a) -> (!Bool, !.IntKeyTree a)
iktUInsert overide int_key value IKT_Leaf
	= (True, IKT_Node int_key value IKT_Leaf IKT_Leaf)
iktUInsert overide int_key value (IKT_Node key2 value2 left right)
	| int_key<key2
		# (is_new, left`)
				= iktUInsert overide int_key value left
		= (is_new, IKT_Node key2 value2 left` right)
	| int_key>key2
		# (is_new, right`)
				= iktUInsert overide int_key value right
		= (is_new, IKT_Node key2 value2 left right`)
	| overide
		= (False, IKT_Node int_key value left right)
	= (False, IKT_Node key2 value2 left right)

iktFlatten :: !(IntKeyTree a) -> [(IntKey, a)]
iktFlatten ikt
	= flatten ikt []
  where
	flatten IKT_Leaf accu
		= accu
	flatten (IKT_Node int_key value left right) accu
		= flatten left [(int_key, value) : flatten right accu]

iktUSearch :: !IntKey !*(IntKeyTree a) -> (!.Optional a,!.IntKeyTree a)
iktUSearch int_key IKT_Leaf
	= (No, IKT_Leaf)
iktUSearch int_key (IKT_Node key2 value left right)
	| int_key<key2
		# (opt_result, left)
			= iktUSearch int_key left
		= (opt_result, IKT_Node key2 value left right)
	| int_key>key2
		# (opt_result, right)
			= iktUSearch int_key right
		= (opt_result, IKT_Node key2 value left right)
	# (_, yes_value)
			= yes value
	= (yes_value, IKT_Node key2 value left right)

yes :: !x -> (!Bool, !.Optional x) // to minimize allocation
yes value = (True, Yes value)
		
iktSearch :: !IntKey !(IntKeyTree a) -> .Optional a
iktSearch int_key IKT_Leaf
	= No
iktSearch int_key (IKT_Node key2 value left right)
	| int_key<key2
		= iktSearch int_key left
	| int_key>key2
		= iktSearch int_key right
	= Yes value
		
iktSearch` :: !IntKey !(IntKeyTree a) -> a
iktSearch` int_key (IKT_Node key2 value left right)
	| int_key<key2
		= iktSearch` int_key left
	| int_key>key2
		= iktSearch` int_key right
	= value
iktSearch` int_key IKT_Leaf
	= abort "iktSearch`: key not found"
		
instance toString (IntKeyTree a) | toString a
  where
	toString ikt
		# list
			= iktFlatten ikt
		= listToString "," list


listToString _ []
	= "[]"
listToString del l
	= "["+++lts l
  where
	lts [a]
		= toString a+++"]"
	lts [h:t]
		= toString h+++del+++lts t

instance toString {!a} | toString a
  where
	toString arr
		# list
			= arrayToList arr
		= listToString " , " list
	  where
		arrayToList :: {!a} -> [a]
		arrayToList arr = [el \\ el<-:arr]
		
instance toString (IntKeyHashtable a) |toString a
  where
	toString (IntKeyHashtable ikh_rehash_threshold ikh_nr_of_entries ikh_bitmask ikh_entries)
		= "(IKH "+++toString ikh_rehash_threshold+++" "+++toString ikh_nr_of_entries
			+++" "+++toString ikh_bitmask+++" "+++toString ikh_entries

instance toString (a, b) | toString a & toString b
  where
	toString (a, b)
		= "("+++toString a+++","+++toString b+++")"
		