implementation module StdArrayExtensions

import StdEnv

// extensions for StdArray

createStrictArr :: !Int !a -> .{!a}
createStrictArr size el
	= createArray size el

createLazyArr :: !Int !a -> .{a}
createLazyArr size el
	= createArray size el

createString :: !Int !Char -> .String
createString size el
	= createArray size el

createUnboxedIntArr :: !Int !Int -> .{#Int}
createUnboxedIntArr size el
	= createArray size el

createUnboxedRealArr :: !Int !Real -> .{#Real}
createUnboxedRealArr size el
	= createArray size el

class updateArrElt a :: !(.e -> .e) !Int !*(a .e) -> *(a .e)

instance updateArrElt {}
  where
	updateArrElt f index array
		# (e,array) = array![index]
		= {array & [index] = f e}

instance updateArrElt {!}
  where
	updateArrElt f index array
		# (e,array) = array![index]
		= {array & [index] = f e}

class accArrElt a :: !(.e -> (!.x, !.e)) !Int !*(a .e) -> (!.x, !*(a .e))

instance accArrElt {}
  where
	accArrElt f index array
		# (e,array) = array![index]
		  (x,e) = f e
		= (x, {array & [index] = e})

instance accArrElt {!}
  where
	accArrElt f index array
		# (e,array) = array![index]
		  (x,e) = f e
		= (x, {array & [index] = e})

findlArrElt pred array i
	:== findl array i
  where
	findl array i
		| i>=size array || pred array.[i]
			= i
		= findl array (i+1)

findrArrElt pred array i
	:== findr array i
  where
	findr array i
		| i<0 || pred array.[i]
			= i
		= findr array (i-1)

createStrictArrIncFoldSt :: !Int !(Int .st -> (.a, .st)) !.st -> (!.{!.a}, !.st)
createStrictArrIncFoldSt size create_element st
	| size<0
		= abort "createStrictArrIncFoldSt: called with negative size parameter\n"
	# new_array = createUninitializedStrictArray size
	= createStrictArrIncFoldSt_loop 0 size create_element new_array st
	where
		createStrictArrIncFoldSt_loop :: !Int !Int !(Int .st -> (.a, .st)) !*{!.a} !.st -> (!.{!.a.}, !.st)
		createStrictArrIncFoldSt_loop frm to create_element new_array st
			| frm<>to
				# (new_element, st) = create_element frm st
				# new_array = {new_array & [frm] = new_element}
				= createStrictArrIncFoldSt_loop (frm+1) to create_element new_array st
				= (new_array, st)

createStrictArrDecFoldSt :: !Int !(Int .st -> (.a, .st)) !.st -> (!.{!.a}, !.st)
createStrictArrDecFoldSt size create_element st
	| size<0
		= abort "createStrictArrDecFoldSt: called with negative size parameter\n"
	# new_array = createUninitializedStrictArray size
	= createStrictArrDecFoldSt_loop (size-1) create_element new_array st
	where
		createStrictArrDecFoldSt_loop :: !Int !(Int .st -> (.a, .st)) !*{!.a} !.st -> (!.{!.a.}, !.st)
		createStrictArrDecFoldSt_loop frm create_element new_array st
			| frm>=0
				# (new_element, st) = create_element frm st
				# new_array = {new_array & [frm] = new_element}
				= createStrictArrDecFoldSt_loop (frm-1) create_element new_array st
				= (new_array, st)

createUninitializedStrictArray :: !Int -> *{!.e}
createUninitializedStrictArray size
	= code inline {
		create_array_ _ 1 0
	}
