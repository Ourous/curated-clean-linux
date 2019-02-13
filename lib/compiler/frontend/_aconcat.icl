implementation module _aconcat

import StdArray,StdInt,StdEnum, StdList

arrayConcat a1 a2
	:==r2
where
	r2={r1 & [i+s1]=a2.[i] \\ i<-[0..s2-1]}
	r1={r0 & [i]=a1.[i] \\ i<-[0..s1-1]}
	r0=_createArray (s1+s2)
	s1=size a1
	s2=size a2

arrayPlusList a l
	:==r2
where
	r2={r1 & [i+s1]=e \\ i<-[0..s2-1] & e<-l}
	r1={r0 & [i]=a.[i] \\ i<-[0..s1-1]}
	r0=_createArray (s1+s2)
	s1=size a
	s2=length l

arrayPlusRevList a l
	:==r2
where
	r2={r1 & [sr-i]=e \\ i<-[1..s2] & e<-l}
	r1={r0 & [i]=a.[i] \\ i<-[0..s1-1]}
	r0=_createArray sr
	sr=s1+s2
	s1=size a
	s2=length l

arrayCopyBegin a s
	:== copy_elements a r0 0
where
	r0=_createArray s
	copy_elements a1 a2 i
		| i<size a2
			# (e,a1) = a1![i]
			= copy_elements a1 {a2 & [i]=e} (i+1)
			= (a2,a1)

arrayCopy a
	:== arrayCopyBegin a1 s
	where
		(s, a1) = usize a

arrayAndElementsCopy copy_element_function array
	:== copy array1 (_createArray n) 0 n
	where
		(n, array1) = usize array

		copy array array_copy i n
			| i == n
				= (array_copy, array)
				# (element, array) = array![i]
				# (copy_element, element)
					=	copy_element_function element
				# array = {array & [i] = element}
				= copy array {array_copy & [i] = copy_element} (i+1) n
