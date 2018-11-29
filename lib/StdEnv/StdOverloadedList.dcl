definition module StdOverloadedList

import StdClass,StdBool,StdMisc,StdStrictLists

instance == [!a] | == a
instance == [a!] | == a
instance == [!a!] | == a
instance == [#a] | == a & UList a
	special
		a=Int; a=Char; a=Real
instance == [#a!] | == a & UTSList a
	special
		a=Int; a=Char; a=Real

instance < [!a] | Ord a
instance < [a!] | Ord a
instance < [!a!] | Ord a
instance < [#a] | Ord a & UList a
	special
		a=Int; a=Char; a=Real
instance < [#a!] | Ord a & UTSList a
	special
		a=Int; a=Char; a=Real

instance length [!]
instance length [ !]
instance length [!!]

Length :: !u:(l v:e) -> Int | List l e,[u<=v]
	special
		l=[]; l=[!]; l=[ !]; l=[!!]
		l=[#],e=Int ; l=[#!],e=Int
		l=[#],e=Char; l=[#!],e=Char
		l=[#],e=Real; l=[#!],e=Real

LengthM xs :== length_ 0 xs
	where
		length_ n [|x:xs] = length_ (inc n) xs
		length_ n [|]     = n

instance % [!a]
instance % [a!]
instance % [!a!]
instance % [#e] | UList e
	special e=Int; e=Char; e=Real		
instance % [#e!] | UTSList e
	special e=Int; e=Char; e=Real

instance toString [!x] | toChar x
instance toString [x!] | toChar x
instance toString [!x!] | toChar x
instance toString [#x] | toChar x & UList x
	special x=Int; x=Char
instance toString [#x!] | toChar x & UTSList x
	special x=Int; x=Char

instance fromString [!x] | fromChar x
instance fromString [x!] | fromChar x
instance fromString [!x!] | fromChar x
instance fromString [#x] | fromChar x & UList x
	special x=Int; x=Char
instance fromString [#x!] | fromChar x & UTSList x
	special x=Int; x=Char

(!!|) infixl 9 :: !u:(l v:e) !Int -> v:e | List l e,[u<=v]
	special
		l=[]; l=[!]; l=[ !]; l=[!!]
		l=[#],e=Int ; l=[#!],e=Int
		l=[#],e=Char; l=[#!],e=Char
		l=[#],e=Real; l=[#!],e=Real		

(!!$) infixl 9
(!!$) l n
	:== (!!|) l n
	where
	(!!|) [|] _
		= subscript_error
	(!!|) list i
		= index list i
		where
			index ::!u:(l v:e) !Int -> v:e | List l e,[u<=v]
			index [|hd:tl] 0
				= hd
			index [|hd:tl] n
				= index tl (n - 1)
			index [|] _
				= subscript_error

	subscript_error => abort "Subscript error in !!||,index too large"

(++|) infixr 5 :: !u:(l v:e) u:(l v:e) -> u:(l v:e) | List l e,[u<=v]
//(++|) infixr 5 :: !.(l .e) u:(l .e) -> u:(l .e) | List l e
	special
		l=[]; l=[!]; l=[ !]; l=[!!]
		l=[#],e=Int ; l=[#!],e=Int
		l=[#],e=Char; l=[#!],e=Char
		l=[#],e=Real; l=[#!],e=Real		

(++$) infixr 5
(++$) l1 l2 :== (++||) l1 l2
	where
		(++||) [|hd:tl]	list	= [|hd:tl ++|| list]
		(++||) nil 		list	= list

Flatten l :== flatten l
	where
		flatten [|h:t]	= h ++| flatten t
		flatten [|]		= [|]

FlattenM l :== flatten l
	where
		flatten [|h:t]	= h ++$ flatten t
		flatten [|]		= [|]

IsEmpty :: !u:(l v:e) -> .Bool | List l e,[u<=v]
	special
		l=[]; l=[!]; l=[ !]; l=[!!]
		l=[#],e=Int ; l=[#!],e=Int
		l=[#],e=Char; l=[#!],e=Char
		l=[#],e=Real; l=[#!],e=Real

IsEmptyM l :== isEmpty l
	where
		isEmpty	[|] = True
		isEmpty	_  = False

Hd :: !u:(l v:e) -> v:e | List l e,[u<=v]
	special
		l=[]; l=[!]; l=[ !]; l=[!!]
		l=[#],e=Int ; l=[#!],e=Int
		l=[#],e=Char; l=[#!],e=Char
		l=[#],e=Real; l=[#!],e=Real

HdM l :== hd_ l
	where
		hd_ [|a:x]	= a
		hd_ [|]		= abort "HdM of [|]"

Tl :: !u:(l v:e) -> u:(l v:e) | List l e,[u<=v]
	special
		l=[]; l=[!]; l=[ !]; l=[!!]
		l=[#],e=Int ; l=[#!],e=Int
		l=[#],e=Char; l=[#!],e=Char
		l=[#],e=Real; l=[#!],e=Real

TlM l :== tl_ l
	where
		tl_ [|a:x]	= x
		tl_ [|]		= abort "TlM of [|]"

Last :: !u:(l v:e) -> v:e | List l e,[u<=v]
	special
		l=[]; l=[!]; l=[ !]; l=[!!]
		l=[#],e=Int ; l=[#!],e=Int
		l=[#],e=Char; l=[#!],e=Char
		l=[#],e=Real; l=[#!],e=Real

LastM l :== last_ l
	where
		last_ [|a]	= a
		last_ [|a:tl]= last_ tl
		last_ [|]	= abort "LastM of [|]"

Init :: !u:(l v:e) -> .(l v:e) | List l e,[u<=v]
	special
		l=[]; l=[!]; l=[ !]; l=[!!]
		l=[#],e=Int ; l=[#!],e=Int
		l=[#],e=Char; l=[#!],e=Char
		l=[#],e=Real; l=[#!],e=Real

InitM l :== init_ l
	where
		init_ [|]     = [|]
		init_ [|x]    = [|]
		init_ [|x:xs] = [|x: init_ xs]

Take :: !Int u:(l v:e) -> u:(l v:e) | List l e,[u<=v]
	special
		l=[]; l=[!]; l=[ !]; l=[!!]
		l=[#],e=Int ; l=[#!],e=Int
		l=[#],e=Char; l=[#!],e=Char
		l=[#],e=Real; l=[#!],e=Real

TakeM n l :== if (n<=0) [|] (take n l)
	where
		take n [|x:xs]
			| n<=1
				= [|x]
				= [|x:take (n-1) xs]
		take n [|]
			| n<=0
				= [|]
				= [|]

TakeWhile f l :== takeWhile l
	where
		takeWhile [|a:x] | f a	= [|a:takeWhile x]
								= [|]
		takeWhile [|]			= [|]

Drop :: !Int !u:(l v:e) -> u:(l v:e) | List l e,[u<=v]
	special
		l=[]; l=[!]; l=[ !]; l=[!!]
		l=[#],e=Int ; l=[#!],e=Int
		l=[#],e=Char; l=[#!],e=Char
		l=[#],e=Real; l=[#!],e=Real

DropM n l :== drop n l
	where
		drop n xs | n<=0 = xs
		drop n [|x:xs] = drop (n - 1) xs
		drop n [|] = [|]

DropWhile f l :== dropWhile l
	where
		dropWhile cons=:[|a:x] | f a	= dropWhile x
										= cons
		dropWhile []					= [|]

Span p l :== span l
	where
		span list=:[|x:xs]
			| p x
				# (ys,zs) = span xs
				= ([|x:ys],zs)
				= ([|],list)
		span [|]
			= ([|], [|])

//Filter :: (e -> .Bool) !.(l e) -> .(l e) | List l e
Filter f l :== filter l
where
	filter [|a:x]
		| f a
			= [|a:filter x]
			= filter x
	filter nil = nil

FilterM f l :== filter l
where
	filter [|a:x]
		| f a
			= [|a:filter x]
			= filter x
	filter [|] = [|]

Reverse :: !u:(l v:e) -> u:(l v:e) | List l e,[u<=v]
	special
		l=[]; l=[!]; l=[ !]; l=[!!]
		l=[#],e=Int ; l=[#!],e=Int
		l=[#],e=Char; l=[#!],e=Char
		l=[#],e=Real; l=[#!],e=Real

//ReverseM :: !.(a .b) -> .(c .b) | List a b & List c b
ReverseM list :== reverse_ list [|]
where
	reverse_ [|hd:tl] list	= reverse_ tl [|hd:list]
	reverse_ [|] list		= list

Insert r x l :== insert_ x l
where
	insert_ x ls=:[|y : ys]
		| r x y		= 	[|x : ls]
					=	[|y : insert_ x ys]
	insert_ x [] 	= 	[|x]

InsertAt :: !Int v:e u:(l v:e) -> u:(l v:e) | List l e,[u<=v]
	special
		l=[]; l=[!]; l=[ !]; l=[!!]
		l=[#],e=Int ; l=[#!],e=Int
		l=[#],e=Char; l=[#!],e=Char
		l=[#],e=Real; l=[#!],e=Real

InsertAtM n l1 l2 :== insertAt_ n l1 l2
	where
		insertAt_ 0 x ys	= [|x:ys]
		insertAt_ _ x [|] = [|x]
		insertAt_ n x [|y:ys] = [|y : insertAt_ (n-1) x ys]

RemoveAt :: !Int !u:(l v:e) -> u:(l v:e) | List l e,[u<=v]
	special
		l=[]; l=[!]; l=[ !]; l=[!!]
		l=[#],e=Int ; l=[#!],e=Int
		l=[#],e=Char; l=[#!],e=Char
		l=[#],e=Real; l=[#!],e=Real

RemoveAtM n l :== removeAt_ n l
	where
		removeAt_ 0 [|y : ys] = ys
		removeAt_ n [|y : ys] = [|y : removeAt_ (n-1) ys]
		removeAt_ n [|] = [|]

UpdateAt :: !Int v:e !u:(l v:e) -> u:(l v:e) | List l e,[u<=v]
	special
		l=[]; l=[!]; l=[ !]; l=[!!]
		l=[#],e=Int ; l=[#!],e=Int
		l=[#],e=Char; l=[#!],e=Char
		l=[#],e=Real; l=[#!],e=Real

UpdateAtM n x l :== updateAt_ n x l
	where
		updateAt_ 0 x [|] = [|]
		updateAt_ 0 x [|y:ys] = [|x:ys]
		updateAt_ _ x [|] = [|]
		updateAt_ n x [|y:ys] = [|y : updateAt_ (n-1) x ys]

SplitAt :: !Int u:(l v:e) -> (.(l v:e),u:(l v:e)) | List l e,[u<=v]
	special
		l=[]; l=[!]; l=[ !]; l=[!!]
		l=[#],e=Int ; l=[#!],e=Int
		l=[#],e=Char; l=[#!],e=Char
		l=[#],e=Real; l=[#!],e=Real

SplitAtM n l :== splitAt_ n l
	where
		splitAt_ 0     xs	=	([|],xs)
		splitAt_ _     [|]	=	([|],[|])
		splitAt_ n [|x:xs]	=	([|x:xs`],xs``)
			where
				(xs`,xs``) = splitAt_ (n-1) xs

Map f l :== map_ l
	where
		map_ [|a:x]	= [|f a:map_ x]
		map_ [|]	= [|]

MapM f l :== map_ l
	where
		map_ [|a:x]	= [|f a:map_ x]
		map_ [|]	= [|]

Iterate f x	:== iterate_ x
	where
		iterate_ x = [|x:iterate_ (f x)]

IndexList :: !(l e) -> (l Int) | List l e & List l Int
	special
		l=[]; l=[!]; l=[ !]; l=[!!]
		l=[#],e=Int ; l=[#!],e=Int
		l=[#],e=Char; l=[#!],e=Char
		l=[#],e=Real; l=[#!],e=Real

IndexListM x :== indexList_ 0 x
where
	indexList_ n [|a:x]	= [|n:indexList_ (n+1) x]
	indexList_ n [|]	= [|]

Repeatn :: !Int e -> .(l e) | List l e
	special
		l=[]; l=[!]; l=[ !]; l=[!!]
		l=[#],e=Int ; l=[#!],e=Int
		l=[#],e=Char; l=[#!],e=Char
		l=[#],e=Real; l=[#!],e=Real

RepeatnM n a :== repeatn_ n a
	where
		repeatn_ 0 _ = [|]
		repeatn_ n a = [|a:repeatn_ (dec n) a]

Repeat x :== cons
	where
		cons = [|x:cons]

Unzip:: !u:(l v:(.a,.b)) -> .(u:(l .a),u:(l .b)) | List l a & List l b & List l (a,b), [u<=v]
	special
		l=[]; l=[!]; l=[ !]; l=[!!]

UnzipM l :== unzip_ l
	where
		unzip_ [|] = ([|], [|])
		unzip_ [|(x,y) : xys] = ([|x : xs],[|y : ys])
			where
				(xs,ys) = unzip_ xys

Zip2 :: !u:(l .a) u:(l .b) -> u:(l v:(.a,.b)) | List l a & List l b & List l (a,b), [u<=v]
	special
		l=[]; l=[!]; l=[ !]; l=[!!]

Zip2M l1 l2 :== zip2_ l1 l2
	where
		zip2_ [|a:as] [|b:bs] = [|(a,b):zip2_ as bs]
		zip2_ as bs			= [|]

Zip :: !.(!u:(l .a),u:(l .b)) -> u:(l v:(.a,.b)) | List l a & List l b & List l (a,b), [u<=v]
	special
		l=[]; l=[!]; l=[ !]; l=[!!]

ZipM t :== zip_ t
	where
		zip_ (x,y) = Zip2M x y

Diag3 xs ys zs :== [| (x,y,z) \\ ((x,y),z) <- Diag2 (Diag2 xs  ys) zs ]

Diag2 l1 l2 :== diag2_ l1 l2
	where
		diag2_ [|] ys = [|]
		diag2_ xs [|] = [|]
		diag2_ xs ys = [| (ae,be) \\ (a,b) <- takeall xs [|] ys [|], ae <|- a & be <|- b ]
		where
			takeall xin xout yin yout
			| morex&&morey	= [|(nxout,   nyout) : takeall nxin nxout nyin     nyout ]
			| morey			= [|( xout,Tl nyout) : takeall  xin  xout nyin (Tl nyout)]
			| morex			= [|(nxout,    yout) : takeall nxin nxout  yin      yout ]
							= shift xout yout
			where
				(morex,nxin,nxout) = takexnext xin xout
				(morey,nyin,nyout) = takeynext yin yout
		
				takexnext [|x:xs] accu	= (True, xs,[|x:accu])
				takexnext [|]     accu 	= (False,[|],accu)
		
				takeynext [|y:ys] accu	= (True, ys,accu++|[|y])
				takeynext [|]     accu	= (False,[|],accu)
			
				shift xout [|_:ys]	= [|(xout,ys): shift xout ys]
				shift _    [|] 		= [|]

Foldl op r l
	:==	foldl r l
	where
		foldl r [|]		= r
		foldl r [|a:x]	= foldl (op r a) x

Foldr op r l
	:== foldr l
	where
		foldr [|] = r
		foldr [|a:x] = op a (foldr x)

Scan op r l :== scan_ r l
	where
		scan_ r [|a:x]	= [|r:scan_ (op r a) x]
		scan_ r [|]	= [|r]

And :: !u:(l v:Bool) -> Bool | List l Bool,[u<=v]
	special
		l=[]; l=[!]; l=[ !]; l=[!!]; l=[#]; l=[#!]

Or :: !u:(l v:Bool) -> Bool | List l Bool,[u<=v]
	special
		l=[]; l=[!]; l=[ !]; l=[!!]; l=[#]; l=[#!]

Any p l :== any_ l
	where		
		any_ [|]
			= False
		any_ [|b : tl]
			= p b || any_ tl

All p l :== all_ l
	where
		all_ [|]
			=	True
		all_ [|b : tl]
			= p b && all_ tl

IsMember:: e !.(l e) -> Bool | List l e & Eq e
	special
		l=[],e=Int; l=[],e=Char; l=[],e=Real;
		l=[!],e=Int; l=[!],e=Char; l=[!],e=Real;
		l=[ !],e=Int; l=[!],e=Char; l=[ !],e=Real;
		l=[!!],e=Int; l=[!!],e=Char; l=[!!],e=Real;
		l=[#],e=Int ; l=[#],e=Char; l=[#],e=Real
		l=[#!],e=Int; l=[#!],e=Char; l=[#!],e=Real

IsMemberM x l :== isMember_ x l
	where
		isMember_ x [|hd:tl] = hd==x || isMember_ x tl
		isMember_ x [|]	= False

IsAnyMember	:: !(l e) !(l e) -> Bool | List l e & Eq e
	special
		l=[],e=Int; l=[],e=Char; l=[],e=Real;
		l=[!],e=Int; l=[!],e=Char; l=[!],e=Real;
		l=[ !],e=Int; l=[!],e=Char; l=[ !],e=Real;
		l=[!!],e=Int; l=[!!],e=Char; l=[!!],e=Real;
		l=[#],e=Int ; l=[#],e=Char; l=[#],e=Real
		l=[#!],e=Int; l=[#!],e=Char; l=[#!],e=Real

IsAnyMemberM l1 l2 :== isAnyMember_ l1 l2
	where
		isAnyMember_ [|x:xs] list = IsMemberM x list || isAnyMember_ xs list
		isAnyMember_ [|] list = False

RemoveMember:: e !u:(l e) -> u:(l e) | List l e & Eq e
	special
		l=[],e=Int; l=[],e=Char; l=[],e=Real;
		l=[!],e=Int; l=[!],e=Char; l=[!],e=Real;
		l=[ !],e=Int; l=[!],e=Char; l=[ !],e=Real;
		l=[!!],e=Int; l=[!!],e=Char; l=[!!],e=Real;
		l=[#],e=Int ; l=[#],e=Char; l=[#],e=Real
		l=[#!],e=Int; l=[#!],e=Char; l=[#!],e=Real

RemoveMemberM e l :== removeMember_ e l
	where
		removeMember_ e [|a:as]
			| a==e		= as
						= [|a:removeMember_ e as]
		removeMember_ e [|] = [|]	

RemoveMembers :: !u:(l e) !.(l e) -> u:(l e) | List l e & Eq e
	special
		l=[],e=Int; l=[],e=Char; l=[],e=Real;
		l=[!],e=Int; l=[!],e=Char; l=[!],e=Real;
		l=[ !],e=Int; l=[!],e=Char; l=[ !],e=Real;
		l=[!!],e=Int; l=[!!],e=Char; l=[!!],e=Real;
		l=[#],e=Int ; l=[#],e=Char; l=[#],e=Real
		l=[#!],e=Int; l=[#!],e=Char; l=[#!],e=Real

RemoveMembersM x l :== removeMembers_ x l
	where
		removeMembers_ x [|]	= x
		removeMembers_ x [|b:y]	= removeMembers_ (RemoveMemberM b x) y

RemoveDup :: !.(l e) -> .(l e) | List l e & Eq e
	special
		l=[],e=Int; l=[],e=Char; l=[],e=Real;
		l=[!],e=Int; l=[!],e=Char; l=[!],e=Real;
		l=[ !],e=Int; l=[!],e=Char; l=[ !],e=Real;
		l=[!!],e=Int; l=[!!],e=Char; l=[!!],e=Real;
		l=[#],e=Int ; l=[#],e=Char; l=[#],e=Real
		l=[#!],e=Int; l=[#!],e=Char; l=[#!],e=Real

RemoveDupM l :== removeDup_ l
	where
		removeDup_ [|x:xs] = [|x:removeDup_ (Filter ((<>) x) xs)]
		removeDup_ _      = [|]

RemoveIndex :: !e !u:(l e) -> (Int,u:(l e)) | List l e & Eq e
	special
		l=[],e=Int; l=[],e=Char; l=[],e=Real;
		l=[!],e=Int; l=[!],e=Char; l=[!],e=Real;
		l=[ !],e=Int; l=[!],e=Char; l=[ !],e=Real;
		l=[!!],e=Int; l=[!!],e=Char; l=[!!],e=Real;
		l=[#],e=Int ; l=[#],e=Char; l=[#],e=Real
		l=[#!],e=Int; l=[#!],e=Char; l=[#!],e=Real

RemoveIndexM e xs :== removei e xs 0
	where
		removei e [|x:xs] i
			| x==e
				= (i,xs)
				= (j,[|x:res])
				with
					(j,res) = removei e xs (inc i)
		removei e [|] i = abort "Error in RemoveIndexM: element not found"

Limit :: !.(l e) -> e | List l e & == e
	special
		l=[],e=Int; l=[],e=Real;
		l=[!],e=Int; l=[!],e=Real;
		l=[ !],e=Int; l=[ !],e=Real;
		l=[!!],e=Int; l=[!!],e=Real;
		l=[#],e=Int ; l=[#],e=Real
		l=[#!],e=Int; l=[#!],e=Real

LimitM l :== limit_
	where
		limit_ [|a:cons=:[|b:x]]
			| a==b		= a
						= limit_ cons
		limit_ _ 		= abort "incorrect use of LimitM"

Sum:: !.(l e) -> e |  List l e & + , zero e
	special
		l=[],e=Int; l=[],e=Real;
		l=[!],e=Int; l=[!],e=Real;
		l=[ !],e=Int; l=[ !],e=Real;
		l=[!!],e=Int; l=[!!],e=Real;
		l=[#],e=Int ; l=[#],e=Real
		l=[#!],e=Int; l=[#!],e=Real

SumM xs :== accsum zero xs
where
	accsum n [|x:xs] = accsum (n + x) xs
	accsum n [|]     = n

Prod :: !.(l e) -> e | List l e & * e & one e
	special
		l=[],e=Int; l=[],e=Real;
		l=[!],e=Int; l=[!],e=Real;
		l=[ !],e=Int; l=[ !],e=Real;
		l=[!!],e=Int; l=[!!],e=Real;
		l=[#],e=Int ; l=[#],e=Real
		l=[#!],e=Int; l=[#!],e=Real

ProdM xs :== accprod one xs
where
	accprod n [|x:xs] = accprod (n * x) xs
	accprod n [|]     = n

Avg :: !.(l e) -> e | List l e & / e & IncDec e
	special
		l=[],e=Int; l=[],e=Real;
		l=[!],e=Int; l=[!],e=Real;
		l=[ !],e=Int; l=[ !],e=Real;
		l=[!!],e=Int; l=[!!],e=Real;
		l=[#],e=Int ; l=[#],e=Real
		l=[#!],e=Int; l=[#!],e=Real

AvgM l :== avg_ l
	where
	avg_ [|] = abort "avg called with empty list"
	avg_ x  = accavg zero zero x
		where
		accavg n nelem [|x:xs] = accavg (n + x) (inc nelem) xs
		accavg n nelem [|]     = n / nelem
