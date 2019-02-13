implementation module Database.Native

import StdArray
import StdBool
import StdFile
import StdInt
import StdList
import StdOverloadedList
import StdString
import StdTuple

from Data.Func import $, hyperstrict
import Data.Functor
import Data.Maybe
import Text.GenJSON

:: *NativeDB v a :== *{!*Entry v a}

instance == Index where == (Index a) (Index b) = a == b
instance < Index where < (Index a) (Index b) = a < b

newDB :: ![v] -> *NativeDB v a
newDB vs = {{value=hyperstrict v,included=True,annotations=[!!]} \\ v <- vs}

saveDB :: !*(NativeDB v a) !*File -> *(!*NativeDB v a, !*File) | JSONEncode{|*|} v
saveDB db f
# (s,db) = usize db
# f = f <<< toString s <<< "\n"
= loop 0 (s-1) db f
where
	loop :: !Int !Int !*{!*Entry v a} !*File -> *(*{!*Entry v a}, !*File) | JSONEncode{|*|} v
	loop i s es f
	| i > s = (es,f)
	# (val,es) = es![i].value
	# f = f <<< toJSON val <<< '\n'
	= loop (i+1) s es f

openDB :: !*File -> *(!Maybe (*NativeDB v a), !*File) | JSONDecode{|*|} v
openDB f
# (line,f) = freadline f
# n = toInt (line % (0, size line - 2))
# (es,f) = loop n f
= case es of
	Nothing -> (Nothing, f)
	Just es -> (Just $ newDB es, f)
where
	loop :: !Int !*File -> *(Maybe [v], !*File) | JSONDecode{|*|} v
	loop 0 f = (Just [], f)
	loop n f
	# (end,f) = fend f
	| end = (Nothing, f)
	# (line,f) = freadline f
	= case fromJSON $ fromString line of
		Nothing -> (Nothing, f)
		Just e  -> case loop (n-1) f of
			(Nothing, f) -> (Nothing, f)
			(Just es, f) -> (Just [e:es], f)

resetDB :: !*(NativeDB v a) -> *NativeDB v a
resetDB db
# (s,db) = usize db
= upd (s-1) db
where
	upd :: !Int !*{!*Entry v a} -> *{!*Entry v a}
	upd -1 es = es
	upd i  es = upd (i-1) {es & [i].included=True, [i].annotations=[!!]}

allEntries :: !*(NativeDB v a) -> *(![!v!], !*NativeDB v a)
allEntries db
# (s,db) = usize db
= collect (s-1) db
where
	collect :: !Int !*{!*Entry v a} -> *(![!v!], !*{!*Entry v a})
	collect -1 es = ([!!], es)
	collect i  es
	# (r,es) = collect (i-1) es
	# (val,es) = es![i].value
	= ([!val:r!], es)

getEntries :: !*(NativeDB v a) -> *(![!(v, [!a!])!], !*NativeDB v a)
getEntries db
# (s,db) = usize db
= collect (s-1) db
where
	collect :: !Int !*{!*Entry v a} -> *(![!(v, [!a!])!], !*{!*Entry v a})
	collect -1 es = ([!!], es)
	collect i  es
	# (inc,es) = es![i].included
	| not inc = collect (i-1) es
	# (r,es) = collect (i-1) es
	# (val,es) = es![i].value
	# (annots,es) = es![i].annotations
	= ([!(val,annots):r!], es)

getEntriesWithIndices :: !*(NativeDB v a) -> *(![!(Index, v, [!a!])!], !*NativeDB v a)
getEntriesWithIndices db
# (s,db) = usize db
= collect (s-1) db
where
	collect :: !Int !*{!*Entry v a} -> *(![!(Index, v, [!a!])!], !*{!*Entry v a})
	collect -1 es = ([!!], es)
	collect i  es
	# (inc,es) = es![i].included
	| not inc = collect (i-1) es
	# (r,es) = collect (i-1) es
	# (val,es) = es![i].value
	# (annots,es) = es![i].annotations
	= ([!(Index i,val,annots):r!], es)

mapInPlace :: !(Int v -> v) !*(NativeDB v a) -> *(NativeDB v a)
mapInPlace f db
# (s,db) = usize db
= upd 0 s db
where
	//upd :: !Int !Int !*{!Entry v a} -> *{!Entry v a}
	upd i s es
	| i == s = es
	#! (e,es) = es![i]
	#! e & value = hyperstrict $ f i e.value
	= upd (i+1) s {es & [i]=e}

search :: !SearchMode !(v -> (Bool, [!a!])) !*(NativeDB v a) -> *NativeDB v a
search mode f db
# (s,db) = usize db
= upd (s - 1) db
where
	//upd :: (!Int !*{!Entry v a} -> *{!Entry v a})
	upd = case mode of
		Intersect   -> intersect
		AddExcluded -> addExcluded

	intersect -1 es = es
	intersect i  es
	#! (inc,es) = es![i].included
	| not inc = intersect (i-1) es
	#! (val,es) = es![i].value
	#! (annots,es) = es![i].annotations
	#! (inc,new_annots) = f val
	= intersect (i-1) {es & [i].included=inc, [i].annotations=new_annots ++$ annots}

	addExcluded -1 es = es
	addExcluded i  es
	#! (inc,es) = es![i].included
	#! (val,es) = es![i].value
	#! (annots,es) = es![i].annotations
	#! (new_inc,new_annots) = f val
	= addExcluded (i-1) {es & [i].included=inc||new_inc, [i].annotations=new_annots ++$ annots}

searchIndex :: !Index ![!a!] !*(NativeDB v a) -> *NativeDB v a
searchIndex (Index i) new_annots db
# (annots,db) = db![i].annotations
= {db & [i].included=True, [i].annotations=new_annots ++$ annots}

unsearchIndex :: !Index !*(NativeDB v a) -> *NativeDB v a
unsearchIndex (Index i) db = {db & [i].included=False}

searchIndices :: !SearchMode ![(!Index, ![!a!])] !*(NativeDB v a) -> *NativeDB v a
searchIndices mode idxs db = case mode of
	Intersect
		# (s,db) = usize db
		-> upd_intersect 0 (s-1) idxs db
	AddExcluded
		-> foldr upd_addexcluded db idxs
where
	upd_addexcluded :: !(!Index, ![!a!]) !*{!*Entry v a} -> *{!*Entry v a}
	upd_addexcluded (Index i,new_annots) es
	# (annots,es) = es![i].annotations
	= {es & [i].included=True, [i].annotations=new_annots ++$ annots}

	upd_intersect :: !Int !Int ![(!Index, ![!a!])] !*{!*Entry v a} -> *{!*Entry v a}
	upd_intersect i s _  es | i > s = es
	upd_intersect i s [] es = upd_intersect (i+1) s [] {es & [i].included=False}
	upd_intersect i s allidxs=:[(Index idx,new_annots):idxs] es
	| i <> idx = upd_intersect (i+1) s allidxs {es & [i].included=False}
	# (inc,es) = es![i].included
	| not inc = upd_intersect (i+1) s idxs {es & [i].included=False}
	# (annots,es) = es![i].annotations
	= upd_intersect (i+1) s idxs {es & [i].annotations=new_annots ++$ annots}

unsearchIndices :: ![Index] !*(NativeDB v a) -> *NativeDB v a
unsearchIndices idxs db = upd idxs db
where
	upd :: ![Index] !*{!*Entry v a} -> *{!*Entry v a}
	upd [] es = es
	upd [Index i:is] es = upd is {es & [i].included=False}

unsearchIndices` :: !{#Index} !*(NativeDB v a) -> *NativeDB v a
unsearchIndices` idxs db
# (sz,idxs) = usize idxs
= upd (sz-1) idxs db
where
	upd :: !Int !{#Index} !*{!*Entry v a} -> *{!*Entry v a}
	upd -1 _ es = es
	upd i idxs es
	# (Index ei) = idxs.[i]
	= upd (i-1) idxs {es & [ei].included=False}

searchWithIndices :: !(v -> (Bool, ![!a!])) ![Index] !*(NativeDB v a) -> *NativeDB v a
searchWithIndices prop idxs db = upd prop idxs db
where
	upd :: !(v -> (Bool, ![!a!])) ![Index] !*{!*Entry v a} -> *{!*Entry v a}
	upd _ [] es = es
	upd prop [Index i:is] es
	# (val,es) = es![i].value
	= case prop val of
		(False, _)
			-> upd prop is {es & [i].included=False}
		(True, new_annots)
			# (annots,es) = es![i].annotations
			-> upd prop is {es & [i].included=True, [i].annotations=new_annots ++$ annots}

searchWithIndices` :: !(v -> (Bool, ![!a!])) !{#Index} !*(NativeDB v a) -> *NativeDB v a
searchWithIndices` prop idxs db
# (sz,idxs) = usize idxs
= upd prop (sz-1) idxs db
where
	upd :: !(v -> (Bool, ![!a!])) !Int !{#Index} !*{!*Entry v a} -> *{!*Entry v a}
	upd _ -1 _ es = es
	upd prop i idxs es
	# (Index ei) = idxs.[i]
	# (val,es) = es![ei].value
	= case prop val of
		(False, _)
			-> upd prop (i-1) idxs {es & [ei].included=False}
		(True, new_annots)
			# (annots,es) = es![ei].annotations
			-> upd prop (i-1) idxs {es & [ei].included=True, [ei].annotations=new_annots ++$ annots}

getValueByIndex :: !Index !*(NativeDB v a) -> *(!v, !*(NativeDB v a))
getValueByIndex (Index i) db = db![i].value

getAnnotationsByIndex :: !Index !*(NativeDB v a) -> *(![!a!], !*(NativeDB v a))
getAnnotationsByIndex (Index i) db = db![i].annotations

isIndexIncluded :: !Index !*(NativeDB v a) -> *(!Bool, !*(NativeDB v a))
isIndexIncluded (Index i) db = db![i].included

getIndices :: ![Index] !*(NativeDB v a) -> *(![!Entry v a!], !*(NativeDB v a))
getIndices is db = get is db
where
	get :: ![Index] !*{!*Entry v a} -> *(![!Entry v a!], !*{!*Entry v a})
	get [] db = ([!!], db)
	get [Index i:is] db
	# (inc,db) = db![i].included
	# (val,db) = db![i].value
	# (annots,db) = db![i].annotations
	# (es,db) = get is db
	= ([!{included=inc, value=val, annotations=annots}:es!], db)

getValuesByIndices :: ![Index] !*(NativeDB v a) -> *(![!v!], !*(NativeDB v a))
getValuesByIndices is db = get is db
where
	get :: ![Index] !*{!*Entry v a} -> *(![!v!], !*{!*Entry v a})
	get [] db = ([!!], db)
	get [Index i:is] db
	# (val,db) = db![i].value
	# (es,db) = get is db
	= ([!val:es!], db)

getIndices` :: !{#Index} !*(NativeDB v a) -> *(![!Entry v a!], !*(NativeDB v a))
getIndices` idxs db
# (sz,idxs) = usize idxs
= get (sz-1) idxs [!!] db
where
	get :: !Int !{#Index} ![!Entry v a!] !*{!*Entry v a} -> *(![!Entry v a!], !*{!*Entry v a})
	get -1 _ es db = (es,db)
	get i idxs es db
	# (Index ei) = idxs.[i]
	# (inc,db) = db![ei].included
	# (val,db) = db![ei].value
	# (annots,db) = db![ei].annotations
	= get (i-1) idxs [!{included=inc, value=val, annotations=annots}:es!] db

getValuesByIndices` :: !{#Index} !*(NativeDB v a) -> *(![!v!], !*(NativeDB v a))
getValuesByIndices` idxs db
# (sz,idxs) = usize idxs
= get (sz-1) idxs [!!] db
where
	get :: !Int !{#Index} ![!v!] !*{!*Entry v a} -> *(![!v!], !*{!*Entry v a})
	get -1 _ es db = (es,db)
	get i idxs es db
	# (Index ei) = idxs.[i]
	# (val,db) = db![ei].value
	= get (i-1) idxs [!val:es!] db
