implementation module Database.Native

import StdArray
import StdBool
import StdFile
import StdInt
import StdList
import StdString
import StdTuple

from Data.Func import $, hyperstrict
import Data.Functor
import Data.Maybe
import Text.GenJSON

:: *NativeDB v a = DB !*{!Entry v a}

instance == Index where == (Index a) (Index b) = a == b
instance < Index where < (Index a) (Index b) = a < b

newDB :: ![v] -> *NativeDB v a
newDB vs = DB {{value=hyperstrict v,included=True,annotations=[]} \\ v <- vs}

saveDB :: !*(NativeDB v a) !*File -> *(!*NativeDB v a, !*File) | JSONEncode{|*|} v
saveDB (DB db) f
# (s,db) = usize db
# f = f <<< toString s <<< "\n"
# (db,f) = loop 0 (s-1) db f
= (DB db, f)
where
	loop :: !Int !Int !*{!Entry v a} !*File -> *(*{!Entry v a}, !*File) | JSONEncode{|*|} v
	loop i s es f
	| i > s = (es,f)
	# (e,es) = es![i]
	# f = f <<< toJSON e.value <<< '\n'
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
resetDB (DB db)
# (s,db) = usize db
# db = upd (s-1) db
= DB db
where
	upd :: !Int !*{!Entry v a} -> *{!Entry v a}
	upd -1 es = es
	upd i  es
	# (e,es) = es![i]
	= upd (i-1) {es & [i]={e & included=True, annotations=[]}}

allEntries :: !*(NativeDB v a) -> *(![v], !*NativeDB v a)
allEntries (DB db)
# (s,db) = usize db
# (es,db) = collect (s-1) db
= (es, DB db)
where
	collect :: !Int !*{!Entry v a} -> *(![v], !*{!Entry v a})
	collect -1 es = ([], es)
	collect i  es
	# (e,es) = es![i]
	# (r,es) = collect (i-1) es
	= ([e.value:r], es)

getEntries :: !*(NativeDB v a) -> *(![(v, [a])], !*NativeDB v a)
getEntries (DB db)
# (s,db) = usize db
# (es,db) = collect (s-1) db
= (es,DB db)
where
	collect :: !Int !*{!Entry v a} -> *(![(v, [a])], !*{!Entry v a})
	collect -1 es = ([], es)
	collect i  es
	# (e,es) = es![i]
	# (r,es) = collect (i-1) es
	= (if e.included [(e.value,e.annotations):r] r, es)

getEntriesWithIndices :: !*(NativeDB v a) -> *(![(Index, v, [a])], !*NativeDB v a)
getEntriesWithIndices (DB db)
# (s,db) = usize db
# (es,db) = collect (s-1) db
= (es,DB db)
where
	collect :: !Int !*{!Entry v a} -> *(![(Index, v, [a])], !*{!Entry v a})
	collect -1 es = ([], es)
	collect i  es
	# (e,es) = es![i]
	# (r,es) = collect (i-1) es
	= (if e.included [(Index i,e.value,e.annotations):r] r, es)

mapInPlace :: !(Int v -> v) !*(NativeDB v a) -> *(NativeDB v a)
mapInPlace f (DB db)
# (s,db) = usize db
= DB (upd 0 s db)
where
	//upd :: !Int !Int !*{!Entry v a} -> *{!Entry v a}
	upd i s es
	| i == s = es
	#! (e,es) = es![i]
	#! e & value = hyperstrict $ f i e.value
	= upd (i+1) s {es & [i]=e}

search :: !SearchMode !(v -> (Bool, [a])) !*(NativeDB v a) -> *NativeDB v a
search mode f (DB db)
# (s,db) = usize db
= DB (upd (s - 1) db)
where
	//upd :: (!Int !*{!Entry v a} -> *{!Entry v a})
	upd = case mode of
		Intersect   -> intersect
		AddExcluded -> addExcluded

	intersect -1 es = es
	intersect i  es
	# (e,es) = es![i]
	| not e.included = intersect (i-1) es
	# (include,annotations) = f e.value
	= intersect (i-1) {es & [i]=
		{ e
		& included=include
		, annotations=annotations ++ e.annotations
		}}

	addExcluded -1 es = es
	addExcluded i  es
	# (e,es) = es![i]
	# (include,annotations) = f e.value
	= addExcluded (i-1) {es & [i]=
		{ e
		& included=e.included || include
		, annotations=annotations ++ e.annotations
		}}

searchIndex :: !Index ![a] !*(NativeDB v a) -> *NativeDB v a
searchIndex (Index i) annots (DB db)
# (e,db) = db![i]
# db & [i] =
	{ e
	& included = True
	, annotations = annots ++ e.annotations
	}
= DB db

unsearchIndex :: !Index !*(NativeDB v a) -> *NativeDB v a
unsearchIndex (Index i) (DB db) = DB {db & [i].included=False}

searchIndices :: !SearchMode ![(!Index, ![a])] !*(NativeDB v a) -> *NativeDB v a
searchIndices mode idxs (DB db)
# db = case mode of
	Intersect
		# (s,db) = usize db
		-> upd_intersect 0 (s-1) idxs db
	AddExcluded
		-> foldr upd_addexcluded db idxs
= (DB db)
where
	upd_addexcluded :: !(!Index, ![a]) !*{!Entry v a} -> *{!Entry v a}
	upd_addexcluded (Index i,annots) es
	# (e,es) = es![i]
	# e =
		{ e
		& included = True
		, annotations = annots ++ e.annotations
		}
	= {es & [i]=e}

	upd_intersect :: !Int !Int ![(!Index, ![a])] !*{!Entry v a} -> *{!Entry v a}
	upd_intersect i s _  es
		| i > s = es
	upd_intersect i s [] es
		# (e,es) = es![i]
		= upd_intersect (i+1) s [] {es & [i]={e & included=False}}
	upd_intersect i s allidxs=:[(Index idx,annots):idxs] es
		# (e,es) = es![i]
		# e =
			{ e
			& included = e.included && match
			, annotations = if match (annots ++ e.annotations) e.annotations
			}
		= upd_intersect (i+1) s (if match idxs allidxs) {es & [i]=e}
	where match = i == idx

unsearchIndices :: ![Index] !*(NativeDB v a) -> *NativeDB v a
unsearchIndices idxs (DB db)
# db = upd idxs db
= (DB db)
where
	upd :: ![Index] !*{!Entry v a} -> *{!Entry v a}
	upd [] es = es
	upd [Index i:is] es
	# (e,es) = es![i]
	= upd is {es & [i].included=False}

searchWithIndices :: !(v -> (Bool, ![a])) ![Index] !*(NativeDB v a) -> *NativeDB v a
searchWithIndices prop idxs (DB db)
# db = upd idxs db
= (DB db)
where
	upd [] es = es
	upd [Index i:is] es
	# (e,es) = es![i]
	# e = case prop e.value of
		(False, _)     -> {e & included=False}
		(True, annots) -> {e & included=True, annotations=annots ++ e.annotations}
	= upd is {es & [i]=e}

getIndex :: !Index !*(NativeDB v a) -> *(!Entry v a, !*(NativeDB v a))
getIndex (Index n) (DB db)
# (e,db) = db![n]
= (e, DB db)

getIndices :: ![Index] !*(NativeDB v a) -> *(![Entry v a], !*(NativeDB v a))
getIndices is (DB db)
# (es,db) = get is db
= (es, DB db)
where
	get :: ![Index] !*{!Entry v a} -> *(![Entry v a], !*{!Entry v a})
	get [] db = ([], db)
	get [Index i:is] db
	# (e,db) = db![i]
	# (es,db) = get is db
	= ([e:es], db)