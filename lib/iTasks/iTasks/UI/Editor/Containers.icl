implementation module iTasks.UI.Editor.Containers
/**
* Editor combinators for the builtin containers
*
* To keep everything well-typed there are lots of boiler-plate versions to create the containers
*/
import iTasks.UI.Definition
import iTasks.UI.Editor
import Data.Error
import Text.GenJSON
from Data.Map import :: Map

import StdBool, StdList, StdTuple

//Empty container
group :: UIType -> Editor ()
group type = {Editor|genUI=genUI,onEdit=onEdit,onRefresh=onRefresh}
where
	genUI _ _ vst			    = (Ok (uia type emptyAttr,newCompoundMask),vst)
	onEdit _ _ val mask vst 	= (Ok (NoChange,mask),val,vst)
	onRefresh _ _ val mask vst  = (Ok (NoChange,mask),val,vst)

groupl :: UIType (Editor a) -> Editor [a]
groupl type editor = {Editor|genUI=genUI,onEdit=onEdit,onRefresh=onRefresh}
where
	genUI dp val vst = case genUIAll dp 0 val vst of
		(Error e,vst) = (Error e,vst)
		(Ok (uis,masks),vst) = (Ok (UI type emptyAttr uis, CompoundMask masks),vst)

	genUIAll dp i [] vst = (Ok ([],[]),vst)
	genUIAll dp i [v:vs] vst = case editor.Editor.genUI (dp ++ [i]) v vst of
		(Error e,vst) = (Error e,vst)
		(Ok (ui,m),vst) = case genUIAll dp (i + 1) vs vst of
			(Error e,vst) = (Error e,vst)
			(Ok (uis,ms),vst) = (Ok ([ui:uis],[m:ms]),vst)

	onEdit dp ([i:tp],e) val (CompoundMask masks) vst
		| i < 0 || i >= length val || i >= length masks  = (Error "Event route out of range",val,vst)
		| otherwise = case editor.Editor.onEdit (dp ++ [i]) (tp,e) (val !! i) (masks !! i) vst of
			(Error e,ival,vst) = (Error e,val,vst)
			(Ok (NoChange,imask),ival,vst)
				= (Ok (NoChange,CompoundMask (updateAt i imask masks)),updateAt i ival val,vst)
			(Ok (change,imask),ival,vst)
				= (Ok (ChangeUI [] [(i,ChangeChild change)],CompoundMask (updateAt i imask masks)),updateAt i ival val,vst)

	onRefresh dp new old (CompoundMask masks) vst = case onRefreshAll dp 0 new old masks vst of
		(Error e,val,vst) = (Error e,val,vst)
		(Ok ([],masks),val,vst) = (Ok (NoChange,CompoundMask masks),val,vst)
		(Ok (changes,masks),val,vst) = (Ok (ChangeUI [] changes,CompoundMask masks),new,vst)

	onRefreshAll dp i [n:ns] [o:os] [m:ms] vst
		 = case editor.Editor.onRefresh (dp ++ [i]) n o m vst of
			(Error e,v,vst) = (Error e,[],vst)
			(Ok (c,m),v,vst) = case onRefreshAll dp (i + 1) ns os ms vst of
				(Error e,vs,vst) = (Error e,vs,vst)
				(Ok (cs,ms),vs,vst) = (Ok ([(i,ChangeChild c):cs],[m:ms]),[v:vs],vst)

	onRefreshAll dp i ns [] _ vst //There are new elements in the list 
		= case genUIAll dp i ns vst of
			(Error e,vst)    = (Error e,[],vst)
			(Ok (us,ms),vst) = (Ok ([(n,InsertChild u) \\ u <- us & n <- [i..]],ms),ns,vst)

	onRefreshAll dp i [] os ms vst //Elements have been removed from the list
		= (Ok (repeatn (length os) (i,RemoveChild),[]),[],vst) 

groupL :: UIType [Editor a] -> Editor [a]
groupL type editors = {Editor|genUI=genUI,onEdit=onEdit,onRefresh=onRefresh}
where
	genUI dp val vst = case genUIAll 0 editors dp val vst of
		(Error e,vst) = (Error e,vst)
		(Ok (uis,masks),vst) = (Ok (UI type emptyAttr uis, CompoundMask masks),vst)

	genUIAll i _ dp [] vst = (Ok ([],[]),vst)
	genUIAll i [ed:eds] dp [v:vs] vst = case ed.Editor.genUI (dp ++ [i]) v vst of
		(Error e,vst) = (Error e,vst)
		(Ok (ui,m),vst) = case genUIAll (i + 1) eds dp vs vst of
			(Error e,vst) = (Error e,vst)
			(Ok (uis,ms),vst) = (Ok ([ui:uis],[m:ms]),vst)

	onEdit dp ([i:tp],e) val (CompoundMask masks) vst
		| i < 0 || i >= length val || i >= length masks  = (Error "Event route out of range",val,vst)
		= case (editors !! i).Editor.onEdit (dp ++ [i]) (tp,e) (val !! i) (masks !! i) vst of
			(Error e,ival,vst) = (Error e,val,vst)
			(Ok (NoChange,imask),ival,vst)
				= (Ok (NoChange,CompoundMask (updateAt i imask masks)),updateAt i ival val,vst)
			(Ok (change,imask),ival,vst)
				= (Ok (ChangeUI [] [(i,ChangeChild change)],CompoundMask (updateAt i imask masks)),updateAt i ival val, vst)

	onRefresh dp new old (CompoundMask masks) vst = case onRefreshAll 0 editors dp new old masks vst of
		(Error e,val,vst) = (Error e,val,vst)
		(Ok ([],masks),val,vst) = (Ok (NoChange,CompoundMask masks),val,vst)
		(Ok (changes,masks),val,vst) = (Ok (ChangeUI [] changes,CompoundMask masks),new,vst)

	onRefreshAll i [ed:eds] dp [n:ns] [o:os] [m:ms] vst
		 = case ed.Editor.onRefresh (dp ++ [i]) n o m vst of
			(Error e,v,vst) = (Error e,[],vst)
			(Ok (c,m),v,vst) = case onRefreshAll (i + 1) eds dp ns os ms vst of
				(Error e,vs,vst) = (Error e,vs,vst)
				(Ok (cs,ms),vs,vst) = (Ok ([(i,ChangeChild c):cs],[m:ms]),[v:vs],vst)

	onRefreshAll i [ed:eds] dp ns [] _ vst //There are new elements in the list 
		= case genUIAll i eds dp ns vst of
			(Error e,vst)    = (Error e,[],vst)
			(Ok (us,ms),vst) = (Ok ([(n,InsertChild u) \\ u <- us & n <- [i..]],ms),ns,vst)

	onRefreshAll i eds dp [] os ms vst //Elements have been removed from the list
		= (Ok (repeatn (length os) (i,RemoveChild),[]),[],vst) 

group1 :: UIType (Editor a) -> Editor a
group1 type editor1 = {Editor|genUI=genUI,onEdit=onEdit,onRefresh=onRefresh}
where
	genUI dp val vst
		= case editor1.Editor.genUI (dp ++ [0]) val vst of
			(Error e,vst) = (Error e,vst)
			(Ok (ui1,mask1),vst) = (Ok (UI type emptyAttr [ui1], CompoundMask [mask1]),vst)

	onEdit dp ([0:tp],e) val (CompoundMask [m1]) vst = case editor1.Editor.onEdit (dp ++ [0]) (tp,e) val m1 vst of
		(Error e,val,vst) = (Error e,val,vst)
		(Ok (NoChange,m1),val,vst) = (Ok (NoChange,CompoundMask [m1]),val,vst)
		(Ok (c1,m1),val,vst) = (Ok (ChangeUI [] [(0,ChangeChild c1)],CompoundMask [m1]),val,vst)
	onEdit _ _ val mask vst = (Error "Event route out of range",val,vst)	

	onRefresh dp old new (CompoundMask [m1]) vst 
		= case editor1.Editor.onRefresh (dp ++ [0]) old new m1 vst of
			(Error e,val,vst) = (Error e,val,vst)
			(Ok (NoChange,m1),val,vst) = (Ok (NoChange,CompoundMask [m1]),val,vst)
			(Ok (c1,m1),val,vst) = (Ok (ChangeUI [] [(0,ChangeChild c1)],CompoundMask [m1]),val,vst)

group2 :: UIType (Editor a) (Editor b) -> Editor (a,b)
group2 type editor1 editor2 = {Editor|genUI=genUI,onEdit=onEdit,onRefresh=onRefresh}
where
	genUI dp (val1,val2) vst
		= case editor1.Editor.genUI (dp ++ [0]) val1 vst of
			(Error e,vst) = (Error e,vst)
			(Ok (ui1,m1),vst) = case editor2.Editor.genUI (dp ++ [1]) val2 vst of
				(Error e,vst) = (Error e,vst)
				(Ok (ui2,m2),vst) = (Ok (UI type emptyAttr [ui1,ui2], CompoundMask [m1,m2]),vst)

	onEdit dp ([0:tp],e) (val1,val2) (CompoundMask [m1,m2]) vst = case editor1.Editor.onEdit (dp ++ [0]) (tp,e) val1 m1 vst of
		(Error e,val1,vst) = (Error e,(val1,val2),vst)
		(Ok (NoChange,m1),val1,vst) = (Ok (NoChange,CompoundMask [m1,m2]),(val1,val2),vst)
		(Ok (c1,m1),val1,vst) = (Ok (ChangeUI [] [(0,ChangeChild c1)],CompoundMask [m1,m2]),(val1,val2),vst)

	onEdit dp ([1:tp],e) (val1,val2) (CompoundMask [m1,m2]) vst = case editor2.Editor.onEdit (dp ++ [1]) (tp,e) val2 m2 vst of
		(Error e,val2,vst) = (Error e,(val1,val2),vst)
		(Ok (NoChange,m2),val2,vst) = (Ok (NoChange,CompoundMask [m1,m2]),(val1,val2),vst)
		(Ok (c2,m2),val2,vst) = (Ok (ChangeUI [] [(1,ChangeChild c2)],CompoundMask [m1,m2]),(val1,val2),vst)
	onEdit _ _ val mask vst = (Error "Event route out of range",val,vst)	
	
	onRefresh dp (n1,n2) (o1,o2) (CompoundMask [m1,m2]) vst 
		= case editor1.Editor.onRefresh (dp ++ [0]) n1 o1 m1 vst of
			(Error e,v1,vst) = (Error e,(v1,o2),vst)
			(Ok (c1,m1),v1,vst) = case editor2.Editor.onRefresh (dp ++ [1]) n2 o2 m2 vst of
				(Error e,v2,vst) = (Error e,(v1,v2),vst)
				(Ok (c2,m2),v2,vst) 
					# changes = [(0,ChangeChild c1),(1,ChangeChild c2)]
					# change = case changes of
						[] = NoChange
						_  = ChangeUI [] changes
					= (Ok (change,CompoundMask [m1,m2]),(v1,v2),vst)

group3 :: UIType (Editor a) (Editor b) (Editor c) -> Editor (a,b,c)
group3 type editor1 editor2 editor3 = {Editor|genUI=genUI,onEdit=onEdit,onRefresh=onRefresh}
where
	genUI dp (val1,val2,val3) vst
		= case editor1.Editor.genUI (dp ++ [0]) val1 vst of
			(Error e,vst) = (Error e,vst)
			(Ok (ui1,m1),vst) = case editor2.Editor.genUI (dp ++ [1]) val2 vst of
				(Error e,vst) = (Error e,vst)
				(Ok (ui2,m2),vst) = case editor3.Editor.genUI (dp ++ [2]) val3 vst of
					(Error e,vst) = (Error e,vst)
					(Ok (ui3,m3),vst) =(Ok (UI type emptyAttr [ui1,ui2,ui3], CompoundMask [m1,m2,m3]),vst)

	onEdit dp ([0:tp],e) (val1,val2,val3) (CompoundMask [m1,m2,m3]) vst = case editor1.Editor.onEdit (dp ++ [0]) (tp,e) val1 m1 vst of
		(Error e,val1,vst) = (Error e,(val1,val2,val3),vst)
		(Ok (NoChange,m1),val1,vst) = (Ok (NoChange,CompoundMask [m1,m2,m3]),(val1,val2,val3),vst)
		(Ok (c1,m1),val1,vst) = (Ok (ChangeUI [] [(0,ChangeChild c1)],CompoundMask [m1,m2,m3]),(val1,val2,val3),vst)

	onEdit dp ([1:tp],e) (val1,val2,val3) (CompoundMask [m1,m2,m3]) vst = case editor2.Editor.onEdit (dp ++ [1]) (tp,e) val2 m2 vst of
		(Error e,val2,vst) = (Error e,(val1,val2,val3),vst)
		(Ok (NoChange,m2),val2,vst) = (Ok (NoChange,CompoundMask [m1,m2,m3]),(val1,val2,val3),vst)
		(Ok (c2,m2),val2,vst) = (Ok (ChangeUI [] [(1,ChangeChild c2)],CompoundMask [m1,m2,m3]),(val1,val2,val3),vst)

	onEdit dp ([2:tp],e) (val1,val2,val3) (CompoundMask [m1,m2,m3]) vst = case editor3.Editor.onEdit (dp ++ [2]) (tp,e) val3 m3 vst of
		(Error e,val3,vst) = (Error e,(val1,val2,val3),vst)
		(Ok (NoChange,m3),val3,vst) = (Ok (NoChange,CompoundMask [m1,m2,m3]),(val1,val2,val3),vst)
		(Ok (c3,m3),val3,vst) = (Ok (ChangeUI [] [(2,ChangeChild c3)],CompoundMask [m1,m2,m3]),(val1,val2,val3),vst)

	onEdit _ _ val mask vst = (Error "Event route out of range",val,vst)	
	
	onRefresh dp (n1,n2,n3) (o1,o2,o3) (CompoundMask [m1,m2,m3]) vst 
		= case editor1.Editor.onRefresh (dp ++ [0]) n1 o1 m1 vst of
			(Error e,v1,vst) = (Error e,(v1,o2,o3),vst)
			(Ok (c1,m1),v1,vst) = case editor2.Editor.onRefresh (dp ++ [1]) n2 o2 m2 vst of
				(Error e,v2,vst) = (Error e,(v1,v2,o3),vst)
				(Ok (c2,m2),v2,vst) = case editor3.Editor.onRefresh (dp ++ [2]) n3 o3 m3 vst of
					(Error e,v3,vst) = (Error e,(v1,v2,v3),vst)
					(Ok (c3,m3),v3,vst)
						# changes = [(0,ChangeChild c1),(1,ChangeChild c2),(2,ChangeChild c3)]
						# change = case changes of
							[] = NoChange
							_  = ChangeUI [] changes
						= (Ok (change,CompoundMask [m1,m2,m3]),(v1,v2,v3),vst)

group4 :: UIType (Editor a) (Editor b) (Editor c) (Editor d) -> Editor (a,b,c,d)
group4 type editor1 editor2 editor3 editor4 = {Editor|genUI=genUI,onEdit=onEdit,onRefresh=onRefresh}
where
	genUI dp (val1,val2,val3,val4) vst
		= case editor1.Editor.genUI (dp ++ [0]) val1 vst of
			(Error e,vst) = (Error e,vst)
			(Ok (ui1,m1),vst) = case editor2.Editor.genUI (dp ++ [1]) val2 vst of
				(Error e,vst) = (Error e,vst)
				(Ok (ui2,m2),vst) = case editor3.Editor.genUI (dp ++ [2]) val3 vst of
					(Error e,vst) = (Error e,vst)
					(Ok (ui3,m3),vst) = case editor4.Editor.genUI (dp ++ [3]) val4 vst of
						(Error e,vst) = (Error e,vst)
						(Ok (ui4,m4),vst) = (Ok (UI type emptyAttr [ui1,ui2,ui3,ui4], CompoundMask [m1,m2,m3,m4]),vst)

	onEdit dp ([0:tp],e) (val1,val2,val3,val4) (CompoundMask [m1,m2,m3,m4]) vst = case editor1.Editor.onEdit (dp ++ [0]) (tp,e) val1 m1 vst of
		(Error e,val1,vst) = (Error e,(val1,val2,val3,val4),vst)
		(Ok (NoChange,m1),val1,vst) = (Ok (NoChange,CompoundMask [m1,m2,m3,m4]),(val1,val2,val3,val4),vst)
		(Ok (c1,m1),val1,vst) = (Ok (ChangeUI [] [(0,ChangeChild c1)],CompoundMask [m1,m2,m3,m4]),(val1,val2,val3,val4),vst)

	onEdit dp ([1:tp],e) (val1,val2,val3,val4) (CompoundMask [m1,m2,m3,m4]) vst = case editor2.Editor.onEdit (dp ++ [1]) (tp,e) val2 m2 vst of
		(Error e,val2,vst) = (Error e,(val1,val2,val3,val4),vst)
		(Ok (NoChange,m2),val2,vst) = (Ok (NoChange,CompoundMask [m1,m2,m3,m4]),(val1,val2,val3,val4),vst)
		(Ok (c2,m2),val2,vst) = (Ok (ChangeUI [] [(1,ChangeChild c2)],CompoundMask [m1,m2,m3,m4]),(val1,val2,val3,val4),vst)

	onEdit dp ([2:tp],e) (val1,val2,val3,val4) (CompoundMask [m1,m2,m3,m4]) vst = case editor3.Editor.onEdit (dp ++ [2]) (tp,e) val3 m3 vst of
		(Error e,val3,vst) = (Error e,(val1,val2,val3,val4),vst)
		(Ok (NoChange,m3),val3,vst) = (Ok (NoChange,CompoundMask [m1,m2,m3,m4]),(val1,val2,val3,val4),vst)
		(Ok (c3,m3),val3,vst) = (Ok (ChangeUI [] [(2,ChangeChild c3)],CompoundMask [m1,m2,m3,m4]),(val1,val2,val3,val4),vst)

	onEdit dp ([3:tp],e) (val1,val2,val3,val4) (CompoundMask [m1,m2,m3,m4]) vst = case editor4.Editor.onEdit (dp ++ [3]) (tp,e) val4 m4 vst of
		(Error e,val4,vst) = (Error e,(val1,val2,val3,val4),vst)
		(Ok (NoChange,m4),val4,vst) = (Ok (NoChange,CompoundMask [m1,m2,m3,m4]),(val1,val2,val3,val4),vst)
		(Ok (c4,m4),val4,vst) = (Ok (ChangeUI [] [(3,ChangeChild c4)],CompoundMask [m1,m2,m3,m4]),(val1,val2,val3,val4),vst)

	onEdit _ _ val mask vst = (Error "Event route out of range",val,vst)	

	onRefresh dp (n1,n2,n3,n4) (o1,o2,o3,o4) (CompoundMask [m1,m2,m3,m4]) vst 
		= case editor1.Editor.onRefresh (dp ++ [0]) n1 o1 m1 vst of
			(Error e,v1,vst) = (Error e,(v1,o2,o3,o4),vst)
			(Ok (c1,m1),v1,vst) = case editor2.Editor.onRefresh (dp ++ [1]) n2 o2 m2 vst of
				(Error e,v2,vst) = (Error e,(v1,v2,o3,o4),vst)
				(Ok (c2,m2),v2,vst) = case editor3.Editor.onRefresh (dp ++ [2]) n3 o3 m3 vst of
					(Error e,v3,vst) = (Error e,(v1,v2,v3,o4),vst)
					(Ok (c3,m3),v3,vst) = case editor4.Editor.onRefresh (dp ++ [3]) n4 o4 m4 vst of
						(Error e,v4,vst) = (Error e,(v1,v2,v3,v4),vst)
						(Ok (c4,m4),v4,vst)
							# changes = [(0,ChangeChild c1),(1,ChangeChild c2),(2,ChangeChild c3),(3,ChangeChild c4)]
							# change = case changes of
								[] = NoChange
								_  = ChangeUI [] changes
							= (Ok (change,CompoundMask [m1,m2,m3,m4]),(v1,v2,v3,v4),vst)

group5 :: UIType (Editor a) (Editor b) (Editor c) (Editor d) (Editor e) -> Editor (a,b,c,d,e)
group5 type editor1 editor2 editor3 editor4 editor5 = {Editor|genUI=genUI,onEdit=onEdit,onRefresh=onRefresh}
where
	genUI dp (val1,val2,val3,val4,val5) vst
		= case editor1.Editor.genUI (dp ++ [0]) val1 vst of
			(Error e,vst) = (Error e,vst)
			(Ok (ui1,m1),vst) = case editor2.Editor.genUI (dp ++ [1]) val2 vst of
				(Error e,vst) = (Error e,vst)
				(Ok (ui2,m2),vst) = case editor3.Editor.genUI (dp ++ [2]) val3 vst of
					(Error e,vst) = (Error e,vst)
					(Ok (ui3,m3),vst) = case editor4.Editor.genUI (dp ++ [3]) val4 vst of
						(Error e,vst) = (Error e,vst)
						(Ok (ui4,m4),vst) = case editor5.Editor.genUI (dp ++ [4]) val5 vst of
							(Error e,vst) = (Error e,vst)
							(Ok (ui5,m5),vst) = (Ok (UI type emptyAttr [ui1,ui2,ui3,ui4,ui5], CompoundMask [m1,m2,m3,m4,m5]),vst)
	onEdit dp ([0:tp],e) (val1,val2,val3,val4,val5) (CompoundMask [m1,m2,m3,m4,m5]) vst = case editor1.Editor.onEdit (dp ++ [0]) (tp,e) val1 m1 vst of
		(Error e,val1,vst) = (Error e,(val1,val2,val3,val4,val5),vst)
		(Ok (NoChange,m1),val1,vst) = (Ok (NoChange,CompoundMask [m1,m2,m3,m4,m5]),(val1,val2,val3,val4,val5),vst)
		(Ok (c1,m1),val1,vst) = (Ok (ChangeUI [] [(0,ChangeChild c1)],CompoundMask [m1,m2,m3,m4,m5]),(val1,val2,val3,val4,val5),vst)

	onEdit dp ([1:tp],e) (val1,val2,val3,val4,val5) (CompoundMask [m1,m2,m3,m4,m5]) vst = case editor2.Editor.onEdit (dp ++ [1]) (tp,e) val2 m2 vst of
		(Error e,val2,vst) = (Error e,(val1,val2,val3,val4,val5),vst)
		(Ok (NoChange,m2),val2,vst) = (Ok (NoChange,CompoundMask [m1,m2,m3,m4,m5]),(val1,val2,val3,val4,val5),vst)
		(Ok (c2,m2),val2,vst) = (Ok (ChangeUI [] [(1,ChangeChild c2)],CompoundMask [m1,m2,m3,m4,m5]),(val1,val2,val3,val4,val5),vst)

	onEdit dp ([2:tp],e) (val1,val2,val3,val4,val5) (CompoundMask [m1,m2,m3,m4,m5]) vst = case editor3.Editor.onEdit (dp ++ [2]) (tp,e) val3 m3 vst of
		(Error e,val3,vst) = (Error e,(val1,val2,val3,val4,val5),vst)
		(Ok (NoChange,m3),val3,vst) = (Ok (NoChange,CompoundMask [m1,m2,m3,m4,m5]),(val1,val2,val3,val4,val5),vst)
		(Ok (c3,m3),val3,vst) = (Ok (ChangeUI [] [(2,ChangeChild c3)],CompoundMask [m1,m2,m3,m4,m5]),(val1,val2,val3,val4,val5),vst)

	onEdit dp ([3:tp],e) (val1,val2,val3,val4,val5) (CompoundMask [m1,m2,m3,m4,m5]) vst = case editor4.Editor.onEdit (dp ++ [3]) (tp,e) val4 m4 vst of
		(Error e,val4,vst) = (Error e,(val1,val2,val3,val4,val5),vst)
		(Ok (NoChange,m4),val4,vst) = (Ok (NoChange,CompoundMask [m1,m2,m3,m4,m5]),(val1,val2,val3,val4,val5),vst)
		(Ok (c4,m4),val4,vst) = (Ok (ChangeUI [] [(3,ChangeChild c4)],CompoundMask [m1,m2,m3,m4,m5]),(val1,val2,val3,val4,val5),vst)

	onEdit dp ([4:tp],e) (val1,val2,val3,val4,val5) (CompoundMask [m1,m2,m3,m4,m5]) vst = case editor5.Editor.onEdit (dp ++ [4]) (tp,e) val5 m5 vst of
		(Error e,val5,vst) = (Error e,(val1,val2,val3,val4,val5),vst)
		(Ok (NoChange,m5),val5,vst) = (Ok (NoChange,CompoundMask [m1,m2,m3,m4,m5]),(val1,val2,val3,val4,val5),vst)
		(Ok (c5,m5),val5,vst) = (Ok (ChangeUI [] [(4,ChangeChild c5)],CompoundMask [m1,m2,m3,m4,m5]),(val1,val2,val3,val4,val5),vst)

	onEdit _ _ val mask vst = (Error "Event route out of range",val,vst)	

	onRefresh dp (n1,n2,n3,n4,n5) (o1,o2,o3,o4,o5) (CompoundMask [m1,m2,m3,m4,m5]) vst 
		= case editor1.Editor.onRefresh (dp ++ [0]) n1 o1 m1 vst of
			(Error e,v1,vst) = (Error e,(v1,o2,o3,o4,o5),vst)
			(Ok (c1,m1),v1,vst) = case editor2.Editor.onRefresh (dp ++ [1]) n2 o2 m2 vst of
				(Error e,v2,vst) = (Error e,(v1,v2,o3,o4,o5),vst)
				(Ok (c2,m2),v2,vst) = case editor3.Editor.onRefresh (dp ++ [2]) n3 o3 m3 vst of
					(Error e,v3,vst) = (Error e,(v1,v2,v3,o4,o5),vst)
					(Ok (c3,m3),v3,vst) = case editor4.Editor.onRefresh (dp ++ [3]) n4 o4 m4 vst of
						(Error e,v4,vst) = (Error e,(v1,v2,v3,v4,o5),vst)
						(Ok (c4,m4),v4,vst) = case editor5.Editor.onRefresh (dp ++ [4]) n5 o5 m5 vst of
							(Error e,v5,vst) = (Error e,(v1,v2,v3,v4,v5),vst)
							(Ok (c5,m5),v5,vst)
								# changes = [(0,ChangeChild c1),(1,ChangeChild c2),(2,ChangeChild c3),(3,ChangeChild c4),(4,ChangeChild c5)]
								# change = case changes of
									[] = NoChange
									_  = ChangeUI [] changes
								= (Ok (change,CompoundMask [m1,m2,m3,m4,m5]),(v1,v2,v3,v4,v5),vst)

groupc :: UIType (Editor Int) [(a -> a, Editor a)] -> Editor (Int, a)
groupc type choiceEditor fieldEditors = {Editor|genUI=genUI,onEdit=onEdit,onRefresh=onRefresh}
where
	genUI dp (choice,val) vst
		= case choiceEditor.Editor.genUI (dp ++ [0]) choice vst of
			(Error e,vst) = (Error e,vst)
			(Ok (uiSelector,maskSelector),vst)
				| (containsInvalidFields maskSelector)  //Only generate the field UI if a selection has been made
					= (Ok (UI type emptyAttr [uiSelector], CompoundMask [maskSelector]),vst)
				| otherwise
					# (selectFun,editor) = fieldEditors !! choice
					= case editor.Editor.genUI (dp ++ [1]) (selectFun val) vst of 
						(Error e,vst) = (Error e,vst)
						(Ok (uiField,maskField),vst)	
							 = (Ok (UI type emptyAttr [uiSelector,uiField], CompoundMask [maskSelector,maskField]),vst)
	
	//Handle choice changes 
	onEdit dp ([0:tp],choiceEdit) (currentChoice,val) mask=:(CompoundMask [maskSelector:optMaskField]) vst
		= case choiceEditor.Editor.onEdit (dp ++ [0]) (tp,choiceEdit) currentChoice maskSelector vst of
			(Error e,choice,vst) = (Error e,(choice,val),vst)
			(Ok (choiceUIChange,maskSelector),newChoice,vst)
				//Based on the effect of the selection change we may need to update the field editor
				| optMaskField =:[] //Previously no choice was made
					| containsInvalidFields maskSelector //Still no choice has been made
						# change = ChangeUI [] [(0,ChangeChild choiceUIChange)]
						# mask = CompoundMask [maskSelector]
						= (Ok (change,mask), (newChoice,val), vst)
					| otherwise //A choice has been made -> create an initial UI
						# (selectFun,editor) = fieldEditors !! newChoice
						# val = selectFun val
						= case editor.Editor.genUI (dp ++ [1]) val vst of 
							(Error e,vst) = (Error e,(newChoice,val),vst)
							(Ok (uiField,maskField),vst)
								# change = ChangeUI [] [(0,ChangeChild choiceUIChange),(1,InsertChild uiField)]
								# mask = CompoundMask [maskSelector,maskField]
								= (Ok (change,mask), (newChoice,val), vst)
				| otherwise // Previously an editor was chosen
					| containsInvalidFields maskSelector //The selection has been cleared 
						# change = ChangeUI [] [(0,ChangeChild choiceUIChange),(1,RemoveChild)]
						# mask = CompoundMask [maskSelector]
						= (Ok (change,mask), (newChoice,val), vst)
					| newChoice == currentChoice //The selection stayed the same
						# change = ChangeUI [] [(0,ChangeChild choiceUIChange)]
						# mask = CompoundMask [maskSelector:optMaskField]
						= (Ok (change,mask), (newChoice,val), vst)
					| otherwise //The selection changed -> replace with an initial UI of the new choice	
						# (selectFun,editor) = fieldEditors !! newChoice
						# val = selectFun val
						= case editor.Editor.genUI (dp ++ [1]) val vst of 
							(Error e,vst) = (Error e,(newChoice,val),vst)
							(Ok (uiField,maskField),vst)
								# change = ChangeUI [] [(0,ChangeChild choiceUIChange),(1,ChangeChild (ReplaceUI uiField))]
								# mask = CompoundMask [maskSelector,maskField]
								= (Ok (change,mask), (newChoice,val), vst)

	//Handle edits in the field editor
	onEdit dp ([1:tp],fieldEdit) (choice,val) mask=:(CompoundMask [maskSelector,maskField]) vst
		# (_,editor) = fieldEditors !! choice
		= case editor.Editor.onEdit (dp ++ [1]) (tp,fieldEdit) val maskField vst of 
			(Error e,val,vst) = (Error e,(choice,val),vst)
			(Ok (fieldChange,maskField),val,vst) 
				# change = ChangeUI [] [(1,ChangeChild fieldChange)]
				# mask = CompoundMask [maskSelector,maskField]
				= (Ok (change,mask), (choice,val), vst)

	onEdit _ (tp,e) val mask vst = (Error "Event route out of range",val,vst)	

	onRefresh dp (newChoice,newField) (oldChoice,oldField) mask=:(CompoundMask [maskSelector:optMaskField]) vst 
		//Update the choice selector
		= case choiceEditor.Editor.onRefresh (dp ++ [0]) newChoice oldChoice maskSelector vst of
			(Error e,_,vst) = (Error e,(oldChoice,oldField),vst)
			(Ok (choiceUIChange,maskSelector),newChoice,vst)
				| optMaskField =:[] //Previously no choice was made
					| containsInvalidFields maskSelector //Still no choice has been made
						# change = ChangeUI [] [(0,ChangeChild choiceUIChange)]
						# mask = CompoundMask [maskSelector]
						= (Ok (change,mask), (newChoice,newField), vst)
					| otherwise //A choice has been made -> create an initial UI
						# (selectFun,editor) = fieldEditors !! newChoice
						# newField = selectFun newField
						= case editor.Editor.genUI (dp ++ [1]) newField vst of 
							(Error e,vst) = (Error e,(oldChoice,oldField),vst)
							(Ok (uiField,maskField),vst)
								# change = ChangeUI [] [(0,ChangeChild choiceUIChange),(1,InsertChild uiField)]
								# mask = CompoundMask [maskSelector,maskField]
								= (Ok (change,mask), (newChoice,newField), vst)
				| otherwise // Previously an editor was chosen
					| containsInvalidFields maskSelector //The selection has been cleared 
						# change = ChangeUI [] [(0,ChangeChild choiceUIChange),(1,RemoveChild)]
						# mask = CompoundMask [maskSelector]
						= (Ok (change,mask), (newChoice,newField), vst)
					| newChoice == oldChoice //The selection stayed the same -> update the field
						# (selectFun,editor) = fieldEditors !! newChoice
						= case editor.Editor.onRefresh (dp ++ [1]) newField oldField (hd optMaskField) vst of
							(Error e,_,vst) = (Error e,(oldChoice,oldField),vst)
							(Ok (fieldUIChange,maskField),newField,vst)
								# change = ChangeUI [] [(0,ChangeChild choiceUIChange),(1,ChangeChild fieldUIChange)]
								# mask = CompoundMask [maskSelector,maskField]
								= (Ok (change,mask), (newChoice,newField), vst)
					| otherwise //The selection changed -> replace with an initial UI of the new choice	
						# (selectFun,editor) = fieldEditors !! newChoice
						# newField = selectFun newField
						= case editor.Editor.genUI (dp ++ [1]) newField vst of 
							(Error e,vst) = (Error e,(oldChoice,oldField),vst)
							(Ok (uiField,maskField),vst)
								# change = ChangeUI [] [(0,ChangeChild choiceUIChange),(1,ChangeChild (ReplaceUI uiField))]
								# mask = CompoundMask [maskSelector,maskField]
								= (Ok (change,mask), (newChoice,newField), vst)

//# UIContainer
container :: Editor ()
container = group UIContainer

containerl :: (Editor a) -> Editor [a]
containerl e = groupl UIContainer e

containerL :: [Editor a] -> Editor [a]
containerL es = groupL UIContainer es

container1 :: (Editor a) -> Editor a
container1 e1 = group1 UIContainer e1

container2 :: (Editor a) (Editor b) -> Editor (a,b)
container2 e1 e2 = group2 UIContainer e1 e2

container3 :: (Editor a) (Editor b) (Editor c) -> Editor (a,b,c)
container3 e1 e2 e3 = group3 UIContainer e1 e2 e3

container4 :: (Editor a) (Editor b) (Editor c) (Editor d) -> Editor (a,b,c,d)
container4 e1 e2 e3 e4 = group4 UIContainer e1 e2 e3 e4

container5 :: (Editor a) (Editor b) (Editor c) (Editor d) (Editor e) -> Editor (a,b,c,d,e)
container5 e1 e2 e3 e4 e5 = group5 UIContainer e1 e2 e3 e4 e5

containerc :: (Editor Int) [(a -> a, Editor a)] -> Editor (Int,a)
containerc ec es = groupc UIContainer ec es

//# UIPanel
panel :: Editor ()
panel = group UIPanel

panell :: (Editor a) -> Editor [a]
panell e = groupl UIPanel e

panelL :: [Editor a] -> Editor [a]
panelL es = groupL UIPanel es

panel1 :: (Editor a) -> Editor a
panel1 e1 = group1 UIPanel e1

panel2 :: (Editor a) (Editor b) -> Editor (a,b)
panel2 e1 e2 = group2 UIPanel e1 e2

panel3 :: (Editor a) (Editor b) (Editor c) -> Editor (a,b,c)
panel3 e1 e2 e3 = group3 UIPanel e1 e2 e3

panel4 :: (Editor a) (Editor b) (Editor c) (Editor d) -> Editor (a,b,c,d)
panel4 e1 e2 e3 e4 = group4 UIPanel e1 e2 e3 e4

panel5 :: (Editor a) (Editor b) (Editor c) (Editor d) (Editor e) -> Editor (a,b,c,d,e)
panel5 e1 e2 e3 e4 e5 = group5 UIPanel e1 e2 e3 e4 e5

panelc :: (Editor Int) [(a -> a, Editor a)] -> Editor (Int,a)
panelc ec es = groupc UIPanel ec es

//# UITabSet
tabset :: Editor ()
tabset = group UITabSet

tabsetl :: (Editor a) -> Editor [a]
tabsetl e = groupl UITabSet e

tabsetL :: [Editor a] -> Editor [a]
tabsetL es = groupL UITabSet es

tabset1 :: (Editor a) -> Editor a
tabset1 e1 = group1 UITabSet e1

tabset2 :: (Editor a) (Editor b) -> Editor (a,b)
tabset2 e1 e2 = group2 UITabSet e1 e2

tabset3 :: (Editor a) (Editor b) (Editor c) -> Editor (a,b,c)
tabset3 e1 e2 e3 = group3 UITabSet e1 e2 e3

tabset4 :: (Editor a) (Editor b) (Editor c) (Editor d) -> Editor (a,b,c,d)
tabset4 e1 e2 e3 e4 = group4 UITabSet e1 e2 e3 e4

tabset5 :: (Editor a) (Editor b) (Editor c) (Editor d) (Editor e) -> Editor (a,b,c,d,e)
tabset5 e1 e2 e3 e4 e5 = group5 UITabSet e1 e2 e3 e4 e5

tabsetc :: (Editor Int) [(a -> a, Editor a)] -> Editor (Int,a)
tabsetc ec es = groupc UITabSet ec es

//# UIWindow
window :: Editor ()
window = group UIWindow

windowl :: (Editor a) -> Editor [a]
windowl e = groupl UIWindow e

windowL :: [Editor a] -> Editor [a]
windowL es = groupL UIWindow es

window1 :: (Editor a) -> Editor a
window1 e1 = group1 UIWindow e1

window2 :: (Editor a) (Editor b) -> Editor (a,b)
window2 e1 e2 = group2 UIWindow e1 e2

window3 :: (Editor a) (Editor b) (Editor c) -> Editor (a,b,c)
window3 e1 e2 e3 = group3 UIWindow e1 e2 e3

window4 :: (Editor a) (Editor b) (Editor c) (Editor d) -> Editor (a,b,c,d)
window4 e1 e2 e3 e4 = group4 UIWindow e1 e2 e3 e4

window5 :: (Editor a) (Editor b) (Editor c) (Editor d) (Editor e) -> Editor (a,b,c,d,e)
window5 e1 e2 e3 e4 e5 = group5 UIWindow e1 e2 e3 e4 e5

windowc :: (Editor Int) [(a -> a, Editor a)] -> Editor (Int,a)
windowc ec es = groupc UIWindow ec es

//# UIMenu
menu :: Editor ()
menu = group UIMenu

menul :: (Editor a) -> Editor [a]
menul e = groupl UIMenu e

menuL :: [Editor a] -> Editor [a]
menuL es = groupL UIMenu es

menu1 :: (Editor a) -> Editor a
menu1 e1 = group1 UIMenu e1

menu2 :: (Editor a) (Editor b) -> Editor (a,b)
menu2 e1 e2 = group2 UIMenu e1 e2

menu3 :: (Editor a) (Editor b) (Editor c) -> Editor (a,b,c)
menu3 e1 e2 e3 = group3 UIMenu e1 e2 e3

menu4 :: (Editor a) (Editor b) (Editor c) (Editor d) -> Editor (a,b,c,d)
menu4 e1 e2 e3 e4 = group4 UIMenu e1 e2 e3 e4

menu5 :: (Editor a) (Editor b) (Editor c) (Editor d) (Editor e) -> Editor (a,b,c,d,e)
menu5 e1 e2 e3 e4 e5 = group5 UIMenu e1 e2 e3 e4 e5

menuc :: (Editor Int) [(a -> a, Editor a)] -> Editor (Int,a)
menuc ec es = groupc UIMenu ec es

//# UIToolBar
toolbar :: Editor ()
toolbar = group UIToolBar

toolbarl :: (Editor a) -> Editor [a]
toolbarl e = groupl UIToolBar e

toolbarL :: [Editor a] -> Editor [a]
toolbarL es = groupL UIToolBar es

toolbar1 :: (Editor a) -> Editor a
toolbar1 e1 = group1 UIToolBar e1

toolbar2 :: (Editor a) (Editor b) -> Editor (a,b)
toolbar2 e1 e2 = group2 UIToolBar e1 e2

toolbar3 :: (Editor a) (Editor b) (Editor c) -> Editor (a,b,c)
toolbar3 e1 e2 e3 = group3 UIToolBar e1 e2 e3

toolbar4 :: (Editor a) (Editor b) (Editor c) (Editor d) -> Editor (a,b,c,d)
toolbar4 e1 e2 e3 e4 = group4 UIToolBar e1 e2 e3 e4

toolbar5 :: (Editor a) (Editor b) (Editor c) (Editor d) (Editor e) -> Editor (a,b,c,d,e)
toolbar5 e1 e2 e3 e4 e5 = group5 UIToolBar e1 e2 e3 e4 e5

toolbarc :: (Editor Int) [(a -> a, Editor a)] -> Editor (Int,a)
toolbarc ec es = groupc UIToolBar ec es

//# UIButtonBar
buttonbar :: Editor ()
buttonbar = group UIButtonBar

buttonbarl :: (Editor a) -> Editor [a]
buttonbarl e = groupl UIButtonBar e

buttonbarL :: [Editor a] -> Editor [a]
buttonbarL es = groupL UIButtonBar es

buttonbar1 :: (Editor a) -> Editor a
buttonbar1 e1 = group1 UIButtonBar e1

buttonbar2 :: (Editor a) (Editor b) -> Editor (a,b)
buttonbar2 e1 e2 = group2 UIButtonBar e1 e2

buttonbar3 :: (Editor a) (Editor b) (Editor c) -> Editor (a,b,c)
buttonbar3 e1 e2 e3 = group3 UIButtonBar e1 e2 e3

buttonbar4 :: (Editor a) (Editor b) (Editor c) (Editor d) -> Editor (a,b,c,d)
buttonbar4 e1 e2 e3 e4 = group4 UIButtonBar e1 e2 e3 e4

buttonbar5 :: (Editor a) (Editor b) (Editor c) (Editor d) (Editor e) -> Editor (a,b,c,d,e)
buttonbar5 e1 e2 e3 e4 e5 = group5 UIButtonBar e1 e2 e3 e4 e5

buttonbarc :: (Editor Int) [(a -> a, Editor a)] -> Editor (Int,a)
buttonbarc ec es = groupc UIButtonBar ec es

//# UIList
list :: Editor ()
list = group UIList

listl :: (Editor a) -> Editor [a]
listl e = groupl UIList e 

listL :: [Editor a] -> Editor [a]
listL es = groupL UIList es

list1 :: (Editor a) -> Editor a
list1 e1 = group1 UIList e1

list2 :: (Editor a) (Editor b) -> Editor (a,b)
list2 e1 e2 = group2 UIList e1 e2

list3 :: (Editor a) (Editor b) (Editor c) -> Editor (a,b,c)
list3 e1 e2 e3 = group3 UIList e1 e2 e3

list4 :: (Editor a) (Editor b) (Editor c) (Editor d) -> Editor (a,b,c,d)
list4 e1 e2 e3 e4 = group4 UIList e1 e2 e3 e4

list5 :: (Editor a) (Editor b) (Editor c) (Editor d) (Editor e) -> Editor (a,b,c,d,e)
list5 e1 e2 e3 e4 e5 = group5 UIList e1 e2 e3 e4 e5

listc :: (Editor Int) [(a -> a, Editor a)] -> Editor (Int,a)
listc ec es = groupc UIList ec es

//# UIListItem
listitem :: Editor ()
listitem = group UIListItem

listiteml :: (Editor a) -> Editor [a]
listiteml e = groupl UIListItem e

listitemL :: [Editor a] -> Editor [a]
listitemL es = groupL UIListItem es

listitem1 :: (Editor a) -> Editor a
listitem1 e1 = group1 UIListItem e1

listitem2 :: (Editor a) (Editor b) -> Editor (a,b)
listitem2 e1 e2 = group2 UIListItem e1 e2

listitem3 :: (Editor a) (Editor b) (Editor c) -> Editor (a,b,c)
listitem3 e1 e2 e3 = group3 UIListItem e1 e2 e3

listitem4 :: (Editor a) (Editor b) (Editor c) (Editor d) -> Editor (a,b,c,d)
listitem4 e1 e2 e3 e4 = group4 UIListItem e1 e2 e3 e4

listitem5 :: (Editor a) (Editor b) (Editor c) (Editor d) (Editor e) -> Editor (a,b,c,d,e)
listitem5 e1 e2 e3 e4 e5 = group5 UIListItem e1 e2 e3 e4 e5

listitemc :: (Editor Int) [(a -> a, Editor a)] -> Editor (Int,a)
listitemc ec es = groupc UIListItem ec es

//# UIDebug
debug :: Editor ()
debug = group UIDebug

debugl :: (Editor a) -> Editor [a]
debugl e = groupl UIDebug e

debugL :: [Editor a] -> Editor [a]
debugL es = groupL UIDebug es

debug1 :: (Editor a) -> Editor a
debug1 e1 = group1 UIDebug e1

debug2 :: (Editor a) (Editor b) -> Editor (a,b)
debug2 e1 e2 = group2 UIDebug e1 e2

debug3 :: (Editor a) (Editor b) (Editor c) -> Editor (a,b,c)
debug3 e1 e2 e3 = group3 UIDebug e1 e2 e3

debug4 :: (Editor a) (Editor b) (Editor c) (Editor d) -> Editor (a,b,c,d)
debug4 e1 e2 e3 e4 = group4 UIDebug e1 e2 e3 e4

debug5 :: (Editor a) (Editor b) (Editor c) (Editor d) (Editor e) -> Editor (a,b,c,d,e)
debug5 e1 e2 e3 e4 e5 = group5 UIDebug e1 e2 e3 e4 e5

debugc :: (Editor Int) [(a -> a, Editor a)] -> Editor (Int,a)
debugc ec es = groupc UIDebug ec es

