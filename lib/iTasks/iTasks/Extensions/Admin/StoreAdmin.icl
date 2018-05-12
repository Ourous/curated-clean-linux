implementation module iTasks.Extensions.Admin.StoreAdmin
import iTasks
import qualified iTasks.Internal.Store
import qualified iTasks.Internal.Task
import Data.Error

manageStore :: Task ()
manageStore
    =   selectStore
    >^* [OnAction ActionDelete (hasValue deleteStore)]
    @! ()

selectStore :: Task (String,String) //Namespace and store name
selectStore
    =   (enterChoiceWithShared (Title "Namespace") [] storeNamespaces
    >&> \sNamespace -> whileUnchanged sNamespace
        \mbNamespace -> case mbNamespace of
            Nothing = enterChoice (Title "Stores") [ChooseFromGrid id] []
            Just ns = enterChoiceWithShared (Title "Stores") [ChooseFromGrid id] (sdsFocus ns storeNames) @ (\s -> (ns,s))
    ) <<@ (ArrangeWithSideBar 0 TopSide 55 False)

//Low-level access
deleteStore :: (String,String) -> Task ()
deleteStore (namespace,storename) = 'iTasks.Internal.Task'.mkInstantTask eval
where
    eval _ iworld = case 'iTasks.Internal.Store'.deleteValue namespace storename iworld of
		(Ok (),iworld) = (Ok (),iworld)
		(Error msg,iworld) = (Error (exception msg),iworld)
