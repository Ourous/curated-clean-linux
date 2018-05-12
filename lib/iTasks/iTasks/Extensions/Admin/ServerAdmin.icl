implementation module iTasks.Extensions.Admin.ServerAdmin
import iTasks

manageServer :: Task ()
manageServer
    =   enterChoiceWithShared "Session instances" [ChooseFromGrid id] currentSessions
    -&&-
        enterChoiceWithShared "Persistent instances" [ChooseFromGrid id] currentProcesses
    @!  ()
