implementation module iTasks.Extensions.Admin.ServerAdmin
import iTasks

manageServer :: Task ()
manageServer
    =   (Hint "Session instances" @>> enterChoiceWithShared [ChooseFromGrid id] currentSessions)
    -&&-
        (Hint "Persistent instances" @>> enterChoiceWithShared [ChooseFromGrid id] currentProcesses)
    @!  ()
