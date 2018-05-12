implementation module iTasks.Extensions.Development.Tools
import iTasks
import iTasks.Internal.IWorld, iTasks.Internal.SDS
import System.Environment
import System.OS

CPM_EXE :== IF_POSIX_OR_WINDOWS "bin/cpm" "Tools\\cpm.exe"

cpmExecutable :: ROShared () FilePath
cpmExecutable = createReadOnlySDSError read
where
	read _ iworld=:{IWorld|world} = case getEnvironmentVariable "CLEAN_HOME" world of
		(Nothing,world) = (Error (exception "CLEAN_HOME environment variable not set"),{IWorld|iworld & world = world})
		(Just clean_home,world) = (Ok (clean_home </> CPM_EXE),{IWorld|iworld & world = world})
