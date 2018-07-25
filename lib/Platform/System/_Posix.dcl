definition module System._Posix

from System._Pointer import :: Pointer
from StdInt import IF_INT_64_OR_32
from System.Time import :: Tm

WNOHANG		:==	0x00000001
WUNTRACED	:== 0x00000002
MAXPATHLEN	:== 1024

/** @type Int */
DIRENT_D_NAME_OFFSET	:== IF_INT_64_OR_32 19 11

S_IFMT		:== 0170000
S_IFIFO		:== 0010000
S_IFCHR		:== 0020000
S_IFDIR		:== 0040000
S_IFBLK		:== 0060000
S_IFREG		:== 0100000
S_IFLNK		:== 0120000
S_IFSOCK	:== 0140000
S_IFWHT		:== 0160000

STDIN_FILENO  :== 0
STDOUT_FILENO :== 1
STDERR_FILENO :== 2

FIONREAD   :== 0x541B

F_SETFD    :== 2
FD_CLOEXEC :== 1

O_RDWR     :== 02
O_NOCTTY   :== 0400

TCSANOW    :== 0
TIOCSCTTY  :== 0x540E

ECHO       :== 0x8
ECHONL     :== 0x40
ICANON     :== 0x2

//Posix API calls
errno		:: !*w -> (!Int,!*w)
strerr		:: !Int -> Pointer
stat		:: !{#Char} !{#Char} !*w -> (!Int,!*w)
unlink		:: !{#Char} !*w -> (!Int,!*w)
fork		:: !*w -> (!Int,!*w)
execvp		:: !{#Char} !{#Pointer} !*w -> (!Int,!*w)
waitpid		:: !Int !{#Int} !Int !*w -> (!Int,!*w)
exit		:: !Int !*w -> (!.a,!*w) 
getcwd		:: !{#Char} !Int !*w -> (!Pointer,!*w)
chdir		:: !{#Char} !*w -> (!Int,!*w)
mkdir		:: !{#Char} !Int !*w -> (!Int,!*w)
rmdir		:: !{#Char} !*w -> (!Int,!*w)
rename		:: !{#Char} !{#Char} !*w -> (!Int,!*w)
opendir		:: !{#Char} !*w -> (!Pointer,!*w)
closedir	:: !Pointer !*w -> (!Int,!*w)
readdir		:: !Pointer !*w -> (!Pointer,!*w)
pipe        :: !Pointer !*w -> (!Int, !*w)
posix_openpt :: !Int !*w -> (!Int, !*w)
grantpt     :: !Int !*w -> (!Int, !*w)
unlockpt    :: !Int !*w -> (!Int, !*w)
ptsname     :: !Int !*w -> (!Pointer, !*w)
open        :: !Pointer !Int !*w -> (!Int, !*w)
tcgetattr   :: !Int !Pointer !*w -> (!Int, !*w)
cfmakeraw   :: !Pointer !*w -> *w
tcsetattr   :: !Int !Int !Pointer !*w -> (!Int, !*w)
setsid      :: !*w -> *w
dup2        :: !Int !Int !*w -> (!Int, !*w)
close       :: !Int !*w -> (!Int, !*w)
ioctl       :: !Int !Int !Pointer !*w -> (!Int, !*w)
// variant requiring an argument as third parameter
fcntlArg    :: !Int !Int !Int !*w -> (!Int, !*w)
read        :: !Int !Pointer !Int !*w -> (!Int, !*w)
write       :: !Int !{#Char} !Int !*w -> (!Int, !*w)
select_     :: !Int !Pointer !Pointer !Pointer !Pointer !*w -> (!Int, !*w)
kill        :: !Int !Int !*w -> (!Int, !*w)
timegm      :: !{#Int} -> Int
clock_gettime :: !Int !Pointer !*w -> (!Int, !*w)

//Memory (impure)
malloc	:: !Int -> Pointer
mallocSt	:: !Int !*w -> (!Pointer, !*w)
free	:: !Pointer -> Int
freeSt  :: !Pointer !*w -> *w
memcpy_string_to_pointer :: !Pointer !{#Char} !Int -> Pointer

//Posix datastructures
:: Stat =
	{ st_dev			:: !Int
	, st_ino			:: !Int
	, st_mode			:: !Int
	, st_nlink			:: !Int
	, st_uid			:: !Int
	, st_gid			:: !Int
	, st_rdev			:: !Int
	, st_size			:: !Int
	, st_blocks			:: !Int
	, st_blksize		:: !Int
	, st_ctimespec		:: !Int
	, st_mtimespec		:: !Int
	, st_atimespec		:: !Int
	}
//Mapping to/from byte arrays
unpackStat	:: !{#Char} -> Stat
sizeOfStat	:: Int
