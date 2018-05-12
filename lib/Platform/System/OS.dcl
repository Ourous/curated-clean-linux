definition module System.OS

OS_NAME :== "Linux (64-bit)"
OS_PATH_SEPARATOR :== '/'
OS_NEWLINE :== "\n"

IF_POSIX_OR_WINDOWS posix windows   :== posix

IF_WINDOWS win other				:== other
IF_WINDOWS32 win other				:== other
IF_WINDOWS64 win other				:== other
IF_POSIX posix other				:== posix
IF_LINUX linux other				:== linux
IF_LINUX32 linux other				:== other
IF_LINUX64 linux other				:== linux
IF_MAC mac other					:== other
IF_ANDROID android other                        :== other
