
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <time.h>
#include <dirent.h>
#include <unistd.h>
#include "Clean.h"

#ifndef NAME_MAX
#define NAME_MAX 500
#endif

#define FALSE 0
#define TRUE (!FALSE)

// error codes:
#define NoDirError           0
#define OtherDirError       -1
#define DoesntExist         -2
#define BadName             -3
#define NotEnoughSpace      -4
#define AlreadyExists       -5
#define NoPermission        -6
#define MoveIntoOffspring   -7
#define MoveAcrossDisks     -8
#define NotYetRemovable     -9

uid_t           gUserId;
gid_t           gGroupId;
struct DIR      *gpDir;
struct dirent   *gpDirent;
struct stat     gFileStat;
char            *gPath;
int             gPathLength;
CleanStringVariable(gFileName,NAME_MAX+1);

static int unix_error_to_clean_error(int errCode)
{
    switch(errCode) {
        case ENOENT:
        case ENOTDIR:       return DoesntExist;
        case ENAMETOOLONG:  return BadName;
        case EMLINK:
        case ENOSPC:        return NotEnoughSpace;
        case EISDIR:
        case EEXIST:        return AlreadyExists;
        case EPERM:
        case EACCES:
        case EROFS:         return NoPermission;
        case EINVAL:        return MoveIntoOffspring;
        case EXDEV:         return MoveAcrossDisks;
        case ENOTEMPTY:     return NotYetRemovable;
        default:            return OtherDirError;
        };
}

static int openSearch(const char *path, int length)
{
    int     i;
    
    gPathLength = length;
    gPath   = (char*) malloc(gPathLength+NAME_MAX+2);
    if (!gPath) {
        gpDir   = NULL;
        return 0;
        }
    else {
        memcpy(gPath, path, gPathLength);
        gPath[gPathLength]  = '\0';
        gpDir   = (struct DIR*) opendir(gPath);
        if (!gpDir)
            return unix_error_to_clean_error(errno);
        gPath[gPathLength]  = '/'; // filename will be added later
        gUserId = getuid();
        gGroupId    = getgid();
        return NoDirError;
        };
}

int findFirstFileC(CleanString cs_path)
{
    int errCode;
    
    errCode = openSearch(CleanStringCharacters(cs_path), CleanStringLength(cs_path));
    if (errCode)
        return errCode;
    else
        return findNextFileC(0);
}

void getCommonFileInfoC(long also_get_file_name,
                        CleanString *pFileName, long *pFileSizeLow, long *pFileSizeHigh,
                        long *pYear, long *pMonth, long *pDay, long *pDayNr,
                        long *pHours, long *pMinutes, long *pSeconds,
                        long *pIsDirectory, long *pIsReadOnly)
// requires gFileName and gFileStat to be initialized
{
    struct tm   *pModificationTime;
    int         mask;
    static void *null = NULL; /* int 0 with the size of a Clean INT */

    *pFileName      = also_get_file_name ? (CleanString) gFileName : (CleanString) &null;
    *pFileSizeLow   = gFileStat.st_size;
    *pFileSizeHigh  = 0;
    pModificationTime= localtime(&gFileStat.st_mtime);
    *pYear          = pModificationTime->tm_year+1900;
    *pMonth         = pModificationTime->tm_mon+1;
    *pDay           = pModificationTime->tm_mday;
    *pDayNr         = pModificationTime->tm_wday+1;
    *pHours         = pModificationTime->tm_hour;
    *pMinutes       = pModificationTime->tm_min;
    *pSeconds       = pModificationTime->tm_sec % 60;
    *pIsDirectory   = (gFileStat.st_mode & S_IFDIR) != 0;
    mask            = gUserId==gFileStat.st_uid ? S_IWUSR
                        : gGroupId==gFileStat.st_gid ? S_IWGRP
                            : S_IWOTH;
    *pIsReadOnly    = (gFileStat.st_mode & mask) == 0;
}

int findNextFileC(int dummy)
// return values: 0=ok, 1=no further files in directory
{
    int i;

    gpDirent    = readdir((void*)gpDir);
    if (!gpDirent)
        return 1;

    for(i=0; gpDirent->d_name[i]!='\0' && i<NAME_MAX; i++) {
        CleanStringCharacters(gFileName)[i] = gpDirent->d_name[i];
        gPath[gPathLength+1+i]  = gpDirent->d_name[i];
        };
    CleanStringLength(gFileName)= i;
    gPath[gPathLength+1+i]      = '\0';
    gFileStat.st_size   = 0;
    gFileStat.st_mode   = 0;
    lstat(gPath,&gFileStat);
    return 0;
}

int getPlatformIdC(int dummy)
{
#define UnixPlatform 0 
    return UnixPlatform;
}

void getWindowsFileInfoC()
{}


void getMacFileInfoC()
{}

void getUnixFileInfoC(long *pModeBits, long *pOwnerUserId, long *pOwnerGroupId,
                        long *pLAYear, long *pLAMonth, long *pLADay, long *pLADayNr,    // last access time
                        long *pLAHours, long *pLAMinutes, long *pLASeconds)            // dito 
{
    struct tm   *pModificationTime;

    *pModeBits      = gFileStat.st_mode;
    *pOwnerUserId   = gFileStat.st_uid;
    *pOwnerGroupId  = gFileStat.st_gid;
    pModificationTime= localtime(&gFileStat.st_atime);
    *pLAYear        = pModificationTime->tm_year+1900;
    *pLAMonth       = pModificationTime->tm_mon+1;
    *pLADay         = pModificationTime->tm_mday;
    *pLADayNr       = pModificationTime->tm_wday+1;
    *pLAHours       = pModificationTime->tm_hour;
    *pLAMinutes     = pModificationTime->tm_min;
    *pLASeconds     = pModificationTime->tm_sec % 60;
}


void closeSearchC()
{
    free(gPath);
    if (gpDir)
        closedir((void*)gpDir);
}

int findSingleFileC(CleanString cs_path)
{
    int err,i,length;
    char *path_chars;
    
    gFileStat.st_size   = 0;
    gFileStat.st_mode   = 0;

    path_chars = CleanStringCharacters(cs_path);
    if (lstat(path_chars,&gFileStat))
        return unix_error_to_clean_error(errno);
        
    i = CleanStringLength(cs_path)-2;
    while (i>=0 && path_chars[i]!='/')
        i--;
    // the last path element ranges from path_chars[i+1] to path_chars[CleanStringLength(cs_path)-2]

    length = CleanStringLength(cs_path)-2-i;
    CleanStringLength(gFileName) = length;
    memcpy(CleanStringCharacters(gFileName), path_chars+i+1, length);
        
    return NoDirError;
}

void closeSingleSearchC()
{}

int createDirectoryC(CleanString cs_path)
{
    int err;
    
    err = mkdir(CleanStringCharacters(cs_path),
                 S_IRWXU | S_IRGRP | S_IXGRP | S_IROTH | S_IXOTH);
    // access rights for newly created directory: rwx--x--x
    if (err)
        return unix_error_to_clean_error(errno);
    else
        return NoDirError;
}

int fremoveC(CleanString cs_path)
{
    int err;
    
    err = remove(CleanStringCharacters(cs_path));
    if (err) {
        if (errno==EEXIST)
            return NotYetRemovable;
        else
            return unix_error_to_clean_error(errno);
        }
    else
        return NoDirError;
}

// Try this code with slackware linux, if the testLinux program doesn't work
//int fremoveC(CleanString cs_path)
//{
//  int err;
//  
//  err = unlink(CleanStringCharacters(cs_path));
//  if (err) {
//      if (errno==EPERM) {
//          err = rmdir(CleanStringCharacters(cs_path));
//          if (err)
//              return unix_error_to_clean_error(errno);
//          else
//              return NoDirError;
//          }
//      else
//          return unix_error_to_clean_error(errno);
//      }
//  else
//      return NoDirError;
//}

#define OK 0
#define STRING_TOO_SMALL 1

int getCurrentDirectory_SE(CleanString cs)
{
    if (getcwd(CleanStringCharacters(cs), CleanStringLength(cs))) {
        // success. convert C String to Clean string
        int i;
        i = 0;
        while(CleanStringCharacters(cs)[i]!='\0')
            i++;
        CleanStringLength(cs) = i;
        return OK;
        }
    else {
        // failure
        if (errno==EACCES) {
            // the permission to read the current directory was denied (how ever)
            // return root directory
            CleanStringLength(cs) = 1;
            CleanStringCharacters(cs)[0] = '/';
            return OK;
            }
        else
            return STRING_TOO_SMALL;
        };
}       

void get_mac_dir_parent_and_name_C()
{
}

int setCurrentDirectoryC(CleanString csPath)
{
    int err;
    
    err = chdir(CleanStringCharacters(csPath));
    if (err)
        return unix_error_to_clean_error(errno);
    else
        return NoDirError;
}

void getMacDiskNameC()
{}

void get_windows_disk_available_bits_C()
{}

void macRenameC()
{}

void macMoveC()
{}

int fmoveC(int overwrite, CleanString from, CleanString to)
{
    int err,clean_err;
    
    if (overwrite) {
        err = rename(CleanStringCharacters(from), CleanStringCharacters(to));
        if (err && errno==ENOTDIR) {
            // from is a directory and to a file
            // try again after removing to
            err = remove(CleanStringCharacters(to));
            if (!err)
                err = rename(CleanStringCharacters(from), CleanStringCharacters(to));
            };
        clean_err = err ? unix_error_to_clean_error(errno) : NoDirError;
        }
    else {
        struct stat fileStat;
        if (stat(CleanStringCharacters(to),&fileStat)) {
            err = rename(CleanStringCharacters(from), CleanStringCharacters(to));
            clean_err = err ? unix_error_to_clean_error(errno) : NoDirError;
            }
        else
            clean_err = AlreadyExists;
        };
    return clean_err;
}
