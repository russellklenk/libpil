// path.cc: Implements the Linux and Windows path manipulation functions.
#include <stdlib.h>
#include <memory.h>
#include <errno.h>

#include "platform.h"

// Figure out the starting and ending points of the directory, filename and extension information in a Linux path string.
// o_parts: The PIL_PATH_PARTS_LINUX to update. The Root, RootEnd and PathFlags fields must be initialized by the caller.
// strinfo: Information about the input path string.
static int
LinuxPathExtractPathParts
(
    struct PIL_PATH_PARTS_LINUX *o_parts,
    struct PIL_STRING_INFO      *strinfo
)
{
    char     *iter = strinfo->BufferEnd;
    char *path_end = strinfo->BufferEnd;
    char *extn_beg = strinfo->BufferEnd;
    char *extn_end = strinfo->BufferEnd;
    char *name_beg = o_parts->RootEnd;
    char *name_end = o_parts->RootEnd;
    char *dirs_beg = o_parts->RootEnd;
    char *dirs_end = o_parts->RootEnd;
    uint32_t flags = o_parts->PathFlags;

    while (name_end < path_end) {
        if (name_end[0] == '\\') { // Normalize to system standard.
            name_end[0] = (char) '/';
        }
        if (name_end[0] != '/') {
            name_end = PIL_Utf8StringNextCodepoint(nullptr, nullptr, name_end);
        } else {
            // Encountered a path separator.
            // Update the end of the directory path string.
            // Reset the filename string to be zero length.
            dirs_end = name_end;
            name_beg = name_end + 1;
            name_end = name_end + 1;
            flags   |= PIL_PATH_FLAG_DIRECTORY;
        }
    }
    if (dirs_beg[0] == '/') {
        // Skip the leading path separator.
        if (dirs_beg == dirs_end) {
            // There is no actual path component - something like "/".
            flags &= ~PIL_PATH_FLAG_DIRECTORY;
            dirs_end++;
        }
        dirs_beg++;
    }
    if (name_beg != name_end) {
        // Is this a filename or part of the directory path?
        // Consider 'a.b' and '.a.b' and 'a.' to be filenames, but not '.a'.
        while (iter >= name_beg) {
            if (*iter == '.' && iter != name_beg) {
                name_end = iter;
                extn_beg = iter + 1;
                flags    = PIL_PATH_FLAG_FILENAME | PIL_PATH_FLAG_EXTENSION | flags;
            }
            iter = PIL_Utf8StringPrevCodepoint(nullptr, nullptr, iter);
        }
        if ((flags & PIL_PATH_FLAG_FILENAME) == 0) {
            dirs_end = name_end;
            flags   |= PIL_PATH_FLAG_DIRECTORY;
            name_beg = path_end;
            name_end = path_end;
        }
    } else {
        // No filename is present.
        name_beg  = path_end;
        name_end  = path_end;
    }
    o_parts->Path         = dirs_beg;
    o_parts->PathEnd      = dirs_end;
    o_parts->Filename     = name_beg;
    o_parts->FilenameEnd  = name_end;
    o_parts->Extension    = extn_beg;
    o_parts->ExtensionEnd = extn_end;
    o_parts->PathFlags    = flags;
    return 0;
}

// Figure out the starting and ending points of the directory, filename and extension information in a Win32 path string.
// o_parts: The PIL_PATH_PARTS_WIN32 to update. The Root, RootEnd and PathFlags fields must be initialized by the caller.
// strinfo: Information about the input path string.
static int
Win32PathExtractPathParts
(
    struct PIL_PATH_PARTS_WIN32 *o_parts,
    struct PIL_STRING_INFO      *strinfo
)
{
    char     *iter = strinfo->BufferEnd;
    char *path_end = strinfo->BufferEnd;
    char *extn_beg = strinfo->BufferEnd;
    char *extn_end = strinfo->BufferEnd;
    char *name_beg = o_parts->RootEnd;
    char *name_end = o_parts->RootEnd;
    char *dirs_beg = o_parts->RootEnd;
    char *dirs_end = o_parts->RootEnd;
    uint32_t flags = o_parts->PathFlags;

    while (name_end < path_end) {
        if (name_end[0] == '/') { // Normalize to system standard.
            name_end[0] =  '\\';
        }
        if (name_end[0] != '\\') {
            name_end = PIL_Utf8StringNextCodepoint(nullptr, nullptr, name_end);
        } else {
            // Encountered a path separator.
            // Update the end of the directory path string.
            // Reset the filename string to be zero length.
            dirs_end = name_end;
            name_beg = name_end + 1;
            name_end = name_end + 1;
            flags   |= PIL_PATH_FLAG_DIRECTORY;
        }
    }
    if (dirs_beg[0] == '\\') {
        // Skip the leading path separator.
        if (dirs_beg == dirs_end) {
            // There is no actual path component - something like "C:\".
            flags &= ~PIL_PATH_FLAG_DIRECTORY;
            dirs_end++;
        }
        dirs_beg++;
    }
    if (name_beg != name_end) {
        // Is this a filename or part of the directory path?
        // Consider 'a.b' and '.a.b' and 'a.' to be filenames, but not '.a'.
        while (iter >= name_beg) {
            if (*iter == '.' && iter != name_beg) {
                name_end = iter;
                extn_beg = iter + 1;
                flags    = PIL_PATH_FLAG_FILENAME | PIL_PATH_FLAG_EXTENSION | flags;
            }
            iter = PIL_Utf8StringPrevCodepoint(nullptr, nullptr, iter);
        }
        if ((flags & PIL_PATH_FLAG_FILENAME) == 0) {
            dirs_end = name_end;
            flags   |= PIL_PATH_FLAG_DIRECTORY;
            name_beg = path_end;
            name_end = path_end;
        }
    } else {
        // No filename is present.
        name_beg  = path_end;
        name_end  = path_end;
    }
    o_parts->Path         = dirs_beg;
    o_parts->PathEnd      = dirs_end;
    o_parts->Filename     = name_beg;
    o_parts->FilenameEnd  = name_end;
    o_parts->Extension    = extn_beg;
    o_parts->ExtensionEnd = extn_end;
    o_parts->PathFlags    = flags;
    return 0;
}

PIL_API(size_t)
PIL_LinuxPathStringMaxChars
(
    void
)
{
    return PIL_LINUX_PATH_STRING_MAX_CHARS;
}

PIL_API(size_t)
PIL_Win32PathStringMaxChars
(
    void
)
{
    return PIL_WIN32_PATH_STRING_MAX_CHARS;
}

PIL_API(char*)
PIL_LinuxPathBufferCreate
(
    struct PIL_STRING_INFO *o_strinfo,
    struct PIL_STRING_INFO *o_bufinfo,
    struct PIL_STRING_INFO   *strinfo,
    char const                *strbuf
)
{
    return PIL_Utf8StringCreate(o_strinfo, o_bufinfo, strinfo, PIL_LINUX_PATH_STRING_MAX_CHARS, strbuf);
}

PIL_API(char*)
PIL_Win32PathBufferCreate
(
    struct PIL_STRING_INFO *o_strinfo,
    struct PIL_STRING_INFO *o_bufinfo,
    struct PIL_STRING_INFO   *strinfo,
    char const                *strbuf
)
{
    return PIL_Utf8StringCreate(o_strinfo, o_bufinfo, strinfo, PIL_WIN32_PATH_STRING_MAX_CHARS, strbuf);
}

PIL_API(void)
PIL_PathBufferDelete
(
    void *pathbuf
)
{
    free(pathbuf);
}

PIL_API(int32_t)
PIL_LinuxPathStringParse
(
    struct PIL_PATH_PARTS_LINUX *o_parts,
    struct PIL_STRING_INFO    *o_strinfo,
    struct PIL_STRING_INFO      *strinfo,
    char const                   *strbuf
)
{
    PIL_STRING_INFO sinfo;
    char        *path_beg = nullptr;
    char        *path_end = nullptr;
    size_t      inp_chars = 0;

    if (o_parts == nullptr) {
        assert(o_parts != nullptr);
        if (o_strinfo) {
            memset(o_strinfo, 0, sizeof(PIL_STRING_INFO));
        } errno = EINVAL;
        return -1;
    }
    if (strbuf != nullptr) {
        if (strinfo != nullptr) {
            sinfo = *strinfo;
        } else {
            PIL_Utf8StringInfo(&sinfo, strbuf);
        }
    } else {
        assert(strbuf != nullptr);
        memset(o_parts, 0, sizeof(PIL_PATH_PARTS_LINUX));
        o_parts->PathFlags = PIL_PATH_FLAG_INVALID;
        if (o_strinfo) {
            memset(o_strinfo, 0, sizeof(PIL_STRING_INFO));
        } errno = EINVAL;
        return -1;
    }

    inp_chars          = sinfo.LengthChars;
    path_beg           = sinfo.Buffer;
    path_end           = sinfo.BufferEnd;
    o_parts->Root      = path_beg; o_parts->RootEnd      = path_end;
    o_parts->Path      = path_end; o_parts->PathEnd      = path_end;
    o_parts->Filename  = path_end; o_parts->FilenameEnd  = path_end;
    o_parts->Extension = path_end; o_parts->ExtensionEnd = path_end;
    o_parts->PathFlags = PIL_PATH_FLAGS_NONE;
    if (o_strinfo) {
        memcpy(o_strinfo, &sinfo, sizeof(PIL_STRING_INFO));
    }

    if (inp_chars >= 1) {
        if (path_beg[0] == '/') {
            o_parts->Root      = path_beg;
            o_parts->RootEnd   = path_beg + 1;
            o_parts->PathFlags = PIL_PATH_FLAG_ABSOLUTE | PIL_PATH_FLAG_ROOT;
        } else {
            o_parts->Root      = path_beg;
            o_parts->RootEnd   = path_beg;
            o_parts->PathFlags = PIL_PATH_FLAG_RELATIVE;
        }
        return LinuxPathExtractPathParts(o_parts, &sinfo);
    } else {
        o_parts->PathFlags = PIL_PATH_FLAG_INVALID;
        errno = EINVAL;
        return -1;
    }
}

PIL_API(int32_t)
PIL_Win32PathStringParse
(
    struct PIL_PATH_PARTS_WIN32 *o_parts,
    struct PIL_STRING_INFO    *o_strinfo,
    struct PIL_STRING_INFO      *strinfo,
    char const                   *strbuf
)
{
    PIL_STRING_INFO sinfo;
    char        *path_beg = nullptr;
    char        *path_end = nullptr;
    char        *root_beg = nullptr;
    char        *root_end = nullptr;
    size_t      inp_chars = 0;
    uint32_t        flags = PIL_PATH_FLAGS_NONE;

    if (o_parts == nullptr) {
        assert(o_parts != nullptr);
        if (o_strinfo) {
            memset(o_strinfo, 0, sizeof(PIL_STRING_INFO));
        } errno = EINVAL;
        return -1;
    }
    if (strbuf != nullptr) {
        if (strinfo != nullptr) {
            sinfo = *strinfo;
        } else {
            PIL_Utf8StringInfo(&sinfo, strbuf);
        }
    } else {
        assert(strbuf != nullptr);
        memset(o_parts, 0, sizeof(PIL_PATH_PARTS_WIN32));
        o_parts->PathFlags = PIL_PATH_FLAG_INVALID;
        if (o_strinfo) {
            memset(o_strinfo, 0, sizeof(PIL_STRING_INFO));
        } errno = EINVAL;
        return -1;
    }

    inp_chars          = sinfo.LengthChars;
    path_beg           = sinfo.Buffer;
    path_end           = sinfo.BufferEnd;
    o_parts->Root      = path_beg; o_parts->RootEnd      = path_end;
    o_parts->Path      = path_end; o_parts->PathEnd      = path_end;
    o_parts->Filename  = path_end; o_parts->FilenameEnd  = path_end;
    o_parts->Extension = path_end; o_parts->ExtensionEnd = path_end;
    o_parts->PathFlags = PIL_PATH_FLAGS_NONE;
    if (o_strinfo) {
        memcpy(o_strinfo, &sinfo, sizeof(PIL_STRING_INFO));
    }

    if (inp_chars >= 3) {
        if (path_beg[0] == '\\' && path_beg[1] == '\\') {
            // Absolute path; may be device, UNC, long device, long UNC or long DOS.
            if ((inp_chars >= 5) && (path_beg[2] == '?') && (path_beg[3] == '\\')) {
                // May be long UNC or long DOS.
                if ((inp_chars >= 6) && ((path_beg[4] >= 'A' && path_beg[4] <= 'Z') || (path_beg[4] >= 'a' && path_beg[4] <= 'z')) && (path_beg[5] == ':')) {
                    // Long DOS path.
                    o_parts->Root      = path_beg + 4;
                    o_parts->RootEnd   = path_beg + 6;
                    o_parts->PathFlags = PIL_PATH_FLAG_ABSOLUTE | PIL_PATH_FLAG_LONG | PIL_PATH_FLAG_ROOT;
                    return Win32PathExtractPathParts(o_parts, &sinfo);
                } else if ((inp_chars >= 6) && (path_beg[4] == '.' && path_beg[5] == '\\')) {
                    // Long device path.
                    root_beg = path_beg + 6;
                    root_end = path_beg + 6;
                    flags    = PIL_PATH_FLAG_ABSOLUTE | PIL_PATH_FLAG_LONG | PIL_PATH_FLAG_DEVICE | PIL_PATH_FLAG_ROOT;
                    goto scan_for_end_of_root;
                } else {
                    // Long UNC path.
                    root_beg = path_beg + 4;
                    root_end = path_beg + 4;
                    flags    = PIL_PATH_FLAG_ABSOLUTE | PIL_PATH_FLAG_LONG | PIL_PATH_FLAG_NETWORK | PIL_PATH_FLAG_ROOT;
                    goto scan_for_end_of_root;
                }
            } else if ((inp_chars >= 5) && (path_beg[2] == '.') && (path_beg[3] == '\\')) {
                // Device path, limit MAX_PATH characters.
                root_beg = path_beg + 4;
                root_end = path_beg + 4;
                flags    = PIL_PATH_FLAG_ABSOLUTE | PIL_PATH_FLAG_DEVICE | PIL_PATH_FLAG_ROOT;
                goto scan_for_end_of_root;
            } else {
                // UNC path, limit MAX_PATH characters.
                root_beg = path_beg + 2;
                root_end = path_beg + 2;
                flags    = PIL_PATH_FLAG_ABSOLUTE | PIL_PATH_FLAG_NETWORK | PIL_PATH_FLAG_ROOT;
                goto scan_for_end_of_root;
            }
        } else if (path_beg[0] == '\\' || path_beg[0] == '/') {
            // Absolute path with a root of '\' (MSDN says this is valid).
            if (path_beg[0] == '/') {
                path_beg[0] = '\\';
            }
            o_parts->Root      = path_beg;
            o_parts->RootEnd   = path_beg + 1;
            o_parts->PathFlags = PIL_PATH_FLAG_ABSOLUTE | PIL_PATH_FLAG_ROOT;
            return Win32PathExtractPathParts(o_parts, &sinfo);
        } else if (((path_beg[0] >= 'A' && path_beg[0] <= 'Z') || (path_beg[0] >= 'a' && path_beg[0] <= 'z')) && (path_beg[1] == ':')) {
            // Absolute DOS path with a drive letter root.
            o_parts->Root      = path_beg;
            o_parts->RootEnd   = path_beg + 2;
            o_parts->PathFlags = PIL_PATH_FLAG_ABSOLUTE | PIL_PATH_FLAG_ROOT;
            return Win32PathExtractPathParts(o_parts, &sinfo);
        } else {
            // Assume this is a relative path.
            o_parts->Root      = path_beg;
            o_parts->RootEnd   = path_beg;
            o_parts->PathFlags = PIL_PATH_FLAG_RELATIVE;
            return Win32PathExtractPathParts(o_parts, &sinfo);
        }
    } else if (inp_chars == 2) {
        // C:, .., .\, aa, .a, etc.
        if (((path_beg[0] >= 'A' && path_beg[0] <= 'Z') || (path_beg[0] >= 'a' && path_beg[0] <= 'z')) && (path_beg[1] == ':')) {
            // Absolute DOS path with drive letter root; no path information.
            o_parts->Root      = path_beg;
            o_parts->RootEnd   = path_beg + 2;
            o_parts->PathFlags = PIL_PATH_FLAG_ABSOLUTE | PIL_PATH_FLAG_ROOT;
            return 0;
        } else {
            // Assume a relative path, directory info only.
            o_parts->Root      = path_beg;
            o_parts->RootEnd   = path_beg;
            o_parts->Path      = path_beg;
            o_parts->PathFlags = PIL_PATH_FLAG_RELATIVE | PIL_PATH_FLAG_DIRECTORY;
            if (path_beg[0] == '.' && (path_beg[1] == '\\' || path_beg[1] == '/')) {
                // Relative path, directory info only.
                if (path_beg[1] == '/') {
                    path_beg[1] = '\\';
                }
                o_parts->PathEnd = path_beg + 1;
            } else {
                // Assume this is a relative directory path.
                o_parts->PathEnd = path_beg + 2;
            }
            return 0;
        }
    } else {
        // /, ., a, etc. - just a single character.
        if (path_beg[0] == '/') {
            path_beg[0] = '\\';
        }
        if (path_beg[0] == '\\') {
            // Treat this as an absolute path, the root of the filesystem.
            o_parts->Root      = path_beg;
            o_parts->RootEnd   = path_beg;
            o_parts->Path      = path_beg;
            o_parts->PathEnd   = path_beg + 1;
            o_parts->PathFlags = PIL_PATH_FLAG_ABSOLUTE | PIL_PATH_FLAG_DIRECTORY;
        } else {
            // Assume this is a relative path, directory info only.
            o_parts->Root      = path_beg;
            o_parts->RootEnd   = path_beg;
            o_parts->Path      = path_beg;
            o_parts->PathEnd   = path_beg + 1;
            o_parts->PathFlags = PIL_PATH_FLAG_RELATIVE | PIL_PATH_FLAG_DIRECTORY;
        }
        return 0;
    }

scan_for_end_of_root:
    while (root_end < path_end) {
        if (root_end[0] == '\\') {
            break;
        }
        if (root_end[0] == '/') {
            root_end[0]  = '\\';
            break;
        }
        root_end = PIL_Utf8StringNextCodepoint(nullptr, nullptr, root_end);
    }
    if (root_end == path_end) {
        // No additional components will be found.
        return 0;
    }
    o_parts->Root      = root_beg;
    o_parts->RootEnd   = root_end;
    o_parts->PathFlags = flags;
    return Win32PathExtractPathParts(o_parts, &sinfo);
}

#if 0
PIL_API(char*)
PIL_LinuxPathBufferAppend
(
    struct PIL_STRING_INFO *o_dstinfo, 
    struct PIL_STRING_INFO   *dstinfo, 
    struct PIL_STRING_INFO   *appinfo, 
    char                      *dstbuf, 
    char const                *appstr
);

PIL_API(char*)
PIL_Win32PathBufferAppend
(
    struct PIL_STRING_INFO *o_dstinfo, 
    struct PIL_STRING_INFO   *dstinfo, 
    struct PIL_STRING_INFO   *appinfo, 
    char                      *dstbuf, 
    char const                *appstr
)
{
    PIL_STRING_INFO dsinfo;
    PIL_STRING_INFO ssinfo;
    
    memset(&dsinfo, 0, sizeof(PIL_STRING_INFO));
    memset(&ssinfo, 0, sizeof(PIL_STRING_INFO));

    if (appstr != nullptr) {
        if (appinfo != nullptr) {
            memcpy(&ssinfo, appinfo, sizeof(PIL_STRING_INFO));
        } else {
            PIL_Utf8StringInfo(&ssinfo, appstr);
        }
    }
    if (dstbuf != nullptr) {
        if (dstinfo != nullptr) {
            memcpy(&dsinfo, dstinfo, sizeof(PIL_STRING_INFO));
        } else {
            PIL_Utf8StringInfo(&dsinfo, dstbuf);
        }
    }
    
    if (ssinfo.LengthChars == 0) { // Nothing to append.
        if (o_dstinfo) {
            memcpy(o_dstinfo, &dsinfo, sizeof(PIL_STRING_INFO));
        } return dstbuf;
    }
    if (dsinfo.LengthChars + ssinfo.LengthChars > PIL_WIN32_PATH_STRING_MAX_CHARS) {
        if (o_dstinfo) {
            memcpy(o_dstinfo, &dsinfo, sizeof(PIL_STRING_INFO));
        } errno = ENAMETOOLONG;
        return nullptr;
    }

    if (dstbuf == nullptr) {
        if ((dstbuf = PIL_Win32PathBufferCreate(&dsinfo, nullptr, appinfo, appstr)) != nullptr) {
            while (dstbuf && *dstbuf) {
                if (*dstbuf == '/') {
                    *dstbuf = '\\';
                } dstbuf = PIL_Utf8StringNextCodepoint(nullptr, nullptr, dstbuf);
            }
            if (o_dstinfo) {
                memcpy(o_dstinfo, &dsinfo, sizeof(PIL_STRING_INFO));
            } return dsinfo.Buffer;
        } else {
            if (o_dstinfo) {
                memset(o_dstinfo, 0, sizeof(PIL_STRING_INFO));
            } return nullptr; // Allocation failed.
        }
    }

    if (dsinfo.LengthChars > 0 && dsinfo.BufferEnd[-2] != '\\') {
        if (dsinfo.LengthChars + ssinfo.LengthChars + 1 > PIL_WIN32_PATH_STRING_MAX_CHARS) {
            if (o_dstinfo) {
                memcpy(o_dstinfo, &dsinfo, sizeof(PIL_STRING_INFO));
            } errno = ENAMETOOLONG;
            return nullptr;
        }
        dsinfo.BufferEnd[-1] = '\\';
    }
    while (ssinfo.Buffer != ssinfo.BufferEnd) {
        if (ssinfo.Buffer[0] != '/') {
            dsinfo.BufferEnd[-1] = ssinfo.Buffer++;
        } else {
            // TODO: Finish me
        }
    }
}
#endif

