// version.cc: Implement the version querying routines.
#include "platform.h"

PIL_API(int32_t)
PIL_GetVersion
(
    int32_t *o_major, 
    int32_t *o_minor, 
    int32_t *o_patch
)
{
    if (o_major) *o_major = PIL_VERSION_MAJOR;
    if (o_minor) *o_minor = PIL_VERSION_MINOR;
    if (o_patch) *o_patch = PIL_VERSION_PATCH;
    return 0;
}

PIL_API(char const*)
PIL_GetVersionString
(
    void
)
{
    return PIL_VERSION_STRING;
}

