#ifndef __LIBPIL_PLATFORM_LINUX_H__
#define __LIBPIL_PLATFORM_LINUX_H__

#pragma once

#ifndef PIL_NO_INCLUDES
#   include <dlfcn.h>
#endif

typedef struct PIL_RUNTIME_MODULE {
    void             *ModuleHandle;
} PIL_RUNTIME_MODULE;

#endif /* __LIBPIL_PLATFORM_LINUX_H__ */

