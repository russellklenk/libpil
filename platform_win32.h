#ifndef __LIBPIL_PLATFORM_WIN32_H__
#define __LIBPIL_PLATFORM_WIN32_H__

#pragma once

#ifndef PIL_NO_INCLUDES
#   include <Windows.h>
#endif

typedef struct PIL_RUNTIME_MODULE {
    HMODULE           ModuleHandle;
} PIL_RUNTIME_MODULE;

#endif /* __LIBPIL_PLATFORM_WIN32_H__ */

