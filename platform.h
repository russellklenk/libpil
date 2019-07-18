// platform.h: The main header file for the platform interface layer (libpil).
#ifndef __LIBPIL_PLATFORM_H__
#define __LIBPIL_PLATFORM_H__

#pragma once

#ifndef PIL_NO_INCLUDES
#   include <assert.h>
#   include <stddef.h>
#   include <stdint.h>
#   if   defined(_MSC_VER) && (_MSC_VER > 1800)
#       include <uchar.h>
#   elif defined(__linux__)
#       include <uchar.h>
#   endif
#endif

// If __STDC_UTF_16__ is defined (in uchar.h) then use existing char16_t. Otherwise, define it ourselves to be a 16-bit unsigned integer.
#ifndef __STDC_UTF_16__
    typedef unsigned short                   char16_t;
#endif

// If __STDC_UTF_32__ is defined (in uchar.h) then use existing char32_t. Otherwise, define it ourselves to be a 32-bit unsigned integer.
#ifndef __STDC_UTF_32__
    typedef unsigned int                     char32_t;
#endif

// Define the version of the Platform Interface Layer provided by this header.
#ifndef PIL_VERSION_CONSTANTS
#   define PIL_VERSION_CONSTANTS
#   define PIL_VERSION_MAJOR                 1
#   define PIL_VERSION_MINOR                 0
#   define PIL_VERSION_PATCH                 0
#   define PIL_VERSION_STRINGIZE_(x)         #x
#   define PIL_VERSION_STRINGIZE(x)          PIL_VERSION_STRINGIZE_(x)
#   define PIL_VERSION_STRING                                                  \
        PIL_VERSION_STRINGIZE(PIL_VERSION_MAJOR) "." PIL_VERSION_STRINGIZE(PIL_VERSION_MINOR) "." PIL_VERSION_STRINGIZE(PIL_VERSION_PATCH) " (" PIL_TARGET_PLATFORM_NAME "," PIL_TARGET_ARCHITECTURE_NAME "," PIL_TARGET_COMPILER_NAME ")"
#endif

// Define values used to identify the current target platform.
#ifndef PIL_PLATFORM_CONSTANTS
#   define PIL_PLATFORM_CONSTANTS
#   define PIL_PLATFORM_UNKNOWN              0
#   define PIL_PLATFORM_iOS                  1
#   define PIL_PLATFORM_ANDROID              2
#   define PIL_PLATFORM_WIN32                3
#   define PIL_PLATFORM_WINRT                4
#   define PIL_PLATFORM_MACOS                5
#   define PIL_PLATFORM_LINUX                6
#endif

// Define values used to identify the current compiler.
#ifndef PIL_COMPILER_CONSTANTS
#   define PIL_COMPILER_CONSTANTS
#   define PIL_COMPILER_UNKNOWN              0
#   define PIL_COMPILER_MSVC                 1
#   define PIL_COMPILER_GNUC                 2
#   define PIL_COMPILER_CLANG                3
#endif

// Define values used to identify the target processor architecture.
// Only 64-bit architectures are supported, due to reliance on 64-bit atomic operations.
#ifndef PIL_ARCHITECTURE_CONSTANTS
#   define PIL_ARCHITECTURE_CONSTANTS
#   define PIL_ARCHITECTURE_UNKNOWN          0
#   define PIL_ARCHITECTURE_X64              1
#   define PIL_ARCHITECTURE_ARM64            2
#   define PIL_ARCHITECTURE_PPC              3
#endif

// Define values used to identify the endianess of the target system.
#ifndef PIL_ENDIANESS_CONSTANTS
#   define PIL_ENDIANESS_CONSTANTS
#   define PIL_ENDIANESS_UNKNOWN             0
#   define PIL_ENDIANESS_LSB_FIRST           1
#   define PIL_ENDIANESS_MSB_FIRST           2
#endif

// The PIL_TARGET_COMPILER preprocessor value can be used to specify or query the current compiler.
#ifndef PIL_TARGET_COMPILER
#   define PIL_TARGET_COMPILER               PIL_COMPILER_UNKNOWN
#   define PIL_TARGET_COMPILER_NAME          "Unknown"
#endif

// The PIL_TARGET_PLATFORM preprocessor value can be used to specify or query the current target platform.
#ifndef PIL_TARGET_PLATFORM
#   define PIL_TARGET_PLATFORM               PIL_PLATFORM_UNKNOWN
#   define PIL_TARGET_PLATFORM_NAME          "Unknown"
#endif

// The PIL_TARGET_ARCHITECTURE preprocessor value can be used to specify or query the current target processor architecture.
#ifndef PIL_TARGET_ARCHITECTURE
#   define PIL_TARGET_ARCHITECTURE           PIL_ARCHITECTURE_UNKNOWN
#   define PIL_TARGET_ARCHITECTURE_NAME      "Unknown"
#endif

// The PIL_SYSTEM_ENDIANESS preprocessor value can be used to specify or  query the endianess of the host system. 
// Default to little endian since most processor architectures these days are configurable. 
// GCC defines the __BYTE_ORDER__ preprocessor value that can be used to test at compile time.
#ifndef PIL_SYSTEM_ENDIANESS
#   if defined(__BYTE_ORDER__) && (__BYTE_ORDER__ == __ORDER_BIG_ENDIAN__)
#       define PIL_SYSTEM_ENDIANESS          PIL_ENDIANESS_MSB_FIRST
#   else
#       define PIL_SYSTEM_ENDIANESS          PIL_ENDIANESS_LSB_FIRST
#   endif
#endif

// Perform compiler detection based on preprocessor directives.
#if PIL_TARGET_COMPILER == PIL_COMPILER_UNKNOWN
#   if   defined(_MSC_VER)
#       undef  PIL_TARGET_COMPILER
#       undef  PIL_TARGET_COMPILER_NAME
#       define PIL_TARGET_COMPILER           PIL_COMPILER_MSVC
#       define PIL_TARGET_COMPILER_NAME      "MSVC"
#   elif defined(__clang__)
#       undef  PIL_TARGET_COMPILER
#       undef  PIL_TARGET_COMPILER_NAME
#       define PIL_TARGET_COMPILER           PIL_COMPILER_CLANG
#       define PIL_TARGET_COMPILER_NAME      "Clang"
#   elif defined(__GNUC__)
#       undef  PIL_TARGET_COMPILER
#       undef  PIL_TARGET_COMPILER_NAME
#       define PIL_TARGET_COMPILER           PIL_COMPILER_GNUC
#       define PIL_TARGET_COMPILER_NAME      "GNU"
#   else
#       error  platform.h: Failed to detect target compiler. Update compiler detection.
#   endif
#endif

// Perform processor architecture detection based on preprocessor directives.
#if PIL_TARGET_ARCHITECTURE == PIL_ARCHITECTURE_UNKNOWN
#   if   defined(__aarch64__) || defined(_M_ARM64)
#       undef  PIL_TARGET_ARCHITECTURE
#       undef  PIL_TARGET_ARCHITECTURE_NAME
#       define PIL_TARGET_ARCHITECTURE       PIL_ARCHITECTURE_ARM64
#       define PIL_TARGET_ARCHITECTURE_NAME  "ARM64"
#   elif defined(__arm__) || defined(_M_ARM)
#       error  platform.h: Only 64-bit ARM platforms are supported.
#   elif defined(__amd64__) || defined(__x86_64__) || defined(_M_X64) || defined(_M_AMD64)
#       undef  PIL_TARGET_ARCHITECTURE
#       undef  PIL_TARGET_ARCHITECTURE_NAME
#       define PIL_TARGET_ARCHITECTURE       PIL_ARCHITECTURE_X64
#       define PIL_TARGET_ARCHITECTURE_NAME  "x86_64"
#   elif defined(__i386__) || defined(__i486__) || defined(__i586__) || defined(__i686__) || defined(_M_IX86) || defined(_X86_)
#       error  platform.h: Only 64-bit Intel platforms are supported.
#   elif defined(__ppc__) || defined(__powerpc__) || defined(__PPC__)
#       undef  PIL_TARGET_ARCHITECTURE
#       undef  PIL_TARGET_ARCHITECTURE_NAME
#       define PIL_TARGET_ARCHITECTURE       PIL_ARCHITECTURE_PPC
#       define PIL_TARGET_ARCHITECTURE_NAME  "PowerPC"
#   else
#       error  platform.h: Failed to detect target architecture. Update architecture detection.
#   endif
#endif

// Perform platform detection based on preprocessor directives.
#if PIL_TARGET_PLATFORM == PIL_PLATFORM_UNKNOWN
#   if   defined(ANDROID)
#       undef  PIL_TARGET_PLATFORM
#       undef  PIL_TARGET_PLATFORM_NAME
#       define PIL_TARGET_PLATFORM           PIL_PLATFORM_ANDROID
#       define PIL_TARGET_PLATFORM_NAME      "Android"
#   elif defined(__APPLE__)
#       include <TargetConditionals.h>
#       if   defined(TARGET_OS_IPHONE) || defined(TARGET_IPHONE_SIMULATOR)
#           undef  PIL_TARGET_PLATFORM
#           undef  PIL_TARGET_PLATFORM_NAME
#           define PIL_TARGET_PLATFORM       PIL_PLATFORM_iOS
#           define PIL_TARGET_PLATFORM_NAME  "iOS"
#       else
#           undef  PIL_TARGET_PLATFORM
#           undef  PIL_TARGET_PLATFORM_NAME
#           define PIL_TARGET_PLATFORM       PIL_PLATFORM_MACOS
#           define PIL_TARGET_PLATFORM_NAME  "MacOS"
#       endif
#   elif defined(_WIN32) || defined(_WIN64) || defined(__cplusplus_winrt)
#       if   defined(__cplusplus_winrt)
#           undef  PIL_TARGET_PLATFORM
#           undef  PIL_TARGET_PLATFORM_NAME
#           define PIL_TARGET_PLATFORM       PIL_PLATFORM_WINRT
#           define PIL_TARGET_PLATFORM_NAME  "WinRT/UWP"
#       else
#           undef  PIL_TARGET_PLATFORM
#           undef  PIL_TARGET_PLATFORM_NAME
#           define PIL_TARGET_PLATFORM       PIL_PLATFORM_WIN32
#           define PIL_TARGET_PLATFORM_NAME  "Win32"
#       endif
#   elif defined(__linux__) || defined(__gnu_linux__)
#       undef  PIL_TARGET_PLATFORM
#       undef  PIL_TARGET_PLATFORM_NAME
#       define PIL_TARGET_PLATFORM           PIL_PLATFORM_LINUX
#       define PIL_TARGET_PLATFORM_NAME      "Linux"
#   else
#       error  platform.h: Failed to detect target platform. Update platform detection.
#   endif
#endif

// Abstract away some commonly-used compiler directives.
#if   PIL_TARGET_COMPILER == PIL_COMPILER_MSVC
#   define PIL_NEVER_INLINE                  __declspec(noinline)
#   define PIL_FORCE_INLINE                  __forceinline
#   define PIL_STRUCT_ALIGN(_x)              __declspec(align(_x))
#   define PIL_ALIGN_OF(_x)                  __alignof(_x)
#   define PIL_RESTRICT                      __restrict
#   define PIL_SHARED_EXPORT                 __declspec(dllexport)
#   define PIL_SHARED_IMPORT                 __declspec(dllimport)
#   define PIL_OFFSET_OF(_type, _field)      offsetof(_type, _field)
#   define PIL_UNUSED_ARG(_x)                (void)(_x)
#   define PIL_UNUSED_LOCAL(_x)              (void)(_x)
#   ifdef __cplusplus
#       define PIL_INLINE                    inline
#   else
#       define PIL_INLINE                  
#   endif
#elif PIL_TARGET_COMPILER == PIL_COMPILER_GNUC || PIL_TARGET_COMPILER == PIL_COMPILER_CLANG
#   define PIL_NEVER_INLINE                  __attribute__((noinline))
#   define PIL_FORCE_INLINE                  __attribute__((always_inline))
#   define PIL_STRUCT_ALIGN(_x)              __attribute__((aligned(_x)))
#   define PIL_ALIGN_OF(_x)                  __alignof__(_x)
#   define PIL_RESTRICT                      __restrict
#   define PIL_SHARED_EXPORT                 
#   define PIL_SHARED_IMPORT                 
#   define PIL_UNUSED_ARG(_x)                (void)(sizeof(_x))
#   define PIL_UNUSED_LOCAL(_x)              (void)(sizeof(_x))
#   define PIL_OFFSET_OF(_type, _field)      offsetof(_type, _field)
#   ifdef __cplusplus
#       define PIL_INLINE                    inline
#   else
#       define PIL_INLINE                  
#   endif
#endif

// #define PIL_STATIC to make all function declarations and definitions static. 
// This is useful if the library implementation needs to be included several times within a project.
#ifdef  PIL_STATIC
#   define PIL_API(_rt)                      static _rt
#else
#   define PIL_API(_rt)                      extern _rt
#endif

// Assign a value to an output argument.
// _dst: A pointer to the destination location.
// _val: The value to assign to the destination location.
#ifndef PIL_Assign
#define PIL_Assign(_dst, _val)                                                 \
    if ((_dst)) *(_dst) = (_val) 
#endif

// Calculate the number of items in a fixed-length array.
// _array The fixed-length array.
// Returns A uint32_t specifying the number of items in the array.
#ifndef PIL_CountOf
#define PIL_CountOf(_array)                                                    \
    ((uint32_t)(sizeof((_array)) / sizeof((_array)[0])))
#endif

// Retrieve the byte offset of a field from the start of a structure.
// _type: The typename, which must be a struct or a class type.
// _field: The fieldname within the struct or class type whose offset will be retrieved.
#ifndef PIL_OffsetOf
#define PIL_OffsetOf(_type, _field)                                            \
    PIL_OFFSET_OF(_type, _field)
#endif

// Retrieve the size of a particular type, in bytes. 
// _type: A typename, such as int, specifying the type whose size is to be retrieved.
#ifndef PIL_SizeOf
#define PIL_SizeOf(_type)                                                      \
    sizeof(_type)
#endif

// Retrieve the alignment of a particular type, in bytes.
// _type: A typename, such as int, specifying the type whose alignment is to be retrieved.
#ifndef PIL_AlignOf
#define PIL_AlignOf(_type)                                                     \
    PIL_ALIGN_OF(_type)
#endif

// Align a non-zero size up to the nearest even multiple of a given power-of-two.
// _quantity: The size value to align up.
// _alignment: The desired power-of-two alignment.
#ifndef PIL_AlignUp
#define PIL_AlignUp(_quantity, _alignment)                                     \
    (((_quantity) + ((_alignment)-1)) & ~((_alignment)-1))
#endif

// For a given address, return the address aligned for a particular type.
// _address: The unaligned address.
// _type: A typename, such as int, specifying the type that is to be accessed.
#ifndef PIL_AlignFor
#define PIL_AlignFor(_address, _type)                                          \
    ((void*)(((uint8_t*)(_address)) + ((((PIL_ALIGN_OF(_type))-1)) & ~((PIL_ALIGN_OF(_type))-1))))
#endif

// For a given type, calculate the maximum number of bytes that will need to be allocated for a single instance.
// _type: A typename, such as int, specifying the type whose allocation size is being queried.
#ifndef PIL_AllocationSizeType
#define PIL_AllocationSizeType(_type)                                          \
    ((sizeof(_type)) + (PIL_ALIGN_OF(_type)-1))
#endif

// For a given type, calculate the maximum number of bytes that will need to be allocated for an array of a given capacity.
// _type: A typename, such as int, specifying the type whose allocation size is being queried.
// _count: The number of elements in the array.
#ifndef PIL_AllocationSizeArray
#define PIL_AllocationSizeArray(_type, _count)                                 \
    ((sizeof(_type) * (_count)) + (PIL_ALIGN_OF(_type)-1))
#endif

// Given a struct size and required alignment, calculate the maximum number of bytes that will need to be allocated for an array of a given capacity.
// _objsize: The object size, in bytes.
// _objalign: The required alignment of the object, in bytes.
// _count: The number of elements in the array.
#ifndef PIL_AllocationSizeArrayRaw
#define PIL_AllocationSizeArrayRaw(_objsize, _objalign, _count)                \
    (((_objsize) * (_count)) + ((_objalign)-1))
#endif

// Helper macro for populating a dispatch table with functions loaded at runtime.
// If the function is not found, the entry point is updated to point to a stub implementation provided by the caller.
// This macro relies on specific naming conventions:
// - The signature must be PFN_FuncName where FuncName corresponds to the _func argument.
// - The dispatch table field must be FuncName where FuncName corresponds to the _func argument.
// - The stub function must be named FuncName_Stub where FuncName corresponds to the _func argument.
// _disp: A pointer to the dispatch table to populate.
// _module: A pointer to the PIL_RUNTIME_MODULE representing the module loaded into the process address space.
// _func: The name of the function to dynamically load.
#ifndef PIL_RuntimeFunctionResolve
#define PIL_RuntimeFunctionResolve(_disp, _module, _func)                      \
    for (;;) {                                                                 \
        (_disp)->_func=(PFN_##_func)PIL_RuntimeModuleResolve((_module),#_func);\
        if ((_disp)->_func == NULL) {                                          \
            (_disp)->_func  = _func##_Stub;                                    \
        } break;                                                               \
    }
#endif

// Set a runtime-resolved function entry point to point at the stub implementation provided by the application.
// _disp: A pointer to the dispatch table to populate.
// _func: The name of the function to dynamically load.
#ifndef PIL_RuntimeFunctionSetStub
#define PIL_RuntimeFunctionSetStub(_disp, _func)                               \
    (_disp)->_func=(PFN_##_func) _func##_Stub
#endif

// Determine whether a runtime-resolved function was resolved to its stub implementation, meaning that it is not implemented on the host.
// _disp: A pointer to the dispatch table.
// _func: The name of the function to check.
// Returns non-zero if the dispatch table entry for _func points to the stub implementation.
#ifndef PIL_RuntimeFunctionIsMissing
#define PIL_RuntimeFunctionIsMissing(_disp, _func)                             \
    (_disp)->_func == _func##_Stub
#endif

// Forward-declare the types exported from libpil.
struct  PIL_CONTEXT;
struct  PIL_CONTEXT_INIT;                                   // Defined in platform.h.
struct  PIL_RUNTIME_MODULE;                                 // Defined in platform/dylib.h.

typedef int  (*PIL_PFN_Unknown)(void);                      // Signature for a dynamically-resolved function. The calling code will have to cast the function pointer to its specific type.

typedef struct PIL_CONTEXT_INIT {
    char const *ApplicationName;                            // A nul-terminate string specifying a name for the application creating the context.
    int32_t     AppVersionMajor;                            // The major version component of the application.
    int32_t     AppVersionMinor;                            // The minor version component of the application.
    int32_t     AppVersionPatch;                            // The patch version component of the application.
} PIL_CONTEXT_INIT;

#ifdef __cplusplus
extern "C" {
#endif

// Retrieve the version of the Platform Interface Layer.
// o_major: On return, this location is updated with the PIL major version component.
// o_minor: On return, this location is updated with the PIL minor version component.
// o_patch: On return, this location is updated with the PIL patch version component.
// Returns zero if the platform interface layer is supported on the host system, or non-zero otherwise.
PIL_API(int32_t)
PIL_GetVersion
(
    int32_t *o_major, 
    int32_t *o_minor, 
    int32_t *o_patch
);

// Retrieve a string describing the version of the Platform Interface Layer.
// Returns a nul-terminated ASCII string specifying the library version. Do not attempt to modify or free the returned string.
PIL_API(char const*)
PIL_GetVersionString
(
    void
);

// Initialize a PIL_RUNTIME_MODULE instance such that PIL_RuntimeModuleIsValid will return zero.
// module: The PIL_RUNTIME_MODULE to initialize.
PIL_API(void)
PIL_RuntimeModuleInit
(
    struct PIL_RUNTIME_MODULE *module
);

// Attempt to load a shared library into the process address space.
// module: The PIL_RUNTIME_MODULE to update with the handle of the loaded module.
// path: A nul-terminated, UTF-8 encoded string specifying the path and filename of the shared library to load.
// Returns zero if the module is successfully loaded.
PIL_API(int32_t)
PIL_RuntimeModuleLoad
(
    struct PIL_RUNTIME_MODULE *module, 
    char const                  *path
);

// Decrement the reference count on a shared library loaded into the process address space.
// If the reference count reaches zero, the shared library is unloaded from the process address space.
// module: The PIL_RUNTIME_MODULE representing the module to unload, initialized by a prior call to PIL_RuntimeModuleLoad.
PIL_API(void)
PIL_RuntimeModuleUnload
(
    struct PIL_RUNTIME_MODULE *module
);

// Determine whether a PIL_RUNTIME_MODULE refers to a valid shared library handle.
// module: The PIL_RUNTIME_MODULE to inspect.
// Returns non-zero if the runtime module appears to be a valid reference, or zero if the module is invalid.
PIL_API(int32_t)
PIL_RuntimeModuleIsValid
(
    struct PIL_RUNTIME_MODULE *module
);

// Resolve a function within a shared library that has been loaded into the process address space.
// module: The PIL_RUNTIME_MODULE representing the shared library.
// symbol: A nul-terminated ASCII string specifying the mangled name of the exported symbol.
// Returns the address of the entry point within the process address space, or null if no symbol with the specified identifier could be found.
PIL_API(PIL_PFN_Unknown)
PIL_RuntimeModuleResolve
(
    struct PIL_RUNTIME_MODULE *module, 
    char const                *symbol
);

#ifdef __cplusplus
}; /* extern "C" */
#endif

// Pull in the appropriate platform-specific header to pick up definitions for platform-specific types.
#if   PIL_TARGET_PLATFORM == PIL_PLATFORM_WIN32
#   include "platform_win32.h"
#elif PIL_TARGET_PLATFORM == PIL_PLATFORM_LINUX
#   include "platform_linux.h"
#else
#   error No Platform Interface Layer platform-specific header for your platform!
#endif

#endif /* __LIBPIL_PLATFORM_H__ */

