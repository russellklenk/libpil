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

// Define various shifts and masks used when working with item handles.
// These values are used when constructing and breaking apart PIL_HANDLE_BITS values.
// A PIL_HANDLE_BITS value consists of the following packed into a 32-bit unsigned integer:
// 31.|....................|...........0
//   F|IIIIIIIIIIIIIIIIIIII|GGGGGGGGGGG
// Starting from the most signficant bit:
// - F is set if the handle was valid at some point (used to distinguish valid and invalid handles).
// - I is an index value specifying the SparseIndex array index, which is then used to look up the corresponding dense index.
// - G is a generation counter used to differentiate handle slots that have been recycled.
#ifndef PIL_HANDLE_CONSTANTS
#   define PIL_HANDLE_CONSTANTS
#   define PIL_HANDLE_BITS_INVALID           0UL
#   define PIL_HANDLE_GENER_BITS             11
#   define PIL_HANDLE_INDEX_BITS             20
#   define PIL_HANDLE_FLAG_BITS              1

#   define PIL_HANDLE_GENER_SHIFT            0
#   define PIL_HANDLE_INDEX_SHIFT           (PIL_HANDLE_GENER_SHIFT + PIL_HANDLE_GENER_BITS)
#   define PIL_HANDLE_FLAG_SHIFT            (PIL_HANDLE_INDEX_SHIFT + PIL_HANDLE_INDEX_BITS)

#   define PIL_HANDLE_GENER_MASK           ((1UL << PIL_HANDLE_GENER_BITS) - 1)
#   define PIL_HANDLE_INDEX_MASK           ((1UL << PIL_HANDLE_INDEX_BITS) - 1)
#   define PIL_HANDLE_FLAG_MASK            ((1UL << PIL_HANDLE_FLAG_BITS ) - 1)

#   define PIL_HANDLE_GENER_MASK_PACKED     (PIL_HANDLE_GENER_MASK << PIL_HANDLE_GENER_SHIFT)
#   define PIL_HANDLE_INDEX_MASK_PACKED     (PIL_HANDLE_INDEX_MASK << PIL_HANDLE_INDEX_SHIFT)
#   define PIL_HANDLE_FLAG_MASK_PACKED      (PIL_HANDLE_FLAG_MASK  << PIL_HANDLE_FLAG_SHIFT )
#   define PIL_HANDLE_GENER_ADD_PACKED      (1UL << PIL_HANDLE_GENER_SHIFT)
#endif

// Define various constants related to the data table implementation.
// TABLE_MIN_OBJECT_COUNT: The minimum capacity for a table.
// TABLE_MAX_OBJECT_COUNT: The maximum capacity for a table.
#ifndef PIL_TABLE_CONSTANTS
#   define PIL_TABLE_CONSTANTS
#   define PIL_TABLE_MIN_OBJECT_COUNT        1UL
#   define PIL_TABLE_MAX_OBJECT_COUNT       (1UL << PIL_HANDLE_INDEX_BITS)
#   define PIL_TABLE_CHUNK_SIZE              1024
#endif

// Define the well-known allocator tag for host heap memory allocations.
#ifndef PIL_HEAP_ALLOCATOR_TAG
#   define PIL_HEAP_ALLOCATOR_TAG                                              PIL_MakeTag('H','E','A','P')
#endif

// Define the well-known allocator tag for host virtual memory allocations.
#ifndef PIL_VMM_ALLOCATOR_TAG
#   define PIL_VMM_ALLOCATOR_TAG                                               PIL_MakeTag('V','M','E','M')
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

// Swap the bytes in a two-byte value.
// _v: The value to byte swap.
// Returns the byte swapped value.
#ifndef PIL_ByteSwap2
#define PIL_ByteSwap2(_v)                                                      \
    ( (((_v) >> 8) & 0x00FF) |                                                 \
      (((_v) << 8) & 0xFF00) )
#endif

// Swap the bytes in a four-byte value.
// _v: The value to byte swap.
// Returns the byte swapped value.
#ifndef PIL_ByteSwap4
#define PIL_ByteSwap4(_v)                                                      \
    ( (((_v) >> 24) & 0x000000FF) |                                            \
      (((_v) >>  8) & 0x0000FF00) |                                            \
      (((_v) <<  8) & 0x00FF0000) |                                            \
      (((_v) << 24) & 0xFF000000) )
#endif

// Swap the bytes in an eight-byte value.
// _v: The value to byte swap.
// Returns the byte swapped value.
#ifndef PIL_ByteSwap8
#define PIL_ByteSwap8(_v)                                                      \
    ( (((_v) >> 56) & 0x00000000000000FFULL) |                                 \
      (((_v) >> 40) & 0x000000000000FF00ULL) |                                 \
      (((_v) >> 24) & 0x0000000000FF0000ULL) |                                 \
      (((_v) >>  8) & 0x00000000FF000000ULL) |                                 \
      (((_v) <<  8) & 0x000000FF00000000ULL) |                                 \
      (((_v) << 24) & 0x0000FF0000000000ULL) |                                 \
      (((_v) << 40) & 0x00FF000000000000ULL) |                                 \
      (((_v) << 56) & 0xFF00000000000000ULL) )
#endif

// Make a packed 32-bit value from four ASCII characters.
// _a: An ASCII character.
// _b: An ASCII character. 
// _c: An ASCII character.
// _d: An ASCII character.
// Returns a 32-bit unsigned integer value comprised of the given characters.
#ifndef PIL_MakeTag
#define PIL_MakeTag(_a, _b, _c, _d)                                            \
    (((uint32_t)(uint8_t)(_a)      ) |                                         \
     ((uint32_t)(uint8_t)(_b) <<  8) |                                         \
     ((uint32_t)(uint8_t)(_c) << 16) |                                         \
     ((uint32_t)(uint8_t)(_d) << 24))
#endif

// Make a packed version number from individual components.
// _major: The major version component, in [0, 1024).
// _minor: The minor version component, in [0, 1024).
// _patch: The patch version component, in [0, 4096).
// Returns a 32-bit unsigned integer value representing the given version.
#ifndef PIL_MakeVersion
#define PIL_MakeVersion(_major, _minor, _patch)                                \
    ((((uint32_t)(_major) & 0x3FFUL) << 22) |                                  \
     (((uint32_t)(_minor) & 0x3FFUL) << 12) |                                  \
      ((uint32_t)(_patch) & 0xFFFUL))
#endif

// Extract the major component of a packed version number.
// _packed: A packed version number created with PIL_MakeVersion.
// Returns the major version number component.
#ifndef PIL_Version_GetMajor
#define PIL_Version_GetMajor(_packed)                                          \
    (((uint32_t)(_packed) >> 22) & 0x3FFUL)
#endif

// Extract the minor component of a packed version number.
// _packed: A packed version number created with PIL_MakeVersion.
// Returns the minor version number component.
#ifndef PIL_Version_GetMinor
#define PIL_Version_GetMinor(_packed)                                          \
    (((uint32_t)(_packed) >> 12) & 0x3FFUL)
#endif

// Extract the patch component of a packed version number.
// _packed: A packed version number created with PIL_MakeVersion.
// Returns the patch version number component.
#ifndef PIL_Version_GetPatch
#define PIL_Version_GetPatch(_packed)                                          \
    (((uint32_t)(_packed) & 0xFFFUL))
#endif

// Allocate host memory with the correct size and alignment for an instance of a given type from a memory arena.
// _arena: The PIL_MEMORY_ARENA from which the allocation is being made.
// _type : A typename, such as int, specifying the type being allocated.
// Returns a pointer to the start of the allocated memory block, or NULL.
#ifndef PIL_MemoryArenaAllocateHostType
#define PIL_MemoryArenaAllocateHostType(_arena, _type)                         \
    ((_type*) PIL_MemoryArenaAllocateHost(NULL, (_arena), sizeof(_type), PIL_ALIGN_OF(_type)))
#endif

// Allocate memory with the correct size and alignment for an array of instance of a given type from a memory arena.
// _arena: The PIL_MEMORY_ARENA from which the allocation is being made.
// _type : A typename, such as int, specifying the type being allocated.
// _count: The number of elements in the array.
// Returns a pointer to the start of the allocated memory block, or NULL.
#ifndef PIL_MemoryArenaAllocateHostArray
#define PIL_MemoryArenaAllocateHostArray(_arena, _type, _count)                \
    ((_type*) PIL_MemoryArenaAllocateHost(NULL, (_arena), sizeof(_type) * (_count), PIL_ALIGN_OF(_type)))
#endif

// Allocate memory with the given object size and alignment for an array of object data from a memory arena.
// _arena: The PIL_MEMORY_ARENA from which the allocation is being made.
// _objsize: The object size, in bytes.
// _count: The number of elements in the array.
// _align: The object alignment, in bytes.
// Returns a pointer to the start of the allocated memory block, or NULL.
#ifndef PIL_MemoryArenaAllocateHostArrayRaw
#define PIL_MemoryArenaAllocateHostArrayRaw(_arena, _objsize, _align, _count)  \
    ((uint8_t*) PIL_MemoryArenaAllocateHost(NULL, (_arena), (_objsize) * (_count), (_align)))
#endif

// Allocate a fixed-size array on the system heap.
// _type : A typename, such as int, specifying the type being allocated.
// _count: The number of elements in the array.
// Returns a pointer to the start of the allocated array, or NULL.
#ifndef PIL_HostMemoryAllocateHeapArray
#define PIL_HostMemoryAllocateHeapArray(_type, _count)                         \
    ((_type*) PIL_HostMemoryAllocateHeap(NULL, sizeof(_type) * (_count), PIL_ALIGN_OF(_type)))
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

// Read the number of live items in the table from the PIL_TABLE_INDEX.
// _x: A pointer to a PIL_TABLE_INDEX structure.
// Returns the number of live items in the table.
#ifndef PIL_TableIndex_GetCount
#define PIL_TableIndex_GetCount(_x)                                            \
    (_x)->ActiveCount
#endif

// Read the number of items for which storage is committed from the PIL_TABLE_INDEX.
// _x: A pointer to a PIL_TABLE_INDEX structure.
// Returns the number of items that can be stored in the table without increasing the commitment.
#ifndef PIL_TableIndex_GetCommit
#define PIL_TableIndex_GetCommit(_x)                                           \
    (_x)->CommitCount
#endif

// Read the maximum capacity of a table from the PIL_TABLE_INDEX.
// _x: A pointer to a PIL_TABLE_INDEX structure.
// Returns the capacity of the table, in items.
#ifndef PIL_TableIndex_GetCapacity
#define PIL_TableIndex_GetCapacity(_x)                                         \
    (_x)->TableCapacity
#endif

// Read a handle value from a PIL_TABLE_INDEX.
// _x: A pointer to a PIL_TABLE_INDEX structure.
// _i: The zero-based dense index of the handle to read. The dense index is extracted from the sparse index value.
// Returns the corresponding PIL_HANDLE_BITS.
#ifndef PIL_TableIndex_GetHandle
#define PIL_TableIndex_GetHandle(_x, _i)                                       \
    (_x)->HandleArray[(_i)]
#endif

// Retrieve the base address of the storage buffer for a PIL_TABLE_DATA.
// _type: The typename to return.
// _d: The PIL_TABLE_DATA structure to query.
// Returns a pointer (_type*) to the first element of the storage buffer.
#ifndef PIL_TableData_GetBuffer
#define PIL_TableData_GetBuffer(_type, _d)                                     \
    (_type*)((_d)->StorageBuffer)
#endif

// Retrieve the number of bytes between elements in a PIL_TABLE_DATA.
// _d: The PIL_TABLE_DATA structure to query.
// Returns the number of bytes between elements.
#ifndef PIL_TableData_GetElementSize
#define PIL_TableData_GetElementSize(_d)                                       \
    (_d)->ElementSize
#endif

// Retrieve a pointer to the _i'th element in a PIL_TABLE_DATA.
// _type: The typename to return.
// _d: The PIL_TABLE_DATA structure to query.
// _i: The zero-based index of the item to retrieve.
// Returns a pointer (_type*) to the given element.
#ifndef PIL_TableData_GetElementPointer
#define PIL_TableData_GetElementPointer(_type, _d, _i)                         \
    (_type*)(((uint8_t*)(_d)->StorageBuffer) +((_i) * (_d)->ElementSize))
#endif

// Retrieve the PIL_TABLE_TYPE for a table.
// _td: A pointer to a PIL_TABLE_DESC structure.
// Returns one of the values of the PIL_TABLE_TYPE enumeration.
#ifndef PIL_Table_GetType
#define PIL_Table_GetType(_td)                                                 \
    (_td)->TableType
#endif

// Retrieve the number of active items in a table.
// _td: A pointer to a PIL_TÁBLE_DESC structure.
// Returns the number of active items in the table.
#ifndef PIL_Table_GetCount
#define PIL_Table_GetCount(_td)                                                \
    (_td)->Index->ActiveCount
#endif

// Retrieve the capacity of a table.
// _td: A pointer to a PIL_TABLE_DESC structure.
// Returns the maximum number of items that can be stored in the table.
#ifndef PIL_Table_GetCapacity
#define PIL_Table_GetCapacity(_td)                                             \
    (_td)->Index->TableCapacity
#endif

// Retrieve the number of data streams in a table.
// _td: A pointer to a PIL_TABLE_DESC structure.
// Returns the number of data streams defined for the table.
#ifndef PIL_Table_GetStreamCount
#define PIL_Table_GetStreamCount(_td)                                          \
    (_td)->StreamCount
#endif

// Retrieve the _ix'th handle for a table.
// _td: A pointer to a PIL_TABLE_DESC structure.
// _ix: The zero-based index of the item to return.
// Returns the PIL_HANDLE_BITS for the given item.
#ifndef PIL_Table_GetHandle
#define PIL_Table_GetHandle(_td, _ix)                                          \
    (_td)->Index->HandleArray[(_ix)]
#endif

// Retrieve a pointer to the first element in the active handle stream for a table.
// _td: A pointer to a PIL_TABLE_DESC structure.
// Returns a pointer (PIL_HANDLE_BITS*) to the first active identifier in the table. If equal to the address returned by PIL_Table_GetHandleEnd(_td), the table is empty.
#ifndef PIL_Table_GetHandleBegin
#define PIL_Table_GetHandleBegin(_td)                                          \
    (_td)->Index->HandleArray
#endif

// Retrieve a pointer to one-past the last element in the active handle stream for a table.
// _td: A pointer to a PIL_TABLE_DESC structure.
// Returns a pointer (PIL_HANDLE_BITS*) to one-past the last active identifier. Do not dereference the returned pointer.
#ifndef PIL_Table_GetHandleEnd
#define PIL_Table_GetHandleEnd(_td)                                            \
   ((_td)->Index->HandleArray + (_td)->Index->ActiveCount)
#endif

// Retrieve a pointer to the first element in a table data stream.
// _type: The typename to return.
// _td: A pointer to a TABLE_DESC structure.
// _si: The zero-based index of the data stream to query.
// Returns a pointer to the first active element in the data stream. If equal to the address returned by PIL_Table_GetStreamEnd(_type, _td, _si), the table is empty.
#ifndef PIL_Table_GetStreamBegin
#define PIL_Table_GetStreamBegin(_type, _td, _si)                              \
    ((_type*)((_td)->Streams[(_si)]->StorageBuffer))
#endif

// Retrieve a pointer to one-past the last element in a table data stream.
// _type: The typename to return.
// _td: A pointer to a PIL_TABLE_DESC structure.
// _si: The zero-based index of the data stream to query.
// Returns a pointer to one-past the last active element in the data stream. Do not dereference the returned pointer.
#ifndef PIL_Table_GetStreamEnd
#define PIL_Table_GetStreamEnd(_type, _td, _si)                                \
    ((_type*)(((uint8_t*)((_td)->Streams[(_si)]->StorageBuffer)) + ((_td)->Streams[(_si)]->ElementSize * (_td)->Index->ActiveCount)))
#endif

// Retrieve the number of bytes between elements in a table data stream.
// _td: A pointer to a PIL_TABLE_DESC structure.
// _si: The zero-based index of the data stream to query.
// Returns the number of bytes between elements in the data stream.
#ifndef PIL_Table_GetStreamElementSize
#define PIL_Table_GetStreamElementSize(_td, _si)                               \
    ((_td)->Streams[(_si)]->ElementSize)
#endif

// Retrieve a pointer to the _ei'th element in a table data stream.
// _type: The typename to return.
// _td: A pointer to a PIL_TABLE_DESC structure.
// _si: The zero-based index of the data stream to access.
// _ei: The zero-based index of the item to retrieve.
// Returns a pointer (_type*) to the given element.
#ifndef PIL_Table_GetStreamElement
#define PIL_Table_GetStreamElement(_type, _td, _si, _ei)                       \
    ((_type*)(((uint8_t*)((_td)->Streams[(_si)]->StorageBuffer)) + ((_td)->Streams[(_si)]->ElementSize * (_ei))))
#endif

// Forward-declare the types exported from the platform interface layer.
struct  PIL_CONTEXT;
struct  PIL_CONTEXT_INIT;                                                      // Defined in platform.h.
struct  PIL_RUNTIME_MODULE;                                                    // Defined in platform_*.h.
struct  PIL_MEMORY_BLOCK;                                                      // Defined in platform.h.
struct  PIL_MEMORY_ARENA;                                                      // Defined in platform.h.
struct  PIL_MEMORY_ARENA_INIT;                                                 // Defined in platform.h.
struct  PIL_MEMORY_ARENA_MARKER;                                               // Defined in platform.h.
struct  PIL_TABLE_INDEX;                                                       // Defined in platform.h.
struct  PIL_TABLE_INIT;                                                        // Defined in platform.h.
struct  PIL_TABLE_DATA;                                                        // Defined in platform.h.
struct  PIL_TABLE_DESC;                                                        // Defined in platform.h.
struct  PIL_TABLE_DATA_STREAM_DESC;                                            // Defined in platform.h.
struct  PIL_GPU_DRIVER;                                                        // Defined in platform.h.
struct  PIL_GPU_DRIVER_INIT;                                                   // Defined in platform.h.
struct  PIL_GPU_DRIVER_INFO;                                                   // Defined in platform.h.
struct  PIL_GPU_DRIVER_STATE;                                                  // Opaque type with no definition.
struct  PIL_GPU_DEVICE;                                                        // Defined in platform.h.
struct  PIL_GPU_DEVICE_INIT;                                                   // Defined in platform.h.
struct  PIL_GPU_DEVICE_STATE;                                                  // Opaque type with no definition.

typedef uint32_t PIL_HANDLE_BITS;                                              // Handles are opaque 32-bit integer values.

typedef int  (*PIL_PFN_Unknown         )(void);                                // Signature for a dynamically-resolved function. The calling code will have to cast the function pointer to its specific type.
typedef int  (*PIL_PFN_GPU_DriverInit  )(struct PIL_GPU_DRIVER_INIT*, void**); // Signature for a function that initializes a GPU driver.
typedef void (*PIL_PFN_GPU_DriverShut  )(struct PIL_GPU_DRIVER_STATE*);        // Signature for a function that shuts down a GPU driver.
typedef int  (*PIL_PFN_GPU_DriverQuery )(struct PIL_GPU_DRIVER_STATE*, struct PIL_GPU_DRIVER_INFO*);         // Signature for a function that retrieves information about available GPUs.
typedef int  (*PIL_PFN_GPU_DeviceCreate)(struct PIL_GPU_DRIVER_STATE*, struct PIL_GPU_DEVICE_INIT*, void**); // Signature for a function that creates a logical GPU device interface.
typedef void (*PIL_PFN_GPU_DeviceDelete)(struct PIL_GPU_DRIVER_STATE*, struct PIL_GPU_DEVICE_STATE*);        // Signature for a function that destroys a logical GPU device interface.

typedef enum   PIL_MEMORY_ALLOCATOR_TYPE {                                     // Define the allowed values for memory allocator types. An allocator can manage either host or device memory. Device memory may or may not be visible to the host CPU.
    PIL_MEMORY_ALLOCATOR_TYPE_INVALID                 =  0UL,                  // This value is invalid and should not be used.
    PIL_MEMORY_ALLOCATOR_TYPE_HOST_VMM                =  1UL,                  // The allocator is a host memory allocator, returning address space from the system virtual memory manager.
    PIL_MEMORY_ALLOCATOR_TYPE_HOST_HEAP               =  2UL,                  // The allocator is a host memory allocator, returning address space from the system heap.
    PIL_MEMORY_ALLOCATOR_TYPE_DEVICE                  =  3UL,                  // The allocator is a device memory allocator.
} PIL_MEMORY_ALLOCATOR_TYPE;

typedef enum   PIL_TABLE_TYPE {                                                // Define the recognized types of data tables.
    PIL_TABLE_TYPE_INVALID                            =  0UL,                  // This value is invalid and should not be used.
    PIL_TABLE_TYPE_PRIMARY                            =  1UL,                  // The table stores data associated with primary keys (identifiers are created by the table itself.)
    PIL_TABLE_TYPE_FOREIGN                            =  2UL,                  // The table stores data associated with foreign keys (identifiers are created by a different table, but can be used to look up items within this table.)
} PIL_TABLE_TYPE;

typedef enum   PIL_GPU_DRIVER_TYPE {                                           // Define the recognized types of GPU drivers.
    PIL_GPU_DRIVER_TYPE_UNKNOWN                       =  0UL,                  // This value is invalid and should not be used.
    PIL_GPU_DRIVER_TYPE_PLATFORM_DEFAULT              =  1UL,                  // Select the default GPU driver for the host platform.
    PIL_GPU_DRIVER_TYPE_VULKAN1                       =  2UL,                  // Use Vulkan 1.x to interface to the GPUs on the host.
} PIL_GPU_DRIVER_TYPE;

typedef enum   PIL_GPU_DEVICE_ORDINAL {                                        // Define special values that can be supplied for PIL_GPU_DEVICE_INIT::DeviceOrdinal.
    PIL_GPU_DEVICE_ORDINAL_ANY                        =  -1L,                  // Select the first physical GPU device meeting the application requirements.
} PIL_GPU_DEVICE_ORDINAL;

typedef enum   PIL_MEMORY_ARENA_FLAGS {                                        // Flags that can be bitwise OR'd to control the behavior of an arena memory allocator.
    PIL_MEMORY_ARENA_FLAGS_NONE                       = (0UL <<  0),           // No flags are specified. Specifying no flags will cause arena creation to fail.
    PIL_MEMORY_ARENA_FLAG_INTERNAL                    = (1UL <<  0),           // The memory arena should allocate memory internally, and free the memory when the arena is destroyed.
    PIL_MEMORY_ARENA_FLAG_EXTERNAL                    = (1UL <<  1),           // The memory arena uses memory supplied and managed by the application.
} PIL_MEMORY_ARENA_FLAGS;

typedef enum   PIL_HOST_MEMORY_ALLOCATION_FLAGS {                              // Flags that can be bitwise OR'd to control the allocation attributes for a single host memory allocation.
    PIL_HOST_MEMORY_ALLOCATION_FLAGS_DEFAULT         = (0UL <<  0),            // The memory can be read and written by the host, and ends with a guard page.
    PIL_HOST_MEMORY_ALLOCATION_FLAG_READ             = (1UL <<  0),            // The memory can be read by the host.
    PIL_HOST_MEMORY_ALLOCATION_FLAG_WRITE            = (1UL <<  1),            // The memory can be written by the host.
    PIL_HOST_MEMORY_ALLOCATION_FLAG_EXECUTE          = (1UL <<  2),            // The allocation can contain code that can be executed by the host.
    PIL_HOST_MEMORY_ALLOCATION_FLAG_NOGUARD          = (1UL <<  3),            // The allocation will not end with a guard page.
    PIL_HOST_MEMORY_ALLOCATION_FLAGS_READWRITE       =                         // The committed memory can be read and written by the host.
        PIL_HOST_MEMORY_ALLOCATION_FLAG_READ         | 
        PIL_HOST_MEMORY_ALLOCATION_FLAG_WRITE
} PIL_HOST_MEMORY_ALLOCATION_FLAGS;

typedef enum   PIL_DEVICE_MEMORY_ALLOCATION_FLAGS {                            // Flags that can be bitwise OR'd to control the allocation attributes for a single device memory allocation.
    PIL_DEVICE_MEMORY_ALLOCATION_FLAGS_DEFAULT       = (0UL <<  0),            // Equivalent to PIL_HOST_MEMORY_ALLOCATION_FLAG_DEVICE_LOCAL.
    PIL_DEVICE_MEMORY_ALLOCATION_FLAG_DEVICE_LOCAL   = (1UL <<  0),            // Allocate in device-local memory, which may not be directly visible to the host CPU.
    PIL_DEVICE_MEMORY_ALLOCATION_FLAG_HOST_VISIBLE   = (1UL <<  1),            // The returned address should be visible to the host CPU.
    PIL_DEVICE_MEMORY_ALLOCATION_FLAG_COHERENT       = (1UL <<  2),            // The memory should be host CPU cache coherent.
    PIL_DEVICE_MEMORY_ALLOCATION_FLAG_WRITE_COMBINED = (1UL <<  3),            // The memory should be write-combined.
} PIL_DEVICE_MEMORY_ALLOCATION_FLAGS;

typedef enum   PIL_GPU_DRIVER_FEATUE_FLAGS {                                   // Flags that can be bitwise OR'd to define GPU driver selection requirements or capabilities.
    PIL_GPU_DRIVER_FEATURE_FLAGS_NONE                = (0UL <<  0),            // The driver is not supported on the host system.
    PIL_GPU_DRIVER_FEATURE_FLAG_DEBUG                = (1UL <<  0),            // The driver supports debugging features at some cost in performance.
    PIL_GPU_DRIVER_FEATURE_FLAG_COMPUTE              = (1UL <<  1),            // The driver supports creating GPU compute engine interfaces.
    PIL_GPU_DRIVER_FEATURE_FLAG_DISPLAY              = (1UL <<  2),            // The driver supports creating GPU display engine interfaces.
    PIL_GPU_DRIVER_FEATURE_FLAG_PRESENT              = (1UL <<  3),            // The driver supports creating GPU presentation engine interfaces.
} PIL_GPU_DRIVER_FEATURE_FLAGS;

typedef enum   PIL_GPU_DEVICE_FEATURE_FLAGS {                                  // Flags that can be bitwise OR'd to define GPU device selection requirements or capabilities.
    PIL_GPU_DEVICE_FEATURE_FLAGS_NONE                = (0UL <<  0),            // No flags are specified. Device creation will always fail.
    PIL_GPU_DEVICE_FEATURE_FLAG_COMPUTE              = (1UL <<  0),            // Device selection will consider only GPUs that support compute capability.
    PIL_GPU_DEVICE_FEATURE_FLAG_DISPLAY              = (1UL <<  1),            // Device selection will consider only GPUs that support display capability.
    PIL_GPU_DEVICE_FEATURE_FLAG_PRESENT              = (1UL <<  2),            // Device selection will consider only GPUs that support presentation capability.
    PIL_GPU_DEVICE_FEATURE_FLAG_DISCRETE             = (1UL <<  3),            // Device selection will consider discrete GPUs.
    PIL_GPU_DEVICE_FEATURE_FLAG_INTEGRATED           = (1UL <<  4),            // Device selection will consider integrated GPUs.
    PIL_GPU_DEVICE_FEATURE_FLAG_EXCLUSIVE            = (1UL <<  5),            // Device creation should fail if the selected GPU already has an active logical device.
} PIL_GPU_DEVICE_FEATURE_FLAGS;

typedef union  PIL_ADDRESS_OR_OFFSET {                                         // A union representing either an offset from some base location, or a valid address in the host process address space.
    uint64_t                        BaseOffset;                                // The offset value, specified relative to some base location.
    void                          *HostAddress;                                // The address in the host process address space.
} PIL_ADDRESS_OR_OFFSET;

typedef struct PIL_CONTEXT_INIT {                                              // 
    char const                *ApplicationName;                                // A nul-terminated string specifying a name for the application creating the context.
    int32_t                    AppVersionMajor;                                // The major version component of the application.
    int32_t                    AppVersionMinor;                                // The minor version component of the application.
    int32_t                    AppVersionPatch;                                // The patch version component of the application.
} PIL_CONTEXT_INIT;

typedef struct PIL_MEMORY_BLOCK {                                              // Data associated with a host or device memory allocation.
    uint64_t                    BytesCommitted;                                // The number of bytes that can be accessed by the application.
    uint64_t                     BytesReserved;                                // The number of bytes of address space reserved by the allocation.
    uint64_t                       BlockOffset;                                // The byte offset of the start of the allocated region. This value is set for both host and device allocations.
    uint8_t                       *HostAddress;                                // The address of the allocated region in the host process address space. This value is NULL for device allocations.
    uint32_t                   AllocationFlags;                                // One or more bitwise-OR'd values of the PIL_HOST_MEMORY_ALLOCATION_FLAGS enumeration.
    uint32_t                      AllocatorTag;                                // The tag of the memory allocator that created the block (produced with PIL_MakeTag).
} PIL_MEMORY_BLOCK;

typedef struct PIL_MEMORY_ARENA {                                              // An arena-style memory allocator.
    char const                  *AllocatorName;                                // A nul-terminated string specifying the name of the allocator. Used for debugging.
    uint64_t                       MemoryStart;                                // The address or offset of the start of the memory block from which sub-allocations are returned.
    uint64_t                        NextOffset;                                // The byte offset of the next permanent allocation to return.
    uint64_t                     MaximumOffset;                                // The maximum value of NextOffset.
    uint64_t                        NbReserved;                                // The number of bytes of reserved address space.
    uint64_t                       NbCommitted;                                // The number of bytes of committed address space.
    uint32_t                     AllocatorType;                                // One of the values of the PIL_MEMORY_ALLOCATOR_TYPE enumeration specifying whether the memory allocator allocates host or device memory.
    uint32_t                      AllocatorTag;                                // An opaque 32-bit value used to tag allocations from the arena.
    uint32_t                   AllocationFlags;                                // One or more bitwise-OR'd values of the PIL_HOST_MEMORY_ALLOCATION_FLAGS or PIL_DEVICE_MEMORY_ALLOCATION_FLAGS enumeration.
    uint32_t                        ArenaFlags;                                // One or more bitwise-OR'd values of the PIL_MEMORY_ARENA_FLAGS enumeration.
} PIL_MEMORY_ARENA;

typedef struct PIL_MEMORY_ARENA_INIT {                                         // Data used to configure an arena-style memory allocator.
    char const                  *AllocatorName;                                // A nul-terminated string specifying the name of the allocator. Used for debugging.
    uint64_t                       ReserveSize;                                // The number of bytes of address space reserved for the memory block.
    uint64_t                     CommittedSize;                                // The number of bytes of address space committed in the memory block.
    PIL_ADDRESS_OR_OFFSET          MemoryStart;                                // The offset or host address of the start of the allocated memory block.
    uint32_t                     AllocatorType;                                // One of the values of the PIL_MEMORY_ALLOCATOR_TYPE enumeration specifying whether the memory allocator allocates host or device memory.
    uint32_t                      AllocatorTag;                                // An opaque 32-bit value used to tag allocations from the arena.
    uint32_t                   AllocationFlags;                                // One or more bitwise-OR'd values of the PIL_HOST_MEMORY_ALLOCATION_FLAGS or PIL_DEVICE_MEMORY_ALLOCATION_FLAGS enumeration.
    uint32_t                        ArenaFlags;                                // One or more bitwise-OR'd values of the PIL_MEMORY_ARENA_FLAGS enumeration.
} PIL_MEMORY_ARENA_INIT;

typedef struct PIL_MEMORY_ARENA_MARKER {                                       // Stores the state of an arena allocator at a specific point in time. A marker can be used to roll-back the allocator state to the marked point in time, invalidating all allocations made since that point.
    struct PIL_MEMORY_ARENA             *Arena;                                // The PIL_MEMORY_ARENA from which the marker was obtained.
    uint64_t                             State;                                // A value encoding the state of the memory arena when the marker was obtained.
} PIL_MEMORY_ARENA_MARKER;

typedef struct PIL_TABLE_INDEX {                                               // Data associated with an index mapping a 32-bit integer item ID to a dense array index.
    uint32_t                      *SparseIndex;                                // A fully committed sparse array used to map item handles to indices in PAL_TABLE_DATA and the HandleArray.
    PIL_HANDLE_BITS               *HandleArray;                                // A partially committed, densely-packed array of the handles associated with each item in the table.
    uint32_t                       ActiveCount;                                // The number of items in the table that are valid.
    uint32_t                     HighWatermark;                                // The maximum number of items ever present in the table.
    uint32_t                       CommitCount;                                // The maximum number of items that can be stored in the table without committing additional memory.
    uint32_t                     TableCapacity;                                // The maximum capacity of the table, in items.
} PIL_TABLE_INDEX;

typedef struct PIL_TABLE_DATA {                                                // Describes a buffer used to store tightly-packed item data records in a table. All items in a PIL_TABLE_DATA have the same type, but a table may have more than one associated PIL_TABLE_DATA.
    uint8_t                     *StorageBuffer;                                // The start of the storage buffer used for storing item data.
    uint32_t                       ElementSize;                                // The size of the record type stored in the table data.
    uint32_t                          Reserved;                                // Reserved for future use. Set to zero.
} PIL_TABLE_DATA;

typedef struct PIL_TABLE_DESC {                                                // Provides access to all of the components of a table.
    struct PIL_TABLE_INDEX              *Index;                                // The PIL_TABLE_INDEX used to map handles to their corresponding dense array indices.
    struct PIL_TABLE_DATA            **Streams;                                // The array of densely-packed table data streams. The array has StreamCount entries.
    uint32_t                       StreamCount;                                // The number of table data stream entries in the Streams array.
    uint32_t                         TableType;                                // One of the values of the PIL_TABLE_TYPE enumeration.
} PIL_TABLE_DESC;

typedef struct PIL_TABLE_DATA_STREAM_DESC {                                    // Describes a PIL_TABLE_DATA representing a table data stream.
    struct PIL_TABLE_DATA                *Data;                                // A pointer to the PIL_TABLE_DATA that owns the data stream.
    uint32_t                              Size;                                // The size of the record type, in bytes.
    uint32_t                          Reserved;                                // Reserved for future use. Set to zero.
} PIL_TABLE_DATA_STREAM_DESC;

typedef struct PIL_TABLE_INIT {                                                // Data used to configure a data table.
    struct PIL_TABLE_INDEX              *Index;                                // The PIL_TABLE_INDEX used to map handles to their corresponding dense array indices.
    struct PIL_TABLE_DATA_STREAM_DESC *Streams;                                // An array of StreamCount descriptors of the PIL_TABLE_DATA objects to initialize.
    uint32_t                       StreamCount;                                // The number of entries in the Streams array.
    uint32_t                     TableCapacity;                                // The maximum number of items that can be stored in the table.
    uint32_t                     InitialCommit;                                // The number of items for which storage is committed during table initialization.
    uint32_t                         TableType;                                // One of the values of the PIL_TABLE_TYPE enumeration.
} PIL_TABLE_INIT;

typedef struct PIL_GPU_DRIVER {                                                // The dispatch table used to interface with a GPU driver.
    struct PIL_GPU_DRIVER_STATE         *State;                                // Internal state allocated by the GPU driver.
    PIL_PFN_GPU_DriverInit        DriverInitFn;                                // The function used to create and initialize a GPU driver interface.
    PIL_PFN_GPU_DriverShut        DriverShutFn;                                // The function used to shutdown and free resources allocated by a GPU driver interface.
    PIL_PFN_GPU_DriverQuery      DriverQueryFn;                                // The function used to query for supported GPUs and capabilities on the host system.
    PIL_PFN_GPU_DeviceCreate    DeviceCreateFn;                                // The function used to create and initialize a logical GPU device.
    PIL_PFN_GPU_DeviceDelete    DeviceDeleteFn;                                // The function used to free resources and shutdown a logical GPU device.
} PIL_GPU_DRIVER;

typedef struct PIL_GPU_DRIVER_INIT {                                           // Data used to configure the creation of a GPU driver interface.
    uint32_t                  RequiredFeatures;                                // One or more bitwise OR'd values of the PIL_GPU_DRIVER_FEATURE_FLAGS enumeration specifying features required by the application.
    uint32_t                    MaxDeviceCount;                                // The maximum number of logical GPU devices that will be created by the application.
} PIL_GPU_DRIVER_INIT;

typedef struct PIL_GPU_DRIVER_INFO {                                           // Data providing basic information about a GPU driver and its capabilities.
    uint32_t                     DriverVersion;                                // The packed version of the GPU driver, created with PIL_MakeVersion. Use the PIL_Version_GetYYYYY macros to extract individual components.
    uint32_t                       DeviceCount;                                // The number of physical GPUs supporting the GPU driver interface installed in the host system.
    uint32_t                 SupportedFeatures;                                // One or more bitwise OR'd values of the PIL_GPU_DRIVER_FEATURE_FLAGS enumeration specifying features available to the application.
    char                       DriverName[128];                                // A nul-terminated ASCII string, suitable for display, describing the driver.
} PIL_GPU_DRIVER_INFO;

typedef struct PIL_GPU_DEVICE {                                                // The dispatch table used to interface with a GPU device.
    struct PIL_GPU_DEVICE_STATE   *DeviceState;                                // Internal state allocated by the logical GPU device.
    struct PIL_GPU_DRIVER_STATE   *DriverState;                                // Internal state allocated by the GPU driver.
    uint32_t                     DeviceOrdinal;                                // The zero-based index of the physical device bound to the logical GPU device interface.
    uint32_t                 SupportedFeatures;                                // One or more bitwise OR'd values of the PIL_GPU_DEVICE_FEATURE_FLAGS enumeration specifying the features supported by the device.
    // ... resource creation
} PIL_GPU_DEVICE;

typedef struct PIL_GPU_DEVICE_INIT {                                           // Data used to configure the creation of a logical GPU device.
    uint32_t                  RequiredFeatures;                                // One or more bitwise OR'd values of the PIL_GPU_DEVICE_FEATURE_FLAGS enumeration specifying the features required by the application.
    int32_t                      DeviceOrdinal;                                // The zero-based index of the physical device to use, or PIL_GPU_DEVICE_ORDINAL_ANY to select any device meeting the feature requirements.
} PIL_GPU_DEVICE_INIT;

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

// Mix the bits in a 32-bit value.
// input: The input value.
// Returns the input value with its bits mixed.
PIL_API(uint32_t)
PIL_BitsMix32
(
    uint32_t input
);

// Mix the bits in a 64-bit value.
// input: The input value.
// Returns the input value with its bits mixed.
PIL_API(uint64_t)
PIL_BitsMix64
(
    uint64_t input
);

// Compute a 32-bit non-cryptographic hash of some data.
// data: The data to hash.
// length: The number of bytes of data to hash.
// seed: An initial value used to seed the hash.
// Returns a 32-bit unsigned integer computed from the data.
PIL_API(uint32_t)
PIL_HashData32
(
    void const *data, 
    size_t    length, 
    uint32_t    seed
);

// Compute a 64-bit non-cryptographic hash of some data.
// data: The data to hash.
// length: The number of bytes of data to hash.
// seed: An initial value used to seed the hash.
// Returns a 64-bit unsigned integer computed from the data.
PIL_API(uint64_t)
PIL_HashData64
(
    void const *data, 
    size_t    length, 
    uint64_t    seed 
);

// Allocate memory from the system heap.
// o_block: Pointer to a MEMORY_BLOCK to populate with information about the allocation.
// n_bytes: The minimum number of bytes to allocate.
// alignment: The required alignment, in bytes. The PIL_AlignOf(T) macro can be used to obtain the necessary alignment for a given type. 
// Returns a pointer to the start of the aligned memory block, or NULL if the allocation request could not be satisfied.
PIL_API(void*)
PIL_HostMemoryAllocateHeap
(
    struct PIL_MEMORY_BLOCK *o_block, 
    size_t                   n_bytes, 
    size_t                 alignment
);

// Free a memory block returned from the system heap.
// host_addr: An address returned by PIL_HostMemoryAllocateHeap.
PIL_API(void)
PIL_HostMemoryFreeHeap
(
    void *host_addr
);

// Allocate address space from the host virtual memory manager.
// The memory block is aligned to at least the operating system page size.
// o_block: Pointer to a PIL_MEMORY_BLOCK to populate with information about the allocation.
// reserve_bytes: The number of bytes of process address space to reserve.
// commit_bytes: The number of bytes of process address space to commit. This value can be zero.
// alloc_flags: One or more bitwise OR'd values from the PIL_HOST_MEMORY_ALLOCATION_FLAGS enumeration.
// Returns a pointer to the start of the reserved address space, or NULL if the allocation could not be satisfied.
PIL_API(void*)
PIL_HostMemoryReserveAndCommit
(
    struct PIL_MEMORY_BLOCK *o_block, 
    size_t             reserve_bytes, 
    size_t              commit_bytes, 
    uint32_t             alloc_flags
);

// Increase the number of bytes committed in a memory block allocated from the host virtual memory manager.
// The commitment is rounded up to the next even multiple of the operating system page size.
// o_block: Pointer to a PIL_MEMORY_BLOCK describing the memory block attributes after the commitment increase.
// block: Pointer to a PIL_MEMORY_BLOCK describing the memory block attributes prior to the commitment increase.
// commit_bytes: The total amount of address space within the memory block that should be committed, in bytes.
// Returns non-zero if at least commit_bytes are committed within the memory block, or zero if the commitment could not be met.
PIL_API(int32_t)
PIL_HostMemoryIncreaseCommitment
(
    struct PIL_MEMORY_BLOCK *o_block, 
    struct PIL_MEMORY_BLOCK   *block, 
    size_t              commit_bytes
);

// Flush the host CPU instruction cache after writing dynamically-generated code to a memory block.
// block: Pointer to a PIL_MEMORY_BLOCK describing the memory block containing the dynamically-generated code.
PIL_API(void)
PIL_HostMemoryFlush
(
    struct PIL_MEMORY_BLOCK const *block
);

// Decommit and release a block of memory returned by the system virtual memory manager.
// block: Pointer to a PIL_MEMORY_BLOCK describing the memory region to release.
PIL_API(void)
PIL_HostMemoryRelease
(
    struct PIL_MEMORY_BLOCK *block
);

// Check a PIL_MEMORY_BLOCK to determine whether it represents a valid allocation (as opposed to a failed allocation).
// block: The PIL_MEMORY_BLOCK to inspect.
// Returns non-zero if the block describes a valid allocation, or zero if block describes a failed allocation.
PIL_API(int32_t)
PIL_MemoryBlockIsValid
(
    struct PIL_MEMORY_BLOCK const *block
);

// Compare two PIL_MEMORY_BLOCK instances to determine if a reallocation moved the memory block.
// old_block: A description of the memory block prior to the reallocation.
// new_block: A description of the memory block after the reallocation.
// Returns non-zero if the memory block moved, or zero if the memory block did not move.
PIL_API(int32_t)
PIL_MemoryBlockDidMove
(
    struct PIL_MEMORY_BLOCK const *old_block, 
    struct PIL_MEMORY_BLOCK const *new_block
);

// Create a memory arena using the specified configuration.
// The memory arena can sub-allocate from either an internal or an external block of memory.
// o_arena: The PIL_MEMORY_ARENA to initialize.
// init: Data used to configure the behavior of the memory arena.
// Returns zero if the memory arena is successfully created, or -1 if an error occurred.
PIL_API(int)
PIL_MemoryArenaCreate
(
    struct PIL_MEMORY_ARENA         *o_arena, 
    struct PIL_MEMORY_ARENA_INIT const *init
);

// Free resources associated with a memory arena.
// For a memory arena managing an internal memory block, this frees the memory block.
// arena: The memory arena to delete.
PIL_API(void)
PIL_MemoryArenaDelete
(
    struct PIL_MEMORY_ARENA *arena
);

// Allocate memory from an arena.
// o_block: Pointer to a PIL_MEMORY_BLOCK to populate with information about the allocation.
// arena: The memory arena from which the memory will be allocated.
// size: The minimum number of bytes to allocate.
// alignment: The required alignment of the returned address, in bytes.
// Returns zero if the allocation is successful and o_block is populated with information about the allocation, or -1 if the allocation request could not be satisfied.
PIL_API(int)
PIL_MemoryArenaAllocate
(
    struct PIL_MEMORY_BLOCK *o_block, 
    struct PIL_MEMORY_ARENA   *arena, 
    size_t                      size, 
    size_t                 alignment
);

// Allocate host memory from an arena.
// The arena must have type PIL_MEMORY_ALLOCATOR_TYPE_HOST_HEAP or PIL_MEMORY_ALLOCATOR_TYPE_HOST_VMM.
// o_block: Pointer to an optional PIL_MEMORY_BLOCK to populate with information about the allocation.
// arena: The memory arena from which the memory will be allocated.
// size: The minimum number of bytes to allocate.
// alignment: The required alignment of the returned address, in bytes.
// Returns a pointer to the start of the memory block, or NULL if the allocation request could not be satisfied.
PIL_API(void*)
PIL_MemoryArenaAllocateHost
(
    struct PIL_MEMORY_BLOCK *o_block, 
    struct PIL_MEMORY_ARENA   *arena, 
    size_t                      size, 
    size_t                 alignment
);

// Retrieve a marker representing the state of the memory arena at the time of the call.
// arena: The PIL_MEMORY_ARENA whose state will be returned.
// Returns an object representing the state of the arena at the time of the call, which can be used to invalidate all allocations made after the call.
PIL_API(struct PIL_MEMORY_ARENA_MARKER)
PIL_MemoryArenaMark
(
    struct PIL_MEMORY_ARENA *arena
);

// Convert a memory arena marker into a valid address within the memory block managed by the arena.
// The arena must have type PIL_MEMORY_ALLOCATOR_TYPE_HOST_HEAP or PIL_MEMORY_ALLOCATOR_TYPE_HOST_VMM.
// marker: The PIL_MEMORY_ARENA_MARKER to query.
// Returns the address in host memory corresponding to the address, or NULL if the marker or arena is invalid.
PIL_API(uint8_t*)
PIL_MemoryArenaMarkerToHostAddress
(
    struct PIL_MEMORY_ARENA_MARKER marker
);

// Calculate the difference, in bytes, between two memory arena markers.
// The markers must have been obtained from the same arena and the arena must have type MEMORY_ALLOCATOR_TYPE_HOST_HEAP or MEMORY_ALLOCATOR_TYPE_HOST_VMM.
// marker1: A memory arena marker.
// marker2: A memory arena marker.
// Returns the number of bytes between the two markers.
PIL_API(ptrdiff_t)
PIL_MemoryArenaMarkerDifference
(
    struct PIL_MEMORY_ARENA_MARKER marker1, 
    struct PIL_MEMORY_ARENA_MARKER marker2
);

// Reset the state of the memory arena, invalidating all existing allocations.
// arena: The PIL_MEMORY_ARENA to reset.
PIL_API(void)
PIL_MemoryArenaReset
(
    struct PIL_MEMORY_ARENA *arena
);

// Reset the state of the memory arena back to a previously obtained marker.
// This invalidates all allocations from the arena made since the marker was obtained.
// arena: The PIL_MEMORY_ARENA to reset.
// marker: The marker representing the reset point.
PIL_API(void)
PIL_MemoryArenaResetToMarker
(
    struct PIL_MEMORY_ARENA        *arena, 
    struct PIL_MEMORY_ARENA_MARKER marker 
);

// Perform an internal self-consistency check on a PIL_TABLE_INDEX structure.
// This routine is intended to be used for debugging purposes. In debug builds, asserts fire for validation errors.
// index: Pointer to a PIL_TABLE_INDEX to validate.
// Returns non-zero if the index is valid, or zero if the index is corrupted.
PIL_API(int32_t)
PIL_ValidateTableIndex
(
    struct PIL_TABLE_INDEX *index
);

// Given a pointer to a data element within a PIL_TABLE_DATA buffer, retrieve the corresponding dense array index if the item.
// The caller must ensure that element_address points to a valid address within the supplied table data buffer.
// stream: Pointer to a PIL_TABLE_DATA describing the data buffer containing the item.
// element_address: A pointer to the start of an element within the table data buffer.
// Returns the zero-based index of the element.
PIL_API(uint32_t)
PIL_TableData_GetElementIndex
(
    struct PIL_TABLE_DATA *stream, 
    void         *element_address
);

// Allocate resources for a data table.
// init: Data describing the table structure to initialize.
// Returns zero if the table is successfully initialized, or non-zero if an error occurred.
PIL_API(int32_t)
PIL_TableCreate
(
    struct PIL_TABLE_INIT *init
);

// Ensure that a data table can accomodate a given number of items.
// If necessary and possible, the table committment is increased to meet the need.
// table: Pointer to a PIL_TABLE_DESC describing the index and data streams.
// total_need: The total number of items the caller needs to store in the table (for example, PIL_TableIndex_GetCount() + 1).
// chunk_size: The chunk size for the table. The committment is increased to an even multiple of this value to reduce the overall number of allocations.
// Returns non-zero if the table can store at least total_need items, or zero if an error occurred.
PIL_API(int32_t)
PIL_TableEnsure
(
    struct PIL_TABLE_DESC *table, 
    uint32_t          total_need, 
    uint32_t          chunk_size
);

// Free all resources allocated for a data table.
// table: Pointer to a PIL_TABLE_DESC describing the table index and data streams.
PIL_API(void)
PIL_TableDelete
(
    struct PIL_TABLE_DESC *table
);

// Reset a table back to empty, invalidating all existing item identifiers.
// The table data should have already had any necessary cleanup performed prior to this call.
// table: Pointer to a PIL_TABLE_DESC describing the table index and data streams.
PIL_API(void)
PIL_TableDropAll
(
    struct PIL_TABLE_DESC *table
);

// Invalidate a single table item identifier.
// The table data should have already had any necessary cleanup performed prior to this call.
// The caller is responsible for ensuring that bits represents a valid table item identifier.
// table: Pointer to a PIL_TABLE_DESC describing the table index and data streams.
// item : The PIL_HANDLE_BITS identifying the item to drop.
// Returns the identifier of the item that was moved as a result of the deletion, or PIL_HANDLE_BITS_INVALID if no item was moved.
PIL_API(PIL_HANDLE_BITS)
PIL_TableDropOne
(
    struct PIL_TABLE_DESC *table, 
    PIL_HANDLE_BITS         item
);

// Resolve an item identifier into a record index.
// The resulting record index can be supplied to PIL_TableData_GetElementPointer to retrieve the item data within a data stream.
// o_record_index: Pointer to the location to update with the item dense array index.
// table: Pointer to a PIL_TABLE_DESC describing the table index and data streams.
// item : The PIL_HANDLE_BITS identifying the item to resolve.
// Returns non-zero if the item is successfully resolved and o_record_index was set to the item dense array index, or zero if the item identifier is invalid.
PIL_API(int32_t)
PIL_TableResolve
(
    uint32_t     *o_record_index, 
    struct PIL_TABLE_DESC *table, 
    PIL_HANDLE_BITS         item
);

// Generate a new item identifier and assign it a slot within the table data streams.
// This operation is valid only on tables of type PIL_TABLE_TYPE_PRIMARY.
// The caller is responsible for ensuring that the table has sufficient committed capacity using the PIL_TableEnsure function.
// o_record_index: Pointer to the location to update with the new item's dense array index, which can be supplied to PIL_TableData_GetElementPointer.
// table: Pointer to a PIL_TABLE_DESC describing the table index and data streams.
// Returns the identifier of the new item, or PIL_HANDLE_BITS_INVALID if the table is full.
PIL_API(PIL_HANDLE_BITS)
PIL_TableCreateId
(
    uint32_t     *o_record_index, 
    struct PIL_TABLE_DESC *table
);

// Insert a foreign item identifier and assign it a slot within the table data streams.
// This operation is valid only on tables of type PIL_TABLE_TYPE_FOREIGN.
// The caller is responsible for ensuring that the table has sufficient committed capacity using the PIL_TableEnsure function.
// o_record_index: Pointer to the location to update with the new item's dense array index, which can be supplied to PIL_TableData_GetElementPointer.
// table: Pointer to a PIL_TABLE_DESC describing the table index and data streams.
// item : The PIL_HANDLE_BITS, created by a call to PIL_TableCreateId on a different table, identifying the item to insert.
// Returns non-zero if the item is inserted successfully, or zero if the item cannot be inserted.
PIL_API(int32_t)
PIL_TableInsertId
(
    uint32_t     *o_record_index, 
    struct PIL_TABLE_DESC *table, 
    PIL_HANDLE_BITS         item
);

// Open a connection to a GPU interface on the host and retrieve information about available GPUs.
// driver: The PIL_GPU_DRIVER structure to populate.
// config: Information about the application and its GPU requirements.
// driver_info: Pointer to a PIL_GPU_DRIVER_INFO structure to populate with information about the physical GPUs available on the host.
// driver_type: One of the values of the PIL_GPU_DRIVER_TYPE enumeration indicating the type of driver to use for GPU access.
// Returns zero if the driver interface is available on the host, or non-zero if an error occurred.
PIL_API(int32_t)
PIL_GpuDriverOpen
(
    struct PIL_GPU_DRIVER           *driver, 
    struct PIL_GPU_DRIVER_INIT      *config, 
    struct PIL_GPU_DRIVER_INFO *driver_info, 
    PIL_GPU_DRIVER_TYPE         driver_type
);

// Close a connection to a GPU interface on the host.
// Any logical devices should be deleted prior to closing the driver.
// Any resources allocated by the driver are freed when the driver is closed.
// driver: The PIL_GPU_DRIVER representing the driver interface to close.
PIL_API(void)
PIL_GpuDriverClose
(
    struct PIL_GPU_DRIVER *driver
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

