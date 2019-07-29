// memmgr_win32.cc: Implement the platform-specific memory management routines.
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include "platform.h"

PIL_API(void*)
PIL_HostMemoryAllocateHeap
(
    struct PIL_MEMORY_BLOCK *o_block, 
    size_t                   n_bytes, 
    size_t                 alignment
)
{
    void *p = nullptr;
    int ret = 0;

    if (alignment < sizeof(void*)) {
        alignment = sizeof(void*);
    }
    if ((p = _aligned_malloc(n_bytes, alignment)) != nullptr) {
        if (o_block) {
            o_block->BytesCommitted  =(uint64_t) n_bytes;
            o_block->BytesReserved   =(uint64_t) n_bytes;
            o_block->BlockOffset     = 0;
            o_block->HostAddress     =(uint8_t*) p;
            o_block->AllocationFlags = PIL_HOST_MEMORY_ALLOCATION_FLAGS_READWRITE | PIL_HOST_MEMORY_ALLOCATION_FLAG_NOGUARD;
            o_block->AllocatorTag    = PIL_HEAP_ALLOCATOR_TAG;
        }
    } else {
        ret = errno; // EINVAL or ENOMEM.
        if (o_block) {
            memset(o_block, 0, sizeof(PIL_MEMORY_BLOCK));
        }
        assert(ret != EINVAL && "Invalid value for alignment argument");
    }
    return p;
}

PIL_API(void)
PIL_HostMemoryFreeHeap
(
    void *host_addr
)
{
    _aligned_free(host_addr);
}

PIL_API(void*)
PIL_HostMemoryReserveAndCommit
(
    struct PIL_MEMORY_BLOCK *o_block, 
    size_t             reserve_bytes, 
    size_t              commit_bytes, 
    uint32_t             alloc_flags
)
{
    SYSTEM_INFO sysinfo;
    void          *base = NULL;
    size_t    page_size = 0;
    size_t        extra = 0;
    size_t  min_reserve = 0;
    DWORD        access = 0;
    DWORD         flags = MEM_RESERVE;
    DWORD         error = ERROR_SUCCESS;

    GetNativeSystemInfo(&sysinfo);
    min_reserve = sysinfo.dwPageSize;
    page_size   = sysinfo.dwPageSize;
    if (reserve_bytes < min_reserve) {
        reserve_bytes = min_reserve;
    }
    if (commit_bytes > reserve_bytes) {
        assert(commit_bytes <= reserve_bytes);
        SetLastError(ERROR_INVALID_PARAMETER);
        return NULL;
    }

    // VMM allocations are rounded up to the next even multiple of the system 
    //   page size, and have a starting address that is an even multiple of the 
    //   system allocation granularity (usually 64KB).
    reserve_bytes = PIL_AlignUp(reserve_bytes, page_size);
    if (alloc_flags == PIL_HOST_MEMORY_ALLOCATION_FLAGS_DEFAULT) {
        alloc_flags  = PIL_HOST_MEMORY_ALLOCATION_FLAGS_READWRITE;
    }
    if (alloc_flags  & PIL_HOST_MEMORY_ALLOCATION_FLAG_READ) {
        access = PAGE_READONLY;
    }
    if (alloc_flags  & PIL_HOST_MEMORY_ALLOCATION_FLAG_WRITE) {
        access = PAGE_READWRITE;
    }
    if (alloc_flags & PIL_HOST_MEMORY_ALLOCATION_FLAG_EXECUTE) {
        access = PAGE_EXECUTE_READWRITE;
        commit_bytes = reserve_bytes;
    }
    if((alloc_flags & PIL_HOST_MEMORY_ALLOCATION_FLAG_NOGUARD) == 0) {
        // Commit an extra page as a guard page.
        extra = page_size;
    }

    if ((base = VirtualAlloc(NULL, reserve_bytes + extra, flags, access)) == NULL) {
        // Reservation failed.
        goto cleanup_and_fail;
    }
    if (commit_bytes > 0) {
        commit_bytes = PIL_AlignUp(commit_bytes, page_size);
        if (VirtualAlloc(base, commit_bytes, MEM_COMMIT, access) != base) {
            // Commit failed.
            goto cleanup_and_fail;
        }
    }
    if (extra > 0) {
        if (VirtualAlloc((uint8_t*)base + reserve_bytes, page_size, MEM_COMMIT, access|PAGE_GUARD) == NULL) {
            // Commit failed for guard page.
            goto cleanup_and_fail;
        }
    }
    if (o_block) {
        o_block->BytesCommitted  = commit_bytes;
        o_block->BytesReserved   = reserve_bytes;
        o_block->BlockOffset     = 0;
        o_block->HostAddress     =(uint8_t*) base;
        o_block->AllocationFlags = alloc_flags;
        o_block->AllocatorTag    = PIL_VMM_ALLOCATOR_TAG;
    }
    return base;

cleanup_and_fail:
    error = GetLastError();
    if (base != nullptr) {
        VirtualFree(base, 0, MEM_RELEASE);
    }
    if (o_block) {
        ZeroMemory(o_block, sizeof(PIL_MEMORY_BLOCK));
    } SetLastError(error);
    return NULL;
}

PIL_API(int32_t)
PIL_HostMemoryIncreaseCommitment
(
    struct PIL_MEMORY_BLOCK *o_block, 
    struct PIL_MEMORY_BLOCK   *block, 
    size_t              commit_bytes
)
{
    uint8_t      *address;
    uint64_t       blkofs;
    uint64_t  num_reserve;
    uint64_t   old_commit;
    uint64_t   new_commit;
    uint32_t  alloc_flags;
    uint32_t    alloc_tag;
    uint64_t max_increase;
    uint64_t req_increase;

    if (block == NULL) {
        assert(block != NULL);
        goto cleanup_and_fail;
    }
    if (block->BytesReserved == 0 || block->HostAddress == NULL) {
        assert(block->BytesReserved != 0);
        assert(block->HostAddress != NULL);
        goto cleanup_and_fail;
    }

    // Copy values out of block to avoid aliasing.
    old_commit   = block->BytesCommitted;
    new_commit   = block->BytesCommitted;
    num_reserve  = block->BytesReserved;
    blkofs       = block->BlockOffset;
    address      = block->HostAddress;
    alloc_flags  = block->AllocationFlags;
    alloc_tag    = block->AllocatorTag;
    max_increase = num_reserve  - old_commit;
    req_increase = commit_bytes - old_commit;

    if (block->BytesCommitted < commit_bytes) {
        SYSTEM_INFO sysinfo;
        size_t    page_size = 0;
        DWORD        access = 0;

        if (req_increase > max_increase) {
            assert(req_increase <= max_increase);
            SetLastError(ERROR_NOT_ENOUGH_MEMORY);
            goto cleanup_and_fail;
        }

        GetNativeSystemInfo(&sysinfo);
        page_size  = sysinfo.dwPageSize;
        new_commit = PIL_AlignUp(old_commit+req_increase, page_size);
        if (alloc_flags & PIL_HOST_MEMORY_ALLOCATION_FLAG_READ) {
            access = PAGE_READONLY;
        }
        if (alloc_flags & PIL_HOST_MEMORY_ALLOCATION_FLAG_WRITE) {
            access = PAGE_READWRITE;
        }
        if (alloc_flags & PIL_HOST_MEMORY_ALLOCATION_FLAG_EXECUTE) {
            access = PAGE_EXECUTE_READWRITE;
        }
        if (VirtualAlloc(address, new_commit, MEM_COMMIT, access) == NULL) {
            goto cleanup_and_fail;
        }
    }
    if (o_block) {
        o_block->BytesCommitted  = new_commit;
        o_block->BytesReserved   = num_reserve;
        o_block->BlockOffset     = blkofs;
        o_block->HostAddress     = address;
        o_block->AllocationFlags = alloc_flags;
        o_block->AllocatorTag    = alloc_tag;
    }
    return 1;

cleanup_and_fail:
    if (o_block) {
        if (block) {
            CopyMemory(o_block, block, sizeof(PIL_MEMORY_BLOCK));
        } else {
            ZeroMemory(o_block, sizeof(PIL_MEMORY_BLOCK));
        }
    }
    return 0;
}

PIL_API(void)
PIL_HostMemoryFlush
(
    struct PIL_MEMORY_BLOCK const *block
)
{
    (void) FlushInstructionCache(GetCurrentProcess(), block->HostAddress, block->BytesCommitted);
}

PIL_API(void)
PIL_HostMemoryRelease
(
    struct PIL_MEMORY_BLOCK *block
)
{
    if (block != nullptr) {
        if (block->HostAddress != nullptr && block->BytesReserved != 0) {
            BOOL   res  = VirtualFree(block->HostAddress, 0, MEM_RELEASE);
            assert(res != 0 && "VirtualFree call failed - address space leak");
            PIL_UNUSED_LOCAL(res);
        }
    }
}

