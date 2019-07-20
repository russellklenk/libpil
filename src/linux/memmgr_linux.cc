// memmgr_linux.cc: Implement the platform-specific memory management routines.
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <execinfo.h>
#include <sys/mman.h>
#include "platform.h"

// To get the system page size: sysconf(_SC_PAGESIZE)
// To get the direct I/O alignment requirement for a file: pathconf(path, _PC_REC_XFER_ALIGN)

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
        // posix_memalign only supports alignments of at least pointer-width.
        alignment = sizeof(void*);
    }
    if ((ret = posix_memalign(&p, alignment, n_bytes)) == 0) {
        if (o_block) {
            o_block->BytesCommitted  =(uint64_t) n_bytes;
            o_block->BytesReserved   =(uint64_t) n_bytes;
            o_block->BlockOffset     = 0;
            o_block->HostAddress     =(uint8_t*) p;
            o_block->AllocationFlags = PIL_HOST_MEMORY_ALLOCATION_FLAGS_READWRITE | PIL_HOST_MEMORY_ALLOCATION_FLAG_NOGUARD;
            o_block->AllocatorTag    = PIL_MakeTag('H', 'E', 'A', 'P');
        }
    } else {
        // Allocation failed. ret is EINVAL or ENOMEM.
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
    free(host_addr);
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
    void          *base = nullptr;
    size_t    page_size = 0;
    size_t        extra = 0;
    size_t  min_reserve = 0;
    int          access = PROT_NONE;
    int           flags = MAP_PRIVATE | MAP_ANONYMOUS;

    page_size   = sysconf(_SC_PAGESIZE);
    min_reserve = page_size;
    if (reserve_bytes < min_reserve) {
        reserve_bytes = min_reserve;
    }
    if (commit_bytes > reserve_bytes) {
        assert(commit_bytes <= reserve_bytes);
        errno = EINVAL;
        return nullptr;
    }

    // VMM allocations are rounded up to the next even multiple of the system
    //   page size, and have a starting address that is an even multiple of the
    //   system allocation granularity (usually 64KB).
    reserve_bytes = PIL_AlignUp(reserve_bytes, page_size);
    if (alloc_flags == PIL_HOST_MEMORY_ALLOCATION_FLAGS_DEFAULT) {
        alloc_flags  = PIL_HOST_MEMORY_ALLOCATION_FLAGS_READWRITE;
    }
    if (alloc_flags  & PIL_HOST_MEMORY_ALLOCATION_FLAG_READ) {
        access = PROT_READ;
    }
    if (alloc_flags  & PIL_HOST_MEMORY_ALLOCATION_FLAG_WRITE) {
        access = PROT_READ | PROT_WRITE;
    }
    if (alloc_flags  & PIL_HOST_MEMORY_ALLOCATION_FLAG_EXECUTE) {
        access = PROT_READ | PROT_WRITE | PROT_EXEC;
        commit_bytes = reserve_bytes;
    }
    if((alloc_flags & PIL_HOST_MEMORY_ALLOCATION_FLAG_NOGUARD) == 0) {
        // Commit an extra page as a guard page.
        extra = page_size;
    }

    if ((base = mmap(nullptr, reserve_bytes + extra, PROT_NONE, flags, -1, 0)) == MAP_FAILED) {
        // The reservation failed.
        goto cleanup_and_fail;
    }
    if (commit_bytes > 0) {
        commit_bytes = PIL_AlignUp(commit_bytes, page_size);
        if (mprotect(base, commit_bytes, access) != 0) {
            // The commit failed.
            goto cleanup_and_fail;
        }
    }
    if (o_block) {
        o_block->BytesCommitted  = commit_bytes;
        o_block->BytesReserved   = reserve_bytes;
        o_block->BlockOffset     = 0;
        o_block->HostAddress     =(uint8_t*) base;
        o_block->AllocationFlags = alloc_flags;
        o_block->AllocatorTag    = PIL_MakeTag('V','M','E','M');
    }
    return base;

cleanup_and_fail:
    if (base != nullptr && base != MAP_FAILED) {
        munmap(base, reserve_bytes + extra);
    }
    if (o_block) {
        memset(o_block, 0, sizeof(PIL_MEMORY_BLOCK));
    }
    return nullptr;
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
        size_t page_size = sysconf(_SC_PAGESIZE);
        int       access = PROT_NONE;

        if (req_increase > max_increase) {
            errno = ENOMEM;
            assert(req_increase <= max_increase);
            goto cleanup_and_fail;
        }

        new_commit = PIL_AlignUp(old_commit+req_increase, page_size);
        if (alloc_flags & PIL_HOST_MEMORY_ALLOCATION_FLAG_READ) {
            access = PROT_READ;
        }
        if (alloc_flags & PIL_HOST_MEMORY_ALLOCATION_FLAG_WRITE) {
            access = PROT_READ | PROT_WRITE;
        }
        if (alloc_flags & PIL_HOST_MEMORY_ALLOCATION_FLAG_EXECUTE) {
            access = PROT_READ | PROT_WRITE | PROT_EXEC;
        }
        if (mprotect(address, new_commit, access) != 0) {
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
            memcpy(o_block, block, sizeof(PIL_MEMORY_BLOCK));
        } else {
            memset(o_block, 0    , sizeof(PIL_MEMORY_BLOCK));
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
    PIL_UNUSED_ARG(block);
}

PIL_API(void)
PIL_HostMemoryRelease
(
    struct PIL_MEMORY_BLOCK *block
)
{
    if (block != nullptr) {
        if (block->HostAddress != nullptr && block->BytesReserved != 0) {
            int res = munmap(block->HostAddress, block->BytesReserved);
            assert(res == 0 && "Call to munmap failed");
            memset(block, 0, sizeof(PIL_MEMORY_BLOCK));
            PIL_UNUSED_LOCAL(res);
        }
    }
}

