// table.cc: Implements the data table routines exported by the PIL.
#include <string.h>
#include "platform.h"

// Construct a PIL_HANDLE_BITS from its constituient parts.
// _sparse_index: The zero-based index within the sparse portion of the PIL_TABLE_INDEX that is allocated to the item.
// _generation  : The generation value of the data slot allocated to the item.
// Returns the PIL_HANDLE_BITS identifying the item.
#ifndef MakeHandleBits
#define MakeHandleBits(_sparse_index, _generation)                             \
     (PIL_HANDLE_FLAG_MASK_PACKED |                                            \
    (((_sparse_index) & PIL_HANDLE_INDEX_MASK) << PIL_HANDLE_INDEX_SHIFT) |    \
    (((_generation  ) & PIL_HANDLE_GENER_MASK) << PIL_HANDLE_GENER_SHIFT))
#endif

// Extract whether or not a PIL_HANDLE_BITS represents a possibly-valid item.
// _bits: The PIL_HANDLE_BITS value.
// Returns Non-zero if the PIL_HANDLE_BITS identifies an item that was valid at some point.
#ifndef HandleBitsExtractLive
#define HandleBitsExtractLive(_bits)                                           \
    (((_bits) & PIL_HANDLE_FLAG_MASK_PACKED) >> PIL_HANDLE_FLAG_SHIFT)
#endif

// Extract the generation value of the data slot associated with a PIL_HANDLE_BITS.
// _bits: The PIL_HANDLE_BITS value.
// Returns The generation value portion of the handle.
///
#ifndef HandleBitsExtractGeneration
#define HandleBitsExtractGeneration(_bits)                                     \
    (((_bits) & PIL_HANDLE_GENER_MASK_PACKED) >> PIL_HANDLE_GENER_SHIFT)
#endif

// Extract the sparse slot index encoded within a PIL_HANDLE_BITS.
// _bits: The PIL_HANDLE_BITS value.
// Returns The zero-based index within the sparse portion of the PIL_TABLE_INDEX allocated to the item.
#ifndef HandleBitsExtractSparseIndex
#define HandleBitsExtractSparseIndex(_bits)                                    \
    (((_bits) & PIL_HANDLE_INDEX_MASK_PACKED) >> PIL_HANDLE_INDEX_SHIFT)
#endif

// Extract a value indicating whether or not a sparse index slot represents a valid item.
// _word: The word read from the sparse index.
// Returns Non-zero if the slot is associated with a valid item.
#ifndef SparseIndexExtractLive
#define SparseIndexExtractLive(_word)                                          \
    (((_word) & PIL_HANDLE_FLAG_MASK_PACKED) >> PIL_HANDLE_FLAG_SHIFT)
#endif

// Extract the generation value of the data slot associated with a sparse index slot.
// _word: The word read from the sparse index.
// Returns The generation value portion of the index value.
#ifndef SparseIndexExtractGeneration
#define SparseIndexExtractGeneration(_word)                                    \
    (((_word) & PIL_HANDLE_GENER_MASK_PACKED) >> PIL_HANDLE_GENER_SHIFT)
#endif

// Extract the dense array index encoded within a sparse index slot.
// _word: The word read from the sparse index.
// Returns The dense index portion of the index value.
#ifndef SparseIndexExtractDenseIndex
#define SparseIndexExtractDenseIndex(_word)                                    \
    (((_word) & PIL_HANDLE_INDEX_MASK_PACKED) >> PIL_HANDLE_INDEX_SHIFT)
#endif

// Move the data for a given table slot from one location to another.
// desc: Pointer to a PIL_TABLE_DESC describing the table data streams.
// dst_index: The destination index.
// src_index: The source index.
static inline void
MoveTableItemData
(
    struct PIL_TABLE_DESC *desc,
    uint32_t          dst_index,
    uint32_t          src_index
)
{
    PIL_TABLE_DATA **streams = desc->Streams;
    void              *src_p;
    void              *dst_p;
    uint32_t            i, n;
    for (i = 0, n = desc->StreamCount; i < n; ++i) {
        src_p = PIL_TableData_GetElementPointer(void, streams[i], src_index);
        dst_p = PIL_TableData_GetElementPointer(void, streams[i], dst_index);
        memcpy(dst_p, src_p, streams[i]->ElementSize);
    }
}

// Drop all items in a PRIMARY table.
// table: The table to update.
static inline void
TableDeleteAllIds
(
    struct PIL_TABLE_DESC *table
)
{   assert(PIL_Table_GetType(table) == PIL_TABLE_TYPE_PRIMARY);
    PIL_TABLE_INDEX *index = table->Index;
    uint32_t *sparse_array = index->SparseIndex;
    uint32_t *handle_array = index->HandleArray;
    uint32_t  handle_value;
    uint32_t  sparse_index;
    uint32_t    generation;
    uint32_t          i, n;
    for (i = 0, n = index->ActiveCount; i < n; ++i) {
        handle_value = handle_array[i];
        generation   = HandleBitsExtractGeneration (handle_value);
        sparse_index = HandleBitsExtractSparseIndex(handle_value);
        sparse_array[sparse_index] = ((generation + 1) & PIL_HANDLE_GENER_MASK) << PIL_HANDLE_GENER_SHIFT;
        handle_array[i]  = sparse_index;
    } index->ActiveCount = 0;
}

// Drop a single item in a PRIMARY table.
// table: The table to update.
// bits : The identifier of the item to drop.
// Returns the identifier of the item that was moved as a result of the deletion, or PIL_HANDLE_BITS_INVALID if no item was moved.
static inline PIL_HANDLE_BITS
TableDeleteId
(
    struct PIL_TABLE_DESC *table, 
    PIL_HANDLE_BITS         bits
)
{   assert(PIL_Table_GetType(table) == PIL_TABLE_TYPE_PRIMARY);
    PIL_TABLE_INDEX         *index   = table->Index;
    uint32_t         *sparse_array   = index->SparseIndex;
    PIL_HANDLE_BITS  *handle_array   = index->HandleArray;
    PIL_HANDLE_BITS    moved_value   = PIL_HANDLE_BITS_INVALID;
    uint32_t            last_dense   = index->ActiveCount - 1;
    uint32_t          sparse_index   = HandleBitsExtractSparseIndex(bits);
    uint32_t          sparse_value;
    uint32_t           moved_gener;
    uint32_t           moved_index;
    uint32_t           dense_index;
    uint32_t            generation;

    if (sparse_index < index->TableCapacity) {
        sparse_value = sparse_array[sparse_index];
        generation   = SparseIndexExtractGeneration(sparse_value);
        dense_index  = SparseIndexExtractDenseIndex(sparse_value);
        sparse_array[sparse_index] = ((generation + 1) & PIL_HANDLE_GENER_MASK) << PIL_HANDLE_GENER_SHIFT;
        // If the deleted item is not the last slot in the dense array,
        //   swap the last live item into the slot vacated by the deleted
        //   item in order to keep the handle and data arrays densely packed.
        if (dense_index != last_dense) {
            moved_value  = handle_array[last_dense];
            moved_gener  = HandleBitsExtractGeneration(moved_value);
            moved_index  = HandleBitsExtractSparseIndex(moved_value);
            MoveTableItemData(table, dense_index, last_dense);
            sparse_array[moved_index] = PIL_HANDLE_FLAG_MASK_PACKED | (dense_index << PIL_HANDLE_INDEX_SHIFT) | (moved_gener << PIL_HANDLE_GENER_SHIFT);
            handle_array[dense_index] = moved_value;
        }
        // Return sparse_index to the free list.
        handle_array[last_dense] = (sparse_index << PIL_HANDLE_INDEX_SHIFT) | ((generation + PIL_HANDLE_GENER_ADD_PACKED) & PIL_HANDLE_GENER_MASK);
        index->ActiveCount = last_dense;
    }
    return moved_value;
}

// Drop a single item in a FOREIGN table.
// table: The table to update.
// bits : The identifier of the item to drop.
// Returns the identifier of the item that was moved as a result of the deletion, or PIL_HANDLE_BITS_INVALID if no item was moved.
static inline PIL_HANDLE_BITS
TableRemoveId
(
    struct PIL_TABLE_DESC *table, 
    PIL_HANDLE_BITS         bits
)
{   assert(PIL_Table_GetType(table) == PIL_TABLE_TYPE_FOREIGN);
    PIL_TABLE_INDEX         *index   = table->Index;
    uint32_t         *sparse_array   = index->SparseIndex;
    PIL_HANDLE_BITS  *handle_array   = index->HandleArray;
    PIL_HANDLE_BITS    moved_value   = PIL_HANDLE_BITS_INVALID;
    uint32_t            last_dense   = index->ActiveCount - 1;
    uint32_t          sparse_index   = HandleBitsExtractSparseIndex(bits);
    uint32_t          sparse_value;
    uint32_t           moved_gener;
    uint32_t           moved_index;
    uint32_t           dense_index;

    if (sparse_index < index->TableCapacity) {
        sparse_value = sparse_array[sparse_index];
        dense_index  = SparseIndexExtractDenseIndex(sparse_value);
        sparse_array[sparse_index] = 0;
        // If the deleted item is not the last slot in the dense array,
        //   swap the last live item into the slot vacated by the deleted
        //   item in order to keep the handle and data arrays densely packed.
        if (dense_index != last_dense) {
            moved_value  = handle_array[last_dense];
            moved_gener  = HandleBitsExtractGeneration(moved_value);
            moved_index  = HandleBitsExtractSparseIndex(moved_value);
            MoveTableItemData(table, dense_index, last_dense);
            sparse_array[moved_index] = PIL_HANDLE_FLAG_MASK_PACKED | (dense_index << PIL_HANDLE_INDEX_SHIFT) | (moved_gener << PIL_HANDLE_GENER_SHIFT);
            handle_array[dense_index] = moved_value;
        }
        index->ActiveCount = last_dense;
    }
    return moved_value;
}

// Drop all items in a FOREIGN table.
// table: The table to update.
static inline void
TableRemoveAllIds
(
    struct PIL_TABLE_DESC *table
)
{   assert(PIL_Table_GetType(table) == PIL_TABLE_TYPE_FOREIGN);
    PIL_TABLE_INDEX *index = table->Index;
    uint32_t *sparse_array = index->SparseIndex;
    size_t    sparse_bytes = index->TableCapacity * sizeof(uint32_t);
    memset(sparse_array, 0 , sparse_bytes);
    index->ActiveCount = 0;
}

PIL_API(int32_t)
PIL_ValidateTableIndex
(
    struct PIL_TABLE_INDEX *index
)
{
    uint32_t *sparse = index->SparseIndex;
    uint32_t *handle = index->HandleArray;
    uint32_t   count = index->ActiveCount;
    uint32_t  inited = index->HighWatermark;
    uint32_t       i;

    for (i = 0; i < count; ++i) {
        PIL_HANDLE_BITS h = handle[i];
        uint32_t       si = HandleBitsExtractSparseIndex(h);
        uint32_t        s = sparse[si];
        if (HandleBitsExtractLive(h) == 0 || SparseIndexExtractLive(s) == 0) {
            // Both should say that they're live.
            assert(HandleBitsExtractLive (h) != 0);
            assert(SparseIndexExtractLive(s) != 0);
            return 0;
        }
        if (HandleBitsExtractGeneration(h) != SparseIndexExtractGeneration(s)) {
            // Generation values must match.
            assert(HandleBitsExtractGeneration(h) == SparseIndexExtractGeneration(s));
            return 0;
        }
        if (SparseIndexExtractDenseIndex(s) != i) {
            // Dense index should point at this slot.
            assert(SparseIndexExtractDenseIndex(s) == i);
            return 0;
        }
    }
    for (i = count; i < inited; ++i) {
        PIL_HANDLE_BITS h = handle[i];
        uint32_t       si = HandleBitsExtractSparseIndex(h);
        uint32_t        s = sparse[si];
        if (HandleBitsExtractLive(h) != 0 || SparseIndexExtractLive(s) != 0) {
            // Both should say that they're dead.
            assert(HandleBitsExtractLive (h) == 0);
            assert(SparseIndexExtractLive(s) == 0);
            return 0;
        }
    }
    return 1;
}

PIL_API(uint32_t)
PIL_TableData_GetElementIndex
(
    struct PIL_TABLE_DATA *stream, 
    void         *element_address
)
{
    uint8_t *beg = PIL_TableData_GetBuffer(uint8_t, stream);
    uint32_t siz = PIL_TableData_GetElementSize(stream);
    uint32_t idx =(uint32_t)((((uint8_t*) element_address) - beg) / siz);
    return   idx;
}

PIL_API(int32_t)
PIL_TableCreate
(
    struct PIL_TABLE_INIT *init
)
{
    uint8_t                  *index_ptr = nullptr;
    uint32_t                *sparse_ptr = nullptr;
    PIL_HANDLE_BITS         *handle_ptr = nullptr;
    void                    *stream_ptr = nullptr;
    size_t                 index_commit = 0;
    size_t                sparse_commit = 0;
    size_t                handle_commit = 0;
    size_t                stream_commit = 0;
    size_t               handle_reserve = 0;
    size_t               stream_reserve = 0;
    size_t                index_reserve = 0;
    PIL_TABLE_INDEX              *index = nullptr;
    PIL_TABLE_DATA_STREAM_DESC *streams = nullptr;
    uint32_t               stream_count = 0;
    uint32_t                       i, n;
    PIL_MEMORY_BLOCK        index_block;
    PIL_MEMORY_BLOCK       stream_block;

    if (init == nullptr) {
        assert(init != nullptr && "The init argument is required");
        return -1;
    }
    index            = init->Index;
    streams          = init->Streams;
    stream_count     = init->StreamCount;
    if (init->Index == nullptr || (init->Streams == nullptr && init->StreamCount > 0)) {
        assert(init->Index   != nullptr && "A table index is required");
        assert(init->Streams != nullptr && "Expected init->StreamCount table data descriptors");
        return -1;
    }
    if (init->TableCapacity < PIL_TABLE_MIN_OBJECT_COUNT) {
        assert(init->TableCapacity >= PIL_TABLE_MIN_OBJECT_COUNT);
        return -1;
    }
    if (init->TableCapacity > PIL_TABLE_MAX_OBJECT_COUNT) {
        assert(init->TableCapacity <= PIL_TABLE_MAX_OBJECT_COUNT);
        return -1;
    }
    for (i = 0, n = init->StreamCount; i < n; ++i) {
        if (streams[i].Data == nullptr || streams[i].Size == 0) {
            assert(init->Streams[i].Data != nullptr);
            assert(init->Streams[i].Size != 0);
            return -1;
        }
    }

    // Reserve process address space for the table index.
    // The index sparse array is fully-committed up front.
    // The index handle array may be partially committed.
    sparse_commit  = init->TableCapacity * sizeof(uint32_t);
    handle_commit  = init->InitialCommit * sizeof(PIL_HANDLE_BITS);
    handle_reserve = init->TableCapacity * sizeof(PIL_HANDLE_BITS);
    index_reserve  = sparse_commit + handle_reserve;
    index_commit   = sparse_commit + handle_commit;
    if ((index_ptr =(uint8_t*) PIL_HostMemoryReserveAndCommit(&index_block, index_reserve, index_commit, PIL_HOST_MEMORY_ALLOCATION_FLAGS_READWRITE)) == nullptr) {
        goto cleanup_and_fail;
    }
    sparse_ptr     =(uint32_t       *)(index_ptr + 0);
    handle_ptr     =(PIL_HANDLE_BITS*)(index_ptr + sparse_commit);

    // Reserve process address space for the data streams.
    for (i = 0,  n = stream_count; i < n; ++i) {
        stream_reserve = init->TableCapacity * streams[i].Size;
        stream_commit  = init->InitialCommit * streams[i].Size;
        if ((stream_ptr= PIL_HostMemoryReserveAndCommit(&stream_block, stream_reserve, stream_commit, PIL_HOST_MEMORY_ALLOCATION_FLAGS_READWRITE)) == nullptr) {
            goto cleanup_and_fail;
        }
        streams[i].Data->StorageBuffer =(uint8_t*) stream_ptr;
        streams[i].Data->ElementSize   = streams[i].Size;
        streams[i].Data->Reserved      = streams[i].Reserved;
    }
    index->SparseIndex   = sparse_ptr;
    index->HandleArray   = handle_ptr;
    index->ActiveCount   = 0;
    index->HighWatermark = 0;
    index->CommitCount   = init->InitialCommit;
    index->TableCapacity = init->TableCapacity;
    return 0;

cleanup_and_fail:
    for (i = 0,  n = stream_count; i < n; ++i) {
        stream_block.BytesCommitted  = init->InitialCommit * streams[i].Size;
        stream_block.BytesReserved   = init->TableCapacity * streams[i].Size;
        stream_block.BlockOffset     = 0;
        stream_block.HostAddress     =(uint8_t*) streams[i].Data->StorageBuffer;
        stream_block.AllocationFlags = PIL_HOST_MEMORY_ALLOCATION_FLAGS_READWRITE;
        stream_block.AllocatorTag    = PIL_VMM_ALLOCATOR_TAG;
        PIL_HostMemoryRelease(&stream_block);
        streams[i].Data->StorageBuffer = nullptr;
        streams[i].Data->ElementSize   = 0;
    }
    if (index_ptr != nullptr) {
        index_block.BytesCommitted   = index_commit;
        index_block.BytesReserved    = index_reserve;
        index_block.BlockOffset      = 0;
        index_block.HostAddress      =(uint8_t*) index_ptr;
        index_block.AllocationFlags  = PIL_HOST_MEMORY_ALLOCATION_FLAGS_READWRITE;
        index_block.AllocatorTag     = PIL_VMM_ALLOCATOR_TAG;
        PIL_HostMemoryRelease(&index_block);
    }
    return -1;
}

PIL_API(int32_t)
PIL_TableEnsure
(
    struct PIL_TABLE_DESC *table, 
    uint32_t          total_need, 
    uint32_t          chunk_size
)
{
    PIL_TABLE_INDEX        *index = table->Index;
    PIL_TABLE_DATA      **streams = table->Streams;
    size_t          handle_commit;
    size_t          stream_commit;
    uint32_t          chunk_count;
    uint32_t       old_item_count;
    uint32_t       new_item_count;
    uint32_t       max_item_count;
    uint32_t                 i, n;
    PIL_MEMORY_BLOCK  index_block;
    PIL_MEMORY_BLOCK stream_block;

    if (PIL_TableIndex_GetCommit(table->Index) >= total_need) {
        return 1;
    }
    chunk_count    = (total_need + (chunk_size - 1)) / chunk_size;
    new_item_count = (chunk_size *  chunk_count);
    old_item_count =  PIL_TableIndex_GetCommit(index);
    max_item_count =  PIL_TableIndex_GetCapacity(index);
    if (new_item_count > max_item_count) {
        new_item_count = max_item_count;
    }
    if (new_item_count < total_need) {
        return 0;
    }
    handle_commit               = new_item_count * sizeof(PIL_HANDLE_BITS);
    index_block.BytesCommitted  = old_item_count * sizeof(PIL_HANDLE_BITS);
    index_block.BytesReserved   = max_item_count * sizeof(PIL_HANDLE_BITS);
    index_block.BlockOffset     = 0;
    index_block.HostAddress     =(uint8_t*) PIL_Table_GetHandleBegin(table);
    index_block.AllocationFlags = PIL_HOST_MEMORY_ALLOCATION_FLAGS_READWRITE;
    index_block.AllocatorTag    = PIL_VMM_ALLOCATOR_TAG;
    if (PIL_HostMemoryIncreaseCommitment(nullptr, &index_block, handle_commit) == 0) {
        return 0;
    }
    for (i = 0, n = PIL_Table_GetStreamCount(table); i < n; ++i) {
        stream_commit                = new_item_count * streams[i]->ElementSize;
        stream_block.BytesCommitted  = old_item_count * streams[i]->ElementSize;
        stream_block.BytesReserved   = max_item_count * streams[i]->ElementSize;
        stream_block.BlockOffset     = 0;
        stream_block.HostAddress     = PIL_Table_GetStreamBegin(uint8_t, table, i);
        stream_block.AllocationFlags = PIL_HOST_MEMORY_ALLOCATION_FLAGS_READWRITE;
        stream_block.AllocatorTag    = PIL_VMM_ALLOCATOR_TAG;
        if (PIL_HostMemoryIncreaseCommitment(nullptr, &stream_block, stream_commit) == 0) {
            return 0;
        }
    }
    index->CommitCount = new_item_count;
    return 1;
}

PIL_API(void)
PIL_TableDelete
(
    struct PIL_TABLE_DESC *table
)
{
    if (table != nullptr) {
        PIL_TABLE_INDEX        *index = table->Index;
        PIL_TABLE_DATA      **streams = table->Streams;
        uint32_t       cur_item_count = 0;
        uint32_t       max_item_count = 0;
        uint32_t                 i, n;
        PIL_MEMORY_BLOCK  index_block;
        PIL_MEMORY_BLOCK stream_block;

        if (index != nullptr) {
            cur_item_count = PIL_TableIndex_GetCommit(index);
            max_item_count = PIL_TableIndex_GetCapacity(index);
        }
        for (i = 0, n = PIL_Table_GetStreamCount(table); i < n; ++i) {
            stream_block.BytesCommitted  = cur_item_count  * streams[i]->ElementSize;
            stream_block.BytesReserved   = max_item_count  * streams[i]->ElementSize;
            stream_block.BlockOffset     = 0;
            stream_block.HostAddress     = PIL_Table_GetStreamBegin(uint8_t, table, i);
            stream_block.AllocationFlags = PIL_HOST_MEMORY_ALLOCATION_FLAGS_READWRITE;
            stream_block.AllocatorTag    = PIL_VMM_ALLOCATOR_TAG;
            PIL_HostMemoryRelease(&stream_block);
        }
        if (index != nullptr) {
            index_block.BytesCommitted   =(max_item_count  * sizeof(uint32_t)) + (cur_item_count * sizeof(PIL_HANDLE_BITS));
            index_block.BytesReserved    =(max_item_count  * sizeof(uint32_t)) + (max_item_count * sizeof(PIL_HANDLE_BITS));
            index_block.BlockOffset      = 0;
            index_block.HostAddress      =(uint8_t*) index->SparseIndex;
            index_block.AllocationFlags  = PIL_HOST_MEMORY_ALLOCATION_FLAGS_READWRITE;
            index_block.AllocatorTag     = PIL_VMM_ALLOCATOR_TAG;
            PIL_HostMemoryRelease(&index_block);
            index->SparseIndex   = nullptr;
            index->HandleArray   = nullptr;
            index->ActiveCount   = 0;
            index->CommitCount   = 0;
            index->TableCapacity = 0;
        }
    }
}

PIL_API(void)
PIL_TableDropAll
(
    struct PIL_TABLE_DESC *table
)
{
    if (PIL_Table_GetType(table) == PIL_TABLE_TYPE_PRIMARY) {
        TableDeleteAllIds(table);
        return;
    }
    if (PIL_Table_GetType(table) == PIL_TABLE_TYPE_FOREIGN) {
        TableRemoveAllIds(table);
        return;
    }
}

PIL_API(PIL_HANDLE_BITS)
PIL_TableDropOne
(
    struct PIL_TABLE_DESC *table, 
    PIL_HANDLE_BITS         item
)
{
    if (PIL_Table_GetType(table) == PIL_TABLE_TYPE_PRIMARY) {
        return TableDeleteId(table, item);
    }
    if (PIL_Table_GetType(table) == PIL_TABLE_TYPE_FOREIGN) {
        return TableRemoveId(table, item);
    }
    return PIL_HANDLE_BITS_INVALID;
}

PIL_API(int32_t)
PIL_TableResolve
(
    uint32_t     *o_record_index, 
    struct PIL_TABLE_DESC *table, 
    PIL_HANDLE_BITS         item
)
{
    PIL_TABLE_INDEX  *index = table->Index;
    uint32_t  *sparse_array = index->SparseIndex;
    uint32_t        is_live = HandleBitsExtractLive(item);
    uint32_t   generation_h = HandleBitsExtractGeneration(item);
    uint32_t   sparse_index = HandleBitsExtractSparseIndex(item);
    uint32_t   sparse_value;
    uint32_t   generation_i;

    if (is_live) {
        sparse_value   = sparse_array[sparse_index];
        generation_i   = SparseIndexExtractGeneration(sparse_value);
       *o_record_index = SparseIndexExtractDenseIndex(sparse_value);
        assert (generation_h == generation_i);
        return (generation_h == generation_i);
    } else {
        return 0;
    }
}

PIL_API(PIL_HANDLE_BITS)
PIL_TableCreateId
(
    uint32_t     *o_record_index, 
    struct PIL_TABLE_DESC *table
)
{
    PIL_TABLE_INDEX *index = table->Index;
    uint32_t *sparse_array = index->SparseIndex;
    uint32_t *handle_array = index->HandleArray;
    uint32_t  handle_index = index->ActiveCount;
    uint32_t  sparse_index;
    uint32_t    generation;
    uint32_t    slot_value;
    PIL_HANDLE_BITS   bits;

    assert(index->ActiveCount < index->CommitCount);
    assert(PIL_Table_GetType(table) == PIL_TABLE_TYPE_PRIMARY);
    if (handle_index == index->HighWatermark) {
        index->HighWatermark = handle_index + 1;
        sparse_index         = handle_index;
        generation           = 0;
        slot_value           = 0;
    } else { // Read from the free list
        slot_value           = handle_array[handle_index];
        generation           = HandleBitsExtractGeneration(slot_value);
        sparse_index         = HandleBitsExtractSparseIndex(slot_value);
    }
    bits = MakeHandleBits(sparse_index, generation);
    sparse_array[sparse_index] = PIL_HANDLE_FLAG_MASK_PACKED | (handle_index << PIL_HANDLE_INDEX_SHIFT) | (generation << PIL_HANDLE_GENER_SHIFT);
    handle_array[handle_index] = bits;
   *o_record_index = handle_index;
    index->ActiveCount++;
    return bits;
}

PIL_API(int32_t)
PIL_TableInsertId
(
    uint32_t     *o_record_index, 
    struct PIL_TABLE_DESC *table, 
    PIL_HANDLE_BITS         item
)
{
    PIL_TABLE_INDEX *index = table->Index;
    uint32_t *sparse_array = index->SparseIndex;
    uint32_t *handle_array = index->HandleArray;
    uint32_t  handle_index = index->ActiveCount + 1;
    uint32_t    generation = HandleBitsExtractGeneration(item);
    uint32_t  sparse_index = HandleBitsExtractSparseIndex(item);

    assert(index->ActiveCount < index->CommitCount);
    assert(PIL_Table_GetType(table) == PIL_TABLE_TYPE_FOREIGN);
    if (sparse_index < index->TableCapacity && sparse_array[sparse_index] == 0) {
        sparse_array[sparse_index] = PIL_HANDLE_FLAG_MASK_PACKED | (handle_index << PIL_HANDLE_INDEX_SHIFT) | (generation << PIL_HANDLE_GENER_SHIFT);
        handle_array[handle_index] = item;
       *o_record_index = handle_index;
        return 1;
    }
    return 0;
}

