// gpudrvvk.h: Define the internal types and functions associated with the 
// Vulkan GPU driver and device interfaces.
#ifndef __LIBPIL_GPUDRVVK_H__
#define __LIBPIL_GPUDRVVK_H__

#pragma once

#include "platform.h"
#include "volk.h"

struct  GPU_DRIVER_STATE_VK1;
struct  GPU_DEVICE_STATE_VK1;

typedef struct PHYSICAL_DEVICE_VK1 {                                           // Data associated with a Vulkan-capable physical device.
    VkPhysicalDevice                  DeviceHandle;                            // The Vulkan physical device handle.
    VkPhysicalDeviceFeatures        DeviceFeatures;                            // Information about the features supported by the device.
    VkPhysicalDeviceProperties         DeviceProps;                            // The device properties (vendor ID, etc.)
    VkPhysicalDeviceMemoryProperties   MemoryProps;                            // Information about the device memory heaps.
    uint32_t                            LayerCount;                            // The number of device-level layers exposed by the device.
    uint32_t                        ExtensionCount;                            // The number of device-level extensions exposed by the device.
    uint32_t                   LayerExtensionCount;                            // The number of layer extensions exposed by the device.
    uint32_t                      QueueFamilyCount;                            // The number of command queue families exposed by the device.
    VkQueueFamilyProperties         *QueueFamilies;                            // Information about the command queue families exposed by the device.
    VkLayerProperties                   *LayerList;                            // Information about the device-level layers exposed by the device.
    VkExtensionProperties           *ExtensionList;                            // Information about the device-level extensions exposed by the device.
    VkExtensionProperties      *LayerExtensionList;                            // Information about the layer extensions exposed by the device.
} PHYSICAL_DEVICE_VK1;

typedef struct GPU_DRIVER_STATE_VK1 {                                          // State data associated with a Vulkan 1.x driver instance.
    VkInstance                            Instance;                            // The Vulkan instance representing the connection to the GPU's Vulkan driver.
    VkResult                             LastError;                            // The last error code returned by a global- or instance-level entry point.
    uint32_t                    LogicalDeviceCount;                            // 
    uint32_t                   PhysicalDeviceCount;                            // The number of Vulkan-capable physical devices available on the host.
    uint32_t                 MaxLogicalDeviceCount;                            // 
    struct GPU_DEVICE_STATE_VK1 *LogicalDeviceList;                            // 
    struct PHYSICAL_DEVICE_VK1 *PhysicalDeviceList;                            // An array of PhysicalDeviceCount device descriptors for the physical devices available on the host.
    VkPhysicalDevice        *PhysicalDeviceHandles;                            // An array of PhysicalDeviceCount handles of physical devices available on the host.
    VkPhysicalDeviceType      *PhysicalDeviceTypes;                            // An array of PhysicalDeviceCount device type identifiers the physical devices available on the host.
    PIL_MEMORY_ARENA                   MemoryArena;                            // 
} GPU_DRIVER_STATE_VK1;

typedef struct GPU_DEVICE_STATE_VK1 {                                          // State data associated with a logical Vulkan device.
    VkDevice                         LogicalDevice;                            // The handle of the logical Vulkan device.
    VkPhysicalDevice                PhysicalDevice;                            // The handle of the physical Vulkan device used for storing resources and executing command buffers.
    VolkDeviceTable                  DispatchTable;                            // The dispatch table of Vulkan device-level entry points.
} GPU_DEVICE_STATE_VK1;

#ifdef __cplusplus
extern "C" {
#endif

// Connect to the Vulkan loader and resolve any global- and instance-level Vulkan entry points.
// config: Data used to configure the Vulkan driver connection.
// drvctx: On return, this location points to an instance of GPU_DRIVER_STATE_VK1.
// Returns zero if drvctx was set and Vulkan is available on at least one host GPU, or non-zero if Vulkan is not available.
PIL_API(int)
GpuDriverInit_Vk1
(
    struct PIL_GPU_DRIVER_INIT *config, 
    void                      **drvctx
);

// Free resources associated with a Vulkan driver connection.
// drvctx: Pointer to a GPU_DRIVER_STATE_VK1 returned by a prior call to GpuDriverInit_Vk1.
PIL_API(void)
GpuDriverShut_Vk1
(
    struct PIL_GPU_DRIVER_STATE *drvctx
);

// Retrieve information about the physical GPUs and capabilities provided by the Vulkan driver.
// drvctx: Pointer to a GPU_DRIVER_STATE_VK1 returned by a prior call to GpuDriverInit_Vk1.
// o_info: Pointer to a PIL_GPU_DRIVER_INFO structure to populate.
// Returns zero if o_info was updated with information, or non-zero if an error occurred.
PIL_API(int)
GpuDriverQuery_Vk1
(
    struct PIL_GPU_DRIVER_STATE *drvctx, 
    struct PIL_GPU_DRIVER_INFO  *o_info
);

// Retrieve the last VkResult value returned by a global- or instance-level Vulkan entry point.
// drvctx: Pointer to a GPU_DRIVER_STATE_VK1 returned by a prior call to GpuDriverInit_Vk1.
// Returns a VkResult value. Positive values indicate success while negative values indicate errors.
PIL_API(VkResult)
GpuDriverGetLastError_Vk1
(
    struct PIL_GPU_DRIVER_STATE *drvctx
);

// Set the last VkResult value returned by a global- or instance-level Vulkan entry point.
// drvctx: Pointer to a GPU_DRIVER_STATE_VK1 returned by a prior call to GpuDriverInit_Vk1.
// last_error: The VkResult to store.
// Returns the last_error value.
PIL_API(VkResult)
GpuDriverSetLastError_Vk1
(
    struct PIL_GPU_DRIVER_STATE *drvctx, 
    VkResult                 last_error
);

#ifdef __cplusplus
}; /* extern "C" */
#endif

#endif /* __LIBPIL_GPUDRVVK_H__ */

