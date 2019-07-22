// gpudrvvk.h: Define the internal types and functions associated with the 
// Vulkan GPU driver and device interfaces.
#ifndef __LIBPIL_GPUDRVVK_H__
#define __LIBPIL_GPUDRVVK_H__

#pragma once

#include "volk.h"

struct  GPU_DRIVER_STATE_VK1;
struct  GPU_DEVICE_STATE_VK1;

typedef struct GPU_DRIVER_STATE_VK1 {                                          // State data associated with a Vulkan 1.x driver instance.
    VkInstance                           Instance;                             // The Vulkan instance representing the connection to the GPU's Vulkan driver.
    VkResult                            LastError;                             // The last error code returned by a global- or instance-level entry point.
} GPU_DRIVER_STATE_VK1;

typedef struct GPU_DEVICE_STATE_VK1 {                                          // State data associated with a logical Vulkan device.
    VkDevice                        LogicalDevice;                             // The handle of the logical Vulkan device.
    VkPhysicalDevice               PhysicalDevice;                             // The handle of the physical Vulkan device used for storing resources and executing command buffers.
    VolkDeviceTable                 DispatchTable;                             // The dispatch table of Vulkan device-level entry points.
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

