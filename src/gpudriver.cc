// gpudriver.cc: Provide a null driver implementation and implement the 
// top-level GPU driver interface routines. The null driver routines all fail 
// and perform no actual operation.
#include <string.h>
#include "platform.h"

#include "gpudrvvk.h"

// Connect to the null driver.
// config: Data used to configure the driver connection.
// drvctx: On return, this location points to nullptr.
// Returns zero if drvctx was set and the driver interface is available on at least one host GPU, or non-zero if the driver is not available.
static int
GpuDriverInit_Null
(
    struct PIL_GPU_DRIVER_INIT *config, 
    void                      **drvctx
)
{
    PIL_UNUSED_ARG(config);
    if (drvctx != nullptr) {
       *drvctx  = nullptr;
    }
    return -1;
}

// Free resources associated with a null driver connection.
// drvctx: Pointer to a PIL_GPU_DRIVER_STATE returned by a prior call to GpuDriverInit_Null.
static void
GpuDriverShut_Null
(
    struct PIL_GPU_DRIVER_STATE *drvctx
)
{
    PIL_UNUSED_ARG(drvctx);
}

// Retrieve information about the physical GPUs and capabilities provided by the driver.
// drvctx: Pointer to a PIL_GPU_DRIVER_STATE returned by a prior call to GpuDriverInit_Null.
// o_info: Pointer to a PIL_GPU_DRIVER_INFO structure to populate.
// Returns zero if o_info was updated with information, or non-zero if an error occurred.
static int
GpuDriverQuery_Null
(
    struct PIL_GPU_DRIVER_STATE *drvctx, 
    struct PIL_GPU_DRIVER_INFO  *o_info
)
{
    PIL_UNUSED_ARG(drvctx);
    if (o_info != nullptr) {
        o_info->DriverVersion     = PIL_MakeVersion(0, 0, 0);
        o_info->DeviceCount       = 0;
        o_info->SupportedFeatures = PIL_GPU_DRIVER_FEATURE_FLAGS_NONE;
        o_info->DriverName[0]     = 0;
    }
    return -1;
}

// Create a logical GPU device.
// drvctx: Pointer to a PIL_GPU_DRIVER_STATE returned by a prior call to GpuDriverInit_Null.
// config: Data used to select the physical GPU and configure the logical GPU interface.
// devctx: On return, this location is updated to point to the logical GPU device state.
// Returns zero if the device was created successfully or non-zero if an error occurred.
static int
GpuDeviceCreate_Null
(
    struct PIL_GPU_DRIVER_STATE *drvctx, 
    struct PIL_GPU_DEVICE_INIT  *config, 
    void                       **devctx
)
{
    PIL_UNUSED_ARG(drvctx);
    PIL_UNUSED_ARG(config);
    if (devctx != nullptr) {
       *devctx  = nullptr;
    }
    return -1;
}

// Delete a logical GPU device and free any associated resources.
// drvctx: Pointer to a PIL_GPU_DRIVER_STATE returned by a prior call to GpuDriverInit_Null.
// devctx: Pointer to a PIL_GPU_DEVICE_STATE returned by a prior call to GpuDeviceCreate_Null.
static void
GpuDeviceDelete_Null
(
    struct PIL_GPU_DRIVER_STATE *drvctx, 
    struct PIL_GPU_DEVICE_STATE *devctx
)
{
    PIL_UNUSED_ARG(drvctx);
    PIL_UNUSED_ARG(devctx);
}

// Set the entry points for a PIL_GPU_DRIVER to point to the null driver.
// driver: The PIL_GPU_DRIVER structure to initialize.
static void
GpuDriverSetNullDriver
(
    struct PIL_GPU_DRIVER *driver
)
{
    if (driver != nullptr) {
        driver->State          = nullptr;
        driver->DriverInitFn   = GpuDriverInit_Null;
        driver->DriverShutFn   = GpuDriverShut_Null;
        driver->DriverQueryFn  = GpuDriverQuery_Null;
        driver->DeviceCreateFn = GpuDeviceCreate_Null;
        driver->DeviceDeleteFn = GpuDeviceDelete_Null;
        // ...
    }
}

PIL_API(int32_t)
PIL_GpuDriverOpen
(
    struct PIL_GPU_DRIVER           *driver, 
    struct PIL_GPU_DRIVER_INIT      *config, 
    struct PIL_GPU_DRIVER_INFO *driver_info, 
    PIL_GPU_DRIVER_TYPE         driver_type
)
{
    if (driver == nullptr || config == nullptr) {
        assert(driver != nullptr && "The driver argument is required");
        assert(config != nullptr && "The config argument is required");
        return -1;
    }
    switch (driver_type) {
        case PIL_GPU_DRIVER_TYPE_UNKNOWN:
            { assert(driver_type != PIL_GPU_DRIVER_TYPE_UNKNOWN);
              GpuDriverSetNullDriver(driver);
            } return -1;

        case PIL_GPU_DRIVER_TYPE_PLATFORM_DEFAULT:
            { driver->State          = nullptr;
              driver->DriverInitFn   = GpuDriverInit_Vk1;
              driver->DriverShutFn   = GpuDriverShut_Vk1;
              driver->DriverQueryFn  = GpuDriverQuery_Vk1;
              driver->DeviceCreateFn = nullptr; // TODO
              driver->DeviceDeleteFn = nullptr; // TODO
            } break;

        case PIL_GPU_DRIVER_TYPE_VULKAN1:
            { driver->State          = nullptr;
              driver->DriverInitFn   = GpuDriverInit_Vk1;
              driver->DriverShutFn   = GpuDriverShut_Vk1;
              driver->DriverQueryFn  = GpuDriverQuery_Vk1;
              driver->DeviceCreateFn = nullptr; // TODO
              driver->DeviceDeleteFn = nullptr; // TODO
            } break;

        default:
            { assert(0 && "Unknown driver_type");
              GpuDriverSetNullDriver(driver);
            } return -1;
    }
    if (driver->DriverInitFn(config, (void**)&driver->State) != 0) {
        GpuDriverSetNullDriver(driver);
        return -1;
    }
    if (driver->DriverQueryFn(driver->State, driver_info) != 0) {
        driver->DriverShutFn(driver->State);
        GpuDriverSetNullDriver(driver);
        return -1;
    }
    return 0;
}

PIL_API(void)
PIL_GpuDriverClose
(
    struct PIL_GPU_DRIVER *driver
)
{
    if (driver != nullptr) {
        if (driver->DriverShutFn != nullptr) {
            driver->DriverShutFn(driver->State);
        }
        GpuDriverSetNullDriver(driver);
    }
}

