// gpudriver.cc: Provide a null driver implementation and implement the 
// top-level GPU driver interface routines. The null driver routines all fail 
// and perform no actual operation.
#include <string.h>
#include "platform.h"

#include "gpudrvvk.h"


PIL_API(int32_t)
PIL_GpuDriverOpen
(
    PIL_GPU_DRIVER_TYPE driver_type
)
{
    PIL_UNUSED_ARG(driver_type);
    return -1;
}

