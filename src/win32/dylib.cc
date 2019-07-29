// dylib.cc: Implement the functions for working with runtime-loaded shared libraries.
#include "platform.h"

PIL_API(void)
PIL_RuntimeModuleInit
(
    struct PIL_RUNTIME_MODULE *module
)
{
    if (module != nullptr) {
        module->ModuleHandle = nullptr;
    }
}

PIL_API(int32_t)
PIL_RuntimeModuleLoad
(
    struct PIL_RUNTIME_MODULE *module, 
    char const                  *path
)
{
    if (module != nullptr) {
        if ((module->ModuleHandle = LoadLibraryA(path)) != nullptr) {
            return  0;
        } else {
            return -1;
        }
    } else {
        assert(module != nullptr && "The module argument is required");
        assert(path   != nullptr && "The path argument is required");
        return -1;
    }
}

PIL_API(void)
PIL_RuntimeModuleUnload
(
    struct PIL_RUNTIME_MODULE *module
)
{
    if (module != nullptr) {
        if (module->ModuleHandle != nullptr) {
            FreeLibrary(module->ModuleHandle);
            module->ModuleHandle  = nullptr;
        }
    }
}

PIL_API(int32_t)
PIL_RuntimeModuleIsValid
(
    struct PIL_RUNTIME_MODULE *module
)
{
    if (module != nullptr) {
        if (module->ModuleHandle != nullptr) {
            return 1;
        }
    } return 0;
}

PIL_API(PIL_PFN_Unknown)
PIL_RuntimeModuleResolve
(
    struct PIL_RUNTIME_MODULE *module, 
    char const                *symbol
)
{
    if (module != nullptr) {
        return (PIL_PFN_Unknown) GetProcAddress(module->ModuleHandle, symbol);
    } else {
        return (PIL_PFN_Unknown) nullptr;
    }
}
