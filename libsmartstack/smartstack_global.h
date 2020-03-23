#ifndef SMARTSTACK_GLOBAL_H
#define SMARTSTACK_GLOBAL_H

#include <cstdlib>
#include <limits>

#if defined(_MSC_VER)
#define DLL_EXPORT __declspec(dllexport)
#define DLL_IMPORT __declspec(dllimport)
#elif defined(__GNUC__)
#define DLL_EXPORT __attribute__((visibility("default")))
#define DLL_IMPORT
#if __GNUC__ > 4
#define DLL_LOCAL __attribute__((visibility("hidden")))
#else
#define DLL_LOCAL
#endif
#elif defined(SWIG)
// When compiling the SWIG interface,
// ignore the DLL_IMPORT/DLL_EXPORT Macros
#define DLL_EXPORT
#define DLL_IMPORT
#else
#error("Don't know how to export shared object libraries")
#endif

#if defined(SMARTSTACK_LIBRARY)
#define SMARTSTACK_EXPORT DLL_EXPORT
#else
#define SMARTSTACK_EXPORT DLL_IMPORT
#endif

#endif  // SMARTSTACK_GLOBAL_H
