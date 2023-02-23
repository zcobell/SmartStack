# ##############################################################################
# CMake Build File for SmartStack
#
# Written By: Zach Cobell
#
# ##############################################################################
#
# The CMake build system enable SmartStack to be deployed and built in a cross
# platform environment.
#
# ##############################################################################

# ...Set the default build type
if(DEFINED CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE
      ${CMAKE_BUILD_TYPE}
      CACHE STRING "Choose the type of
        build, options are: None(CMAKE_CXX_FLAGS or CMAKE_C_FLAGS used) Debug
        Release RelWithDebInfo MinSizeRel.")
elseif(COVERAGE)
  set(CMAKE_BUILD_TYPE
      Debug
      CACHE
        STRING
        "Choose the type of build,
            options are: None(CMAKE_CXX_FLAGS or CMAKE_C_FLAGS used) Debug Release
            RelWithDebInfo MinSizeRel.")
else()
  set(CMAKE_BUILD_TYPE
      Release
      CACHE STRING "Choose the type of build,
        options are: None(CMAKE_CXX_FLAGS or CMAKE_C_FLAGS used) Debug Release
        RelWithDebInfo MinSizeRel.")
endif()

# ##############################################################################
# GENERAL OPTIONS
# ##############################################################################
cmake_minimum_required(VERSION 2.8.12)
project(SmartStack)
enable_language(CXX)
include(CheckIncludeFiles)
include(CheckLibraryExists)
include(CheckFunctionExists)
include(GNUInstallDirs)
include(CMakePackageConfigHelpers)
# ##############################################################################

# ##############################################################################
# Enable running tests
# ##############################################################################
if(UNIX OR CYGWIN)
  enable_testing()
endif(UNIX OR CYGWIN)
# ##############################################################################

# ##############################################################################
# Enable Coverage
# ##############################################################################
# OPTION(COVERAGE "Export Code Coverage report from tests" OFF)
set(CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/CMakeModules)
if(COVERAGE)
  if(CMAKE_COMPILER_IS_GNUCXX)
    include(CodeCoverage)
    setup_target_for_coverage(smartstack_coverage tests coverage)
    set(CMAKE_CXX_FLAGS
        "${CMAKE_CXX_FLAGS} -Wall -pedantic -pthread -g -O0 -fprofile-arcs -ftest-coverage"
    )
    set(CMAKE_Fortran_FLAGS
        "${CMAKE_CXX_FLAGS} -Wall -pedantic -pthread -g -O0 -fprofile-arcs -ftest-coverage"
    )
  endif(CMAKE_COMPILER_IS_GNUCXX)
endif(COVERAGE)
# ##############################################################################

# ##############################################################################
# C++ 17/20 Check
# ##############################################################################
include(CheckCXXCompilerFlag)
include(CheckIncludeFileCXX)
check_cxx_compiler_flag("-std=c++17" COMPILER_SUPPORTS_CXX17)
check_cxx_compiler_flag("-std=c++20" COMPILER_SUPPORTS_CXX20)
if(COMPILER_SUPPORTS_CXX20)
  set(CMAKE_CXX_STANDARD 20)
elseif(COMPILER_SUPPORTS_CXX17)
  set(CMAKE_CXX_STANDARD 17)
else(COMPILER_SUPPORTS_CXX20)
  message(
    FATAL_ERROR
      "The compiler ${CMAKE_CXX_COMPILER} has no C++20 or C++17 support. Please use a different C++ compiler."
  )
endif(COMPILER_SUPPORTS_CXX20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
check_include_file_cxx("format" CXX_FORMAT_SUPPORT)
# ##############################################################################

# ##############################################################################
# Compiler flags
# ##############################################################################
set(CMAKE_POSITION_INDEPENDENT_CODE ON)
mark_as_advanced(CLEAR CMAKE_CXX_FLAGS_RELEASE)
mark_as_advanced(CLEAR CMAKE_CXX_FLAGS_DEBUG)
mark_as_advanced(CLEAR CMAKE_C_FLAGS_RELEASE)
mark_as_advanced(CLEAR CMAKE_C_FLAGS_DEBUG)
mark_as_advanced(CLEAR CMAKE_CXX_COMPILER)
mark_as_advanced(CLEAR CMAKE_C_COMPILER)
# ##############################################################################

# ##############################################################################
# LIBRARY VERSION
# ##############################################################################
set(SMARTSTACK_VERSION_MAJOR 0)
set(SMARTSTACK_VERSION_MINOR 3)
set(SMARTSTACK_VERSION_PATCH 0)
set(SMARTSTACK_VERSION_STRING
    ${SMARTSTACK_VERSION_MAJOR}.${SMARTSTACK_VERSION_MINOR}.${SMARTSTACK_VERSION_PATCH}
)
# ##############################################################################

option(SMARTSTACK_BUILDSHARED "Build shared object version of SmartStack" OFF)
if(SMARTSTACK_BUILDSHARED)
  set(SMARTSTACK_LIBTYPE SHARED)
else(SMARTSTACK_BUILDSHARED)
  set(SMARTSTACK_LIBTYPE STATIC)
endif(SMARTSTACK_BUILDSHARED)

option(SMARTSTACK_ENABLE_FORTRAN "Build Fortran interface to SmartStack" OFF)
if(SMARTSTACK_ENABLE_FORTRAN)
  enable_language(Fortran)
  mark_as_advanced(CLEAR CMAKE_Fortran_COMPILER CMAKE_Fortran_FLAGS_DEBUG
                   CMAKE_Fortran_FLAGS_RELEASE)
endif()

# ##############################################################################
# CODE VERSION (GIT)
# ##############################################################################
execute_process(
  COMMAND git describe --always --tags
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
  OUTPUT_VARIABLE GIT_VERSION
  RESULT_VARIABLE GIT_RETURN_VALUE
  OUTPUT_STRIP_TRAILING_WHITESPACE)
if(NOT "${GIT_RETURN_VALUE}" STREQUAL "0")
  set(GIT_VERSION "${SMARTSTACK_VERSION_STRING}.cv")
endif()
message(STATUS "SmartStack Version: ${GIT_VERSION}")
# ##############################################################################

# ##############################################################################
# TESTING AND BENCHMARKING
# ##############################################################################
option(SMARTSTACK_BUILD_TESTS "Build test cases" OFF)
option(SMARTSTACK_BUILD_BENCHMARK "Build benchmark" OFF)
if(SMARTSTACK_BUILD_BENCHMARK)
  set(GOOGLE_BENCHMARK_HOME
      "/opt/google/benchmark"
      CACHE STRING "Location of the Google Benchmark library")
endif(SMARTSTACK_BUILD_BENCHMARK)
# ##############################################################################

# ##############################################################################
# ABSEIL SWISS TABLES
# ##############################################################################
if(NOT CYGWIN)
  option(SMARTSTACK_USE_ABSEIL_FLAT_MAP
         "Use the Abseil Swiss Tables to increase speed" OFF)
  if(SMARTSTACK_USE_ABSEIL_FLAT_MAP)
    if(MSVC)
      # /wd4005  macro-redefinition /wd4068  unknown pragma /wd4244  conversion
      # from 'type1' to 'type2' /wd4267  conversion from 'size_t' to 'type2'
      # /wd4800  force value to bool 'true' or 'false' (performance warning)
      add_compile_options(/wd4005 /wd4068 /wd4244 /wd4267 /wd4800)
      add_definitions(/DNOMINMAX /DWIN32_LEAN_AND_MEAN=1
                      /D_CRT_SECURE_NO_WARNINGS)
    endif(MSVC)
    add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/abseil-cpp
                     EXCLUDE_FROM_ALL)
    message(
      STATUS
        "Using the Abseil Swiss Tables instead of unordered_map for performance"
    )
  endif(SMARTSTACK_USE_ABSEIL_FLAT_MAP)
else(NOT CYGWIN)
  set(SMARTSTACK_USE_ABSEIL_FLAT_MAP FALSE)
endif(NOT CYGWIN)
# ##############################################################################

# LIBFMT
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/fmt)

# ##############################################################################
# SET THE LOCATION OF TEMPORARY STATIC LIBS
# ##############################################################################
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/CMakeFiles)
# ##############################################################################

# ##############################################################################
# SmartStack Library
# ##############################################################################
set(SMARTSTACK_SOURCES
        ${CMAKE_CURRENT_SOURCE_DIR}/src/timer.cpp
        ${CMAKE_CURRENT_SOURCE_DIR}/src/function.cpp
        ${CMAKE_CURRENT_SOURCE_DIR}/src/stack.cpp
        ${CMAKE_CURRENT_SOURCE_DIR}/src/report.cpp
        ${CMAKE_CURRENT_SOURCE_DIR}/src/stackimpl.cpp)


if(SMARTSTACK_ENABLE_FORTRAN)
  set(SMARTSTACK_SOURCES
      ${SMARTSTACK_SOURCES}
          ${CMAKE_CURRENT_SOURCE_DIR}/src/smartstackftn.cpp
          ${CMAKE_CURRENT_SOURCE_DIR}/src/smartstack.F90 src/smartstack_types.h)
endif(SMARTSTACK_ENABLE_FORTRAN)

add_library(smartstack ${SMARTSTACK_LIBTYPE} ${SMARTSTACK_SOURCES})

if(SMARTSTACK_ENABLE_FORTRAN)
  set_target_properties(
    smartstack PROPERTIES Fortran_MODULE_DIRECTORY
                          ${CMAKE_BINARY_DIR}/CMakeFiles/smartstack)
endif()

set(HEADER_LIST ${CMAKE_SOURCE_DIR}/src/smartstack.h)

target_include_directories(smartstack PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/src)

target_compile_definitions(smartstack PRIVATE GIT_VERSION="${GIT_VERSION}")
target_compile_definitions(smartstack PRIVATE SMARTSTACK_LIBRARY)

set_target_properties(
  smartstack PROPERTIES VERSION ${SMARTSTACK_VERSION_STRING}
                        SOVERSION ${SMARTSTACK_VERSION_MAJOR})

set_target_properties(smartstack PROPERTIES PUBLIC_HEADER "${HEADER_LIST}")

if(${CMAKE_INSTALL_PREFIX} STREQUAL "/usr/local"
   OR ${CMAKE_INSTALL_PREFIX} STREQUAL "/usr/local/"
   OR ${CMAKE_INSTALL_PREFIX} STREQUAL "/usr/"
   OR ${CMAKE_INSTALL_PREFIX} STREQUAL "/usr/")
  set(HEADER_DEST "${CMAKE_INSTALL_INCLUDEDIR}/smartstack")
else()
  set(HEADER_DEST ${CMAKE_INSTALL_INCLUDEDIR})
endif()

write_basic_package_version_file(
  smartstackConfigVersion.cmake
  VERSION ${SMARTSTACK_VERSION_STRING}
  COMPATIBILITY SameMajorVersion)
install(
  TARGETS smartstack
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR} COMPONENT SMARTSTACK_RUNTIME
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR} COMPONENT SMARTSTACK_RUNTIME
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR} COMPONENT SMARTSTACK_DEVELOPMENT
  PUBLIC_HEADER DESTINATION ${HEADER_DEST} COMPONENT SMARTSTACK_DEVELOPMENT)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/smartstackConfigVersion.cmake
        DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake)

set_target_properties(smartstack PROPERTIES CMAKE_CXX_VISIBILITY_PRESET hidden)
set_target_properties(smartstack PROPERTIES CMAKE_CXX_INLINES_HIDDEN YES)

if(SMARTSTACK_BUILD_BENCHMARK)
  target_compile_definitions(smartstack PRIVATE SMARTSTACK_BENCHMARKING)
endif(SMARTSTACK_BUILD_BENCHMARK)

if(SMARTSTACK_USE_ABSEIL_FLAT_MAP)
  target_link_libraries(smartstack absl::flat_hash_map)
  add_dependencies(smartstack absl::flat_hash_map)
  target_compile_definitions(smartstack PRIVATE SMARTSTACK_USE_FLAT_MAP)
endif(SMARTSTACK_USE_ABSEIL_FLAT_MAP)

target_compile_definitions(smartstack PRIVATE "SMARTSTACK_USE_LIBFMT")
target_include_directories(smartstack PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/fmt/include)
target_link_libraries(smartstack fmt)
add_dependencies(smartstack fmt)

if(APPLE)
  set(CMAKE_MACOSX_RPATH 0)
  set_target_properties(smartstack PROPERTIES INSTALL_NAME_DIR "smartstack")
  set_target_properties(smartstack PROPERTIES MACOSX_RPATH "smartstack")
endif(APPLE)

# ##############################################################################

if(SMARTSTACK_BUILD_TESTS)
  set(TEST_LIST cxx_testSmartStack.cpp)
  if(SMARTSTACK_ENABLE_FORTRAN)
    set(TEST_LIST ${TEST_LIST} ftn_testSmartStack.F90
                  ftn_testSmartStackParallel.F90)
  endif()

  foreach(TESTFILE ${TEST_LIST})
    get_filename_component(TESTNAME ${TESTFILE} NAME_WE)
    add_executable(${TESTNAME} ${CMAKE_SOURCE_DIR}/tests/${TESTFILE})
    add_dependencies(${TESTNAME} smartstack)
    target_include_directories(
      ${TESTNAME} PRIVATE ${CMAKE_SOURCE_DIR}/src
                          ${CMAKE_BINARY_DIR}/CMakeFiles/smartstack)
    add_dependencies(${TESTNAME} smartstack)
    target_link_libraries(${TESTNAME} smartstack)
    set_target_properties(
      ${TESTNAME} PROPERTIES RUNTIME_OUTPUT_DIRECTORY
                             ${CMAKE_BINARY_DIR}/CMakeFiles/tests)

    get_filename_component(TEST_EXTENSION ${TESTFILE} EXT)

    if(SMARTSTACK_ENABLE_FORTRAN)
      if(${TEST_EXTENSION} STREQUAL ".F90")
        set_target_properties(${TESTNAME} PROPERTIES LINKER_LANGUAGE Fortran)
      endif()
    endif()

    add_test(
      NAME TEST_${TESTNAME}
      COMMAND ${CMAKE_BINARY_DIR}/CMakeFiles/tests/${TESTNAME}
      WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/CMakeFiles/tests)
    if(CYGWIN)
      set_tests_properties(
        TEST_${TESTNAME} PROPERTIES ENVIRONMENT
                                    "PATH=$ENV{PATH}:${CMAKE_BINARY_DIR}")
    else(CYGWIN)
      set_tests_properties(
        TEST_${TESTNAME}
        PROPERTIES ENVIRONMENT
                   "LD_LIBRARY_PATH=$ENV{LD_LIBRARY_PATH}:${CMAKE_BINARY_DIR}")
    endif(CYGWIN)
  endforeach()
endif(SMARTSTACK_BUILD_TESTS)

if(SMARTSTACK_BUILD_BENCHMARK)
  add_executable(smartstack_benchmark ${CMAKE_SOURCE_DIR}/bench/main.cpp)
  target_link_libraries(smartstack_benchmark smartstack fmt benchmark pthread)
  target_link_directories(smartstack_benchmark PRIVATE
                          ${GOOGLE_BENCHMARK_HOME}/lib)
  target_include_directories(
    smartstack_benchmark PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/src
                                 ${GOOGLE_BENCHMARK_HOME}/include)
endif(SMARTSTACK_BUILD_BENCHMARK)
