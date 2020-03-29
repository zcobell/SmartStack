
###########################################################################
#                     CMake Build File for SmartStack
#
#    Written By: Zach Cobell
#
###########################################################################
#
# The CMake build system enable SmartStack to be deployed and built
# in a cross platform environment. 
#
###########################################################################
INCLUDE (CheckIncludeFiles)
INCLUDE (CheckLibraryExists) 
INCLUDE (CheckFunctionExists)
INCLUDE (GNUInstallDirs)
INCLUDE (CMakePackageConfigHelpers)

#...Set the default build type
IF(DEFINED CMAKE_BUILD_TYPE)
    SET(CMAKE_BUILD_TYPE ${CMAKE_BUILD_TYPE} CACHE STRING "Choose the type of
        build, options are: None(CMAKE_CXX_FLAGS or CMAKE_C_FLAGS used) Debug
        Release RelWithDebInfo MinSizeRel.")
ELSEIF(COVERAGE)
        SET(CMAKE_BUILD_TYPE Debug CACHE STRING "Choose the type of build,
            options are: None(CMAKE_CXX_FLAGS or CMAKE_C_FLAGS used) Debug Release
            RelWithDebInfo MinSizeRel.")
ELSE()
    SET(CMAKE_BUILD_TYPE Release CACHE STRING "Choose the type of build,
        options are: None(CMAKE_CXX_FLAGS or CMAKE_C_FLAGS used) Debug Release
        RelWithDebInfo MinSizeRel.")
ENDIF()

###########################################################################
#  GENERAL OPTIONS
###########################################################################
CMAKE_MINIMUM_REQUIRED(VERSION 2.8.12)
PROJECT(SmartStack CXX)
###########################################################################

ENABLE_LANGUAGE(Fortran)
MARK_AS_ADVANCED(CLEAR CMAKE_Fortran_COMPILER CMAKE_Fortran_FLAGS_DEBUG CMAKE_Fortran_FLAGS_RELEASE)

###########################################################################
# Enable running tests
###########################################################################
IF(UNIX OR CYGWIN)
    ENABLE_TESTING()
ENDIF(UNIX OR CYGWIN)
###########################################################################


###########################################################################
# Enable Coverage
###########################################################################
#OPTION(COVERAGE "Export Code Coverage report from tests" OFF)
SET(CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/CMakeModules)
IF(COVERAGE)
    IF(CMAKE_COMPILER_IS_GNUCXX) 
        INCLUDE(CodeCoverage)
        setup_target_for_coverage(smartstack_coverage tests coverage)
        SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -pedantic -pthread -g -O0 -fprofile-arcs -ftest-coverage")
        SET(CMAKE_Fortran_FLAGS "${CMAKE_CXX_FLAGS} -Wall -pedantic -pthread -g -O0 -fprofile-arcs -ftest-coverage")
    ENDIF(CMAKE_COMPILER_IS_GNUCXX)
ENDIF(COVERAGE)
###########################################################################


###########################################################################
# C++ 11/14 Check
###########################################################################
INCLUDE(CheckCXXCompilerFlag)
CHECK_CXX_COMPILER_FLAG("-std=c++14" COMPILER_SUPPORTS_CXX14)
IF(COMPILER_SUPPORTS_CXX14)
    SET(CMAKE_CXX_STANDARD 14)
    SET(CMAKE_CXX_STANDARD_REQUIRED ON)
ELSE(COMPILER_SUPPORTS_CXX14)
    CHECK_CXX_COMPILER_FLAG("-std=c++11" COMPILER_SUPPORTS_CXX11)
    IF(COMPILER_SUPPORTS_CXX11)
        SET(CMAKE_CXX_STANDARD 11)
        SET(CMAKE_CXX_STANDARD_REQUIRED ON)
    ELSE(COMPILER_SUPPORTS_CXX11)
        message(FATAL_ERROR "The compiler ${CMAKE_CXX_COMPILER} has no C++11/14 support. Please use a different C++ compiler.")
    ENDIF(COMPILER_SUPPORTS_CXX11)
ENDIF(COMPILER_SUPPORTS_CXX14)
###########################################################################

###########################################################################
#  Compiler flags 
###########################################################################
SET(CMAKE_POSITION_INDEPENDENT_CODE ON)
MARK_AS_ADVANCED( CLEAR CMAKE_CXX_FLAGS_RELEASE )
MARK_AS_ADVANCED( CLEAR CMAKE_CXX_FLAGS_DEBUG )
MARK_AS_ADVANCED( CLEAR CMAKE_C_FLAGS_RELEASE )
MARK_AS_ADVANCED( CLEAR CMAKE_C_FLAGS_DEBUG )
MARK_AS_ADVANCED( CLEAR CMAKE_CXX_COMPILER )
MARK_AS_ADVANCED( CLEAR CMAKE_C_COMPILER )
###########################################################################


###########################################################################
#  LIBRARY VERSION
###########################################################################
set(SMARTSTACK_VERSION_MAJOR 0)
set(SMARTSTACK_VERSION_MINOR 3)
set(SMARTSTACK_VERSION_PATCH 0)
set(SMARTSTACK_VERSION_STRING ${SMARTSTACK_VERSION_MAJOR}.${SMARTSTACK_VERSION_MINOR}.${SMARTSTACK_VERSION_PATCH})
###########################################################################

OPTION( SMARTSTACK_BUILDSHARED "Build shared object version of SmartStack" OFF )
IF( SMARTSTACK_BUILDSHARED )
    SET( SMARTSTACK_LIBTYPE SHARED )
ELSE( SMARTSTACK_BUILDSHARED )
    SET( SMARTSTACK_LIBTYPE STATIC )
ENDIF( SMARTSTACK_BUILDSHARED )

###########################################################################
# CODE VERSION (GIT)
###########################################################################
EXECUTE_PROCESS( COMMAND git describe --always --tags
                 WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
                 OUTPUT_VARIABLE GIT_VERSION
                 RESULT_VARIABLE GIT_RETURN_VALUE
                 OUTPUT_STRIP_TRAILING_WHITESPACE )
IF( NOT "${GIT_RETURN_VALUE}" STREQUAL "0" )
    SET(GIT_VERSION "${SMARTSTACK_VERSION_STRING}.cv")
ENDIF()
MESSAGE(STATUS "SmartStack Version: ${GIT_VERSION}")
###########################################################################

###########################################################################
# TESTING AND BENCHMARKING
###########################################################################
OPTION(SMARTSTACK_BUILD_TESTS "Build test cases" OFF)
OPTION(SMARTSTACK_BUILD_BENCHMARK "Build benchmark" OFF)
IF(SMARTSTACK_BUILD_BENCHMARK)
    SET( GOOGLE_BENCHMARK_HOME "/opt/google/benchmark" CACHE STRING "Location of the Google Benchmark library" )
ENDIF(SMARTSTACK_BUILD_BENCHMARK)
###########################################################################

###########################################################################
# ABSEIL SWISS TABLES
###########################################################################
IF(NOT CYGWIN)
    OPTION(SMARTSTACK_USE_ABSEIL_FLAT_MAP "Use the Abseil Swiss Tables to increase speed" OFF)
    IF(SMARTSTACK_USE_ABSEIL_FLAT_MAP)
        if(MSVC)
          # /wd4005  macro-redefinition
          # /wd4068  unknown pragma
          # /wd4244  conversion from 'type1' to 'type2'
          # /wd4267  conversion from 'size_t' to 'type2'
          # /wd4800  force value to bool 'true' or 'false' (performance warning)
          add_compile_options(/wd4005 /wd4068 /wd4244 /wd4267 /wd4800)
          add_definitions(/DNOMINMAX /DWIN32_LEAN_AND_MEAN=1 /D_CRT_SECURE_NO_WARNINGS)
        endif(MSVC)
        add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/abseil-cpp EXCLUDE_FROM_ALL)
        MESSAGE(STATUS "Using the Abseil Swiss Tables instead of unordered_map for performance")
    ENDIF(SMARTSTACK_USE_ABSEIL_FLAT_MAP)    
ELSE(NOT CYGWIN)
    SET(SMARTSTACK_USE_ABSEIL_FLAT_MAP FALSE)
ENDIF(NOT CYGWIN)
###########################################################################

###########################################################################
#  SET THE LOCATION OF TEMPORARY STATIC LIBS
###########################################################################
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/CMakeFiles)
###########################################################################

###########################################################################
#  SmartStack Library
###########################################################################
SET( SMARTSTACK_SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/libsmartstack/timer.cpp
                        ${CMAKE_CURRENT_SOURCE_DIR}/libsmartstack/function.cpp
                        ${CMAKE_CURRENT_SOURCE_DIR}/libsmartstack/stack.cpp 
                        ${CMAKE_CURRENT_SOURCE_DIR}/libsmartstack/smartstackftn.cpp
                        ${CMAKE_CURRENT_SOURCE_DIR}/libsmartstack/smartstack.F90 )

ADD_LIBRARY( smartstack ${SMARTSTACK_LIBTYPE} ${SMARTSTACK_SOURCES} )

SET_TARGET_PROPERTIES( smartstack PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/CMakeFiles/smartstack )

SET(HEADER_LIST ${CMAKE_SOURCE_DIR}/libsmartstack/smartstack.h )

TARGET_INCLUDE_DIRECTORIES( smartstack PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/libsmartstack )

TARGET_COMPILE_DEFINITIONS( smartstack PRIVATE GIT_VERSION="${GIT_VERSION}")
TARGET_COMPILE_DEFINITIONS( smartstack PRIVATE SMARTSTACK_LIBRARY )

SET_TARGET_PROPERTIES( smartstack PROPERTIES VERSION ${SMARTSTACK_VERSION_STRING} SOVERSION ${SMARTSTACK_VERSION_MAJOR} )

SET_TARGET_PROPERTIES( smartstack PROPERTIES PUBLIC_HEADER "${HEADER_LIST}" ) 

IF(${CMAKE_INSTALL_PREFIX} STREQUAL "/usr/local" OR ${CMAKE_INSTALL_PREFIX} STREQUAL "/usr/local/" OR
   ${CMAKE_INSTALL_PREFIX} STREQUAL "/usr/" OR ${CMAKE_INSTALL_PREFIX} STREQUAL "/usr/")
    SET(HEADER_DEST "${CMAKE_INSTALL_INCLUDEDIR}/smartstack")
ELSE()
    SET(HEADER_DEST ${CMAKE_INSTALL_INCLUDEDIR})
ENDIF()

WRITE_BASIC_PACKAGE_VERSION_FILE( smartstackConfigVersion.cmake VERSION ${SMARTSTACK_VERSION_STRING} COMPATIBILITY SameMajorVersion )
INSTALL( TARGETS smartstack    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR} COMPONENT SMARTSTACK_RUNTIME
                               LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR} COMPONENT SMARTSTACK_RUNTIME
                               ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR} COMPONENT SMARTSTACK_DEVELOPMENT
                               PUBLIC_HEADER DESTINATION ${HEADER_DEST}    COMPONENT SMARTSTACK_DEVELOPMENT )
INSTALL( FILES ${CMAKE_CURRENT_BINARY_DIR}/smartstackConfigVersion.cmake DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake )

SET_TARGET_PROPERTIES(smartstack PROPERTIES CMAKE_CXX_VISIBILITY_PRESET hidden)
SET_TARGET_PROPERTIES(smartstack PROPERTIES CMAKE_CXX_INLINES_HIDDEN YES)

IF(SMARTSTACK_BUILD_BENCHMARK)
    TARGET_COMPILE_DEFINITIONS( smartstack PRIVATE SMARTSTACK_BENCHMARKING )
ENDIF(SMARTSTACK_BUILD_BENCHMARK)    

IF(SMARTSTACK_USE_ABSEIL_FLAT_MAP)
    TARGET_LINK_LIBRARIES( smartstack absl::flat_hash_map)
    ADD_DEPENDENCIES( smartstack absl::flat_hash_map)
    TARGET_COMPILE_DEFINITIONS( smartstack PRIVATE USE_ABSEIL_FLAT_MAP )
ENDIF(SMARTSTACK_USE_ABSEIL_FLAT_MAP)

IF(APPLE)
    SET(CMAKE_MACOSX_RPATH 0)
    SET_TARGET_PROPERTIES(smartstack PROPERTIES INSTALL_NAME_DIR "smartstack")
    SET_TARGET_PROPERTIES(smartstack PROPERTIES MACOSX_RPATH "smartstack")
ENDIF(APPLE)

###########################################################################


IF(SMARTSTACK_BUILD_TESTS)
    SET(TEST_LIST ftn_testSmartStack.F90 ftn_testSmartStackParallel.F90 cxx_testSmartStack.cpp )
    FOREACH(TESTFILE ${TEST_LIST} ) 
        GET_FILENAME_COMPONENT( TESTNAME ${TESTFILE} NAME_WE )
        ADD_EXECUTABLE( ${TESTNAME} ${CMAKE_SOURCE_DIR}/tests/${TESTFILE} )
        ADD_DEPENDENCIES( ${TESTNAME} smartstack )
        TARGET_INCLUDE_DIRECTORIES( ${TESTNAME} PRIVATE ${CMAKE_SOURCE_DIR}/libsmartstack ${CMAKE_BINARY_DIR}/CMakeFiles/smartstack ) 
        TARGET_LINK_LIBRARIES( ${TESTNAME} smartstack )
        SET_TARGET_PROPERTIES( ${TESTNAME} PROPERTIES RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/CMakeFiles/tests )

        GET_FILENAME_COMPONENT( TEST_EXTENSION ${TESTFILE} EXT ) 

        IF( ${TEST_EXTENSION} STREQUAL ".F90" )
            SET_TARGET_PROPERTIES(${TESTNAME} PROPERTIES LINKER_LANGUAGE Fortran)
        ENDIF()
    
        ADD_TEST( NAME TEST_${TESTNAME} COMMAND ${CMAKE_BINARY_DIR}/CMakeFiles/tests/${TESTNAME}
            WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/CMakeFiles/tests )
        IF(CYGWIN)
           SET_TESTS_PROPERTIES( TEST_${TESTNAME} PROPERTIES ENVIRONMENT "PATH=$ENV{PATH}:${CMAKE_BINARY_DIR}")
        ELSE(CYGWIN)
           SET_TESTS_PROPERTIES( TEST_${TESTNAME} PROPERTIES ENVIRONMENT "LD_LIBRARY_PATH=$ENV{LD_LIBRARY_PATH}:${CMAKE_BINARY_DIR}")
        ENDIF(CYGWIN)
    ENDFOREACH()
ENDIF(SMARTSTACK_BUILD_TESTS)

IF(SMARTSTACK_BUILD_BENCHMARK)
    ADD_EXECUTABLE( smartstack_benchmark ${CMAKE_SOURCE_DIR}/bench/main.cpp )
    TARGET_LINK_LIBRARIES( smartstack_benchmark smartstack benchmark pthread )
    TARGET_LINK_DIRECTORIES( smartstack_benchmark PRIVATE ${GOOGLE_BENCHMARK_HOME}/lib )
    TARGET_INCLUDE_DIRECTORIES( smartstack_benchmark PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/libsmartstack ${GOOGLE_BENCHMARK_HOME}/include )
ENDIF(SMARTSTACK_BUILD_BENCHMARK)
