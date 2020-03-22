
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
PROJECT(SmartStack)
###########################################################################

ENABLE_LANGUAGE(Fortran)
ENABLE_LANGUAGE(CXX)

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

###########################################################################
# CODE VERSION (GIT)
###########################################################################
EXECUTE_PROCESS( COMMAND git describe --always --tags
                    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
                    OUTPUT_VARIABLE GIT_VERSION
                    RESULT_VARIABLE GIT_RETURN_VALUE
                    OUTPUT_STRIP_TRAILING_WHITESPACE )
IF( NOT "${GIT_RETURN_VALUE}" STREQUAL "0" )
    SET(GIT_VERSION "${SMARTSTACK_VERSION_STRING}.cv")
ENDIF()
MESSAGE(STATUS "SmartStack Version: ${GIT_VERSION}")
###########################################################################

###########################################################################
# TESTING 
###########################################################################
OPTION(BUILD_TESTS "Build test cases" OFF)
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

ADD_LIBRARY( smartstack SHARED ${SMARTSTACK_SOURCES} )

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

IF(APPLE)
    SET(CMAKE_MACOSX_RPATH 0)
    SET_TARGET_PROPERTIES(smartstack PROPERTIES INSTALL_NAME_DIR "smartstack")
    SET_TARGET_PROPERTIES(smartstack PROPERTIES MACOSX_RPATH "smartstack")
ENDIF(APPLE)

###########################################################################


ADD_EXECUTABLE(ftn_test ${CMAKE_CURRENT_SOURCE_DIR}/test/fortranTest.F90 )
ADD_DEPENDENCIES( ftn_test smartstack )
TARGET_LINK_LIBRARIES( ftn_test smartstack )
