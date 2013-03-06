# CMakeLists.txt --- CMake build description file.
#
# Copyright (C) 2011, 2012, 2013 Jan Moringen
#
# Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
#
# This file may be licensed under the terms of of the GNU Lesser
# General Public License Version 3 (the ``LGPL''), or (at your option)
# any later version.
#
# Software distributed under the License is distributed on an ``AS
# IS'' basis, WITHOUT WARRANTY OF ANY KIND, either express or
# implied. See the LGPL for the specific language governing rights and
# limitations.
#
# You should have received a copy of the LGPL along with this
# program. If not, go to http://www.gnu.org/licenses/lgpl.html or
# write to the Free Software Foundation, Inc., 51 Franklin Street,
# Fifth Floor, Boston, MA 02110-1301, USA.
#
# The development of this software was supported by:
#   CoR-Lab, Research Institute for Cognition and Robotics
#     Bielefeld University

CMAKE_MINIMUM_REQUIRED(VERSION 2.8)

# Extract version components from an approximately "git describe"-like
# formatted version string.
FUNCTION(GIT_VERSION_COMPONENTS INPUT OUT_REVISION)
    SET(REGEX "^[-.a-z0-9]*([0-9]+)\\.([0-9]+)[-.]([0-9]+)-?(g[a-f0-9]+(:?-dirty)?|)$")
    STRING(REGEX REPLACE ${REGEX} "\\3" ${OUT_REVISION} "${INPUT}")
    SET(${OUT_REVISION} ${${OUT_REVISION}} PARENT_SCOPE)
ENDFUNCTION()

# Git-based version: run git describe, parse the output and store the
# "revision" and "commit" components into version.sexp.
FIND_PROGRAM(GIT_EXECUTABLE "git")
IF(GIT_EXECUTABLE)
    FUNCTION(GIT_BASED_VERSION MAJOR MINOR OUTPUT)
        MATH(EXPR MINOR_MINUS_ONE "${MINOR} - 1")
        SET(RELEASE_TAG "release-${MAJOR}.${MINOR_MINUS_ONE}")
        EXECUTE_PROCESS(COMMAND ${GIT_EXECUTABLE} describe
                                                  --long --dirty=-dirty
                                                  --match "${RELEASE_TAG}"
                        RESULT_VARIABLE RESULT
                        OUTPUT_VARIABLE GIT_DESCRIBE_OUTPUT
                        OUTPUT_STRIP_TRAILING_WHITESPACE
                        ERROR_QUIET)
        IF(RESULT EQUAL 0)
            SET(${OUTPUT} ${GIT_DESCRIBE_OUTPUT} PARENT_SCOPE)
        ELSE()
            MESSAGE(STATUS "git describe failed (not a git repository?) in ${CMAKE_SOURCE_DIR}")
        ENDIF()
    ENDFUNCTION()
ELSE()
    MESSAGE(STATUS "git executable not found")
ENDIF()

# Project definition (version is grovelled from asd system definition,
# possibly in combination with git describe).
SET(SYSTEM_NAME "cl-rsb")
PROJECT(${SYSTEM_NAME})

# Determine major and minor version components from asd file. Run git
# describe to determine remaining patch component.
FUNCTION(EXTRACT_COMPONENT NAME OUTPUT)
    FILE(STRINGS "${CMAKE_SOURCE_DIR}/${SYSTEM_NAME}.asd" TEMP
         REGEX   "def[a-z]+ +\\+version-${NAME}\\+")
    STRING(REGEX REPLACE ".*\\+version-${NAME}\\+ +([0-9]+).*" "\\1"
           ${OUTPUT} ${TEMP})
    SET(${OUTPUT} ${${OUTPUT}} PARENT_SCOPE)
ENDFUNCTION()
EXTRACT_COMPONENT(major CL_RSB_VERSION_MAJOR)
EXTRACT_COMPONENT(minor CL_RSB_VERSION_MINOR)
GIT_BASED_VERSION(${CL_RSB_VERSION_MAJOR} ${CL_RSB_VERSION_MINOR} GIT_DESCRIBE_OUTPUT)
GIT_VERSION_COMPONENTS(${GIT_DESCRIBE_OUTPUT} CL_RSB_VERSION_PATCH)

MESSAGE(STATUS "Project is ${CMAKE_PROJECT_NAME} (${CL_RSB_VERSION_MAJOR}.${CL_RSB_VERSION_MINOR}.${CL_RSB_VERSION_PATCH})")

# Find and check SBCL installation.
SET(SBCL_HOME $ENV{SBCL_HOME})
IF(NOT SBCL_HOME)
    MESSAGE(STATUS "SBCL_HOME is not set. Trying to find SBCL on default path.")
    FIND_PROGRAM(SBCL_EXECUTABLE sbcl)
ELSE()
    MESSAGE(STATUS "SBCL_HOME is set to ${SBCL_HOME}. Trying to find SBCL there.")
    FIND_PROGRAM(SBCL_EXECUTABLE sbcl
                 PATHS "${SBCL_HOME}/../../bin" ${SBCL_HOME}
                 NO_DEFAULT_PATH)
ENDIF()
MESSAGE(STATUS "SBCL executable: ${SBCL_EXECUTABLE}")

# Configure Lisp environment.
SET(LISP_RUNTIME_OPTIONS "" CACHE STRING
    "Runtime options to pass to the Lisp system.")
SET(LISP_INIT_FILE       "" CACHE FILEPATH
    "File to load as Lisp init file.")
SET(LISP_INIT_CODE       "" CACHE STRING
    "Lisp code to be evaulated during initialization.")
SET(LISP_INIT "")
IF(LISP_INIT_FILE)
    SET(LISP_INIT "${LISP_INIT} --userinit ${LISP_INIT_FILE}")
ENDIF()
IF(LISP_INIT_CODE)
    SET(LISP_INIT "${LISP_INIT} --eval \"${LISP_INIT_CODE}\"")
ENDIF()

SET(CL_SOURCE_REGISTRY       "(:source-registry (:tree \\\"${CMAKE_CURRENT_SOURCE_DIR}\\\") :inherit-configuration)")
SET(ASDF_OUTPUT_TRANSLATIONS "(:output-translations (t (\\\"${CMAKE_CURRENT_BINARY_DIR}/fasl-cache\\\" :implementation)) :ignore-inherited-configuration)")

# Tests
ENABLE_TESTING()

SET(DO "--eval \"(proclaim (quote (optimize (debug 3) (safety 3) (speed 0))))\"
        --eval \"(asdf:test-system :${SYSTEM_NAME})\"
        --quit")
IF(NOT CMAKE_SYSTEM_NAME STREQUAL "Windows")
    SET(REDIRECTIONS "OUTPUT_FILE \"${CMAKE_CURRENT_BINARY_DIR}/test.log\"
                      ERROR_FILE  \"${CMAKE_CURRENT_BINARY_DIR}/test.log\"")
ENDIF()
CONFIGURE_FILE("${CMAKE_CURRENT_SOURCE_DIR}/sbcl.cmake.in"
               "${CMAKE_CURRENT_BINARY_DIR}/sbcl.test.cmake"
               @ONLY)
ADD_TEST(NAME    "rsb"
         COMMAND "${CMAKE_COMMAND}" -P "${CMAKE_CURRENT_BINARY_DIR}/sbcl.test.cmake")

# Installation
FILE(GLOB ASD_FILES
     RELATIVE "${CMAKE_CURRENT_SOURCE_DIR}"
     "*.asd")
INSTALL(DIRECTORY      "${CMAKE_CURRENT_SOURCE_DIR}/src"
        DESTINATION    "share/common-lisp/source/${SYSTEM_NAME}"
        FILES_MATCHING PATTERN "*.lisp"
        PATTERN        ".svn" EXCLUDE)
INSTALL(DIRECTORY      "${CMAKE_CURRENT_SOURCE_DIR}/test"
        DESTINATION    "share/common-lisp/source/${SYSTEM_NAME}"
        FILES_MATCHING PATTERN "*.lisp"
        PATTERN        ".svn" EXCLUDE)
INSTALL(DIRECTORY      "${CMAKE_CURRENT_SOURCE_DIR}/data"
        DESTINATION    "share/common-lisp/source/${SYSTEM_NAME}"
        FILES_MATCHING
        PATTERN        "*.proto"
        PATTERN        ".svn" EXCLUDE)
INSTALL(DIRECTORY
        DESTINATION "share/common-lisp/source/${SYSTEM_NAME}")
INSTALL(FILES       ${ASD_FILES}
        DESTINATION "share/common-lisp/source/${SYSTEM_NAME}/")
INSTALL(DIRECTORY
        DESTINATION "share/common-lisp/systems")
INSTALL(FILES       ${ASD_FILES}
        DESTINATION "share/common-lisp/systems/")

# Packaging
SET(PACKAGE_BASE_NAME           "${SYSTEM_NAME}")
SET(CPACK_PACKAGE_VERSION_MAJOR ${CL_RSB_VERSION_MAJOR})
SET(CPACK_PACKAGE_VERSION_MINOR ${CL_RSB_VERSION_MINOR})
SET(CPACK_PACKAGE_VERSION_PATCH ${CL_RSB_VERSION_PATCH})
SET(CPACK_PACKAGE_VERSION       "${CPACK_PACKAGE_VERSION_MAJOR}.${CPACK_PACKAGE_VERSION_MINOR}.${CPACK_PACKAGE_VERSION_PATCH}")
SET(CPACK_PACKAGE_VENDOR        "CoR-Lab, Bielefeld University")
SET(CPACK_PACKAGE_FILE_NAME     "${PACKAGE_BASE_NAME}-${CPACK_PACKAGE_VERSION}")
SET(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_SOURCE_DIR}/COPYING")

INCLUDE("${CMAKE_SOURCE_DIR}/cpack/CPackInclude.cmake")
