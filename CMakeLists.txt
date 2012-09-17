# CMakeLists.txt --- CMake build description file.
#
# Copyright (C) 2011, 2012 Jan Moringen
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
MACRO(GIT_VERSION_COMPONENTS INPUT OUT_MAJOR OUT_MINOR OUT_REVISION OUT_COMMIT)
    SET(REGEX "^[-.a-z0-9]*([0-9]+)\\.([0-9]+)[-.]([0-9]+)-?(g[a-f0-9]+(:?-dirty)?|)$")
    STRING(REGEX REPLACE ${REGEX} "\\1" ${OUT_MAJOR}    "${INPUT}")
    STRING(REGEX REPLACE ${REGEX} "\\2" ${OUT_MINOR}    "${INPUT}")
    STRING(REGEX REPLACE ${REGEX} "\\3" ${OUT_REVISION} "${INPUT}")
    STRING(REGEX REPLACE ${REGEX} "\\4" ${OUT_COMMIT}   "${INPUT}")
ENDMACRO()

# Git-based version.
FIND_PROGRAM(GIT_EXECUTABLE "git")
IF(GIT_EXECUTABLE)
    EXECUTE_PROCESS(COMMAND ${GIT_EXECUTABLE} describe --long --dirty=-dirty
                    RESULT_VARIABLE RESULT
                    OUTPUT_VARIABLE GIT_DESCRIBE_OUTPUT
                    OUTPUT_STRIP_TRAILING_WHITESPACE
                    ERROR_QUIET)
    IF(RESULT EQUAL 0)
        GIT_VERSION_COMPONENTS(${GIT_DESCRIBE_OUTPUT} IGN1 IGN2 REVISION COMMIT)
        FILE(WRITE "${CMAKE_SOURCE_DIR}/version.sexp" "${REVISION} \"${COMMIT}\"")
    ELSE()
        MESSAGE(STATUS "Not a git repository: ${CMAKE_SOURCE_DIR}")
    ENDIF()
ELSE()
    MESSAGE(STATUS "git executable not found")
ENDIF()

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

# Execute Lisp process
MACRO(CONFIGURE_LISP NAME CODE)
    SET(NAME "${NAME}")
    SET(DO   "${CODE} --quit")
    CONFIGURE_FILE("${CMAKE_CURRENT_SOURCE_DIR}/sbcl.cmake.in"
                   "${CMAKE_CURRENT_BINARY_DIR}/sbcl.${NAME}.cmake"
                   @ONLY)
    SET(REDIRECTIONS)
ENDMACRO()
MACRO(CONFIGURE_LISP_REDIRECT NAME CODE)
    SET(REDIRECTIONS "OUTPUT_FILE \"${CMAKE_CURRENT_BINARY_DIR}/${NAME}.out\"
                      ERROR_FILE  \"${CMAKE_CURRENT_BINARY_DIR}/${NAME}.err\"")
    CONFIGURE_LISP(${NAME} ${CODE})
ENDMACRO()
MACRO(EXECUTE_LISP NAME CODE OUTPUT_VARIABLE)
    CONFIGURE_LISP(${NAME} "${CODE}")
    MESSAGE(STATUS "Executing Lisp process \"${NAME}\"")
    EXECUTE_PROCESS(COMMAND cmake -P "${CMAKE_CURRENT_BINARY_DIR}/sbcl.${NAME}.cmake"
                    OUTPUT_VARIABLE ${OUTPUT_VARIABLE}
                    OUTPUT_STRIP_TRAILING_WHITESPACE)
ENDMACRO()

# Project definition (version is grovelled from asd system definition
# and, possibly in combination with version.sexp file written above).
SET(SYSTEM_NAME "cl-rsb")
PROJECT(${SYSTEM_NAME})
SET(REDIRECTIONS "ERROR_QUIET")
EXECUTE_LISP("version"
             "--eval \"(asdf:find-system :${SYSTEM_NAME})\"
              --eval \"(princ (${SYSTEM_NAME}-system:version/string :commit? t))\""
             VERSION_FROM_LISP)
GIT_VERSION_COMPONENTS(${VERSION_FROM_LISP} RSB_VERSION_MAJOR
                                            RSB_VERSION_MINOR
                                            RSB_VERSION_PATCH
                                            RSB_VERSION_COMMIT)
MESSAGE(STATUS "Project is ${CMAKE_PROJECT_NAME} (${VERSION_FROM_LISP})")

# Tests
ENABLE_TESTING()

CONFIGURE_LISP_REDIRECT(test
                        "--eval \"(proclaim (quote (optimize (debug 3) (safety 3) (speed 0))))\"
                         --eval \"(asdf:test-system :${SYSTEM_NAME})\"")
ADD_TEST(NAME    "rsb"
         COMMAND "${CMAKE_COMMAND}" -P "${CMAKE_CURRENT_BINARY_DIR}/sbcl.test.cmake")

# Installation
FILE(GLOB ASD_FILES
     RELATIVE "${CMAKE_CURRENT_SOURCE_DIR}"
     "*.asd")
FILE(GLOB SEXP_FILES
     RELATIVE "${CMAKE_CURRENT_SOURCE_DIR}"
     "*.sexp")
INSTALL(DIRECTORY      "${CMAKE_CURRENT_SOURCE_DIR}/src"
        DESTINATION    "share/common-lisp/source/${CMAKE_PROJECT_NAME}"
        FILES_MATCHING PATTERN "*.lisp")
INSTALL(DIRECTORY      "${CMAKE_CURRENT_SOURCE_DIR}/test"
        DESTINATION    "share/common-lisp/source/${CMAKE_PROJECT_NAME}"
        FILES_MATCHING PATTERN "*.lisp")
INSTALL(DIRECTORY      "${CMAKE_CURRENT_SOURCE_DIR}/data"
        DESTINATION    "share/common-lisp/source/${CMAKE_PROJECT_NAME}"
        FILES_MATCHING PATTERN "*.proto")
INSTALL(DIRECTORY
        DESTINATION "share/common-lisp/source/${CMAKE_PROJECT_NAME}")
INSTALL(FILES       ${ASD_FILES} ${SEXP_FILES}
        DESTINATION "share/common-lisp/source/${CMAKE_PROJECT_NAME}/")

# Packaging
SET(PACKAGE_BASE_NAME           "${CMAKE_PROJECT_NAME}")
SET(CPACK_PACKAGE_VERSION_MAJOR ${RSB_VERSION_MAJOR})
SET(CPACK_PACKAGE_VERSION_MINOR ${RSB_VERSION_MINOR})
SET(CPACK_PACKAGE_VERSION_PATCH ${RSB_VERSION_PATCH})
SET(CPACK_PACKAGE_VERSION       "${CPACK_PACKAGE_VERSION_MAJOR}.${CPACK_PACKAGE_VERSION_MINOR}.${CPACK_PACKAGE_VERSION_PATCH}")
SET(CPACK_PACKAGE_VENDOR        "CoR-Lab, Bielefeld University")
SET(CPACK_PACKAGE_FILE_NAME     "${PACKAGE_BASE_NAME}-${CPACK_PACKAGE_VERSION}")
SET(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_SOURCE_DIR}/COPYING")

INCLUDE("${CMAKE_SOURCE_DIR}/cpack/CPackInclude.cmake")
