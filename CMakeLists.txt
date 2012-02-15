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

# Project definition (version is grovelled from asd system definition)
SET(SYSTEM_NAME "cl-rsb")
PROJECT(${SYSTEM_NAME})
MACRO(EXTRACT_COMPONENT NAME OUTPUT)
    FILE(STRINGS "${CMAKE_SOURCE_DIR}/${SYSTEM_NAME}.asd" TEMP
         REGEX   "defconstant +\\+version-${NAME}\\+")
    STRING(REGEX REPLACE ".*\\+version-${NAME}\\+ +([0-9]+).*" "\\1"
           CL_RSB_VERSION_${OUTPUT} ${TEMP})
ENDMACRO()
EXTRACT_COMPONENT(major    MAJOR)
EXTRACT_COMPONENT(minor    MINOR)
EXTRACT_COMPONENT(revision PATCH)

# Installation
FILE(GLOB ASD_FILES
     RELATIVE "${CMAKE_CURRENT_SOURCE_DIR}"
     "*.asd")
INSTALL(DIRECTORY      "${CMAKE_CURRENT_SOURCE_DIR}/src"
        DESTINATION    "share/common-lisp/source/${SYSTEM_NAME}"
        FILES_MATCHING PATTERN "*.lisp")
INSTALL(DIRECTORY      "${CMAKE_CURRENT_SOURCE_DIR}/test"
        DESTINATION    "share/common-lisp/source/${SYSTEM_NAME}"
        FILES_MATCHING PATTERN "*.lisp")
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

IF(NOT CPACK_GENERATOR)
    IF(CMAKE_SYSTEM_NAME STREQUAL "Linux")
        LIST(APPEND CPACK_GENERATOR "DEB")
    ENDIF()
ENDIF()

IF(CMAKE_SYSTEM_NAME STREQUAL "Linux")

    # Generate postinst and prerm hooks
    SET(POSTINST_SCRIPT "${CMAKE_CURRENT_BINARY_DIR}/postinst")
    SET(PRERM_SCRIPT    "${CMAKE_CURRENT_BINARY_DIR}/prerm")
    FILE(WRITE "${POSTINST_SCRIPT}"
               "#!/bin/sh\n\n                                              \\
                set -e\n                                                   \\
                if [ \"$1\" = \"configure\" ] &&                           \\
                     which register-common-lisp-source > /dev/null; then\n \\
                  register-common-lisp-source \"cl-rsb\"\n                 \\
                fi\n\n")
    FILE(WRITE "${PRERM_SCRIPT}"
               "#!/bin/sh\n\n                                                \\
                set -e\n                                                     \\
                if [ \"$1\" = \"remove\" ]                                   \\
                     || [ \"$1\" = \"upgrade\" ]                             \\
                     || [ \"$1\" = \"deconfigure\" ]; then\n                 \\
                  if which unregister-common-lisp-source > /dev/null; then\n \\
                    unregister-common-lisp-source \"cl-alexandria\"\n        \\
                  fi\n                                                       \\
                fi\n\n")
    EXECUTE_PROCESS(COMMAND chmod +x "${POSTINST_SCRIPT}" "${PRERM_SCRIPT}")

    SET(CPACK_DEBIAN_PACKAGE_NAME          "${PACKAGE_BASE_NAME}")
    SET(CPACK_DEBIAN_PACKAGE_VERSION       "${CPACK_PACKAGE_VERSION}")
    SET(CPACK_DEBIAN_PACKAGE_MAINTAINER    "Jan Moringen (jmoringe@techfak.uni-bielefeld.de)")
    SET(CPACK_DEBIAN_PACKAGE_DESCRIPTION   "Robotics Service Bus (Common Lisp implementation)")
    SET(CPACK_DEBIAN_PACKAGE_PRIORITY      "optional")
    SET(CPACK_DEBIAN_PACKAGE_SECTION       "lisp")
    SET(CPACK_DEBIAN_PACKAGE_ARCHITECTURE  "all")
    SET(CPACK_DEBIAN_PACKAGE_DEPENDS       "common-lisp-controller (>= 5.11)")

    SET(CPACK_DEBIAN_PACKAGE_CONTROL_EXTRA "${POSTINST_SCRIPT};${PRERM_SCRIPT}")

    MESSAGE(STATUS "Debian Package: ${CPACK_DEBIAN_PACKAGE_NAME} (${CPACK_DEBIAN_PACKAGE_VERSION}) [${CPACK_PACKAGE_FILE_NAME}.deb]")
ENDIF()

INCLUDE(CPack)
