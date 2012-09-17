SET(CPACK_GENERATOR "DEB")

SET(CPACK_DEBIAN_PACKAGE_NAME         "${PACKAGE_BASE_NAME}")
SET(CPACK_DEBIAN_PACKAGE_VERSION      "${CPACK_PACKAGE_VERSION}${CPACK_PACKAGE_REVISION}")
SET(CPACK_DEBIAN_PACKAGE_MAINTAINER   "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>")
SET(CPACK_DEBIAN_PACKAGE_DESCRIPTION  "Robotics Service Bus (Common Lisp implementation)
 Common Lisp system implementing the Robotics Service Bus (RSB), a
 lightweight, extensible, event-driven middleware for robotic systems
 and other domains.")
SET(CPACK_DEBIAN_PACKAGE_PRIORITY     "optional")
SET(CPACK_DEBIAN_PACKAGE_SECTION      "lisp")
SET(CPACK_DEBIAN_PACKAGE_ARCHITECTURE "all")
SET(CPACK_DEBIAN_PACKAGE_DEPENDS      "common-lisp-controller (>= 5.11)")

# Generate postinst and prerm hooks.
SET(POSTINST_SCRIPT "${CMAKE_CURRENT_BINARY_DIR}/postinst")
SET(PRERM_SCRIPT    "${CMAKE_CURRENT_BINARY_DIR}/prerm")
FILE(WRITE "${POSTINST_SCRIPT}"
           "#!/bin/sh\n\n                                              \\
            set -e\n                                                   \\
            if [ \"$1\" = \"configure\" ] &&                           \\
                 which register-common-lisp-source > /dev/null; then\n \\
              register-common-lisp-source \"${SYSTEM_NAME}\"\n         \\
            fi\n\n")
FILE(WRITE "${PRERM_SCRIPT}"
           "#!/bin/sh\n\n                                                \\
            set -e\n                                                     \\
            if [ \"$1\" = \"remove\" ]                                   \\
                 || [ \"$1\" = \"upgrade\" ]                             \\
                 || [ \"$1\" = \"deconfigure\" ]; then\n                 \\
              if which unregister-common-lisp-source > /dev/null; then\n \\
                unregister-common-lisp-source \"${SYSTEM_NAME}\"\n       \\
              fi\n                                                       \\
            fi\n\n")
EXECUTE_PROCESS(COMMAND chmod 755 "${POSTINST_SCRIPT}" "${PRERM_SCRIPT}")
SET(CPACK_DEBIAN_PACKAGE_CONTROL_EXTRA "${POSTINST_SCRIPT};${PRERM_SCRIPT}")

# Generate required change log files.
FIND_PROGRAM(LSB_EXECUTABLE "lsb_release")
EXECUTE_PROCESS(COMMAND ${LSB_EXECUTABLE} --short --codename
                OUTPUT_VARIABLE LSB_CODENAME
                OUTPUT_STRIP_TRAILING_WHITESPACE)

EXECUTE_PROCESS(COMMAND ${GIT_EXECUTABLE}
                        log "--format=%ad  %an  <%ae>%n%n%w(76,8,10)%s%w(76,8,8)%n%n%b%n"
                        --date=short
                COMMAND gzip -9
                OUTPUT_FILE "${CMAKE_BINARY_DIR}/changelog.gz")
EXECUTE_PROCESS(COMMAND sh -c "echo -n \"sed -e '\" ; for c in $(${GIT_EXECUTABLE} rev-list --all -- \"${CMAKE_CURRENT_LIST_FILE}\") ; do echo -n \"s/$c/$(${GIT_EXECUTABLE} describe --tags $c | sed -re s/[^0-9]*\\([0-9]+\\)\\.\\([0-9]+\\)-\\([0-9]+\\)-.*/\\\\1.\\'\\$\\(\\(\\\\2+1\\)\\)\\'.\\\\3/)/\\;\" ; done ; echo \"'\""
                OUTPUT_VARIABLE COMMIT_TO_VERSION_SED_RULES)
EXECUTE_PROCESS(COMMAND ${GIT_EXECUTABLE}
                        log "--format=${CPACK_DEBIAN_PACKAGE_NAME} (%H) ${LSB_CODENAME}; urgency=low%n%n%w(76,8,10)%s%w(76,8,8)%n%n%b%n%n%w(200,1,1)-- %an <%ae>  %ad%n"
                        --date=rfc
                        -- "${CMAKE_CURRENT_LIST_FILE}"
                COMMAND sh -c ${COMMIT_TO_VERSION_SED_RULES}
                COMMAND gzip -9
                OUTPUT_FILE "${CMAKE_BINARY_DIR}/changelog.Debian.gz")
INSTALL(FILES "${CMAKE_BINARY_DIR}/changelog.gz"
              "${CMAKE_BINARY_DIR}/changelog.Debian.gz"
        DESTINATION "share/doc/${CPACK_DEBIAN_PACKAGE_NAME}")

# Install copyright file.
INSTALL(FILES "${CMAKE_SOURCE_DIR}/COPYING"
        DESTINATION "share/doc/${CPACK_DEBIAN_PACKAGE_NAME}"
        RENAME      copyright)

SET(CPACK_PACKAGE_FILE_NAME "${CPACK_DEBIAN_PACKAGE_NAME}-${CPACK_DEBIAN_PACKAGE_VERSION}_${CPACK_DEBIAN_PACKAGE_ARCHITECTURE}")

MESSAGE(STATUS "Debian Package: ${CPACK_DEBIAN_PACKAGE_NAME} (${CPACK_DEBIAN_PACKAGE_VERSION}) [${CPACK_PACKAGE_FILE_NAME}.deb]")
