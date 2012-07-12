IF(NOT CPACK_GENERATOR)
    SET(CPACK_GENERATOR "TGZ")
ENDIF()

SET(CPACK_CONFIG_FILE "" CACHE FILEPATH "Path to a CMake lists syntax file providing settings for CPack.")
SET(CPACK_PACKAGE_REVISION "" CACHE STRING "A suffix string which can be appended to package versions to account for e. g. multiple rebuilds without changes to the upstream project of the package.")

IF(CPACK_CONFIG_FILE)
    INCLUDE(${CPACK_CONFIG_FILE})
ENDIF()

MESSAGE(STATUS "Using CPack package generator: ${CPACK_GENERATOR}")

INCLUDE(CPack)
