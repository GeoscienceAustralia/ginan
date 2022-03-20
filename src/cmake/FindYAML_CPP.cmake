#set(YAML_CPP_USE_STATIC_LIBS ON)

SET(YAML_CPP_LIB_SEARCH_PATHS
    /usr/local/lib64
    /usr/lib64
    /lib64
    /lib
    /usr/lib
    /usr/local/lib
)


FIND_LIBRARY(YAML_CPP_LIB NAMES 
	yaml-cpp PATHS ${YAML_CPP_LIB_SEARCH_PATHS}
)

