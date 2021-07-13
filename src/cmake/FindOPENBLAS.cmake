SET(Open_BLAS_INCLUDE_SEARCH_PATHS
    /opt
    /opt/OpenBLAS
    /opt/OpenBLAS/include
    /opt/OpenBLAS/lib
)

SET(Open_BLAS_LIB_SEARCH_PATHS
    /opt/OpenBLAS/lib
    /lib
    /lib64
    /usr/lib
    /usr/lib64
    /usr/local/lib
    /usr/local/lib64
)

FIND_PATH(OpenBLAS_INCLUDE_DIR NAMES 
    cblas.h PATHS ${Open_BLAS_INCLUDE_SEARCH_PATHS}
)

FIND_LIBRARY(OpenBLAS_LIB NAMES 
    openblas PATHS ${Open_BLAS_LIB_SEARCH_PATHS}
)

message("OpenBLAS_INCLUDE_DIR: ${OpenBLAS_INCLUDE_DIR}, OpenBLAS_LIB: ${OpenBLAS_LIB}")
