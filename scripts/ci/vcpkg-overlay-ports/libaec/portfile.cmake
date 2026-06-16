vcpkg_download_distfile(ARCHIVE
    URLS "https://github.com/Deutsches-Klimarechenzentrum/libaec/archive/refs/tags/v1.1.3.tar.gz"
    FILENAME "deutsches-klimarechenzentrum-libaec-v1.1.3.tar.gz"
    SHA512 b64d10f8dd1f8d4c08dcbb5025550c790b01c9138714131456632e37cb58b60f40a94015644db727489fb0365dfc1e7ef0494f890639c8f306f2c90c09299136
)

vcpkg_extract_source_archive(SOURCE_PATH
    ARCHIVE "${ARCHIVE}"
    PATCHES
        static-shared.patch
        cmake-config.patch
)

vcpkg_cmake_configure(
    SOURCE_PATH "${SOURCE_PATH}"
    OPTIONS
        -DBUILD_TESTING=OFF
)
vcpkg_cmake_install()
vcpkg_copy_pdbs()
vcpkg_cmake_config_fixup(CONFIG_PATH "cmake")

file(REMOVE_RECURSE "${CURRENT_PACKAGES_DIR}/debug/include")

file(INSTALL "${CURRENT_PORT_DIR}/usage" DESTINATION "${CURRENT_PACKAGES_DIR}/share/${PORT}")
vcpkg_install_copyright(FILE_LIST "${SOURCE_PATH}/LICENSE.txt")
