#!/usr/bin/env bash
set -euo pipefail

if [ "$#" -ne 2 ]; then
    echo "Usage: $0 <linux|windows-cross> <3.4|5>" >&2
    exit 2
fi

target_platform="$1"
eigen_lane="$2"

case "$target_platform" in
    linux)
        triplet="x64-linux"
        preset="release"
        build_dir="build/linux-Release-eigen${eigen_lane}"
        apt_packages="build-essential cmake ninja-build curl zip unzip tar pkg-config git gfortran jq"
        unsupported_args=()
        ;;
    windows-cross)
        triplet="x64-mingw-static"
        preset="windows-cross-release"
        build_dir="build/windows-cross-Release-eigen${eigen_lane}"
        apt_packages="build-essential cmake ninja-build curl zip unzip tar pkg-config git jq mingw-w64 g++-mingw-w64-x86-64 gcc-mingw-w64-x86-64"
        unsupported_args=(--allow-unsupported)
        ;;
    *)
        echo "Unknown target platform: $target_platform" >&2
        exit 2
        ;;
esac

case "$eigen_lane" in
    3.4)
        baseline="5bf0c55239da398b8c6f450818c9e28d36bf9966"
        eigen_dependency='{"name":"eigen3"}'
        eigen_override='{"name":"eigen3","version":"3.4.1","port-version":1}'
        ;;
    5)
        baseline="6b07d2d37301e9e7c6fcf771536d2ff6585c5912"
        eigen_dependency='{"name":"eigen3","version>=":"5.0.1"}'
        eigen_override='{"name":"eigen3","version":"5.0.1"}'
        ;;
    *)
        echo "Unknown Eigen lane: $eigen_lane" >&2
        exit 2
        ;;
esac

apt-get update
apt-get install -y ${apt_packages}

export VCPKG_ROOT="${BITBUCKET_CLONE_DIR}/vcpkg"
export VCPKG_BUILD_TYPE=release
export VCPKG_OVERLAY_PORTS="${BITBUCKET_CLONE_DIR}/scripts/ci/vcpkg-overlay-ports"

lane_name="${target_platform}-eigen${eigen_lane}"
manifest_dir="${BITBUCKET_CLONE_DIR}/.ci-vcpkg-manifests/${lane_name}"
install_root="${BITBUCKET_CLONE_DIR}/vcpkg_installed/${lane_name}"
binary_cache="${BITBUCKET_CLONE_DIR}/.vcpkg-cache/${lane_name}"

export VCPKG_BINARY_SOURCES="clear;files,${binary_cache},readwrite"

mkdir -p "$manifest_dir" "$install_root" "$binary_cache"

if [ ! -f "$VCPKG_ROOT/bootstrap-vcpkg.sh" ]; then
    rm -rf "$VCPKG_ROOT"
    git clone https://github.com/Microsoft/vcpkg.git "$VCPKG_ROOT"
fi

if [ ! -x "$VCPKG_ROOT/vcpkg" ]; then
    "$VCPKG_ROOT/bootstrap-vcpkg.sh" -disableMetrics
fi

jq \
    --arg baseline "$baseline" \
    --argjson eigen_dependency "$eigen_dependency" \
    --argjson eigen_override "$eigen_override" \
    '
    del(."builtin-baseline", .overrides)
    | .dependencies |= map(
        if . == "eigen3" then $eigen_dependency
        elif (type == "object" and .name == "eigen3") then $eigen_dependency
        else .
        end
      )
    | .["builtin-baseline"] = $baseline
    | .overrides = [$eigen_override]
    ' \
    "${BITBUCKET_CLONE_DIR}/vcpkg.json" > "${manifest_dir}/vcpkg.json"

"$VCPKG_ROOT/vcpkg" install \
    --triplet "$triplet" \
    --x-manifest-root="$manifest_dir" \
    --x-install-root="$install_root" \
    --overlay-ports="$VCPKG_OVERLAY_PORTS" \
    "${unsupported_args[@]}" \
    --clean-after-build

cd "${BITBUCKET_CLONE_DIR}/src"
rm -rf "$build_dir"
cmake --preset "$preset" \
    -DVCPKG_MANIFEST_DIR="$manifest_dir" \
    -DVCPKG_INSTALLED_DIR="$install_root" \
    -DVCPKG_OVERLAY_PORTS="$VCPKG_OVERLAY_PORTS" \
    -DEigen3_DIR="${install_root}/${triplet}/share/eigen3" \
    -B "$build_dir"
cmake --build "$build_dir" --parallel 8
