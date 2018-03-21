#!/usr/bin/env bash

# This is a brutual hack to dig into the seemingly arbitrary directory
# structures produced by ghc and cabal.

die() {
    echo "$1" >&2
    exit 1
}

mydir="$(cd "$(dirname "$0")" && pwd)"

: ${GHC:=/usr/local/opt/ghc/}
: ${COMPILER:=$mydir/../compiler/dist/}

if [ ! -d $GHC ]; then
    die "Cannot find GHC: searched in '$GHC'"
fi

if [ ! -d "$COMPILER" ]; then
    die "Cannot find built compiler: searched in '$COMPILER'"
fi

case $(uname) in
    Linux)
        so_suffix=".so"
        ;;
    Darwin)
        so_suffix=".dylib"
        ;;
esac


includepaths=()
libs=()
rpaths=()
fixup_commands=()

findHeader() {
    local root="$1"
    local header="$2"

    local result="$(find -L "$root" -name "$header")"
    if [ ! -r "$result" ]; then
        die "Failed to find header: $header in $root (or found more than one)"
    fi
    includepaths+=("$(dirname "$result")")
}

findLib() {
    local root="$1"
    local pattern="$2"

    local result="$(find -L "$root" -name "$pattern")"
    if [ ! -r "$result" ]; then
        die "Failed to find lib: $pattern in $root (or found more than one)"
    fi

    local lib_dirname="$(dirname "$result")"
    local lib_basename="$(basename "$result")"

    local lib_name=$lib_basename
    lib_name="${lib_name%${so_suffix}}"
    lib_name="${lib_name#lib}"

    rpaths+=("$lib_dirname")
    libs+=("-L$lib_dirname" "-l$lib_name")
}

findLib    "$GHC" "libHSrts-ghc*${so_suffix}"
findLib    "$GHC" "libffi${so_suffix}"
findHeader "$GHC" "HsFFI.h"
findLib    "$COMPILER" "libHSCompiler-*${so_suffix}"
findHeader "$COMPILER" "LibKeyc_stub.h"

echo "LIBS += \\"
for lib in "${libs[@]}"; do
    echo -e "\t$lib \\"
done
echo

echo "INCLUDEPATH += \\"
for includepath in "${includepaths[@]}"; do
   echo -e "\t$includepath \\"
done
echo

echo "QMAKE_LFLAGS += \\"
for rpath in "${rpaths[@]}"; do
    echo -e "\t-Wl,-rpath,$rpath \\"
done
echo

echo "QMAKE_POST_LINK += \\"
for fixup in "${fixup_commands[@]}"; do
    echo -e "\t$fixup \$(TARGET) ; \\"
done
echo
