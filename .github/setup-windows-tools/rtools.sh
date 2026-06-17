#!/bin/sh
set -e
set -x

# Use the latest stable version of Rtools
if [ "$( uname -s | grep ARM)" ]; then
url="https://github.com/r-windows/rtools-chocolatey/releases/download/6768/rtools45-toolchain-libs-base-aarch64-6768.tar.zst"
else
url="https://github.com/r-windows/rtools-chocolatey/releases/download/6768/rtools45-toolchain-libs-base-6768.tar.zst"
fi

# Download and extract
curl --retry 3 --fail-with-body -sSL $url -o rtools.tar.zst
mkdir -p /c/rtools-base-toolchain
tar xf rtools.tar.zst -C /c/rtools-base-toolchain --strip-components=1
rm -f rtools.tar.zst
