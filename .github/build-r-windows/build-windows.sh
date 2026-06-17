#!/bin/sh
# Run script safely and emit some verbose output
set -e
set -x

# Put some things on the path from the setup-tooling action
export PATH="/c/rtools-base-toolchain/bin:/c/Users/$USER/AppData/Roaming/TinyTeX/bin/windows:$PATH"

# Check we have the required tools
pdflatex --version
texindex --version
texi2any --version
make --version
perl --version

# Build arm64 without sub-arch, see comment in MkRules.rules
if [ "$( uname -s | grep ARM)" ]; then
tcltk="https://github.com/r-windows/rtools-chocolatey/releases/download/6768/tcltk-aarch64-6768-6663.zip"
echo "USE_LLVM=1" >> src/gnuwin32/MkRules.local
echo "WIN=" >> src/gnuwin32/MkRules.local
clang --version
else
tcltk="https://github.com/r-windows/rtools-chocolatey/releases/download/6768/tcltk-6768-6663.zip"
gcc --version
fi

# Get 3rd party files needed for R on Windows
curl --retry 3 --fail-with-body -sSL https://curl.se/ca/cacert.pem -o etc/curl-ca-bundle.crt
curl --retry 3 --fail-with-body -sSL $tcltk -o tcltk.zip
unzip -q tcltk.zip && rm -f tcltk.zip

# Build everything including installer
cd src/gnuwin32
make -j4 distribution
