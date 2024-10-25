#!/usr/bin/env bash
# This is a script to replace original rec packages with the ones suited for CXXR.

# List of recommended packages
#REC_PKGS = "boot class cluster codetools foreign KernSmooth lattice MASS Matrix mgcv nlme nnet rpart spatial survival"
REC_PKGS="boot cluster foreign lattice MASS Matrix mgcv nlme spatial survival"

# Navigate to the recommended library directory
cd src/library/Recommended || exit

# Loop through each package
for pkg in ${REC_PKGS}; do
    echo "Replacing ${pkg}"

    # Extract the existing version number from the tarball
    existing_version=$(ls ${pkg}_*.tar.gz | sed -E 's/.*_([0-9.-]+)\.tar\.gz/\1/')

    # Download the latest package from GitHub
    wget -nv -O "${pkg}.zip" "http://github.com/lukaszdaniel/${pkg}/archive/master.zip"

    # Unzip the downloaded file
    echo "Unpacking ${pkg} zip file"
    unzip -q "${pkg}.zip"
    rm "${pkg}.zip"
    
    # Rename the unzipped folder to match the package name
    echo "Renaming ${pkg}-master to ${pkg}"
    mv "${pkg}-master" "${pkg}"

    # Update DESCRIPTION file
    echo "Updating DESCRIPTION for ${pkg}"
    tar --extract --file="${pkg}_${existing_version}.tar.gz" "${pkg}"/DESCRIPTION

    # Create MD5 file
    echo "Creating MD5 for ${pkg}"
    cd "${pkg}"
    if [ "$(uname)" == "Darwin" ]; then
        find . -type f ! -name 'MD5' -print0 | xargs -0 md5 | sed -e 's/ .\//*/' > MD5
    else
        find . -type f ! -name 'MD5' -print0 | xargs -0 md5sum | sed -e 's/ .\//*/' > MD5
    fi
    cd ..

    # Create a new tarball with the existing version number
    rm "${pkg}_${existing_version}.tar.gz"
    tar --exclude-vcs -czf "${pkg}_${existing_version}.tar.gz" "${pkg}"
    rm -r "${pkg}"
done

# Create symbolic links for all tar.gz files
echo "Creating links"
for i in *.tar.gz; do
    ln -sf "$i" "${i//_*/}.tgz"
done
