#!/usr/bin/env bash
# This is a script to replace original rec packages with the ones suited for CXXR.

# List of recommended packages
#REC_PKGS = boot class cluster codetools foreign KernSmooth lattice MASS Matrix mgcv nlme nnet rpart spatial survival
REC_PKGS="survival"

# Navigate to the recommended library directory
cd src/library/Recommended || exit

# Loop through each package
for pkg in ${REC_PKGS}; do
    echo "Replacing ${pkg}"

    # Extract the existing version number from the tarball
    existing_version=$(ls ${pkg}_*.tar.gz | sed -E 's/.*_([0-9.-]+)\.tar\.gz/\1/')

    # Download the latest package from GitHub
    curl -L "http://github.com/lukaszdaniel/${pkg}/archive/master.zip" --output "${pkg}.zip"

    # Unzip the downloaded file
    unzip "${pkg}.zip"
    rm "${pkg}.zip"
    
    # Rename the unzipped folder to match the package name
    mv "${pkg}-master" "${pkg}"

    # Create MD5 file
    cd "${pkg}"
    find -type f ! -name 'MD5' -print0 | xargs -0 md5sum | sed -e 's/ .\//*/' > MD5
    cd ..

    # Create a new tarball with the existing version number
    tar -czf "${pkg}_${existing_version}.tar.gz" "${pkg}"
    rm -r "${pkg}"
done

# Create symbolic links for all tar.gz files
echo "Creating links"
for i in *.tar.gz; do
    ln -s "$i" "${i//_*/}.tgz"
done
