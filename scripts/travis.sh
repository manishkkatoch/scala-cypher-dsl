#!/bin/bash

echo "running checks..."
./gradlew check

if [[ -z $TRAVIS_TAG ]]; then
    echo "build is not for a tag, that's it then."
else
    VERSION=$(echo $TRAVIS_TAG | cut -d 'v' -f 2)
    echo "the build version will be $VERSION"
    echo "uploading archives..."
    BUILD_VERSION=$VERSION ./gradlew uploadArchives
    echo "upload done."
fi