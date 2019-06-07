#!/usr/bin/env sh
set -e
echo "running for branch $TRAVIS_BRANCH"

if [ "$TRAVIS_BRANCH" == "master" ] || [ "$TRAVIS_BRANCH" == "develop" ]
 then
    echo "running checks"
	./gradlew clean check
else
    echo "Sign, Upload archives to local repo, Upload archives to Sonatype, Close and release repository."
    ./gradlew uploadArchives
fi;