#!/bin/sh

set -e

SBT=${SBT:-`which sbt`}
SBT_COMMANDS=${SBT_COMMANDS:-"compile quick:test"}
COMMIT=${GIT_COMMIT:-master}

usage () {
  echo "Usage: $0 [YY MM DD]" 
  echo "If no parameters are provided than a snapshot release will be created."
  exit 1
}

if [ "$1" = "-h" ]; then
  usage 0
fi

if [ $# -eq 3 ]; then 
  VERSION="$1.$2.$3"
else if [ $# -ne 0 ]; then
  usage 1
fi;fi
  

DEV_URL=https://bitbucket.org/effective/acumen-dev.git

# prep clone
git clone $DEV_URL acumen-rel-working
cd acumen-rel-working 
git checkout $COMMIT

error () {
  echo "Error: $1" 1>&2
  exit 1
}
# add git version to release strings
DATE="`date +%y.%m.%d`"
HASH=`git rev-parse --verify HEAD | cut -b1-8`
REL=${VERSION:-$DATE.$HASH}
REV=$(expr `date +%s` - `date +%s -d "2010-01-01 UTC"`)
DIR_PREFIX=20`echo $REL | tr . _`
REL_DIR=${DIR_PREFIX}_Acumen

if [ -n "$VERSION" ] 
then
  TAG="rel-$REL.$REV"
  git tag $TAG
fi

# censor
touch READY_FOR_CENSOR
../censor.pl

# Fix version strings
echo Fixing version string.
perl -i.bak -pe "s/version := .+/version := \"$REL\"/" build.sbt
perl -i.bak -pe "s/VERSION/$REL/g" README.md socket/README

# update version file
echo Writing version file.
echo "20$REL" > src/main/resources/acumen/version
echo "$REV-$HASH" > src/main/resources/acumen/build_id

# Test to make sure everything is still okay
# Use the quick test so it doesn't take forever and also so that something
# will be created even if some of the "full" propriety based tests fail.
$SBT ${SBT_COMMANDS}

# make release build
cd ..
test ! -e $REL_DIR || error "$REL_DIR exists"
cp -a acumen-rel-working ${DIR_PREFIX}_Acumen
cd $REL_DIR
$SBT assembly
cp target/scala-*/acumen.jar ..
git clean -xfd -e src/main/resources/acumen/version -e src/main/resources/acumen/build_id
rm -rf .git
rm .git*
mv ../acumen.jar .
test ! -e $REL_DIR.zip || error "$REL_DIR.zip exists"
cd ..
zip -9r $REL_DIR.zip $REL_DIR

# and done!

echo "Created $REL_DIR.zip"

if [ -n "$VERSION" ]; then
  cat > release_instructions <<EOF
Make sure everything is in order and upload $REL_DIR.zip.

Than tag the upstream git repository:
  cd acumen-rel-working && git push origin $TAG && cd ..
EOF
  echo "If making a full release see the instructions in \"release_instructions\""
  echo "to finalize the process, that is:"
  echo
  cat release_instructions
fi
