#!/bin/sh

set -e

SBT=${SBT:-`which sbt`}
COMMIT=${GIT_COMMIT:-master}

# add git version to release strings
DATE="`date +%y.%m.%d`"
REV=`git rev-parse --verify HEAD | cut -b1-8`
REL=${VERSION:-$DATE.$REV}
DIR_PREFIX=20`echo $REL | tr . _`
REL_DIR=${DIR_PREFIX}_Acumen

DEV_URL=https://bitbucket.org/effective/acumen-dev.git
REL_URL=https://bitbucket.org/effective/acumen.git

error () {
  echo "Error: $1" 1>&2
  exit 1
}

# prep clone
git clone $DEV_URL acumen-rel-working
cd acumen-rel-working 
git checkout $COMMIT
git remote add rel $REL_URL
git fetch rel
git tag rel-$REL-pre

# perform merge using equivalent of "-s theirs"
# http://stackoverflow.com/questions/173919/git-merge-s-ours-what-about-their
git checkout release
git merge --no-edit -s ours $COMMIT
git branch tmp
git reset --hard $COMMIT
git reset --soft tmp
git commit --amend -C HEAD

touch READY_FOR_CENSOR
../censor.pl

# Update merge commit with censored files
git add -u
git commit --amend -C HEAD

# Fix version strings
echo Fixing version string.
perl -i.bak -pe "s/version := .+/version := \"$REL\"/" build.sbt
perl -i.bak -pe "s/VERSION/$REL/g" README socket/README
git add -u
git commit -m "Update version string."

# update version file
echo Writing version file.
echo "20$REL" > src/main/resources/acumen/version

# Test to make sure everything is still okay
# Use the quick test so it doesn't take forever and also so that something
# will be created even if some of the "full" propriety based tests fail.
$SBT compile quick:test

# tag
git tag rel-$REL

# make release build
cd ..
test ! -e $REL_DIR || error "$REL_DIR exists"
cp -a acumen-rel-working ${DIR_PREFIX}_Acumen
cd $REL_DIR
$SBT proguard
cp target/scala-*/acumen-$REL.jar ..
git clean -xfd -e src/main/resources/acumen/version
rm -rf .git
rm .gitignore
mv ../acumen-$REL.jar .
test ! -e $REL_DIR.zip || error "$REL_DIR.zip exists"
cd ..
zip -9r $REL_DIR.zip $REL_DIR

# and done!

echo "Created $REL_DIR.zip"
##
## These instructions only apply for a major (non-snapshot release).
## Kept around as this script may get fixed to create those too.
##
# cat > final_instructions <<EOF
# Make sure everything is in order and upload $REL_DIR.zip
# and do a:
#   (cd acumen-rel-working
#    git push rel master release rel-$REL rel-$REL-pre
#    git push origin master rel-$REL-pre)
# EOF
# echo "cat final_instructions"
# cat final_instructions
