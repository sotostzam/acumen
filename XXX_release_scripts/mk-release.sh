#!/bin/sh

set -e

if [ "$#" -ne 1 ]; then
  echo "usage: ./mk-release.sh YY.MM.DD"
  echo "  for example ./mk-release.sh 13.10.14"
  exit 1
fi

REL="$1"
DIR_PREFIX=20`echo $REL | tr . _`
REL_DIR=${DIR_PREFIX}_Acumen

DEV_URL=git@bitbucket.org:effective/acumen-dev.git
REL_URL=git@bitbucket.org:effective/acumen.git

error () {
  echo "Error: $1" 1>&2
  exit 1
}

# prep clone
git clone $DEV_URL acumen-rel-working
cd acumen-rel-working 
git remote add rel $REL_URL
git fetch rel
git tag rel-$REL-pre

# perform merge using equivalent of "-s theirs"
# http://stackoverflow.com/questions/173919/git-merge-s-ours-what-about-their
git checkout release
git merge -s ours master
git branch tmp
git reset --hard master
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
sbt compile test

# tag
git tag rel-$REL

# make release build
cd ..
test ! -e $REL_DIR || error "$REL_DIR exists"
cp -a acumen-rel-working ${DIR_PREFIX}_Acumen
cd $REL_DIR
sbt proguard
cp target/scala-*/acumen-$REL.jar ..
git clean -xfd -e src/main/resources/acumen/version
rm -rf .git
rm .gitignore
mv ../acumen-$REL.jar .
test ! -e $REL_DIR.zip || error "$REL_DIR.zip exists"
cd ..
zip -9r $REL_DIR.zip $REL_DIR

# and done!
echo "Done."
cat > final_instructions <<EOF
Make sure everything is in order and upload $REL_DIR.zip
and do a:
  (cd acumen-rel-working
   git push rel master release rel-$REL rel-$REL-pre
   git push origin master rel-$REL-pre)
EOF
echo "cat final_instructions"
cat final_instructions
