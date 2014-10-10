#!/bin/sh

set -e

lein doc
echo "*** Docs built ***"
tmpdir=`mktemp -d /tmp/flatland-useful.XXXXXX`
mv doc/** $tmpdir
rmdir doc
git checkout gh-pages
git rm -rf .
mv $tmpdir/** .
git add -Av .
git commit -m "Updated docs"
echo "*** gh-pages branch updated ***"
rmdir $tmpdir
git checkout -
echo "Run this to complete:"
echo "git push origin gh-pages:gh-pages"
