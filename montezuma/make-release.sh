#!/bin/sh

VERSION=0.1.3a
NAME="montezuma-$VERSION"

rm -rf docs lucene-in-action clean*
rm -rf `find . -name .svn -o -name \*.fasl`
rm make-release.sh
cd ..
mv montezuma "$NAME"
tar czvf "$NAME.tar.gz" $NAME
