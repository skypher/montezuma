#!/bin/sh

rm -rf docs lucene-in-action clean*
rm -rf `find . -name .svn`
rm make-release.sh
cd ..
mv montezuma montezuma-0.1.1
tar czvf montezuma-0.1.1.tar.gz montezuma-0.1.1
