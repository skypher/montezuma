#!/bin/sh

rm -rf docs lucene-in-action clean*
rm -rf `find . -name .svn`
cd ..
mv montezuma montezuma-0.1
tar cjzf montezuma-0.1.tar.gz montezuma-0.1
