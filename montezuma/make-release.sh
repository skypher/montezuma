#!/bin/sh

rm -rf docs lucene-in-action clean*
rm -rf `find . -name .svn`
cd ..
mv montezuma montezuma-0.1
tar cjvf montezuma-0.1.tar.bz2 montezuma-0.1
