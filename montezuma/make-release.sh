#!/bin/sh

rm -rf docs lucene-in-action clean*
cd ..
mv montezuma montezuma-0.1
tar cjvf montezuma-0.1.tar.bz2 montezuma-0.1
