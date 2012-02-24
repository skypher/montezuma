#!/bin/sh

BASE_DIR=`dirname $0`
BUILD=${BASE_DIR}/build
LIB=${BASE_DIR}/lib

java -classpath ${LIB}/json_simple.jar:${LIB}/lucene-1.4.3.jar:${BUILD} PasteIndexer $@
