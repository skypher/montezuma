#!/bin/sh

BASE_DIR=`dirname $0`
BUILD=${BASE_DIR}/build
LIB=${BASE_DIR}/lib
SRC=${BASE_DIR}/src

mkdir ${BUILD} 2>/dev/null

javac -d ${BUILD} -classpath ${LIB}/json_simple.jar:${LIB}/lucene-1.4.3.jar ${SRC}/PasteIndexer.java
