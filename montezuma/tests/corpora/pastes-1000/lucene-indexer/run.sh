#!/bin/sh
echo $0
java -classpath json_simple.jar:lucene-1.4.3.jar:build PasteIndexer
