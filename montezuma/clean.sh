#!/bin/sh
find . -name \*.fasl -exec rm {} \;
find . -name \*.dfsl -exec rm {} \;
find . -name \*.nfasl -exec rm {} \;
