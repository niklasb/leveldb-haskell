#!/bin/sh

cd `dirname $0`
mkdir -p 3rd_party
cd 3rd_party
svn checkout http://snappy.googlecode.com/svn/trunk/ snappy
git clone https://code.google.com/p/leveldb/
