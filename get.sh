#!/bin/bash

set -e
cd `dirname $0`
rm -rf 3rd_party
mkdir -p 3rd_party
cd 3rd_party

svn checkout http://snappy.googlecode.com/svn/trunk/ snappy
pushd snappy
patch < ../../snappy-autogen.patch
popd

git clone https://code.google.com/p/leveldb/
