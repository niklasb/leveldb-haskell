#!/bin/sh

ROOT=`dirname $0`
cd $ROOT
ROOT=`pwd`
SNAPPY_HOME=$ROOT/3rd_party/snappy
LEVELDB_HOME=$ROOT/3rd_party/leveldb

cd $SNAPPY_HOME
./autogen.sh
./configure --disable-shared --with-pic
make -j

cd $LEVELDB_HOME
export LIBRARY_PATH=$SNAPPY_HOME
export C_INCLUDE_PATH=$SNAPPY_HOME
export CPLUS_INCLUDE_PATH=$SNAPPY_HOME
make libleveldb.a

