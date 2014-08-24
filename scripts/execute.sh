#!/bin/bash

DIR=$(cd `dirname $0`; pwd)

export LD_LIBRARY_PATH=$DIR/../libs
exec $DIR/../dist/build/best-haskell/best-haskell
