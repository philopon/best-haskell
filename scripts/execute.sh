#!/bin/bash

export LD_LIBRARY_PATH=./libs
exec ./dist/build/best-haskell/best-haskell $PORT
