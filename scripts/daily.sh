#!/bin/bash

set -e

URI="https://hackage.haskell.org"
TMP=./tmpdata
DIR=$(cd `dirname $0`; pwd)

mkdir -p $TMP

curl -o $TMP/index.tar.gz "$URI/packages/index.tar.gz"
$DIR/../dist/build/register-cabal/register-cabal $TMP/index.tar.gz

curl -o $TMP/downloads.csv "$URI/packages/downloads.csv"
$DIR/../dist/build/register-downloads/register-downloads $TMP/downloads.csv

rm -r $TMP
