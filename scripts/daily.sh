#!/bin/bash

set -e

eval `heroku config -s --app best-haskell | awk '{print "export", $0}'`

URI="https://hackage.haskell.org"
TMP=./tmpdata
DIR=$(cd `dirname $0`; pwd)

mkdir -p $TMP

curl -o $TMP/index.tar.gz "$URI/packages/index.tar.gz"
$DIR/../dist/build/index2json/index2json $TMP/index.tar.gz > $TMP/index.json
node $DIR/../register/register-index.js $TMP/index.json

node $DIR/../register/register-maintainers.js

curl -o $TMP/downloads.csv "$URI/packages/downloads.csv"
node $DIR/../register/register-downloads.js $TMP/downloads.csv
