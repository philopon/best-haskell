#!/bin/bash

set -e

branch=`git symbolic-ref --short HEAD`

cabal configure
cabal build

git checkout --orphan deploy
git rm --cached -r .

while read line; do
  set -a array
  array=($line)
  src="${array[0]}"
  dst="${array[1]}"
  mkdir -p `dirname "$dst"`
  [ "$src" = "$dst" ] || cp "$src" "$dst"
  git add "$dst"
done < deploy/files

git commit -am "deploy: `date`"
git push -f heroku deploy:master
git checkout -f $branch

git branch -D deploy
