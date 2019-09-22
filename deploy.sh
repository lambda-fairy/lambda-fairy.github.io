#!/bin/sh

set -e

cabal new-run lfairy -- rebuild

cd _site
git init
git add .
git commit -m 'Deploy'

git remote add github git@github.com:lambda-fairy/lambda-fairy.github.io.git
git push -f github master
