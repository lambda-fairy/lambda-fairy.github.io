#!/bin/sh

set -e

cabal configure -O0
cabal run --ghc-option=-j -- rebuild

cd _site
git init
git add .
git commit -m 'Deploy'

git remote add github git@github.com:lfairy/lfairy.github.io.git
git push -f github master
