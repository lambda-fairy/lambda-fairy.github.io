#!/bin/sh

set -e

git config --global user.email 'nobody@example.com'
git config --global user.name "$1"

cd _site
git init
git add .
git commit -m 'Deploy'

git remote add github git@github.com:lfairy/lfairy.github.io.git
git push -f github master
