#!/bin/bash

rm 00-index.tar.gz*
wget http://hackage.haskell.org/packages/archive/00-index.tar.gz
rm -rf hackage
mkdir hackage
tar xvzf 00-index.tar.gz -C hackage
