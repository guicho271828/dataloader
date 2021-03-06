#!/bin/bash

rm -rf file
git clone https://github.com/file/file
cd file
autoreconf --install
autoconf
./configure
make
