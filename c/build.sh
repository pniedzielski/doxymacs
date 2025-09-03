#! /bin/sh

set -e

./bootstrap
./configure
make
cp doxymacs_parser "$1/doxymacs_parser"
