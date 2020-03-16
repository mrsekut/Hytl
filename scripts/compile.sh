#!/bin/bash

if [ $# -ne 1 ]; then
    echo ""
    echo "!!! Please specify an argument !!!"
fi

printf $1 >./examples/tmp/tmp.hytl
echo ""
echo "maked ./examples/tmp/tmp.hytl"

stack exec -- hytl-exe -c examples/tmp/tmp.hytl
echo ""
echo "compiled to ./examples/tmp/tmp.ll"

/usr/local/opt/llvm/bin/llc examples/tmp/tmp.ll
echo ""
echo "compiled to ./examples/tmp/tmp.s"

gcc examples/tmp/tmp.s -o examples/tmp/tmp
echo ""
echo "compiled to ./examples/tmp/tmp"

echo ""
echo "-- result ---"
./examples/tmp/tmp
