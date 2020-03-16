#!/bin/bash

if [ $# -ne 1 ]; then
    echo ""
    echo "!!! Please specify an argument !!!"
fi

echo $1 >>./examples/try.hytl
echo "maked ./examples/try.hytl"

stack exec -- hytl-exe -c examples/ex1.hytl
echo "compiled to ./examples/try.ll"

llc examples/ex1.ll
echo "compiled to ./examples/try.s"

gcc examples/ex1.s -o examples/ex1
echo "compiled to ./examples/try"

echo "-- result ---"
./examples/ex1
