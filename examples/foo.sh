#!/usr/bin/env bash

./isogeny.clj -t examples/foo.template \
    -c examples/foo.specific.edn \
    -d examples/foo.default.edn \
    -C '{:override? true}' \
    -a examples/custom-tag.clj \
    -o examples/foo.out \
    --strict \
    -v
