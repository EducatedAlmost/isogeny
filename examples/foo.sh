#!/usr/bin/env bash

isogeny.clj -t foo.template \
    -c foo.specific.edn \
    -d foo.default.edn \
    -C '{}'
    -a custom-tag.clj \
    -o foo.out \
    --strict \
    -v
