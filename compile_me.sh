#!/bin/bash
set +x
set -u 
fpm build --flag "-I/usr/include -I/home/jbrenner/ncio/include" --link-flag "-L/usr/lib/x86_64-linux-gnu -L/home/jbrenner/ncio/lib"
