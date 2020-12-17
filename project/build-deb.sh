#!/bin/bash
fpm -f -s dir -t deb -n showdme-ejabberd -v "1.0" --description "Ejabberd server for showdme app" \
--iteration 3 --prefix /usr/lib/mongooseim \
--after-install ./deb-scripts/postinst \
-d "libc6 >= 2.15" -d "libexpat1 >= 1.95.8" -d "libpam0g >= 0.99.7.1" -d "libssl1.0.0 >= 1.0.0" -d "libtinfo5" -d "zlib1g >= 1:1.1.4" -C rel/mongooseim .
