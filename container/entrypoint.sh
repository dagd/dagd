#!/usr/bin/env bash
set -e
set -x

sleep 10 # wait for mysql container
rm -vf ./sql/current_schema
./sql/patcher.php --yes
cp -v container/dagd-httpd.conf /etc/httpd/conf.d/

# We load mpm_prefork in our config.
rm /etc/httpd/conf.modules.d/00-mpm.conf
httpd -D FOREGROUND
