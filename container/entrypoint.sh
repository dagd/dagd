#!/usr/bin/env bash
set -x

i=0

nc -z db 3306
r=$?

while [[ $r -ne 0 ]]; do
  i=$((i+1))
  sleep 1
  nc -z db 3306
  r=$?
  if [[ $i -ge 30 ]]; then
     echo "Could not connect to mysql within 30 seconds, failing."
     exit 1
  fi
done

set -e

rm -vf ./sql/current_schema
./sql/patcher.php --yes
cp -v container/dagd-httpd.conf /etc/httpd/conf.d/

# We load mpm_prefork in our config.
rm /etc/httpd/conf.modules.d/00-mpm.conf

# Immediately before we start, touch a file to tell CI that we are
# ready to start working.
touch .ready-for-ci

httpd -D FOREGROUND
