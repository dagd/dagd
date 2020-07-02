#!/usr/bin/env bash
set -x

function is_db_live {
  # `nc` seems to segfault on the centos7 aarch64 container image for some
  # reason.
  # nc -z db 3306
  php <<'EOD'
<?php
$errno = null;
$errstr = null;
$res=@fsockopen("db", 3306, $errno, $errstr, 0.5);
if ($res) {
  fclose($res);
  exit(0);
} else {
  exit(1);
}
EOD
}

i=0

is_db_live
r=$?

while [[ $r -ne 0 ]]; do
  i=$((i+1))
  sleep 1
  is_db_live
  r=$?
  if [[ $i -ge 30 ]]; then
     echo "Could not connect to mysql within 30 seconds, failing."
     exit 1
  fi
done

set -e

#rm -vf ./sql/current_schema
#./sql/patcher.php --yes
./scripts/sql -a .
cp -v container/dagd-httpd.conf /etc/httpd/conf.d/

# We load mpm_prefork in our config.
rm /etc/httpd/conf.modules.d/00-mpm.conf

# Immediately before we start, touch a file to tell CI that we are
# ready to start working.
touch .ready-for-ci

httpd -D FOREGROUND
