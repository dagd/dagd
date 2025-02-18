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

if [[ "$1" == "worker" ]]; then
  # Wait for migrations to create the task table...
  sleep 2
  ./scripts/dagd-worker -w
else
  echo 0 > sql/current_schema
  ./scripts/sql -a .
  cp -v container/dagd-httpd.conf /etc/httpd/conf.d/

  # On RHEL8, where we default to php-fpm, start it up.
  if [[ -f /usr/sbin/php-fpm ]]; then
    if [[ ! -d /run/php-fpm ]]; then
      mkdir /run/php-fpm/
    fi
    php-fpm
  fi

  # docker-compose DNS breaks on ubi8 in the following case:
  #   dns_get_record('google.com', DNS_ALL);
  # so add a fallback. We still need it for db access though.
  # The net result here is a slowdown on /dns/ endpoints, but at least tests
  # will pass.
  echo "nameserver 8.8.8.8" >> /etc/resolv.conf
  echo "nameserver 8.8.4.4" >> /etc/resolv.conf

  # Immediately before we start, touch a file to tell CI that we are
  # ready to start working.
  touch .ready-for-ci

  httpd -D FOREGROUND
fi
