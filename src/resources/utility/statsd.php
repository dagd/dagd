<?php

/** Bump statsd stat. */
function statsd_send($name, $inc=1, $metric='c') {
  $statsd_enabled = DaGdConfig::get('general.statsd');
  $statsd_host = DaGdConfig::get('general.statsd_host');
  $statsd_port = DaGdConfig::get('general.statsd_port');
  $statsd_prefix = DaGdConfig::get('general.statsd_prefix');

  if (!$statsd_enabled) {
    return false;
  }

  $payload = $statsd_prefix.$name.':'.$inc.'|'.$metric;

  try {
    $sock = fsockopen('udp://'.$statsd_host, $statsd_port, $errno, $errstr);
    fwrite($sock, $payload);
    fclose($sock);
    return true;
  } catch (Exception $e) {
    // Rethrow the exception if we are in debug mode.
    $debug = DaGdConfig::get('general.debug');
    if ($debug) {
      throw $e;
    }
    return false;
  }
}

function statsd_bump($name, $inc=1) {
  return statsd_send($name, $inc, 'c');
}

function statsd_time($name, $time) {
  return statsd_send($name, $time, 'ms');
}
