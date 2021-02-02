<?php
// Used for the development container, simply to override the db host.
require_once dirname(__FILE__).'/../src/config.dev.php';
DaGdConfig::$config['mysql.host'] = 'db';
DaGdConfig::$config['general.baseurl'] = 'http://localhost:8080';
DaGdConfig::$config['session.encryption_key'] = 'insecure development key';

$insights_key = getenv('GOOGLE_INSIGHTS_API');
if ($insights_key !== false) {
  DaGdConfig::$config['shorten.google_pagespeed_insights_key'] = $insights_key;
}
