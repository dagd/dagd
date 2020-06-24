<?php
// Used for the development container, simply to override the db host.
require_once dirname(__FILE__).'/../src/config.dev.php';
DaGdConfig::$config['mysql.host'] = 'db';
DaGdConfig::$config['general.baseurl'] = 'http://localhost:8080';
DaGdConfig::$config['session.encryption_key'] = 'insecure development key';

// Google doesn't seem to care too much about this key being public.
DaGdConfig::$config['shorten.google_pagespeed_insights_key'] = 'AIzaSyDSAgs4dOw1AGTW1viVI0BW6M8lB5wZHP4';
