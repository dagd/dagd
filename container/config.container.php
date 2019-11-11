<?php
// Used for the development container, simply to override the db host.
require_once dirname(__FILE__).'/../src/config.dev.php';
DaGdConfig::$config['mysql.host'] = 'db';
DaGdConfig::$config['general.baseurl'] = 'http://localhost:8080';