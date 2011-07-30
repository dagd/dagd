<?php
require_once dirname(dirname(__FILE__)).'/resources/global_resources.php';
if (!$_GET['__path__']) {
  throw new Exception(
    'No __path__ GET variable was found.'.
    'Your rewrite rules are incorrect!');
}

