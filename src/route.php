<?php
require_once dirname(__FILE__).'/resources/global_resources.php';
$routes = array(
  '/$' => 'DaGdShortenHomeController',
  '/ip/?$' => 'DaGdIPController',
  '/w/(.+)/?$' => 'DaGdWhoisController',
  '/up/(.+)/?$' => 'DaGdUpController',

  // Keep this last, these are referenced in order.
  // '/(.+)/?$' => 'DaGdShortenRedirectController',
);
