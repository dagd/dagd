<?php
require_once dirname(__FILE__).'/resources/global_resources.php';
$routes = array(
  '/$' => 'DaGdComingSoonController',
  '/ua$' => 'DaGdUserAgentController',
  '/ip/?$' => 'DaGdIPController',
  '/w/(.+)/?$' => 'DaGdWhoisController',
  '/up/(.+)/?$' => 'DaGdComingSoonController',

  // Keep this last, these are referenced in order.
  // '/(.+)/?$' => 'DaGdShortenRedirectController',
);
