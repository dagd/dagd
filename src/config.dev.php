<?php
// This is the general config file for DaGd stuff.
class DaGdConfig {
  public static $config = array(
    'general.debug' => false,
    'general.display_errors' => false,
    'general.baseurl' => 'http://dagd.local', // DO *NOT* include trailing '/'.

    // This array is imploded by | into a regex. Escape any | or # used in it.
    // I am not including ELinks here, because it can support linking
    'general.text_useragent_search' => 'Wget|curl|libcurl|Supybot',
    'general.text_useragent_search' => array(
      'Wget',
      'curl',
      'libcurl',
      'Supybot',
    ),

    
    'general.useragent' => 'da.gd/1.0',
    'general.applications' => array(
      'base', // This absolutely MUST be first.
      'ip',
      'useragent',
      'comingsoon',
      'help',
      'whois',
      'editcount',
      'shorten',
      'pastebin',
      'headers',
    ),
    'general.routemap' => array(
      '/help/?$' => 'DaGdHelpController',
      '/ua/?$' => 'DaGdUserAgentController',
      '/ip/?$' => 'DaGdIPController',
      '/w/(.+)/?$' => 'DaGdWhoisController',
      '/ec/(.+)/?$' => 'DaGdEditCountController',
      '/up/(.+)/?$' => 'DaGdComingSoonController',
      '/headers/?(.+)?/?$' => 'DaGdHeadersController',
      '/(?:p|paste|pastebin)/?(\d+)?/?$' => 'DaGdPastebinController',
      '/(?:(?:shorten|s|)(?:/|$))?(.+)?/?$' => 'DaGdShortenController'),

    // These redirects take place on CLI useragents only.
    'general.cli_routemap' => array(
      '/$' => 'DaGdHelpController',
    ),


    // A hardcoded map of whois servers to use for certain domains.
    'whois.hardcode_map' => array(
      // tld (WITHOUT '.') => server
      'ly' => 'whois.nic.ly',
    ),

    // MySQL settings
    'mysql.host' => 'localhost',
    'mysql.user' => 'root',
    'mysql.password' => '',
    'mysql.database' => 'dagd',
    
  );

  public static function get($key) {
    return self::$config[$key];
  }
}
