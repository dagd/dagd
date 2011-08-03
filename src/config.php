<?php
// This is the general config file for DaGd stuff.
class DaGdConfig {
  private static $config = array(
    'general.debug' => false,
    'general.text_useragent_search' => 'Wget|curl|libcurl|Elinks',
    'general.applications' => array(
      'ip',
      'useragent',
      'comingsoon',
      'aboutus',
      'whois',
    ),

    // A hardcoded map of whois servers to use for certain domains.
    'whois.hardcode_map' => array(
      // tld (WITHOUT '.') => server
      'ly' => 'whois.nic.ly',
    ),
    
  );

  public static function get($key) {
    return self::$config[$key];
  }
}