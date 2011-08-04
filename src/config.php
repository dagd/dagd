<?php
// This is the general config file for DaGd stuff.
class DaGdConfig {
  private static $config = array(
    'general.debug' => false,

    // I am not including ELinks here, because it can support linking
    // and some simple things like bolding text.
    'general.text_useragent_search' => 'Wget|curl|libcurl',
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