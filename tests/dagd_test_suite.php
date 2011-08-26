#!/usr/bin/env php
<?php
// Sorry, this probably isn't as neat as you expected. But this is our test suite. :P


define(
  'TEXT_UA',
  'curl/7.19.7 (x86_64-redhat-linux-gnu) libcurl/7.19.7 '.
    'NSS/3.12.9.0 zlib/1.2.3 libidn/1.18 libssh2/1.2.2');

define(
  'FIREFOX_UA',
  'Mozilla/5.0 (X11; Linux x86_64; rv:9.0a1) Gecko/20110824 Firefox/9.0a1');

require_once dirname(dirname(__FILE__)).'/src/resources/global_resources.php';
$TEST_URL = DaGdConfig::get('general.baseurl');
if (count($argv) > 1) {
  $TEST_URL = $argv[1];
}

$path = null;
$rx_headers = null;

function retrieve($test_path = '/') {
  global $TEST_URL, $path, $rx_headers;
  $path = $test_path;
  $obtain = file_get_contents($TEST_URL.$path);
  if ($obtain) {
    $rx_headers = $http_response_header;
  }
  return strip_tags($obtain);
}

function test_content_type($test_path, $expected_mimetype, $useragent) {
  global $rx_headers;
  $headers = array();

  // Store the original useragent so we can revert.
  $orig_useragent = ini_get('user_agent');

  ini_set('user_agent', $useragent);

  // Pull content so that $http_response_header gets updated.
  retrieve($test_path);

  // Restore
  ini_set('user_agent', $orig_useragent);

  // Create an associative array based on $http_response_header.
  foreach ($rx_headers as $header) {
    $header_split = explode(': ', $header, 2);
    if (count($header_split) > 1) {
      list($name, $value) = $header_split;
      $headers[$name] = $value;
    }
  }
  $correctness = strstr($headers['Content-Type'], $expected_mimetype);
  return test($correctness, 'return correct mimetype ('.$expected_mimetype.')');
}

function test_regex($test_path, $pattern) {
  $content = retrieve($test_path);
  $match = preg_match($pattern, $content);
  return test($match, 'must match pattern: '.$pattern);
}

function fail($text, $exit = true) {
  echo chr(27)."[1;31m".'*** '.$text.' ***'.chr(27)."[0m"."\n";
  if ($exit) {
    exit(1);
  }
}

function pass($text, $exit = false) {
  echo chr(27)."[1;32m".'*** '.$text.' ***'.chr(27)."[0m"."\n";
  if ($exit) {
    exit(0);
  }
}

function test($condition, $summary) {
  global $path;
  if ($condition) {
    pass('Test PASSED ['.$path.']: '.$summary);
    return true;
  } else {
    fail('Test FAILED ['.$path.']: '.$summary);
    return false;
  }
}

/*********** / ***********/

// Ensure that the site itself loads at all.
$content = retrieve();
test($content, 'retrieve /');

test_content_type('/', 'text/plain', TEXT_UA);
test_content_type('/', 'text/html', FIREFOX_UA);

// Check if the site contains the string 'Current commands'.
test_regex('/', '@Current commands@');


/*********** /ip ***********/

// See if /ip contains a number followed by a '.'
test_regex('/ip', '@[0-9]\.@');


/*********** /wp/Phuzion ***********/

// Make sure response is numeric only.
test_regex('/wp/Phuzion', '@^[0-9]+$@');


/*********** /w/xxxxxxx ***********/

// Ensure that whois is functioning.
test_regex('/w/google.com', '@Mountain View@');
