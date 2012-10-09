#!/usr/bin/env php
<?php
// Sorry, this probably isn't as neat as you expected. But this is our test suite. :P

echo "*** NOTE: This test suite expects to be run on a clean database. ***\n";

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
$tests_completed = array(
  'attempted' => 0,
  'successful' => 0,
  'failed' => 0);

function retrieve($test_path = '/', $ignore_errors = false) {
  global $TEST_URL, $path, $rx_headers;
  $rx_headers = null;
  $path = $test_path;
  $context = stream_context_create(
    array(
      'http' => array(
        'ignore_errors' => $ignore_errors,
      ),
    )
  );
  $obtain = @file_get_contents($TEST_URL.$path, false, $context);
  $rx_headers = $http_response_header;
  return strip_tags($obtain);
}

function test_response_code($test_path, $expected_code) {
  global $rx_headers;
  retrieve($test_path);
  $match = preg_match('@'.$expected_code.'@', $rx_headers[0]);
  return test(
    $match,
    'give correct HTTP Response code ('.$expected_code.' ?= '.
    $rx_headers[0].')');
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

function test_regex($test_path, $pattern, $not_include = false) {
  $content = retrieve($test_path, true);
  if ($not_include) {
    $match = !preg_match($pattern, $content);
    return test($match, 'must NOT match pattern: '.$pattern);
  } else {
    $match = preg_match($pattern, $content);
    return test($match, 'must match pattern: '.$pattern);
  }
}

function fail($text, $exit = false) {
  echo chr(27)."[1;31m".'*** '.$text.' ***'.chr(27)."[0m"."\n";
  if ($exit) {
    echo "Exiting with status code '1' - to bypass a git pre-commit hook,\n";
    echo "pass `--no-verify` to your `git commit` command.\n";
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
  global $path, $tests_completed;
  $tests_completed['attempted']++;
  if ($condition) {
    pass('Test PASSED ['.$path.']: '.$summary);
    $tests_completed['successful']++;
    return true;
  } else {
    fail('Test FAILED ['.$path.']: '.$summary);
    $tests_completed['failed']++;
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
test_regex('/', '@various conditions@');


/*********** /ip ***********/

// See if /ip contains a number followed by a '.'
test_regex('/ip', '@[0-9]\.@');


/*********** /wp/Phuzion ***********/

// Make sure response is numeric only.
test_regex('/ec/Phuzion', '@^[0-9]+$@');
test_regex('/ec/Phuzion?lang=en', '@^[0-9]+$@');
test_regex('/ec/Phuzion?lang=fr', '@^[0-9]+$@');
test_response_code('/ec/Phuzion?lang=asdfasdf', 400);
test_response_code('/ec/Phuzion?lang=asd123dsa', 400);


/*********** /w/xxxxxxx ***********/

// Ensure that whois is functioning.
test_regex('/w/google.com', '@Mountain View@');
test_regex('/w/fbco.de', '@Menlo Park@');
test_regex('/w/da.gd', '@Ricky@');
test_regex('/w/4.2.2.2', '@Level 3 Communications@');


/*********** /headers/xxxxxxx ***********/

// Ensure that headers are seen.
test_regex('/headers/', '@Host: @');
test_regex('/headers/', '@X-DaGd-Proxy:@', true);
test_regex('/headers/google.com', '@Server: gws@');
test_regex('/headers/http://google.com/', '@Server: gws@');


/*********** /up/xxxxxxx ***********/

test_regex('/up/google.com', '@^200$@');
test_regex('/up/https://encrypted.google.com', '@^200$@');


/*********** /et/xxxxxxx ***********/
test_response_code('/et/75009720', 302);


/*********** /host/xxxxxxx ***********/
test_regex('/host/google.com', '@2607:f8b0@');
test_regex('/host/facebook.com', '@69.171@');
test_regex('/host/facebook.com', '@face:b00c@');
test_regex('/host/facebook.com?noipv6', '@face:b00c@', true);
test_regex('/host/66.220.158.70', '@facebook.com@');

/*********** /break/ ***********/
test_response_code('/break', 500);
test_regex('/break', '@An error has occurred@');

/*********** /c/store/xxxxxxx/xxxxxxx ***********/
test_response_code(
  '/c/store/g/https://www.google.com/search%3Fq%3D$PARAMETERS',
  200);
test_response_code(
  '/c/store/g/https://www.google.com/search%3Fq%3D$PARAMETERS',
  400);
test_regex(
  '/c/store/g1/https://www.google.com/search%3Fq%3D$PARAMETERS',
  '@Success@');
test_regex(
  '/c/store/g1/https://www.google.com/search%3Fq%3D$PARAMETERS',
  '@already been defined@');

/*********** /c/xxxxxxx/xxxxxxx ***********/
test_response_code('/c/g/foobar', 302);
test_response_code('/c/g%20foobar', 302);
test_response_code('/c/nonexistent/foobar', 400);
test_regex('/c/nonexistent/foobar', '@was not found@');

/*********** /c/[xxxxxxx] ***********/
test_regex('/c/', '@Redirect@');
test_regex('/c/json', '@{"g":@');
test_regex('/c/json/', '@"g1":@');
test_content_type('/c/json/', 'application/json', TEXT_UA);
test_content_type('/c/json/', 'application/json', FIREFOX_UA);
test_content_type('/c/json', 'application/json', TEXT_UA);
test_content_type('/c/json', 'application/json', FIREFOX_UA);
test_content_type('/c', 'text/html', FIREFOX_UA);
test_content_type('/c/', 'text/html', FIREFOX_UA);
test_content_type('/c', 'text/plain', TEXT_UA);
test_content_type('/c/', 'text/plain', TEXT_UA);

/*********** /image/xxxxxxx/[xxxxxxx] ***********/
test_content_type('/image/200x200/', 'image/png', TEXT_UA);
test_content_type('/image/200x200/', 'image/png', FIREFOX_UA);
test_content_type('/image/400x100/jpeg/', 'image/jpeg', TEXT_UA);
test_content_type('/image/400x100/jpg/', 'image/jpeg', TEXT_UA);
test_content_type('/image/400x100/jpeg', 'image/jpeg', TEXT_UA);
test_content_type('/image/400x100', 'image/png', TEXT_UA);
test_content_type('/image/400x100?bgcolor=444444', 'image/png', TEXT_UA);
test_content_type('/image/400x100.gif?bgcolor=444444', 'image/gif', TEXT_UA);
test_content_type('/image/400x100/gif', 'image/gif', FIREFOX_UA);
test_response_code('/image/300', 400);
test_response_code('/image/3000000000x120102102', 400);

/*********** /status/xxxxxxx/[xxxxxxx] ***********/
test_response_code('/status/404', 404);
test_response_code('/status/605', 605);
test_response_code('/status/123/hi', 123);

echo "Report: Completed {$tests_completed['attempted']} tests.\n";
echo "Report: {$tests_completed['successful']} were successful.\n";
echo "Report: {$tests_completed['failed']} failed.\n";