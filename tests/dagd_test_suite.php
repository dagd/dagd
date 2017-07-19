#!/usr/bin/env php
<?php
// Sorry, this probably isn't as neat as you expected. But this is our test suite. :P

require_once dirname(dirname(__FILE__)).'/src/resources/global_resources.php';

class DaGdTest {
  public static $test_url;
  protected $path;
  protected $headers;
  protected $tolerate_failure;
  protected $accept = '*/*';
  private $original_user_agent;

  private static $results = array(
    'pass' => 0,
    'fail' => 0,
    'tolerated fail' => 0,
  );

  public function setTolerateFailure($tolerate_failure) {
    $this->tolerate_failure = $tolerate_failure;
    return $this;
  }

  public function setUserAgent($user_agent) {
    $this->original_user_agent = ini_get('user_agent');
    ini_set('user_agent', $user_agent);
    return $this;
  }

  public function setAccept($type) {
    $this->accept = $type;
    return $this;
  }

  public static function getResultsSummary() {
    return DaGdTest::$results;
  }

  protected function fail($text, $exit = false) {
    if ($this->tolerate_failure) {
      echo chr(27)."[1;33m".'*** (tolerable failure) '.$text.' ***'.chr(27)."[0m"."\n";
      DaGdTest::$results['tolerated fail']++;
    } else {
      echo chr(27)."[1;31m".'*** '.$text.' ***'.chr(27)."[0m"."\n";
      DaGdTest::$results['fail']++;
    }
    if ($exit && !$this->tolerate_failure) {
      echo "Exiting with status code '1' - to bypass a git pre-commit hook,\n";
      echo "pass `--no-verify` to your `git commit` command.\n";
      exit(1);
    }
  }

  protected function pass($text, $exit = false) {
    echo chr(27)."[1;32m".'*** '.$text.' ***'.chr(27)."[0m"."\n";
    DaGdTest::$results['pass']++;
    if ($exit) {
      exit(0);
    }
  }

  protected function retrieve($ignore_errors = false) {
    $context = stream_context_create(
      array(
        'http' => array(
          'ignore_errors' => $ignore_errors,
          'header' => array(
            'Accept: '.$this->accept."\r\n",
          ),
        ),
      )
    );
    $obtain = @file_get_contents($this::$test_url.$this->path, false, $context);
    $this->headers = $http_response_header;
    if ($this->original_user_agent) {
      ini_set('user_agent', $this->original_user_agent);
    }
    return strip_tags($obtain);
  }

  protected function getHeaders() {
    if (!$this->headers) {
      throw new Exception(
        'Call retrieve() before calling getHeaders()!');
    }
    return $this->headers;
  }

  protected function test($condition, $summary) {
    if ($condition) {
      $this->pass('Test PASSED ['.$this->path.']: '.$summary);
      return true;
    } else {
      $this->fail('Test FAILED ['.$this->path.']: '.$summary);
      if ($this->tolerate_failure) {
        return true;
      } else {
        return false;
      }
    }
  }
}

final class DaGdResponseCodeTest extends DaGdTest {
  private $expected_code;

  public function __construct($test_path, $expected_code) {
    $this->path = $test_path;
    $this->expected_code = $expected_code;
  }

  public function run() {
    $this->retrieve();
    $headers = $this->getHeaders();
    $match = preg_match('@'.$this->expected_code.'@', $headers[0]);
    return $this->test(
      $match,
      'give correct HTTP Response code ('.$this->expected_code.' ?= '.
      $headers[0].')');
  }
}

final class DaGdContentTypeTest extends DaGdTest {
  private $expected_mimetype;

  public function __construct($test_path, $expected_mimetype) {
    $this->path = $test_path;
    $this->expected_mimetype = $expected_mimetype;
  }

  public function run() {

    $this->retrieve();
    $headers = $this->getHeaders();

    foreach ($headers as $header) {
      $header_split = explode(': ', $header, 2);
      if (count($header_split) > 1) {
        list($name, $value) = $header_split;
        $headers[$name] = $value;
      }
    }
    $correctness = strstr($headers['Content-Type'], $this->expected_mimetype);
    return $this->test(
      $correctness,
      'return correct mimetype ('.$this->expected_mimetype.')');
  }
}

final class DaGdRegexTest extends DaGdTest {
  private $regex;
  private $invert;

  public function __construct($test_path, $pattern, $invert = false) {
    $this->path = $test_path;
    $this->regex = $pattern;
    $this->invert = $invert;
  }

  public function run() {
    $content = $this->retrieve(true);
    if ($this->invert) {
      $match = !preg_match($this->regex, $content);
      return $this->test($match, 'must NOT match pattern: '.$this->regex);
    } else {
      $match = preg_match($this->regex, $content);
      return $this->test($match, 'must match pattern: '.$this->regex);
    }
  }
}

function id($a) {
  return $a;
}

if (count($argv) > 1) {
  DaGdTest::$test_url = $argv[1];
} else {
  DaGdTest::$test_url = 'http://dagd.local/';
}

/*********** / ***********/

id(new DaGdContentTypeTest('/', 'text/plain'))
  ->setAccept('text/plain')
  ->run();
id(new DaGdContentTypeTest('/', 'text/html'))
  ->setAccept('text/html')
  ->run();
id(new DaGdRegexTest('/', '@various conditions@'))
  ->run();


/*********** /ip ***********/

// See if /ip contains a number followed by a '.'
id(new DaGdRegexTest('/ip', '@[0-9]\.@'))
  ->run();

/*********** /wp/CodeBlock ***********/

id(new DaGdRegexTest('/ec/CodeBlock', '@^[0-9]+$@'))
  ->run();
id(new DaGdRegexTest('/ec/CodeBlock?lang=en', '@^[0-9]+$@'))
  ->run();
id(new DaGdRegexTest('/ec/CodeBlock?lang=fr', '@^[0-9]+$@'))
  ->run();
id(new DaGdResponseCodeTest('/ec/CodeBlock?lang=asdfasdf', 400))
  ->run();

/************ /w/xxxxxxx ************/

/* // Ensure that whois is functioning. */
id(new DaGdRegexTest('/w/google.com', '@Mountain View@'))
  ->run();
id(new DaGdRegexTest('/w/da.gd', '@Ricky@'))
  ->run();
id(new DaGdRegexTest('/w/4.2.2.2', '@Level 3 Communications@'))
  ->run();
id(new DaGdRegexTest('/w/74.94.18.108', '@OrgAbusePhone@'))
  ->run();
id(
  new DaGdRegexTest(
    '/w/2001:470:8:624:211c:aaaa:1111:1111',
    '@contact:Name:Private Customer@'))
  ->run();

/************ /headers/xxxxxxx ************/

id(new DaGdRegexTest('/headers', '@Host: @'))
  ->run();
id(new DaGdRegexTest('/headers', '@X-DaGd-Proxy: @', true))
  ->run();
id(new DaGdRegexTest('/headers/google.com', '@Server: gws@'))
  ->run();
id(new DaGdRegexTest('/headers/http://google.com/', '@Server: gws@'))
  ->run();

/************ /up/xxxxxxx ************/
id(new DaGdRegexTest('/up/google.com', '@^200$@'))
  ->run();
id(new DaGdRegexTest('/up/https://encrypted.google.com/', '@^200$@'))
  ->run();
id(new DaGdRegexTest('/up/http://google.com/404', '@^404$@'))
  ->run();

/************ /et/xxxxxxx ************/
id(new DaGdResponseCodeTest('/et/750009720', 302))
  ->run();

/************ /host/xxxxxxx ************/
id(new DaGdRegexTest('/host/ipv6.google.com', '@:@'))
  ->run();
id(new DaGdRegexTest('/host/google.com', '@[0-9]\.@'))
  ->run();
id(new DaGdRegexTest('/host/google.com?noipv6', '@:@', true))
  ->run();
id(new DaGdRegexTest('/host/4.2.2.2', '@b.resolvers.Level3.net@'))
  ->run();

/************ /break/ ************/
id(new DaGdResponseCodeTest('/break', 500))
  ->run();
id(new DaGdRegexTest('/break', '@An error has occurred@'))
  ->run();

/************ /c/store/xxxxxxx/xxxxxxx ************/
id(
  new DaGdResponseCodeTest(
    '/c/store/g/https://www.google.com/search%3Fq%3D$PARAMETERS',
    200))
  ->setTolerateFailure(true)
  ->run();
id(
  new DaGdResponseCodeTest(
    '/c/store/g/https://www.google.com/search%3Fq%3D$PARAMETERS',
    400))
  ->run();
id(
  new DaGdRegexTest(
    '/c/store/g1/https://www.google.com/search%3Fq%3D$PARAMETERS',
    '@Success@'))
  ->setTolerateFailure(true)
  ->run();
id(
  new DaGdRegexTest(
    '/c/store/g1/https://www.google.com/search%3Fq%3D$PARAMETERS',
    '@already been defined@'))
  ->run();


/************ /c/xxxxxxx/xxxxxxx ************/
id(new DaGdResponseCodeTest('/c/g/foobar', 302))
  ->run();
id(new DaGdResponseCodeTest('/c/g%20foobar', 302))
  ->run();
id(new DaGdResponseCodeTest('/c/nonexistent/foobar', 400))
  ->run();
id(new DaGdRegexTest('/c/nonexistent/foobar', '@was not found@'))
  ->run();

/************ /c/[xxxxxxx] ************/
id(new DaGdRegexTest('/c/', '@Redirect@'))
  ->run();
id(new DaGdRegexTest('/c/json', '@{"g":@'))
  ->run();
id(new DaGdRegexTest('/c/json/', '@"g1":@'))
  ->run();
id(new DaGdContentTypeTest('/c/json/', 'application/json'))
  ->setAccept('text/plain')
  ->run();
id(new DaGdContentTypeTest('/c/json', 'application/json'))
  ->setAccept('text/html')
  ->run();
id(new DaGdContentTypeTest('/c', 'text/html'))
  ->setAccept('text/html')
  ->run();
id(new DaGdContentTypeTest('/c/', 'text/html'))
  ->setAccept('text/html')
  ->run();
id(new DaGdContentTypeTest('/c', 'text/plain'))
  ->setAccept('text/plain')
  ->run();
id(new DaGdContentTypeTest('/c/', 'text/plain'))
  ->setAccept('text/plain')
  ->run();

/************ /image/xxxxxxx/[xxxxxxx] ************/
id(new DaGdContentTypeTest('/image/200x200', 'image/png'))
  ->setAccept('text/html')
  ->run();
id(new DaGdContentTypeTest('/image/200x200', 'image/png'))
  ->setAccept('text/plain')
  ->run();
id(new DaGdContentTypeTest('/image/10x10.jpg', 'image/jpeg'))
  ->setAccept('text/plain')
  ->run();
id(new DaGdContentTypeTest('/image/30x20.gif?bgcolor=333333', 'image/gif'))
  ->setAccept('text/html')
  ->run();
id(new DaGdResponseCodeTest('/image/300', 400))
  ->run();
id(new DaGdResponseCodeTest('/image/300000000x1212121221', 400))
  ->run();

/************ /status/xxxxxxx/[xxxxxxx] ************/
id(new DaGdResponseCodeTest('/status/400', 400))
  ->run();
id(new DaGdResponseCodeTest('/status/403', 403))
  ->run();
id(new DaGdResponseCodeTest('/status/123/hi', 123))
  ->run();

/************ /isp/[xxxxxxx] ************/
id(new DaGdRegexTest(
  '/isp/127.0.0.1',
  '@^Internet Assigned Numbers Authority@'))
  ->run();
id(new DaGdRegexTest('/isp/69.171.237.16', '@^Facebook, Inc\.$@'))
  ->run();
id(new DaGdRegexTest('/isp/98.27.111.11', '@^Time Warner Cable Internet LLC$@'))
  ->run();
id(
  new DaGdRegexTest(
    '/isp/2001:470:8:624:211c:aaaa:1111:1111',
    '@^Private Customer - Hurricane Electric@'))
  ->run();

/************ /help ************/
id(new DaGdRegexTest('/help', '@pixels: /image/200x400/png@'))
  ->setAccept('text/html')
  ->run();
id(new DaGdRegexTest(
  '/help?url_prefix=dagd%20&url_separator=%20&url_request_sep=%20--',
  '@image 200x400 png@'))
  ->setAccept('text/plain')
  ->run();

/************ /dns/[xxxxxxx] ************/
id(new DaGdRegexTest(
  '/dns/google.com',
  '@IN NS@'))
  ->run();

id(new DaGdRegexTest(
  '/dns/google.com',
  '@IN A@'))
  ->run();

/************ /s/[xxxxxxx] ************/
id(new DaGdRegexTest(
  '/s?url=http://google.com/&shorturl=g',
  '@/g@'))
  ->setTolerateFailure(true)
  ->run();

id(new DaGdRegexTest(
  '/s?url=http://facebook.com/&shorturl=fbook',
  '@/fbook@'))
  ->setTolerateFailure(true)
  ->run();

id(new DaGdResponseCodeTest('/g', 302))
  ->run();

id(new DaGdResponseCodeTest('/g/foo', 302))
  ->run();

id(new DaGdResponseCodeTest('/fbook', 302))
  ->run();

$results = DaGdTest::getResultsSummary();
echo chr(27)."[1;32m Passed        : ".$results['pass'].chr(27)."[0m"."\n";
echo chr(27)."[1;31m Failed        : ".$results['fail'].chr(27)."[0m"."\n";
echo chr(27)."[1;33m Tolerated Fail: ".$results['tolerated fail'].chr(27)."[0m".
  "\n";

if ($results['fail'] === 0) {
  exit(0);
} else {
  exit(1);
}
