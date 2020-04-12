#!/usr/bin/env php
<?php
// Sorry, this probably isn't as neat as you expected. But this is our test suite. :P

define('SUCCESS', 0);
define('FAILURE', 1);
define('TOLERATED_FAILURE', 2);

class DaGdTestRunner {
  private $concurrency = 5;
  private $pids = array();
  private $tests = array();
  private $last_started = -1;
  private $return_code = 0;

  private $passes = 0;
  private $tolerated_failures = 0;
  private $failures = 0;

  public function getPasses() {
    return $this->passes;
  }

  public function getToleratedFailures() {
    return $this->tolerated_failures;
  }

  public function getFailures() {
    return $this->failures;
  }

  public function getReturnCode() {
    return $this->return_code;
  }

  public function arm($test) {
    $this->tests[] = $test;
    return $this;
  }

  public function setConcurrency($int) {
    $this->concurrency = $int;
    return $this;
  }

  private function handleRC($rc) {
    switch ($rc) {
    case SUCCESS:
      $this->passes++;
      break;
    case TOLERATED_FAILURE:
      $this->tolerated_failures++;
      break;
    default:
      $this->return_code = 1;
      $this->failures++;
    }
  }

  public function run($start_test = 0) {
    // Run preparatory tests first. These have to be done in a particular
    // order, so we can't just throw them to the dogs...er, child processes.
    $remaining_tests = array();
    foreach ($this->tests as $test) {
      if ($test->getPreparatory()) {
        $rc = $test->run();
        $this->handleRC($rc);
      } else {
        $remaining_tests[] = $test;
      }
    }

    while (true) {
      while (count($this->pids) < $this->concurrency) {
        $this->last_started++;
        if ($this->last_started == count($remaining_tests)) {
          // We can't return here because we still need to wait for the last
          // of the threads to clean up. But we can break out, do that final
          // cleanup, and then return.
          break;
        }
        $pid = pcntl_fork();
        if ($pid === -1) {
          echo 'Could not fork child thread.';
          exit(1);
        }
        if ($pid === 0) {
          $exit = SUCCESS;
          $rc = $remaining_tests[$this->last_started]->run();
          if ($rc != SUCCESS) {
            $exit = $rc;
          }
          exit($exit);
        } else {
          $this->pids[] = $pid;
        }
      }

      foreach ($this->pids as $idx => $pid) {
        $status = null;
        pcntl_waitpid($pid, $status);
        $rc = pcntl_wexitstatus($status);
        $this->handleRC($rc);
        unset($this->pids[$idx]);
      }

      if ($this->last_started == count($remaining_tests)) {
        return $this->return_code;
      }
    }
  }
}

abstract class DaGdTest {
  public static $test_url;
  protected $path;
  protected $response;
  protected $headers;
  protected $tolerate_failure;
  protected $accept = '*/*';
  private $original_user_agent;
  private $preparatory = false;

  abstract public function run();

  public function setTolerateFailure($tolerate_failure) {
    $this->tolerate_failure = $tolerate_failure;
    return $this;
  }

  public function setPreparatory($is_preparatory) {
    $this->preparatory = $is_preparatory;
    return $this;
  }

  public function getPreparatory() {
    return $this->preparatory;
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

  protected function fail($text, $exit = false) {
    if ($this->tolerate_failure) {
      echo chr(27)."[1;33m".'*** (tolerable failure) '.$text.' ***'.chr(27)."[0m"."\n";
    } else {
      echo chr(27)."[1;31m".'*** '.$text.' ***'.chr(27)."[0m"."\n";
    }
    if ($exit && !$this->tolerate_failure) {
      echo "Exiting with status code '1' - to bypass a git pre-commit hook,\n";
      echo "pass `--no-verify` to your `git commit` command.\n";
      exit(1);
    }
  }

  protected function pass($text, $exit = false) {
    echo chr(27)."[1;32m".'*** '.$text.' ***'.chr(27)."[0m"."\n";
    if ($exit) {
      exit(0);
    }
  }

  protected function retrieve($ignore_errors = false) {
    $context = stream_context_create(
      array(
        'http' => array(
          'ignore_errors' => $ignore_errors,
          'follow_location' => false,
          'header' => array(
            'Accept: '.$this->accept."\r\n",
          ),
        ),
      )
    );

    $obtain = @file_get_contents($this::$test_url.$this->path, false, $context);

    $this->response = $http_response_header[0];

    foreach (array_slice($http_response_header, 1) as $header) {
      $header_split = explode(': ', $header, 2);
      if (count($header_split) > 1) {
        list($name, $value) = $header_split;
        $this->headers[$name] = $value;
      }
    }

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

  protected function getResponse() {
    if (!$this->response) {
      throw new Exception(
        'Call retrieve() before calling getResponse()!');
    }
    return $this->response;
  }

  protected function test($condition, $summary) {
    if ($condition) {
      $this->pass('Test PASSED ['.$this->path.']: '.$summary);
      return SUCCESS;
    } else {
      $this->fail('Test FAILED ['.$this->path.']: '.$summary);
      if ($this->tolerate_failure) {
        return TOLERATED_FAILURE;
      } else {
        return FAILURE;
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
    $match = preg_match('@'.$this->expected_code.'@', $this->response);
    return $this->test(
      $match,
      'give correct HTTP Response code ('.$this->expected_code.' ?= '.
      $this->response.')');
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

$runner = new DaGdTestRunner();

/*********** / ***********/
$runner->arm(
  id(new DaGdContentTypeTest('/', 'text/plain'))
    ->setAccept('text/plain'));

$runner->arm(
  id(new DaGdContentTypeTest('/', 'text/html'))
    ->setAccept('text/html'));

$runner->arm(
  id(new DaGdRegexTest('/', '@various conditions@')));

/*********** /ip ***********/

// See if /ip contains a number followed by a '.'
$runner->arm(
  id(new DaGdRegexTest('/ip', '@[0-9]\.@')));

/*********** /ec/CodeBlock ***********/

$runner->arm(
  id(new DaGdRegexTest('/ec/CodeBlock', '@^[0-9]+$@')));
$runner->arm(
  id(new DaGdRegexTest('/ec/CodeBlock?lang=en', '@^[0-9]+$@')));
$runner->arm(
  id(new DaGdRegexTest('/ec/CodeBlock?lang=fr', '@^[0-9]+$@')));

/************ /w/xxxxxxx ************/

/* // Ensure that whois is functioning. */
$runner->arm(
  id(new DaGdRegexTest('/w/google.com', '@ns1.google.com@')));
//$runner->arm(
//  id(new DaGdRegexTest('/w/da.gd', '@bill.ns.cloudflare.com@i')));
$runner->arm(
  id(new DaGdRegexTest('/w/4.2.2.2', '@Level 3 Parent@')));
$runner->arm(
  id(new DaGdRegexTest('/w/74.94.18.108', '@OrgAbusePhone@')));
$runner->arm(
  id(
    new DaGdRegexTest(
      '/w/2001:470:8:624:211c:aaaa:1111:1111',
      '@Hurricane Electric@')));
$runner->arm(
  id(
    new DaGdRegexTest(
      '/w/2001:470:8:624:211c:aaaa:1111:1111',
      '@Hurricane Electric@')));
$runner->arm(
  id(
    new DaGdRegexTest(
      '/w/trees.network',
      '@Registrar WHOIS Server: whois.1api.net@')));
$runner->arm(
  id(new DaGdRegexTest('/w/trees.network','@1API GmbH@')));
$runner->arm(
  id(
    new DaGdRegexTest(
      '/w/donuts.co',
      '@Registrant Organization: Donuts Inc.@')));

/************ /headers/xxxxxxx ************/

$runner->arm(
  id(new DaGdRegexTest('/headers', '@Host: @')));
$runner->arm(
  id(new DaGdRegexTest('/headers', '@X-DaGd-Proxy: @', true)));
$runner->arm(
  id(new DaGdRegexTest('/headers/google.com', '@Server: gws@')));
$runner->arm(
  id(new DaGdRegexTest('/headers/http://google.com/', '@Server: gws@')));

/************ /up/xxxxxxx ************/
$runner->arm(
  id(new DaGdRegexTest('/up/google.com', '@^200$@')));
$runner->arm(
  id(new DaGdRegexTest('/up/https://encrypted.google.com/', '@^200$@')));
$runner->arm(
  id(new DaGdRegexTest('/up/http://google.com/404', '@^404$@')));

/************ /et/xxxxxxx ************/
$runner->arm(
  id(new DaGdResponseCodeTest('/et/750009720', 302)));

/************ /host/xxxxxxx ************/
$runner->arm(
  id(new DaGdRegexTest('/host/ipv6.google.com', '@:@')));
$runner->arm(
  id(new DaGdRegexTest('/host/google.com', '@[0-9]\.@')));
$runner->arm(
  id(new DaGdRegexTest('/host/google.com?noipv6', '@:@', true)));
$runner->arm(
  id(new DaGdRegexTest('/host/4.2.2.2', '@b.resolvers.Level3.net@')));

/************ /break/ ************/
$runner->arm(
  id(new DaGdResponseCodeTest('/break', 500)));
$runner->arm(
  id(new DaGdRegexTest('/break', '@An error has occurred@')));

/************ /c/store/xxxxxxx/xxxxxxx ************/
$runner->arm(
  id(
    new DaGdResponseCodeTest(
      '/c/store/g/https://www.google.com/search?q=$PARAMETERS',
      200))
      ->setPreparatory(true)
      ->setTolerateFailure(true));
$runner->arm(
  id(
    new DaGdResponseCodeTest(
      '/c/store/g/https://www.google.com/search?q=$PARAMETERS',
      400)));
$runner->arm(
  id(
    new DaGdRegexTest(
      '/c/store/g1/https://www.google.com/search?q=$PARAMETERS',
      '@Success@'))
      ->setPreparatory(true)
      ->setTolerateFailure(true));
$runner->arm(
  id(
    new DaGdRegexTest(
      '/c/store/g1/https://www.google.com/search?q=$PARAMETERS',
      '@already been defined@')));

/************ /c/xxxxxxx/xxxxxxx ************/
$runner->arm(
  id(new DaGdResponseCodeTest('/c/g/foobar', 302)));
$runner->arm(
  id(new DaGdResponseCodeTest('/c/g%20foobar', 302)));
$runner->arm(
  id(new DaGdResponseCodeTest('/c/nonexistent/foobar', 400)));
$runner->arm(
  id(new DaGdRegexTest('/c/nonexistent/foobar', '@was not found@')));

/************ /c/[xxxxxxx] ************/
$runner->arm(
  id(new DaGdRegexTest('/c/', '@Redirect@')));
$runner->arm(
  id(new DaGdRegexTest('/c/json', '@{"g":@')));
$runner->arm(
  id(new DaGdRegexTest('/c/json/', '@"g1":@')));
$runner->arm(
  id(new DaGdContentTypeTest('/c/json/', 'application/json'))
  ->setAccept('text/plain'));
$runner->arm(
  id(new DaGdContentTypeTest('/c/json', 'application/json'))
  ->setAccept('text/html'));
$runner->arm(
  id(new DaGdContentTypeTest('/c', 'text/html'))
  ->setAccept('text/html'));
$runner->arm(
  id(new DaGdContentTypeTest('/c/', 'text/html'))
  ->setAccept('text/html'));
$runner->arm(
  id(new DaGdContentTypeTest('/c', 'text/plain'))
  ->setAccept('text/plain'));
$runner->arm(
  id(new DaGdContentTypeTest('/c/', 'text/plain'))
  ->setAccept('text/plain'));

/************ /image/xxxxxxx/[xxxxxxx] ************/
$runner->arm(
  id(new DaGdContentTypeTest('/image/200x200', 'image/png'))
  ->setAccept('text/html'));
$runner->arm(
  id(new DaGdContentTypeTest('/image/200x200', 'image/png'))
  ->setAccept('text/plain'));
$runner->arm(
  id(new DaGdContentTypeTest('/image/10x10.jpg', 'image/jpeg'))
  ->setAccept('text/plain'));
$runner->arm(
  id(new DaGdContentTypeTest('/image/30x20.gif?bgcolor=333333', 'image/gif'))
  ->setAccept('text/html'));
$runner->arm(
  id(new DaGdResponseCodeTest('/image/300', 400)));
$runner->arm(
  id(new DaGdResponseCodeTest('/image/300000000x1212121221', 400)));

/************ /status/xxxxxxx/[xxxxxxx] ************/
$runner->arm(
  id(new DaGdResponseCodeTest('/status/400', 400)));
$runner->arm(
  id(new DaGdResponseCodeTest('/status/403', 403)));
$runner->arm(
  id(new DaGdResponseCodeTest('/status/321/hi', 321)));

/************ /isp/[xxxxxxx] ************/
$runner->arm(
  id(
    new DaGdRegexTest(
      '/isp/127.0.0.1',
      '@^Internet Assigned Numbers Authority@')));
$runner->arm(
  id(new DaGdRegexTest('/isp/69.171.237.16', '@Facebook, Inc\.@')));
$runner->arm(
  id(new DaGdRegexTest('/isp/1.1.1.1', '@Cloudflare@')));
$runner->arm(
  id(
    new DaGdRegexTest(
      '/isp/2001:470:8:624:211c:aaaa:1111:1111',
      '@Tunnelbroker@')));

/************ /help ************/
$runner->arm(
  id(new DaGdRegexTest('/help', '@pixels: /image/200x400/png@'))
  ->setAccept('text/html'));
$runner->arm(
  id(
    new DaGdRegexTest(
      '/help?url_prefix=dagd%20&url_separator=%20&url_request_sep=%20--',
      '@image 200x400 png@'))
      ->setAccept('text/plain'));

/************ /dns/[xxxxxxx] ************/
$runner->arm(
  id(new DaGdRegexTest(
  '/dns/google.com',
  '@IN NS@')));

$runner->arm(
  id(new DaGdRegexTest(
  '/dns/google.com',
  '@IN A@')));

/************ /s/[xxxxxxx] ************/
$runner->arm(
  id(
    new DaGdRegexTest(
      '/s?url=http://google.com/&shorturl=g',
      '@/g@'))
      ->setPreparatory(true)
      ->setTolerateFailure(true));

$runner->arm(
  id(
    new DaGdRegexTest(
      '/s?url=http://facebook.com/&shorturl=fbook',
      '@/fbook@'))
      ->setPreparatory(true)
      ->setTolerateFailure(true));

$runner->arm(
  id(new DaGdResponseCodeTest('/g', 302)));

$runner->arm(
  id(new DaGdResponseCodeTest('/g/foo', 302)));

$runner->arm(
  id(new DaGdResponseCodeTest('/fbook', 302)));

/************ ?strip ************/
$runner->arm(
  id(new DaGdRegexTest('/ip', '@\n$@')));
$runner->arm(
  id(new DaGdRegexTest('/ip?strip=0', '@\n$@')));
$runner->arm(
  id(new DaGdRegexTest('/ip?strip', '@[0-9]$@')));
$runner->arm(
  id(new DaGdRegexTest('/ip?strip=1', '@[0-9]$@')));

/************ /roll/ ************/
$runner->arm(
  id(new DaGdResponseCodeTest('/roll/3d', 404)));
$runner->arm(
  id(new DaGdRegexTest('/roll/d9', '@^[0-9]$@')));
$runner->arm(
  id(new DaGdRegexTest('/roll/3d1', '@^3$@')));
$runner->arm(
  id(new DaGdRegexTest('/roll/3d1+3', '@^6$@')));
$runner->arm(
  id(new DaGdRegexTest('/roll/3d1-1', '@^2$@')));
$runner->arm(
  id(new DaGdRegexTest('/roll/3d10', '@^\d+$@')));

$runner->run();

echo chr(27)."[1;32m Passed        : ".$runner->getPasses().chr(27)."[0m"."\n";
echo chr(27)."[1;31m Failed        : ".$runner->getFailures().chr(27)."[0m"."\n";
echo chr(27)."[1;33m Tolerated Fail: ".$runner->getToleratedFailures().chr(27).
  "[0m"."\n";

exit($runner->getReturnCode());
