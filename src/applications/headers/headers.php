<?php
require_once dirname(__FILE__).'/resources/getallheaders.php';
require_once dirname(__FILE__).'/resources/dagd_get_headers.php';

final class DaGdHeadersController extends DaGdBaseClass {
  public $__help__ = array(
    'title' => 'headers',
    'summary' => 'Show HTTP headers for various conditions.',
    'path' => 'headers',
    'examples' => array(
      array(
        'arguments' => null,
        'summary' => 'The headers your browser is sending in its request',
      ),
      array(
        'arguments' => array('google.com'),
        'summary' =>
          'The headers that "http://google.com/" sends, including redirects',
      ),
      array(
        'arguments' => null,
        'request' => array(
          'url' => 'http://google.com',
        ),
        'summary' =>
          'The headers that "http://google.com/" sends, including redirects',
      ),
      array(
        'arguments' => array('http://google.com'),
        'request' => array(
          'redirects' => '0',
        ),
        'summary' => 'The headers that "http://google.com/" sends, without redirects (redirects can be 0 or 1)',
      ),
    ),
  );

  protected $wrap_html = true;

  public function render() {
    // Allow the url to be passed as a request parameter or as a path segment.
    $site = null;

    if (count($this->route_matches) > 1) {
      $site = $this->route_matches[1];
    } else {
      $site = request_or_default('url');
    }

    if (!empty($site)) {
      if (!preg_match('@^https?://@i', $site)) {
        $site = 'http://'.$site;
      }

      $follow_redirects = request_or_default('redirects', true);

      $headers = id(new DaGdHeaderGetter())
        ->setSite($site)
        ->setFollowRedirects($follow_redirects)
        ->requestHeaders();

      if (empty($headers)) {
        error400('Headers could not be retrieved for that domain.');
        return;
      }

      return implode("", $headers);
    } else {
      // If we didn't get a URL passed, then assume the user is asking for the
      // headers they sent. Send them back.
      $headers = getallheaders();
      foreach ($headers as $key => $value) {
        if (server_or_default('HTTP_X_DAGD_PROXY') == "1") {
          if (strpos($key, 'X-Forwarded-') === 0 ||
              $key == 'X-DaGd-Proxy') {
            continue;
          }
        }

        $response .= $key.': '.$value."\n";
      }
    }
    return $response;
  }
}
