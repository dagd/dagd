<?php
require_once dirname(__FILE__).'/resources/getallheaders.php';
require_once dirname(__FILE__).'/resources/dagd_get_headers.php';

final class DaGdHeadersController extends DaGdController {
  public static function getHelp() {
    return array(
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
          'summary' =>
            'The headers that "http://google.com/" sends, without redirects '.
            '(redirects can be 0 or 1)',
        ),
      ),
    );
  }

  /**
   * Format a header from 'X_FOO_BAR' to 'X-Foo-Bar'.
   */
  private function formatHeaderName($name) {
    $name = strtolower($name);
    $name = str_replace('_', ' ', $name);
    $name = ucwords($name);
    $name = str_replace(' ', '-', $name);
    return $name;
  }

  public function execute(DaGdResponse $response) {
    // Allow the url to be passed as a request parameter or as a path segment.
    $site = null;

    if (count($this->getRequest()->getRouteMatches()) > 1) {
      $site = $this->getRequest()->getRouteMatches()[1];
    } else {
      $site = $this->getRequest()->getParamOrDefault('url');
    }

    if (!empty($site)) {
      if (!preg_match('@^https?://@i', $site)) {
        $site = 'http://'.$site;
      }

      $follow_redirects = $this
        ->getRequest()
        ->getParamOrDefault('redirects', true);

      $headers = id(new DaGdHeaderGetter())
        ->setSite($site)
        ->setFollowRedirects($follow_redirects)
        ->requestHeaders();

      if (empty($headers)) {
        return $this
          ->error(400, 'Headers could not be retrieved for that domain.')
          ->execute($response);
      }

      return implode("", $headers);
    } else {
      // If we didn't get a URL passed, then assume the user is asking for the
      // headers they sent. Send them back.
      $headers = $this->getRequest()->getHeaders();
      $response = '';
      foreach ($headers as $key => $value) {
        $hdrkey = $this->formatHeaderName($key);
        if ($this->getRequest()->getHeader('X-DaGd-Proxy') === '1' &&
            (strpos($hdrkey, 'X-Forwarded-') === 0 ||
             $hdrkey == 'Dagd-Proxy')) {
          continue;
        }
        $response .= $hdrkey.': '.$value."\n";
      }
    }
    return $response;
  }
}
