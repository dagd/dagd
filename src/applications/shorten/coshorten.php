<?php

final class DaGdCoShortenController extends DaGdBaseClass {
  public static $__help__ = array(
    'summary' => 'The dual of a shorturl is a long url. This gets us back to the original URL.',
    'path' => 'coshorten',
    'examples' => array(
      array(
        'arguments' => array('g'),
        'summary' => 'An example short URL with a custom suffix'),
    ));

  protected $wrap_pre = false;
  protected $request_methods = array('GET');

  public function render() {
    $longurl = new DagdShortenController();
    $text = $longurl->getLongURL($this->route_matches[1]);
    $qs = build_given_querystring();
    if ($this->route_matches[2]) {
      return $text.'/'.$this->route_matches[2].$qs;
    } else {
      return $text.$qs;
    }
  }
}