<?php

final class DaGdCoShortenController extends DaGdBaseClass {
    public function getHelp() {
        return array(
            'title' => 'coshorten',
            'summary' => 'The dual of a shorturl is a long url. This gets us back to the original URL.',
            'path' => 'coshorten',
            'examples' => array(
                array(
                    'arguments' => array('g'),
                    'summary' => 'An example short URL with a custom suffix'),
            ));
    }

  protected $wrap_html = true;
  protected $wrap_pre = false;

  public function render() {
    // Controllers are final so we cannot inherit from DaGdShortenController.
    // We must construct it anew and set it it up manually.
    $longurl = id(new DaGdShortenController())
      ->setRouteMatches($this->route_matches)
      ->setReadDB($this->getReadDB())
      ->setWriteDB($this->getWriteDB());
    $text = $longurl->getLongURL($this->route_matches[1]);
    if ($text === null) {
      return error404();
    }
    $qs = build_given_querystring();
    if ($this->route_matches[2]) {
      return $text.'/'.$this->route_matches[2].$qs;
    } else {
      return $text.$qs;
    }
  }
}
