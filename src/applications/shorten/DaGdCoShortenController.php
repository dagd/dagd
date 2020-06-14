<?php

class DaGdCoShortenController extends DaGdShortenController {
    public static function getHelp() {
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

  public function configure() {
    parent::configure()
      ->setStyle(null);
    return $this;
  }

  public function render() {
    $text = $this->getLongURL($this->route_matches[1]);
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
