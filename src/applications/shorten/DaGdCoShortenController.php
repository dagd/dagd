<?php

class DaGdCoShortenController extends DaGdController {
  public static function getHelp() {
    return array(
      'title' => 'coshorten',
      'summary' => 'The dual of a short url is a long url. This gets us back to the original URL.',
      'path' => 'coshorten',
      'examples' => array(
        array(
          'arguments' => array('g'),
          'summary' => 'An example short URL with a custom suffix'),
      ));
  }

  public function execute(DaGdResponse $response) {
    $shorturl = $this->getRequest()->getRoutecomponent(1);

    $query = new DaGdShortURLQuery($this);
    $surl = $query->fromShort($shorturl);

    if ($surl === null) {
      return $this->error(404)->execute();
    }

    $text = $surl->getLongUrl();

    $qs = build_given_querystring();
    if ($path = $this->getRequest()->getRouteComponent(2)) {
      return $text.'/'.$path.$qs;
    } else {
      return $text.$qs;
    }
  }
}
