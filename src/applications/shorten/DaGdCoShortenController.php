<?php

class DaGdCoShortenController extends DaGdController {
  public static function getHelp() {
    return array(
      'title' => 'coshorten',
      'summary' => 'Render the original (long) URL associated with a short URL.',
      'path' => '',
      'examples' => array(
        array(
          'arguments' => array('g+'),
          'summary' => 'Simply append + to any short URL'
        ),
        array(
          'arguments' => array('coshorten/g'),
          'summary' => 'Alternative with /coshorten',
        ),
      ));
  }

  public function execute(DaGdResponse $response) {
    $shorturl = $this->getRequest()->getRoutecomponent('shorturl');

    $query = new DaGdShortURLQuery($this);

    if ($query->isRouteMapConflict($shorturl)) {
      // If a routemap change trumped an existing shorturl, don't allow the old
      // shorturl to be coshortened. Do this before we hit the db.
      return $this->error(404)->finalize();
    }

    $surl = $query->fromShort($shorturl);

    if ($surl === null) {
      return $this->error(404)->finalize();
    }

    $text = $surl->getLongUrl();

    $qs = build_given_querystring();
    if ($path = $this->getRequest()->getRouteComponent('path')) {
      return $text.'/'.$path.$qs;
    } else {
      return $text.$qs;
    }
  }
}
