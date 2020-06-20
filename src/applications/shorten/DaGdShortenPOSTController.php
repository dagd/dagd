<?php

require_once dirname(__FILE__).'/resources/random_string.php';

final class DaGdShortenPOSTController extends DaGdController {
  public function render(DaGdHTMLResponse $response) {
    return $this->storeShortUrl($response);
  }

  /*
  public function render(DaGdHTMLResponse $response) {
    $matches = $this->getRequest()->getRouteMatches();
    if ($matches[1]) {
      return $this->getRedirectResponse($matches);
    }

    $template = $this
      ->getBaseTemplate()
      ->setBody($this->form())
      ->getHtmlTag();
    return $response->setBody($template);
  }
  */

  /**
   * Attempts to store a ShortURL after running a plethora of validations on it.
   */
  private function storeShortUrl($response) {
    $given_shorturl = $this->getRequest()->param('shorturl', '');
    $given_shorturl = substr($given_shorturl, 0, 10);
    $given_longurl = $this->getRequest()->param('url');
    $is_custom_url = strlen($given_shorturl) > 0;

    if (!$given_longurl) {
      return $this
        ->error(
          400,
          'Cannot create something out of nothing. No long URL given.')
        ->finalize($response);
    }

    $query = new DaGdShortURLQuery($this);

    // Does the URL at least look somewhat valid?
    if (!preg_match('@^https?://@', $given_longurl)) {
      statsd_bump('shorturl_invalid_scheme');
      return $this
        ->error(400, 'Long URL must have http:// or https:// scheme.')
        ->finalize($response);
    }

    // Is the user's IP banned?
    if ($query->isBannedIP($this->getRequest()->getClientIP())) {
      statsd_bump('shorturl_author_ip_banned');
      return $this->error(403)->finalize($response);
    }

    // Is the long URL whitelisted or blacklisted?
    if (!$query->isWhitelisted($given_longurl)) {
      // It's not whitelisted...
      if ($query->isBlacklisted($given_longurl)) {
        // ...but it *is* blacklisted.
        return $this
          ->error(400, 'Blacklisted long URL.')
          ->finalize($response);
      }
    }

    // Everything is good so far, let's figure out what shorturl to use.
    if ($given_shorturl) {
      // We are given a short URL (as opposed to being asked to generate a
      // random one). First check if the shorturl is valid.
      if (!$query->isValidShortUrl($given_shorturl)) {
        statsd_bump('shorturl_custom_invalid_regex');
        return $this
          ->error(400, 'Custom short URL contained invalid characters.')
          ->finalize($response);
      }

      // Has the URL been taken yet?
      if (!$query->isFreeShortURL($given_shorturl)) {
        statsd_bump('shorturl_custom_not_free');
        return $this
          ->error(400, 'Short URL already taken. Pick a different one.')
          ->finalize($response);
      }

      // Is someone trying to be sneaky and take the URL of another app?
      if ($query->isRouteMapConflict($given_shorturl)) {
        statsd_bump('shorturl_routemap_conflict');
        return $this
          ->error(
            400,
            'Custom short URL conflicts with other da.gd URLs. '.
            'Pick a different one.')
          ->finalize($response);
      }
    } else {
      // We are not given a short URL. If we already have a random URL for the
      // long URL, use it. Otherwise create a new one. We query by hash to sate
      // some indexing. See 1d3974d741eab5765cf9e20ca5e7277be1747699 for
      // details.
      $existing_shorturl = $query->fromLongByHash($given_longurl);
      if ($existing_shorturl) {
        // If we have a match, we are done.
        return id(new DaGdShortenStoredController())
          ->setShortUrl($existing_shorturl)
          ->setRequest($this->getRequest())
          ->finalize($response);
      } else {
        // Otherwise the short URL does not exist in the database. Find a good
        // random string to use.
        $min = DaGdConfig::get('shorten.random_min_length');
        $max = DaGdConfig::get('shorten.random_max_length');
        $given_shorturl = randstr(rand($min, $max));
        while (!$query->isFreeShortURL($given_shorturl)) {
          debug('Hash collision', 'Calling randstr again');
          statsd_bump('shorturl_random_hash_collision');
          $given_shorturl = randstr(rand($min, $max));
        }
      }
      statsd_bump('shorturl_new_random');
    }

    // TODO: This can throw. We render a "pretty" 500, but maybe we should
    // catch and do some extra stuff.
    $surl = $query->store(
      $this->getRequest()->getClientIP(),
      $given_shorturl,
      $given_longurl,
      $is_custom_url);

    return id(new DaGdShortenStoredController())
      ->setShortUrl($surl)
      ->setRequest($this->getRequest())
      ->finalize($response);
  }
}
