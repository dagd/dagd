<?php

final class DaGdShortenScreenshotController extends DaGdController {
  private function getScreenshot($long_url) {
    $key = 'screenshot_'.hash('sha256', $long_url);

    // This implements DaGdCacheMissCallback, we can pass it directly to
    // get_or_store.
    $ss_getter = new DaGdShortURLScreenshotGetter($long_url);
    return $this->cache()->getOrStore($key, $ss_getter, 60 * 60 * 6);
  }

  public function finalize() {
    $short_url = $this->getRequest()->getRouteComponent(1);

    $query = new DaGdShortURLQuery($this);
    $surl = $query->fromShort($short_url);

    if ($surl === null) {
      return $this->error(404)->finalize();
    }

    $long_url = $surl->getLongUrl();

    $base64 = $this->getScreenshot($long_url);

    // We tell the browser to cache for one hour.
    return id(new DaGdCacheableResponse())
      ->setRequest($this->getRequest())
      ->setContentType('image/jpeg')
      ->setBody(base64_decode($base64))
      ->setCacheDuration(60 * 60);
  }
}
