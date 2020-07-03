<?php

final class DaGdShortenScreenshotController extends DaGdController {
  private function getScreenshot($long_url) {
    $key = 'screenshot_'.hash('sha256', $long_url);
    if ($res = $this->cache()->get($key)) {
      return $res;
    } else {
      $ss_getter = new DaGdShortURLScreenshotGetter($long_url);
      $full_base64 = $ss_getter->getScreenshot();
      $base64 = explode(',', $full_base64, 2)[1];

      // Cache for 6 hours
      return $this->cache()->set($key, $base64, 60 * 60 * 6);
    }
  }

  public function finalize() {
    $short_url = $this->getRequest()->getRouteComponent(1);

    $query = new DaGdShortURLQuery($this);
    $surl = $query->fromShort($short_url);
    $long_url = $surl->getLongUrl();

    $base64 = $this->getScreenshot($long_url);

    // We tell the browser to cache for one hour.
    return id(new DaGdJPEGResponse())
      ->setRequest($this->getRequest())
      ->setBody(base64_decode($base64))
      ->setCacheDuration(60 * 60);
  }
}
