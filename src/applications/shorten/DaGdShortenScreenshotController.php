<?php

final class DaGdShortenScreenshotController extends DaGdController {
  public function finalize() {
    $short_url = $this->getRequest()->getRouteComponent(1);

    $query = new DaGdShortURLQuery($this);
    $surl = $query->fromShort($short_url);
    $long_url = $surl->getLongUrl();

    $ss_getter = new DaGdShortURLScreenshotGetter($long_url);
    $full_base64 = $ss_getter->getScreenshot();
    $base64 = explode(',', $full_base64, 2)[1];

    // We tell the browser to cache for one hour.
    return id(new DaGdJPEGResponse())
      ->setRequest($this->getRequest())
      ->setBody(base64_decode($base64))
      ->setCacheDuration(60 * 60);
  }
}
