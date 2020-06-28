<?php

final class DaGdHTMLResponse extends DaGdResponse {
  public function getHeaders() {
    $headers = array(
      'Content-type' => 'text/html; charset=utf-8',
    );
    $headers = array_merge(parent::getHeaders(), $headers);
    return $headers;
  }

  public function getBody() {
    $body = parent::getBody();
    if (!($body instanceof Tag)) {
      $cls = 'non-object';
      if (is_string($body)) {
        $cls = 'string';
      } else {
        $get_cls = get_class($body);
        if ($cls !== false) {
          $cls = $get_cls;
        }
      }
      throw new Exception(
        'Attempt to render DaGdHTMLResponse but body was not an instance of '.
        'Tag. Got a '.$cls.' instead.');
    }
    $body = "<!doctype html>\n".$body->renderSafe();
    return $body;
  }
}
