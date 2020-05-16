<?php

final class DaGdRedirectResponse extends DaGdResponse {
  private $target;

  public function setTarget($target) {
    $this->target = $target;
    return $this;
  }

  public function getTarget() {
    return $this->target;
  }

  public function getHeaders() {
    $headers = array(
      'Location' => $this->getTarget(),
    );
    $headers = array_merge(parent::getHeaders(), $headers);
    return $headers;
  }

  public function getBody() {
    $body = parent::getBody();
    if (!($body instanceof Tag)) {
      throw new Exception(
        'Attempt to render DaGdHTMLResponse but body was not an instance of '.
        'Tag. Did you mean to use DaGdHTMLStringResponse instead?');
    }
    return $body;
  }
}
