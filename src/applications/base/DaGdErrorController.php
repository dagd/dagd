<?php

abstract class DaGdErrorController extends DaGdController {
  private $message;
  private $html_message;

  /**
   * This message should be text-only.
   */
  public function setMessage($message) {
    $this->message = $message;
    return $this;
  }

  public function getMessage($default = null) {
    return $this->message ? $this->message : $default;
  }

  /**
   * This message can contain a DaGdTag or anything that implements
   * DaGdToTagInterface.
   *
   * The html message is *only* shown on HTML pages, and *in addition* to
   * the regular "message". It can contain extra information and links to
   * resolve issues or learn more about something.
   */
  public function setHtmlMessage($html_message) {
    $this->html_message = $html_message;
    return $this;
  }

  public function getHtmlMessage($default = null) {
    return $this->html_message ? $this->html_message : $default;
  }
}
