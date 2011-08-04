<?php
final class DaGdMarkup {
  private $content;
  public $nl2br = true;

  public function __construct($content) {
    $this->content = $content;
  }

  /*
   * [DaGd Github Project](http://github.com/foo/bar)
   * => <a href="http://github.com/foo/bar">DaGd Github Project</a>
   */
  private function linkify() {
    $this->content =  preg_replace(
      '#\[(.+?)]\((.+?)\)#',
      '<a href="$2">$1</a>',
      $this->content);
    return $this;
  }

  /*
   * ***testing!***
   * => <b>testing!</b>
   */
  private function boldify() {
    $this->content = preg_replace(
      '#\*\*\*(.+?)\*\*\*#',
      '<b>$1</b>',
      $this->content);
    return $this;
  }

  /*
   * ___underlined text!___
   * => <u>underlined text!</u>
   */
  private function underlineify() {
    $this->content = preg_replace(
      '#___(.+?)___#',
      '<u>$1</u>',
      $this->content);
    return $this;
  }

  /*
   * ///italics!///
   * => <i>italics!</i>
   */
  private function italify() {
    $this->content = preg_replace(
      '#\/\/\/(.+?)\/\/\/#',
      '<i>$1</i>',
      $this->content);
    return $this;
  }

  private function toString() {
    if ($this->nl2br) {
      return nl2br($this->content);
    } else {
      return $this->content;
    }
  }

  public function render() {
    return $this
      ->linkify()
      ->boldify()
      ->underlineify()
      ->italify()
      ->toString();
  }

}