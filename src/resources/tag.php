<?php

// Needs setters/getters if we make this non-final.
final class Tag {
  protected $name;
  protected $body;
  protected $attributes = array();
  protected $cdata;
  protected $render_output = '';

  public function __construct(
    $name,
    $body = null,
    array $attributes = array(),
    $cdata = false) {
    $this->name = $name;
    $this->body = $body;
    $this->attributes = $attributes;
    $this->cdata = $cdata;
  }

  protected function renderPotentialInnerTag($body) {
    // This should let us compose tags that have already been constructed.
    if ($body instanceof Tag) {
      return $body->renderSafe();
    } else {
      if ($this->cdata) {
        // This branch is for CDATA tags (style and script) which don't get
        // escaped.
        return $body;
      } else {
        return htmlspecialchars($body, ENT_HTML5, 'UTF-8');
      }
    }
  }

  public function renderSafe() {
    $attrs = array();
    foreach ($this->attributes as $attr => $val) {
      $attrs[] = $attr.'="'.htmlspecialchars($val).'"';
    }
    $tag = '<'.$this->name;
    if (!empty($attrs)) {
      $tag .= ' '.implode(' ', $attrs);
    }

    // Handle self-closing tags, if $body is exactly null.
    if ($this->body === null) {
      $tag .= '/>';
      return $tag;
    }

    $tag .= '>';

    if (is_array($this->body)) {
      foreach ($this->body as $inner_tag) {
        $tag .= $this->renderPotentialInnerTag($inner_tag);
      }
    } else {
      $tag .= $this->renderPotentialInnerTag($this->body);
    }

    $tag .= '</'.$this->name.'>';
    return $tag;
  }
}

function tag(
  $name,
  $body = null,
  array $attributes = array(),
  $cdata = false) {

  return id(new Tag($name, $body, $attributes, $cdata));
}
