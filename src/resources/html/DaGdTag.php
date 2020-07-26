<?php

// Needs setters/getters if we make this non-final.
final class DaGdTag {
  protected $name;
  protected $body;
  protected $attributes = array();
  protected $cdata;
  protected $render_output = '';

  const TAG_ATTR_BARE = 1;

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

  public function addTag(DaGdTag $tag) {
    if (!is_array($this->body)) {
      throw new Exception('Attempt to add DaGdTag to non-array parent');
    }
    $this->body[] = $tag;
    return $this;
  }

  protected function renderPotentialInnerTag($body) {
    // This should let us compose tags that have already been constructed.
    if ($body instanceof DaGdTag) {
      return $body->renderSafe();
    }

    // If we're ever given an object that implements this, just do the right
    // thing.
    if ($body instanceof DaGdToTagInterface) {
      return $body->toTag()->renderSafe();
    }

    if (is_array($body)) {
      $out = '';
      foreach ($body as $tag) {
        $out .= $this->renderPotentialInnerTag($tag);
      }
      return $out;
    }

    if ($this->cdata) {
      // This branch is for CDATA tags (style and script) which don't get
      // escaped.
      return $body;
    }

    return htmlspecialchars($body, ENT_HTML5, 'UTF-8');
  }

  public function renderSafe() {
    $attrs = array();
    foreach ($this->attributes as $attr => $val) {
      $attr_str = $attr;
      if ($val !== self::TAG_ATTR_BARE) {
        if (!is_string($val)) {
          throw new Exception('Attribute value not string: '.class_repr($val));
        }
        $attr_str .= '="'.htmlspecialchars($val).'"';
      }
      $attrs[] = $attr_str;
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
