<?php

final class DaGdImageController extends DaGdBaseClass {
  public static $__help__ = array(
    'summary' => 'Generate arbitrary sized images.',
    'path' => 'image',
    'examples' => array(
      array(
        'summary' => 'Generate a PNG that is 200x400 pixels',
        'arguments' => array(
          '200x400',
          'png',
        ),
      ),
    ));

  private $width;
  private $height;
  private $filetype;

  public function render() {
    $max_width = DaGdConfig::get('image.max_width');
    $max_height = DaGdConfig::get('image.max_height');
    $default_filetype = DaGdConfig::get('image.default_filetype');
    $imagetypes = DaGdConfig::get('image.imagetypes');

    $split = preg_split('@(?:x|\*)@', $this->route_matches[1]);
    if (count($split) !== 2) {
      error400('You must separate width and height with either * or x');
      return false;
    } else {
      $this->width = $split[0];
      $this->height = $split[1];
    }

    if ($this->width > $max_width || $this->height > $max_height) {
      error400(
        'The generated image should be less than '.$max_width.'x'.
        $max_height.'.');
      return false;
    }

    if (count($this->route_matches) === 3) {
      if (in_array($this->route_matches[2], array_keys($imagetypes))) {
        $this->filetype = $this->route_matches[2];
      } else {
        error400('The image type you specified is not supported.');
        return false;
      }
    } else {
      $this->filetype = $default_filetype;
    }

    $this->escape = false;
    $this->wrap_pre = false;
    $this->text_html_strip = false;
    $this->text_content_type = false;

    // Generate the image.
    header('Content-Type: '.$imagetypes[$this->filetype]['contenttype']);
    $image = imagecreate($this->width, $this->height);
    imagecolorallocate($image, 0, 0, 0);
    $text = $this->width.'x'.$this->height;
    $text_color = imagecolorallocate($image, 255, 255, 255);
    imagestring($image, 1, 5, 5, $text, $text_color);
    call_user_func($imagetypes[$this->filetype]['phpfunction'], $image);
    imagedestroy($image);
  }
}