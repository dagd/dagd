<?php

final class DaGdImageController extends DaGdController {
  public static function getHelp() {
    return array(
      'title' => 'image',
      'summary' => 'Generate almost-arbitrarily sized images.',
      'path' => 'image',
      'examples' => array(
        array(
          'summary' => 'Generate a PNG that is 200x400 pixels',
          'arguments' => array(
            '200x400',
            'png',
          ),
        ),
        array(
          'summary' => 'Generate a JPEG that is 20x20 pixels with a background',
          'arguments' => array(
            '20*20',
            'jpg',
          ),
          'request' => array(
            'bgcolor' => '7ca931',
          ),
        ),
      ));
  }

  private $width;
  private $height;
  private $bgcolor;
  private $filetype;

  public function finalize() {
    $max_width = DaGdConfig::get('image.max_width');
    $max_height = DaGdConfig::get('image.max_height');
    $default_filetype = DaGdConfig::get('image.default_filetype');
    $imagetypes = DaGdConfig::get('image.imagetypes');
    $fontpath = DaGdConfig::get('image.fontpath');
    $bg_color_rgb = DaGdConfig::get('image.default_bg_rgb');
    $text_color_rgb = DaGdConfig::get('image.default_text_rgb');

    $split = preg_split('@(?:x|\*)@', $this->getRequest()->getRouteComponent(1));
    if (count($split) !== 2) {
      return $this
        ->error(400, 'You must separate width and height with either * or x')
        ->finalize();
    } else {
      $this->width = $split[0];
      $this->height = $split[1];
    }

    if ($this->width > $max_width || $this->height > $max_height) {
      return $this
        ->error(400, 'The generated image should be less than '.
          $max_width.'x'.$max_height.'.')
        ->finalize();
    }

    if (count($this->getRequest()->getRouteMatches()) === 3) {
      $filetypeComponent = $this->getRequest()->getRouteComponent(2);
      if (in_array($filetypeComponent, array_keys($imagetypes))) {
        $this->filetype = $filetypeComponent;
      } else {
        return $this
          ->error(400, 'The image type you specified is not supported.')
          ->finalize();
      }
    } else {
      $this->filetype = $default_filetype;
    }

    $r = $bg_color_rgb[0];
    $g = $bg_color_rgb[1];
    $b = $bg_color_rgb[2];

    if ($bgcolor = request_or_default('bgcolor')) {
      if (strlen($bgcolor) == 6) {
        $r = $bgcolor[0].$bgcolor[1];
        $g = $bgcolor[2].$bgcolor[3];
        $b = $bgcolor[4].$bgcolor[5];
      } elseif (strlen($bgcolor) == 3) {
        $r = $bgcolor[0].$bgcolor[0];
        $g = $bgcolor[1].$bgcolor[1];
        $b = $bgcolor[2].$bgcolor[2];
      }
    }

    $this->bgcolor = array(
      hexdec($r),
      hexdec($g),
      hexdec($b));

    // Generate the image.
    $mimetype = $imagetypes[$this->filetype]['contenttype'];
    $image = imagecreate($this->width, $this->height);
    imagecolorallocate(
      $image,
      $this->bgcolor[0],
      $this->bgcolor[1],
      $this->bgcolor[2]);

    $text = request_or_default('text', $this->width.'x'.$this->height);
    if ($text == 'off') {
      $text = '';
    }

    $positions = imagettfbbox(30, 0, $fontpath, $text);
    $center_x = ceil(($this->width - $positions[2]) / 2);
    $center_y = ceil(($this->height - $positions[5]) / 2);

    $color = imagecolorallocate(
      $image,
      $text_color_rgb[0],
      $text_color_rgb[1],
      $text_color_rgb[2]);

    imagettftext($image, 30, 0, $center_x, $center_y, $color, $fontpath, $text);

    ob_start();
    call_user_func($imagetypes[$this->filetype]['phpfunction'], $image);
    $image_data = ob_get_contents();
    ob_end_clean();

    imagedestroy($image);

    return id(new DaGdCacheableResponse())
      ->setRequest($this->getRequest())
      ->addHeader('Content-Type', $mimetype)
      ->addHeader('X-Content-Type-Options', 'nosniff')
      ->setBody($image_data);
  }
}
