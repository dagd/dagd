<?php

/**
 * A basic controller for serving static assets.
 */
final class DaGdStaticController extends DaGdController {
  public static function url($relpath) {
    $dir = DaGdStaticController::assetsDir();
    $path = implode('/', array($dir, trim($relpath, '/')));
    // We only use this as a cache-buster. In the absolute worst case of
    // collision, someone potentially loads an old version of an asset.
    $hash = sha1_file($path);
    return '/static/'.$hash.'/'.$relpath;
  }

  private static function assetsDir() {
    $config_path = DaGdConfig::get('general.static_asset_paths')[0];

    // We expect the configured path to be relative to our "src" directory.
    // But if we're given an absolute path, just use it.
    if ($config_path[0] === '/') {
      return rtrim($config_path, '/').'/';
    } else {
      $src = dirname(dirname(dirname(__FILE__)));
      $allowed_path = implode('/', array($src, trim($config_path, '/')));
      return rtrim($allowed_path, '/').'/';
    }
  }

  public function finalize() {
    $hash = $this->getRequest()->getRouteComponent(1);
    $path = $this->getRequest()->getRouteComponent(2);
    $allowed_exts = DaGdConfig::get('general.static_extensions_whitelist');

    // Basic sanity checking -- these alone are NOT enough to be confident that
    // we prevent directory traversal.
    if (strpos($path, '..') !== false || strpos($path, '//') !== false) {
      return $this->error(404)->finalize();
    }

    // This assumes every static file has an extension, but for now that is a
    // fine assumption to make.
    $path_exp = explode('.', $path);
    $mimetype = idx($allowed_exts, end($path_exp));
    if (!$mimetype) {
      return $this->error(404)->finalize();
    }

    $allowed_path = DaGdStaticController::assetsDir();

    // Now munge it together with the path the user wants.
    $wanted = implode(
      '/',
      array(
        $allowed_path,
        trim($path, '/'),
      )
    );

    // And compare to make sure the wanted starts with the allowed.
    // realpath() will resolve things like '..' and anything else weird and give
    // us back an absolute path.
    $configured_real = realpath($allowed_path);
    $wanted_real = realpath($wanted);

    if (strpos($wanted_real, $configured_real) !== 0) {
      return $this->error(404)->finalize();
    }

    // So the wanted file is in a safe location, but does it exist?
    if (!file_exists($wanted_real) || !is_readable($wanted_real)) {
      return $this->error(404)->finalize();
    }

    // If so, we can probably return it.
    return id(new DaGdCacheableResponse())
      ->setRequest($this->getRequest())
      ->setContentType($mimetype)
      ->setBody(file_get_contents($wanted_real));
  }
}
