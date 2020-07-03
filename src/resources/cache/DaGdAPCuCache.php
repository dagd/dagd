<?php

/**
 * Lightweight wrapper around APCu that also does stats collection.
 */
final class DaGdAPCuCache extends DaGdCache {
  private $is_enabled = false;
  private $checked_enabled = false;

  public function getName() {
    return 'APCu';
  }

  public function isEnabled() {
    if (!$this->checked_enabled) {
      $has_extension = extension_loaded('apc') || extension_loaded('apcu');
      $has_config = ini_get('apc.enabled');
      $this->checked_enabled = true;
      $this->is_enabled = $has_extension && $has_config;
    }

    return $this->is_enabled;
  }

  public function set($key, $value, $ttl = 0) {
    parent::set($key, $value, $ttl);

    if ($this->isEnabled()) {
      statsd_bump('cache_set');
      apcu_store($key, $value, $ttl);
    }
    return $value;
  }

  public function contains($key) {
    if ($this->isEnabled()) {
      return apcu_exists($key);
    }
    return false;
  }

  public function get($key, $default = null) {
    parent::get($key, $default);

    if (!$this->isEnabled()) {
      return $default;
    }

    $res = apcu_fetch($key);

    // Try to be nice. If we get back false, see if it's a "false" that was
    // stored in the cache, or if it means we missed.
    if (!$res) {
      if ($this->contains($key)) {
        return $res;
      }
      return $default;
    }
    return $res;
  }
}
