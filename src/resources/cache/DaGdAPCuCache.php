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

  public function getOrStore($key, DaGdCacheMissCallback $cb, $ttl = 0) {
    if (function_exists('apcu_entry')) {
      // apcu_entry only exists in APCu 5.1+
      return apcu_entry($key, array($cb, 'run'), $ttl);
    }
    return parent::getOrStore($key, $cb, $ttl);
  }

  public function set($key, $value, $ttl = 0) {
    if ($this->isEnabled()) {
      parent::set($key, $value, $ttl);
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

  public function get($key, $default = false) {
    if (!$this->isEnabled()) {
      parent::get($key, $default);
      return $default;
    }

    $res = apcu_fetch($key);

    // Try to be nice. If we get back false, see if it's a "false" that was
    // stored in the cache, or if it means we missed.
    if (!$res) {
      if (!$this->contains($key)) {
        statsd_bump('cache_miss');
        return $default;
      }
    }
    statsd_bump('cache_hit');
    return $res;
  }

  public function flush() {
    parent::flush();

    if (!$this->isEnabled()) {
      return false;
    }

    return apcu_clear_cache();
  }

  public function delete($key) {
    parent::delete($key);

    if (!$this->isEnabled()) {
      return false;
    }

    return apcu_delete($key);
  }
}
