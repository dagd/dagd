<?php

/**
 * Lightweight wrapper around memcache that also does stats collection.
 *
 * Requires 'memcache' from PECL.
 */
final class DaGdMemcacheCache extends DaGdCache {
  private $is_enabled = false;
  private $checked_enabled = false;
  private $memcache;

  public function __construct() {
    parent::__construct();

    $servers = DaGdConfig::get('cache.memcache_servers');

    $this->memcache = new Memcache();
    foreach ($servers as $server) {
      $this->memcache->addServer($server['host'], $server['port']);
    }
  }

  public function getName() {
    return 'memcache';
  }

  private function defaultFlags() {
    $zlib = DaGdConfig::get('cache.memcache_zlib');
    return $zlib ? MEMCACHE_COMPRESSED : 0;
  }

  public function isEnabled() {
    if (!$this->checked_enabled) {
      $has_extension = extension_loaded('memcache');
      $has_class = class_exists('Memcache');
      $has_fn = function_exists('memcache_add_server');
      $this->checked_enabled = true;
      $this->is_enabled = $has_extension && $has_class && $has_fn;
    }

    return $this->is_enabled;
  }

  // The memcache lib has no special functionality for get-or-store, but our own
  // default will call contains() by default. In the case of memcache, this
  // would lead to us querying memcache twice, which is silly, so we override
  // it.
  public function getOrStore($key, DaGdCacheMissCallback $cb, $ttl = 0) {
    $res = $this->get($key);

    if ($res === false) {
      return $this->set($key, $cb->run(), $ttl);
    }

    return $res;
  }

  public function set($key, $value, $ttl = 0) {
    if ($this->isEnabled()) {
      parent::set($key, $value, $ttl);

      $this->memcache->set($key, $value, $this->defaultFlags(), $ttl);
    }
    return $value;
  }

  public function contains($key) {
    if (!$this->isEnabled()) {
      return false;
    }

    $flags = $this->defaultFlags();
    $orig_flags = $flags;
    $res = $this->memcache->get($key, $flags);
    return $flags !== $orig_flags;
  }

  public function get($key, $default = false) {
    if (!$this->isEnabled()) {
      return $default;
    }

    parent::get($key, $default);

    $flags = $this->defaultFlags();
    $orig_flags = $flags;
    $res = $this->memcache->get($key, $flags);

    if ($flags === $orig_flags) {
      statsd_bump('cache_miss');
      return $default;
    }

    statsd_bump('cache_hit');
    return $res;
  }

  public function flush() {
    if (!$this->isEnabled()) {
      return false;
    }

    parent::flush();
    return $this->memcache->flush();
  }

  public function delete($key) {
    if (!$this->isEnabled()) {
      return false;
    }

    parent::delete($key);
    return $this->memcache->delete($key);
  }
}
