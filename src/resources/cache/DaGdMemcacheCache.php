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
  // default will call contains() by default. In the cache of memcache, this
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

      $zlib = DaGdConfig::get('cache.memcache_zlib') ? MEMCACHE_COMPRESSED : 0;
      $this->memcache->set($key, $value, $zlib, $ttl);
    }
    return $value;
  }

  public function contains($key) {
    if ($this->isEnabled()) {
      return $this->memcache->get($key) === false;
    }
    return false;
  }

  public function get($key, $default = false) {
    if (!$this->isEnabled()) {
      return $default;
    }

    parent::get($key, $default);

    $zlib = DaGdConfig::get('cache.memcache_zlib') ? MEMCACHE_COMPRESSED : 0;
    $res = $this->memcache->get($key, $zlib);

    if ($res === false) {
      return $default;
    }

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
