<?php

/**
 * A lightweight wrapper around caching mechanisms such as APCu or memcache.
 *
 * The pattern of use here is:
 *
 * if ($res = $cache->get($key, $default) !== null) {
 *   return $res;
 * } else {
 *   $res = do_slow_work();
 *   return $cache->set($key, $res);
 * }
 *
 * Care is needed when using get() because if the value in the cache can be
 * whatever the $default is set to (null if not passed), the caller will have
 * no way of knowing if $res was the result of the cache hit or a cache miss
 * which caused get() to return $default.
 */
abstract class DaGdCache {
  /**
   * Name of the cache.
   */
  abstract public function getName();

  /**
   * Is the cache available/enabled/online?
   */
  abstract public function isEnabled();

  /**
   * Try to get a value from the cache. If it exists and is within the TTL,
   * return it. Otherwise, call $cb->run() to get a new value to store in the
   * cache.
   *
   * The default implementation does this manually with contains/get/set. Caches
   * which have this functionality built in (such as APCu's apcu_entry) should
   * override it and use that instead.
   *
   * Note that in theory, this function *could* return invalid data, if the
   * cache value at $key changes between our call to contains() and our call to
   * get(). In practice, memory is fast and this seems unlikely. Nevertheless,
   * override this with a better way of doing this when at all possible.
   */
  public function getOrStore($key, DaGdCacheMissCallback $cb, $ttl = 0) {
    if ($this->contains($key)) {
      return $this->get($key);
    }

    $res = $cb->run($key);
    $this->set($key, $res, $ttl);
    return $res;
  }

  /**
   * Set a key=value pair in the cache, with an optional TTL.
   */
  public function set($key, $value, $ttl = 0) {
    statsd_bump('cache_set');
  }

  /**
   * Determine if a key exists in the cache.
   */
  abstract public function contains($key);

  /**
   * Get a value from the cache if it exists, otherwise return the default.
   */
  public function get($key, $default = false) {
    statsd_bump('cache_get');
  }

  /**
   * Clear the cache.
   */
  public function flush() {
    statsd_bump('cache_flush');
  }

  /**
   * Delete or invalidate an entry from the cache.
   */
  public function delete($key) {
    statsd_bump('cache_delete');
  }
}
