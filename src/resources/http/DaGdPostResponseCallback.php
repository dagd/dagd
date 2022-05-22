<?php

/**
 * This is a callback that can be registered to DaGdResponse to run after the
 * request completes. The 'real' way to do this is with a task queue, but this
 * will work for simple cases where we need to respond more quickly than we have
 * time to process the request.
 *
 * The primary example and usecase for this is for responding to Discord API
 * commands which have a two-second maximum response time. This is too slow for
 * certain administrative queries on a large dataset.
 */
interface DaGdPostResponseCallback {
  public function run($request, $response);
}
