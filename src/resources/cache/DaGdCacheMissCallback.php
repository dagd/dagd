<?php

interface DaGdCacheMissCallback {
  public function run($key);
}
