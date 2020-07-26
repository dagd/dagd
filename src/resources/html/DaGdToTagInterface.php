<?php

/**
 * A common interface for things which can produce DaGdTag instances.
 *
 * DaGdTag has logic to call into this method if it's given an object that
 * implements this interface.
 */
interface DaGdToTagInterface {
  /**
   * Turns the current object into an instance of DaGdTag.
   */
  public function toTag();
}
