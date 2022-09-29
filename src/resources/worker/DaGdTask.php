<?php

/**
 * Represents a task that can be run by dagd.
 *
 * The general workflow is this:
 * 1) Something subclasses this class and implements the run() method.
 * 2) A controller pushes this particular task onto the queue by inserting a
 *    row into the tasks table containing this class's name.
 * 3) DaGdWorker eventually sees that the task is available, marks it as taken
 *    and runs it by instantiating the class using reflection and calling run().
 * 4) The task is marked as completed and (optionally) its result is stored.
 */
abstract class DaGdTask {
  private $dbh;
  private $id;
  private $unserialized_input;
  private $priority = 50;

  public function __construct($dbh) {
    $this->dbh = $dbh;
  }

  public function setId($id) {
    $this->id = $id;
    return $this;
  }

  public function getId() {
    return $this->id;
  }

  public function getTaskClass() {
    return get_class($this);
  }

  public function setUnserializedInput($input) {
    $this->unserialized_input = $input;
    return $this;
  }

  public function getUnserializedInput() {
    return $this->unserialized_input;
  }

  public function setPriority($priority) {
    $this->priority = $priority;
    return $this;
  }

  public function getPriority() {
    return $this->priority;
  }

  /**
   * Determine a timeout for this task. If the task takes longer than this
   * number of seconds, it will be forcefully killed and the task will be
   * marked as failed. Return null to disable timeouts.
   */
  public function getTimeout() {
    return null;
  }

  /**
   * Override this method and return true if the result should be stored in the
   * database.
   */
  public function shouldStoreResult() {
    return false;
  }

  /**
   * This is the method that is called when the task is run.
   *
   * @return mixed The result of the task.
   */
  abstract public function run($input);

  /**
   * Insert this task into the database.
   *
   * Input is stored after php serialization.
   *
   * Tasks start out as free and are marked as taken when a worker picks them
   * up.
   */
  public function queue() {
    $serialized = serialize($this->getUnserializedInput());
    $task_class = $this->getTaskClass();
    $priority = $this->getPriority();
    $query = $this->dbh->prepare(
      'INSERT INTO tasks (task_class, input, status, priority) '.
      'VALUES (?, ?, \'free\', ?)');
    $query->bind_param('ssi', $task_class, $serialized, $priority);
    $query->execute();
    $query->close();
  }
}
