<?php

/**
 * The worker looks for available tasks in the tasks table and picks one to
 * work on.
 *
 */
final class DaGdWorker {
  private $dbh;
  private $pids = array();

  public function __construct() {
    $this->dbh = $this->connect();
  }

  private function connect() {
    return DaGdStartup::getWritableDbh();
  }

  private function getTimestampTime() {
    return id(new DateTime())->format('Y-m-d H:i:s');
  }

  /**
   * Look for a free task to work on. If one is found, mark it as in progress
   * and return a DaGdTask that represents it.
   *
   * If no free tasks are found, return null.
   */
  public function getFreeTask() {
    $id = null;
    $task_class = null;
    $priority = null;
    $input = null;

    $this->dbh->begin_transaction();
    $query = $this->dbh->prepare(
      'SELECT id, task_class, priority, input FROM tasks WHERE status = '.
      '\'free\' ORDER BY priority ASC, id ASC LIMIT 1 FOR UPDATE');
    $query->execute();
    $query->bind_result($id, $task_class, $priority, $input);
    $query->fetch();
    $query->close();

    if ($id !== null) {
      $timestamp = $this->getTimestampTime();
      $hostname = gethostname();
      $pid = getmypid();
      $query = $this->dbh->prepare(
        'UPDATE tasks SET status=\'taken\', hostname=?, pid=?, taken_at=? '.
        'WHERE id=?');
      $query->bind_param(
        'siss',
        $hostname,
        $pid,
        $timestamp,
        $id);
      $query->execute();
      $query->close();
    }

    $this->dbh->commit();

    if ($id === null) {
      return null;
    }

    return id(new ReflectionClass($task_class))
      ->newInstance($this->dbh)
      ->setID($id)
      ->setPriority($priority)
      ->setUnserializedInput($input);
  }

  /**
   * Mark a task as complete and optionally store its result.
   */
  public function markTaskComplete(
    $task,
    $successful,
    $unserialized_result,
    $duration
  ) {
    // When child processes exit (which is when this method is called), they
    // will close the database connection. We need to ensure we have one. So
    // we establish a new connection here.
    $dbh = $this->connect();
    $id = $task->getId();
    $timestamp = $this->getTimestampTime();
    $query = null;
    $status = $successful ? 'complete' : 'failed';

    if ($task->shouldStoreResult()) {
      $serialized_result = serialize($unserialized_result);
      $query = $dbh->prepare(
        'UPDATE tasks SET status=?, result=?, completed_at=? '.
        'WHERE id=?');
      $query->bind_param(
        'sssi',
        $status,
        $serialized_result,
        $timestamp,
        $id);
    } else {
      $query = $dbh->prepare(
        'UPDATE tasks SET status=?, completed_at=? WHERE id=?');
      $query->bind_param(
        'ssi',
        $status,
        $timestamp,
        $id);
    }

    $query->execute();
    $query->close();
    $dbh->close();

    echo 'Task '.$task->getId().' ('.$task->getTaskClass().') ';
    echo $status.' at '.$timestamp.' (took '.$duration."ms)\n";
  }

  /**
   * Find a task in the tasks table, or block until there is one to work on.
   */
  public function getTaskOrBlock() {
    $task = $this->getFreeTask();
    while ($task === null) {
      sleep(1);
      $task = $this->getFreeTask();
    }
    echo 'Took task '.$task->getId().' ('.$task->getTaskClass().', priority '.
      $task->getPriority().")\n";
    return $task;
  }

  /**
   * Work on tasks!
   */
  public function loop() {
    while (true) {
      $task = $this->getTaskOrBlock();
      $timeout = $task->getTimeout();
      $successful = null;
      $start = null;
      $end = null;

      // Fork an intermediate process which forks a monitor process and a
      // worker process. The monitor process will kill the worker process if
      // it takes too long to complete.
      $intermediate_pid = pcntl_fork();
      if ($intermediate_pid === -1) {
        throw new Exception('Failed to fork intermediate process!');
      } else if ($intermediate_pid === 0) {
        if ($timeout !== null) {
          $monitor_pid = pcntl_fork();
          if ($monitor_pid === -1) {
            throw new Exception('Failed to fork monitor process!');
          } else if ($monitor_pid === 0) {
            // Monitor process
            sleep($timeout);
            exit(0);
          }
        }

        $worker_pid = pcntl_fork();
        if ($worker_pid === -1) {
          throw new Exception('Failed to fork worker process!');
        } else if ($worker_pid === 0) {
          // Worker process
          try {
            $start = microtime(true);
            $result = $task->run($task->getUnserializedInput());
            $end = microtime(true);
            $successful = true;
          } catch (Exception $e) {
            $end = microtime(true);
            $result = $e;
            $successful = false;
          }
          $duration = ($end - $start) * 1000;
          statsd_time('task_run_duration_'.$task->getTaskClass(), $duration);
          statsd_bump('task_run');
          if (!$successful) {
            statsd_bump('task_run_failed');
          } else {
            statsd_bump('task_run_succeeded');
          }
          // Ensure we have a connection to the database. We inherit the handle
          // from the parent, but it may have been closed by another child
          // exiting. So just make a new one to be safe.
          $this->markTaskComplete($task, $successful, $result, $duration);
          exit(0);
        }

        $worker_status = null;
        if ($timeout === null) {
          pcntl_waitpid($worker_pid, $worker_status);
        } else {
          // See which process exits first
          $monitor_status = null;
          $exited_pid = pcntl_wait($status);

          if ($exited_pid === $worker_pid) {
            // Timeout was not reached, task ended on its own
            // Just kill the monitor process
            echo "Worker fork exited, killing monitor fork\n";
            posix_kill($monitor_pid, SIGKILL);
          } else if ($exited_pid === $monitor_pid) {
            // Timeout was reached, kill the worker process
            echo "Monitor fork exited, killing worker fork\n";
            // We also have to mark the task as failed
            $this->markTaskComplete($task, false, null, 0);
            posix_kill($worker_pid, SIGKILL);
          } else {
            echo "Unknown process exited, weird!\n";
          }

          // In this case, we've waited for one process to exit, so we need to
          // wait for the other one to exit as well.
          $other_status = null;
          pcntl_wait($other_status);
          exit(0);
        }
      }
      $intermediate_status = null;
      pcntl_waitpid($intermediate_pid, $intermediate_status);

      // Re-establish the database connection, it gets closed when the
      // inner processes exit, but we need it for the next loop iteration.
      $this->dbh = $this->connect();
    }
  }
}
