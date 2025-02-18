CREATE TABLE tasks (
  id INT UNSIGNED PRIMARY KEY AUTO_INCREMENT,
  task_class VARCHAR(255) NOT NULL,
  status ENUM('free', 'taken', 'complete', 'failed') NOT NULL DEFAULT 'free',
  input BLOB,
  priority INT(3) NOT NULL DEFAULT 50,
  result BLOB,
  hostname VARCHAR(255),
  pid INT(10),
  created_at TIMESTAMP NOT NULL DEFAULT NOW(),
  taken_at TIMESTAMP,
  completed_at TIMESTAMP,
  INDEX tasks_idx (status, priority)
) ENGINE=InnoDB;
