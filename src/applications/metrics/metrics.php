<?php
final class DaGdMetricsController extends DaGdBaseClass {
  public function getHelp() {
    // Private endpoint: Intentionally left out of /help
    return array();
  }

  private function shorten_last_action_epoch($action) {
    $column = null;
    $table = null;

    switch ($action) {
    case 'access':
      $column = 'access_dt';
      $table = 'shorturl_access';
      break;
    case 'creation':
      $column = 'creation_dt';
      $table = 'shorturls';
      break;
    default:
      return error500();
    }

    $dt = null;

    // No index on creation_dt, so this will be faster than
    // select max(creation_dt) since it hits the id PKID.
    $query = $this->getReadDB()->prepare(
      'SELECT unix_timestamp('.$column.') FROM '.$table.' order by '.
      'id desc limit 1');
    $query->execute();
    $query->bind_result($dt);
    $query->fetch();
    $query->close();

    if ($dt === null) {
      // No shorturls created yet
      return -1;
    }

    return $dt;
  }

  public function render() {
    $category = $this->route_matches[1];
    $specifier = $this->route_matches[2];

    // Is the client allowed to be here?
    $allowed_ips = DaGdConfig::get('metrics.allowed_ips');
    $client_ip = client_ip();
    if (!in_array($client_ip, $allowed_ips)) {
      return error403();
    }

    switch ($category) {
    case 'shorten':
      switch ($specifier) {
      case 'last_access_epoch':
        return $this->shorten_last_action_epoch('access');
      case 'last_creation_epoch':
        return $this->shorten_last_action_epoch('creation');
      default:
        return error404();
      }
    default:
      return error404();
    }
  }
}
