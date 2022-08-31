<?php

class DaGdSidebar {
  private $links = array();

  public function setLinks($links) {
    $this->links = $links;
    return $this;
  }

  public function addLink($link) {
    $this->links[] = $link;
    return $this;
  }

  public function getLinks() {
    return $this->links;
  }

  public function getStyle() {
    $style = <<<EOD
#app, #sidebar {
  height: 100%;
  display: flex;
  flex-direction: column;
}

#sidebar ul {
  display: inline-flex;
  list-style: none;
}

.content {
  flex-grow: 1;
}
EOD;
  }
}
