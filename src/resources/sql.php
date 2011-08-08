<?php

// For better or worse, right now this is our gateway to the world of SQL.
$__db_handler = new mysqli(
    DaGdConfig::get('mysql.host'),
    DaGdConfig::get('mysql.user'),
    DaGdConfig::get('mysql.password'),
    DaGdConfig::get('mysql.database'));
