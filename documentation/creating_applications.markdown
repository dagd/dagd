Creating Da.Gd Applications
===========================
... is a fun task that should be loved by all.

Basically it comes down to this:

1. Add a route to src/route.php

    '/test/(.+)/?$' => 'DaGdTestController',

2. Create the application in src/applications. The apps are simple enough that you should be able to copy an existing one and modify it to fit your needs. NOTE: The main file of the application must be named the same as the directory it is in. For example, 'applications/test/' should have a file inside of it named 'test.php'.

3. Tell the application what 'DaGdTestController' actually is. -- That is, import the application into index.php.

    require_application('test');

Remember to check your web server error logs for debugging. Enable `general.debug` in the config to get detailed output (to the browser -- don't enable this in production) about what the app is doing.
