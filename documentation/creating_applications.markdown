Creating Da.Gd Applications
===========================
... is a fun task that should be loved by all.

Basically it comes down to this:

1. Add a route to config.dev.php's `general.routemap` array...

```php
    '/test/(.+)/?$' => array(
      'controller' => 'DaGdTestController',
    ),
```

2. Create the application directory in src/applications. The apps are simple enough that you should be able to copy an existing one and modify it to fit your needs.

The name of controllers referenced in the routemap must be the name of the file they live in.
i.e. `DaGdTestController` should live in `src/applications/test/DaGdTestController.php`.

Remember to check your web server error logs for debugging. Enable `general.debug` in the config to get detailed output (to the browser -- don't enable this in production) about what the app is doing.
