If you'd like to access the production instance of da.gd from your own domain
for some reason, you can do so fairly easily. This can be done e.g. to brand
your company's domain by, for example, setting up dagd.yourcompany.com.

We also (not yet, as of this writing) use this technique for IPv6 connectivity
to da.gd via its AAAA record. IPv6 traffic is proxied externally, because ec2
doesn't support IPv6.

The setup we use for this, looks like this (Apache Config):

```
<VirtualHost *:80>
  ServerName da.gd
  ServerAlias www.da.gd
  RequestHeader add X-DaGd-Proxy "1"
  ProxyPass / http://da.gd/
  ProxyPassReverse / http://da.gd/
</VirtualHost>
```

Two conditions must be met for proxying to work correctly.

* A `X-DaGd-Proxy` header must be added to inform DaGd that the proxy is
  intentional. We do this for two reasons.
  * Clients using /ip before commit 097a4b49 shouldn't notice any change. Some
    clients' workplaces might route outgoing requests through a proxy. In
    turn, this proxy might add a X-Forwarded-For header, which points to the
    client's internal IP. This isn't what a client expects when they go to /ip
    and without checking for an additional header, this functionality would
    change.
  * We strip some headers from the output of /headers when we detect a
    valid `X-DaGd-Proxy` header, such as X-Forwarded-*, and the `X-DaGd-Proxy`
    header itself. In all other cases, we would want, and expect, to see all
    headers, including X-Forwarded-*.

* The `X-Forwarded-For` header must contain the end user's IP address. This is
  so that things like /ip still work properly, as well as places where a
  user's IP is stored in database or log entries.
