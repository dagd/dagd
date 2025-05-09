# da.gd

## What is da.gd?

![dagd-test](https://github.com/dagd/dagd/workflows/dagd-test/badge.svg?branch=master)

da.gd is both a URL shortener and a collection of quick-info tools, written in
PHP and backed by a custom, lightweight framework.

The production site is https://da.gd/ - which primarily brands itself as a URL
shortener. Each URL is checked by multiple safe-browsing APIs in an effort to
combat phishing attempts. Additionally, our volunteer abuse-response team is
very active and quick to take down short URLs in the production site which
lead to phishing sites or other malicious content.

da.gd allows you to use `curl` (or any other http client) to quickly retrieve
various kinds of information such as your IP, useragent, whois for a given
domain or IP, DNS lookups, etc., from an easy-ish-to-remember URL.

Because a goal of this project is to have it work for many purposes/situations,
I encourage feedback, ideas, participation, and interaction with this project.
Have some fun with it :)

Apps are fairly easy to build atop the framework, although the framework is not
currently documented in any meaningful capacity. *I would not suggest using the
framework for real, production sites, there are much better frameworks to use.*
But it's fun to play with and it's been fun to build out.

## What currently works?

`curl da.gd/help` will give you a list of what is currently available on the
production site. The production site is hosted by Tornado VPS and the team there
has been excellent to work with in relaying abuse reports they receive to us.

## Getting a dev environment up

There are now docker-compose files in place in the `container` directory to make
the act of getting a development site up easy.

You can get a quick development environment set up by doing this:

* Ensure you have a working `docker-compose` or `podman-compose` setup.
* Clone the dagd repository
* `cd container`
* For docker-compose: `docker-compose up`
* For podman-compose: `podman-compose up`
* Go to <http://localhost:8080/> in your regular browser.

Note that the development environment is entirely ephemeral. Anything stored in
the database will be lost when you `docker-compose down` or
`podman-compose down`.
That is, **the files in `./container/` are not meant for production use**.

## License

ASL 2.0. See `LICENSE` for more details.
