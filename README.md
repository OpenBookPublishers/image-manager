OBP Image Manager
=================

Expectation management: this is pre-alpha software, in development, but
under an open source licence so that others may evaluate it in-flight.

Medium-term, the plan is to integrate this into the
[PubSweet](https://coko.foundation/category/pubsweet/) framework;
therefore we shall not accept changes which substantively increase the
effort of doing that.

Configuration
-------------

Edit sys.config to get paths right

    cp app/imagemanager/config/sys.config{_example,}
    cp app/imagemanager/config/vm.args{_example,}

Then edit `app/imagemanager/config/sys.config` and `vm.args` and
replace the paths/cookie in the obvious way; if you don't do this, the
whole thing will blow up on the launch-pad with a message like this:
"The sys.config file specified for this release
(/buildroot/imagemanager/config/sys.config) does not exist!"


Installation
------------

Install dependencies thus:

    wget -O app/rebar3 https://s3.amazonaws.com/rebar3/rebar3
    chmod 755 app/rebar3
    ./manage.sh install-client-deps

Build
-----

    ./manage.sh rebuild-client
    ./manage.sh rebuild-server

Run
---

    docker-compose up

URL
---

[http://localhost:8080/public/index.html](http://localhost:8080/public/index.html)

Lifecycle
---------

There are lots of good lifecycle / debug commands stashed in `manage.sh`
