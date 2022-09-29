FROM quay.io/centos/centos:stream9

MAINTAINER rick@elrod.me

RUN dnf install -y epel-release

RUN dnf install -y \
        httpd \
        php \
        php-mysqlnd \
        php-gd \
        php-intl \
        php-json \
        php-pear \
        php-pecl-apcu \
        php-pecl-zendopcache \
        git \
        nc \
        tzdata \
        && pear install Net_DNS2 \
        && dnf clean all

ENV DaGdConfigFile ../container/config.container.php

WORKDIR /srv/dagd

EXPOSE 80

ENTRYPOINT ["bash", "./container/entrypoint.sh"]
