FROM registry.access.redhat.com/ubi8/ubi-minimal

MAINTAINER rick@elrod.me

RUN rpm -ivh https://dl.fedoraproject.org/pub/epel/epel-release-latest-8.noarch.rpm

RUN microdnf install -y \
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
        && microdnf reinstall tzdata || microdnf update tzdata \
        && pear install Net_DNS2 \
        && microdnf clean all

ENV DaGdConfigFile ../container/config.container.php

WORKDIR /srv/dagd

EXPOSE 80

ENTRYPOINT ["bash", "./container/entrypoint.sh"]
