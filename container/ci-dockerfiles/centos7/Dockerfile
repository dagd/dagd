#FROM registry.access.redhat.com/ubi8/ubi-minimal
FROM centos:7

MAINTAINER rick@elrod.me

RUN yum install -y epel-release

RUN yum install -y \
        httpd \
        php \
        php-mysqlnd \
        php-gd \
        php-pear-Net-DNS2 \
        php-pecl-apcu \
        php-pecl-zendopcache \
        git \
        nc \
        && yum clean all

ENV DaGdConfigFile ../container/config.container.php

WORKDIR /srv/dagd

EXPOSE 80

ENTRYPOINT ["bash", "./container/entrypoint.sh"]
