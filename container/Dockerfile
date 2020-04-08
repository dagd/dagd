#FROM registry.access.redhat.com/ubi8/ubi-minimal
FROM centos:7

MAINTAINER rick@elrod.me

RUN yum install -y \
        httpd \
        php \
        php-mysqlnd \
        php-gd \
        php-pear \
        git \
	nc \
        && yum clean all \
        && pear install Net_DNS2

ENV DaGdConfigFile ../container/config.container.php

WORKDIR /srv/dagd

EXPOSE 80

ENTRYPOINT ["bash", "./container/entrypoint.sh"]
