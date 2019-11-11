FROM registry.access.redhat.com/ubi8/ubi-minimal

MAINTAINER rick@elrod.me

RUN microdnf install -y \
        httpd \
        php \
        php-mysqlnd \
        php-gd \
        php-pear \
        git \
        && microdnf clean all \
        && pear install Net_DNS2

ENV DaGdConfigFile ../container/config.container.php

WORKDIR /srv/dagd

EXPOSE 80

ENTRYPOINT ["bash", "./container/entrypoint.sh"]
