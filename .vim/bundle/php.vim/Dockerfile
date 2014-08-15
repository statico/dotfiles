FROM ubuntu:14.04
MAINTAINER Stan Angeloff "stanimir@psp-webtech.co.uk"

RUN echo 'deb http://ppa.launchpad.net/ondrej/php5-5.6/ubuntu trusty main' >> /etc/apt/sources.list && apt-key adv -q --keyserver 'keyserver.ubuntu.com' --recv-keys 'E5267A6C'

RUN apt-get update
RUN apt-get -y install \
    openssl \
;
RUN apt-get -y install \
    php5-cli \
    php5-curl \
    php5-gd \
    php5-gmp \
    php5-intl \
    php5-mcrypt \
    php5-memcache \
    php5-mysql \
    php5-pgsql \
    php5-sqlite \
;

ADD scripts/ /build

WORKDIR /build

ENTRYPOINT ["/usr/bin/php"]
CMD ["update-vim-syntax.php"]
