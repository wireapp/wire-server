FROM alpine:3.10

RUN echo "**** install Python, make ****" && \
    apk add --no-cache bash python3 make && \
    if [ ! -e /usr/bin/python ]; then ln -sf python3 /usr/bin/python ; fi && \
    \
    echo "**** install pip ****" && \
    python3 -m ensurepip && \
    rm -r /usr/lib/python*/ensurepip && \
    pip3 install --no-cache --upgrade pip setuptools wheel && \
    if [ ! -e /usr/bin/pip ]; then ln -s pip3 /usr/bin/pip ; fi && \
    \
    echo "**** install sphinx markdown and awscli ****" && \
    pip install sphinx recommonmark awscli

WORKDIR /mnt
