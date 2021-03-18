FROM alpine:3.13

# FUTUREWORK: make a docker image based on nix?

RUN echo "**** install Python, curl, make ****" && \
    apk add --no-cache bash curl python3 make && \
    if [ ! -e /usr/bin/python ]; then ln -sf python3 /usr/bin/python ; fi && \
    \
    echo "*** install poetry ***" && \
    curl -sSL https://raw.githubusercontent.com/python-poetry/poetry/master/get-poetry.py | python3

RUN echo "*** install package dependencies for python libraries (needed for rst2pdf mostly) ***" && \
    apk add --no-cache jpeg-dev alpine-sdk linux-headers zlib-dev python3-dev

COPY pyproject.toml /mnt/
COPY poetry.lock /mnt/

RUN echo "*** poetry install python packages ***" && \
    cd /mnt && source $HOME/.poetry/env && poetry install

WORKDIR /mnt
