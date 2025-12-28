FROM python:3.7

RUN apt-get update &&\
    apt-get install --no-install-recommends -y \
    build-essential \
    graphviz \
    gcc \
    gfortran \
    libopenmpi-dev \
    libfftw3-dev
RUN pip install --no-cache-dir ford

COPY ford.sh /
RUN ["chmod", "+x", "ford.sh"]
ENTRYPOINT ["/ford.sh"]
