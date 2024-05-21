FROM rust:latest

RUN apt-get update && apt-get install -y git

WORKDIR /usr/src/dwarf

RUN git clone https://github.com/uberfoo/dwarf

RUN cd dwarf && cargo xtask install

ENTRYPOINT ["dwarf"]
