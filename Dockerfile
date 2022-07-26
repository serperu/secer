FROM ubuntu:22.04

ENV DEBIAN_FRONTEND="noninteractive" TZ="Europe/London"

RUN apt-get update
RUN export PATH=$HOME/.local/bin:$PATH
RUN apt-get install -y build-essential erlang git wget unzip autoconf python3 python3-pip
RUN apt-get install -y protobuf-compiler libprotobuf-dev vim

RUN pip3 install mypy
RUN pip3 install types-protobuf


# Install Z3
RUN git clone -b z3-4.8.8 --depth 1 https://github.com/Z3Prover/z3.git
RUN cd z3 ; python3 scripts/mk_make.py \
    && cd build ; make \
    && make install 

# # # INSTALL SECER
# ADD "https://www.random.org/cgi-bin/randbyte?nbytes=10&format=h" skipcache

RUN git clone https://github.com/serperu/secer.git

WORKDIR "/secer"

RUN git clone --recursive https://github.com/cuter-testing/cuter.git
RUN cd cuter ; python3 -m pip install -r requirements.txt
RUN make

RUN echo "export PATH=$PATH:/secer" >> ~/.bashrc

