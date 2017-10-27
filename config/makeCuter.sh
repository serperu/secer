#!/bin/bash

{
	cd ../cuter &&
	./fetch_protoc.sh && 
	git submodule init && git submodule update && 
	git submodule foreach make && 
	autoconf && 
	./configure --with-protoc=$1/cuter/lib/protoc-3.2.0/bin/protoc && 
	make depend && 
	make &&
	rm -f ./config/nocuter.txt 
} || {
	echo "************************************************"
	echo "* CutEr could not be properly installed, SecEr *"
	echo "* is completely functional but this could lead *"
	echo "*            to a lack of precision            *"
	echo "************************************************"
}