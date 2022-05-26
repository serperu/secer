#!/bin/bash

{
	cd cuter &&
	#./fetch_protoc.sh && 
	git submodule init && git submodule update && 
	git submodule foreach make && 
	autoconf && 
	./configure && 
	make depend && 
	make &&
	rm -f ../config/nocuter.txt &&
	echo "************************************" &&
	echo "* SecEr was successfully installed *" &&
	echo "************************************"
} || {
	echo "************************************************"
	echo "* CutEr could not be properly installed, SecEr *"
	echo "* is completely functional but this could lead *"
	echo "*            to a lack of precision            *"
	echo "************************************************"
}
