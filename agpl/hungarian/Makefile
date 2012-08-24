.PHONY: all clean

PARAMS=-Phungarian -XHungarian_Link=Static_Library

all:
	# NOTE: you don't need to build the library first. Just "with" the project file in your project and choose the appropriate linking type.
	#
	# gprmake builds the C and C++ files
	#
	gprmake ${PARAMS}
	#
	# gnatmake builds all the Ada files and does the linking
	# I think gprmake should do this too, since it does when a main procedure is specified. Bug?
	#
	gnatmake ${PARAMS}

clean:
	rm -f obj/* libstatic/* libdynamic/*
