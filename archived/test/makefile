#-------------------------------------------------------------------------------

CC     = gcc
CPP    = g++

CFLAGS = -W all -ansi -pedantic -std=c99 -I /usr/local/include/ -I ./include/ -I ../common -I ../rtklib -g -D DEBUGLOM
CPPFLAGS = -W all -ansi -pedantic -std=c++11 -I ./include/ -I ../common -I ../rtklib -I ../3rdparty/ -g -D DEBUGLOM

LDLIBS  = -lm -lpthread #-lrt

.PHONY: clean all directories

all: test_antenna test_config

test_antenna: ./antenna/test_antenna.c
	$(CC) $(CFLAGS) ./antenna/test_antenna.c ../program/antenna.c -o test_antenna $(LDLIBS)

test_config: ./config/test_config.cpp
	$(CPP) $(CPPFLAGS) ./config/test_config.cpp ../common/config.cpp -o test_config $(LDLIBS)

clean:
	rm -f *.o test_rtklib_antenna test_antenna

