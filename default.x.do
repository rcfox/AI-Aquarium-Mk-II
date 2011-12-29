redo-ifchange $1.c

CFLAGS="-std=gnu99 -ggdb -Wall `pkg-config --cflags --libs guile-2.0`"
INCLUDE="-I$BASEDIR/libtcod-1.5.1/include -I$BASEDIR"

guile-snarf $CFLAGS $INCLUDE $1.c
