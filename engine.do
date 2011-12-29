for file in `find . -name 'libtcod-*' -prune -o -name 't' -prune -o -name '*.c' -print`; do
	DEPS="$DEPS ${file%.c}.o ";
done

LIBS="-L./libtcod-1.5.1 -ltcod -Wl,-rpath,./libtcod-1.5.1 `pkg-config --cflags --libs guile-2.0`"
CFLAGS=""

BASEDIR=`pwd` redo-ifchange $DEPS

gcc $DEPS $LIBS -o $3
