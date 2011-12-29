#include <libguile.h>
#include <libtcod.h>

void* register_functions(void* data)
{
	init_libtcod_console();
	init_libtcod_keys();

	init_libtcod_map();
	return data;
}

int main (int argc, char* argv[])
{
	scm_with_guile (&register_functions, NULL);
	
	scm_shell (argc, argv);
}
