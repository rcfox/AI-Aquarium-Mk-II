#include <libguile.h>
#include <libtcod.h>

void* register_functions(void* data)
{
	return data;
}

int main (int argc, char* argv[])
{
	scm_with_guile (&register_functions, NULL);
	
	TCOD_console_init_root(80,60,"",0,TCOD_RENDERER_GLSL);
	TCOD_sys_set_fps(30);

	scm_shell (argc, argv);
}
