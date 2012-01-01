#include <libguile.h>
#include <libtcod.h>
#include "libtcod_util.h"
#include "scm_util.h"

SCM_DEFINE(scm_console_init, "init-console", 2, 2, 0,
           (SCM width, SCM height, SCM title, SCM max_fps),
           "Initialize the libtcod main window.")
{
	char* t = "";
	int fps = 30;
	if(!SCM_UNBNDP(title)) {
		t = scm_to_locale_string(title);
	}
	if(!SCM_UNBNDP(max_fps)) {
		fps = scm_to_int(max_fps);
	}
	TCOD_console_init_root(scm_to_int(width), scm_to_int(height), t, 0, TCOD_RENDERER_GLSL);
	TCOD_sys_set_fps(fps);

	if(!SCM_UNBNDP(title)) {
		free(t);
	}

	return SCM_UNSPECIFIED;
}

SCM_DEFINE(scm_console_put_char_ex, "draw-character", 5, 0, 0,
           (SCM x, SCM y, SCM ch, SCM fore_colour, SCM back_colour),
           "Put a character at a given place on the console.")
{
	SCM_ASSERT_LIST(3,fore_colour);
	SCM_ASSERT_LIST(4,back_colour);

	int f_red   = scm_to_int(scm_list_ref(fore_colour,scm_from_int(0)));
	int f_green = scm_to_int(scm_list_ref(fore_colour,scm_from_int(1)));
	int f_blue  = scm_to_int(scm_list_ref(fore_colour,scm_from_int(2)));

	int b_red   = scm_to_int(scm_list_ref(back_colour,scm_from_int(0)));
	int b_green = scm_to_int(scm_list_ref(back_colour,scm_from_int(1)));
	int b_blue  = scm_to_int(scm_list_ref(back_colour,scm_from_int(2)));

	TCOD_console_put_char_ex(NULL, scm_to_int(x), scm_to_int(y), scm_to_char(scm_char_to_integer(ch)),
	                         TCOD_color_RGB(f_red,f_green,f_blue),TCOD_color_RGB(b_red,b_green,b_blue));

	return SCM_UNSPECIFIED;
}

SCM_DEFINE(scm_console_set_fore, "set-fore-colour!", 3, 0, 0,
           (SCM x, SCM y, SCM fore_colour),
           "Change the foreground colour at a point on the console.")
{
	SCM_ASSERT_LIST(2,fore_colour);

	int f_red   = scm_to_int(scm_list_ref(fore_colour,scm_from_int(0)));
	int f_green = scm_to_int(scm_list_ref(fore_colour,scm_from_int(1)));
	int f_blue  = scm_to_int(scm_list_ref(fore_colour,scm_from_int(2)));

	TCOD_console_set_char_foreground(NULL, scm_to_int(x), scm_to_int(y), TCOD_color_RGB(f_red,f_green,f_blue));

	return SCM_UNSPECIFIED;
}

DEF_LIBTCOD_SCM(BKGND_NONE);
DEF_LIBTCOD_SCM(BKGND_SET);
DEF_LIBTCOD_SCM(BKGND_MULTIPLY);
DEF_LIBTCOD_SCM(BKGND_LIGHTEN);
DEF_LIBTCOD_SCM(BKGND_DARKEN);
DEF_LIBTCOD_SCM(BKGND_SCREEN);
DEF_LIBTCOD_SCM(BKGND_COLOR_DODGE);
DEF_LIBTCOD_SCM(BKGND_COLOR_BURN);
DEF_LIBTCOD_SCM(BKGND_ADD);
DEF_LIBTCOD_SCM(BKGND_ADDA);
DEF_LIBTCOD_SCM(BKGND_BURN);
DEF_LIBTCOD_SCM(BKGND_OVERLAY);
DEF_LIBTCOD_SCM(BKGND_ALPH);
DEF_LIBTCOD_SCM(BKGND_DEFAULT);

SCM_DEFINE(scm_console_set_back, "set-back-colour!", 3, 2, 0,
           (SCM x, SCM y, SCM back_colour, SCM bg_effect, SCM alpha),
           "Change the background colour at a point on the console.")
{
	SCM_ASSERT_LIST(2,back_colour);
	TCOD_bkgnd_flag_t effect = TCOD_BKGND_SET;
	uint a = 0;
	if(!SCM_UNBNDP(bg_effect)) {
		effect = (TCOD_bkgnd_flag_t)scm_to_int(bg_effect);
	}

	if(!SCM_UNBNDP(alpha)) {
		a = (uint)(255*scm_to_double(alpha)) << 8;
	}

	int f_red   = scm_to_int(scm_list_ref(back_colour,scm_from_int(0)));
	int f_green = scm_to_int(scm_list_ref(back_colour,scm_from_int(1)));
	int f_blue  = scm_to_int(scm_list_ref(back_colour,scm_from_int(2)));

	TCOD_console_set_char_background(NULL, scm_to_int(x), scm_to_int(y), TCOD_color_RGB(f_red,f_green,f_blue),effect | a);

	return SCM_UNSPECIFIED;
}

SCM_DEFINE(scm_console_clear, "clear-console", 0, 0, 0,
           (),
           "Clear the console.")
{
	TCOD_console_clear(NULL);

	return SCM_UNSPECIFIED;
}

SCM_DEFINE(scm_console_flush, "flush-console", 0, 0, 0,
           (),
           "Flush the console.")
{
	TCOD_console_flush();

	return SCM_UNSPECIFIED;
}

void init_libtcod_console()
{
#ifndef SCM_MAGIC_SNARFER
#include "libtcod/console.x"
#endif
}
