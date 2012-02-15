#include <libguile.h>
#include <libtcod.h>
#include "libtcod_util.h"
#include "scm_util.h"

static SCM mouse_to_alist(TCOD_mouse_t mouse)
{
	SCM alist = SCM_EOL;
	alist = scm_acons(scm_from_locale_string("absolute-pos"),scm_cons(scm_from_int(mouse.x),scm_from_int(mouse.y)),alist);
	alist = scm_acons(scm_from_locale_string("absolute-pos-delta"),scm_cons(scm_from_int(mouse.dx),scm_from_int(mouse.dy)),alist);
	alist = scm_acons(scm_from_locale_string("console-pos"),scm_cons(scm_from_int(mouse.cx),scm_from_int(mouse.cy)),alist);
	alist = scm_acons(scm_from_locale_string("console-pos-delta"),scm_cons(scm_from_int(mouse.dcx),scm_from_int(mouse.dcy)),alist);
	alist = scm_acons(scm_from_locale_string("left-pressed"),scm_from_bool(mouse.lbutton),alist);
	alist = scm_acons(scm_from_locale_string("right-pressed"),scm_from_bool(mouse.rbutton),alist);
	alist = scm_acons(scm_from_locale_string("middle-pressed"),scm_from_bool(mouse.mbutton),alist);
	alist = scm_acons(scm_from_locale_string("left-clicked"),scm_from_bool(mouse.lbutton_pressed),alist);
	alist = scm_acons(scm_from_locale_string("right-clicked"),scm_from_bool(mouse.rbutton_pressed),alist);
	alist = scm_acons(scm_from_locale_string("middle-clicked"),scm_from_bool(mouse.mbutton_pressed),alist);
	return alist;
}

SCM_DEFINE(scm_check_mouse, "check-mouse", 0, 0, 0,
           (),
           "Check the mouse status.")
{

	TCOD_mouse_t mouse = TCOD_mouse_get_status();
	return mouse_to_alist(mouse);
}

void init_libtcod_mouse()
{
#ifndef SCM_MAGIC_SNARFER
#include "libtcod/mouse.x"
#endif
}
