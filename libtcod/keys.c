#include <libguile.h>
#include <libtcod.h>
#include "scm_util.h"

static SCM key_to_alist(TCOD_key_t key)
{
	SCM alist = SCM_EOL;
	alist = scm_acons(scm_from_locale_string("shift"),scm_from_bool(key.shift),alist);
	alist = scm_acons(scm_from_locale_string("rctrl"),scm_from_bool(key.rctrl),alist);
	alist = scm_acons(scm_from_locale_string("ralt"),scm_from_bool(key.ralt),alist);
	alist = scm_acons(scm_from_locale_string("lctrl"),scm_from_bool(key.lctrl),alist);
	alist = scm_acons(scm_from_locale_string("lalt"),scm_from_bool(key.lalt),alist);
	alist = scm_acons(scm_from_locale_string("pressed"),scm_from_bool(key.pressed),alist);
	alist = scm_acons(scm_from_locale_string("c"),scm_integer_to_char(scm_from_char(key.c)),alist);
	alist = scm_acons(scm_from_locale_string("vk"),scm_from_int(key.vk),alist);

	return alist;
}

SCM_DEFINE(scm_wait_keys, "wait-keys", 0, 1, 0,
           (SCM flush),
           "Wait for the user to press a key.")
{
	bool f = true;
	if(!SCM_UNBNDP(flush)) {
		f = scm_to_bool(flush);
	}

	TCOD_key_t key = TCOD_console_wait_for_keypress(f);
	return key_to_alist(key);
}

SCM_DEFINE(scm_check_keys, "check-keys", 0, 1, 0,
           (SCM flags),
           "Check to see if the user has pressed a key.")
{
	int f = TCOD_KEY_PRESSED;

	if(!SCM_UNBNDP(flags)) {
		f = scm_to_int(flags);
	}
	
	TCOD_key_t key = TCOD_console_check_for_keypress(f);
	if(key.vk == TCODK_NONE) {
		return scm_from_bool(0);
	}
	return key_to_alist(key);
}

void init_libtcod_keys()
{
#ifndef SCM_MAGIC_SNARFER
#include "libtcod/keys.x"
#endif
}
