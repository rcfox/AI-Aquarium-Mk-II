#include <libguile.h>
#include <libtcod.h>
#include "libtcod_util.h"
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

DEF_LIBTCOD_SCM(KEY_PRESSED);
DEF_LIBTCOD_SCM(KEY_RELEASED);

SCM_DEFINE(scm_check_keys, "check-keys", 0, 1, 0,
           (SCM flags),
           "Check to see if the user has pressed a key.")
{
	int f = TCOD_KEY_PRESSED;

	if(!SCM_UNBNDP(flags)) {
		f = scm_to_int(flags);
	}
	
	TCOD_key_t key = TCOD_console_check_for_keypress(f);
	return key_to_alist(key);
}

#define SET_VK_HASH(key) scm_hash_set_x(hash,scm_from_int(TCODK_##key),scm_string_downcase(scm_from_locale_string(#key)))
SCM_DEFINE(scm_pop_vk_hash, "populate-virtual-keys!", 1, 0, 0,
           (SCM hash),
           "")
{
	SET_VK_HASH(NONE);
	SET_VK_HASH(ESCAPE);
	SET_VK_HASH(BACKSPACE);
	SET_VK_HASH(TAB);
	SET_VK_HASH(ENTER);
	SET_VK_HASH(SHIFT);
	SET_VK_HASH(CONTROL);
	SET_VK_HASH(ALT);
	SET_VK_HASH(PAUSE);
	SET_VK_HASH(CAPSLOCK);
	SET_VK_HASH(PAGEUP);
	SET_VK_HASH(PAGEDOWN);
	SET_VK_HASH(END);
	SET_VK_HASH(HOME);
	SET_VK_HASH(UP);
	SET_VK_HASH(LEFT);
	SET_VK_HASH(RIGHT);
	SET_VK_HASH(DOWN);
	SET_VK_HASH(PRINTSCREEN);
	SET_VK_HASH(INSERT);
	SET_VK_HASH(DELETE);
	SET_VK_HASH(LWIN);
	SET_VK_HASH(RWIN);
	SET_VK_HASH(APPS);
	SET_VK_HASH(0);
	SET_VK_HASH(1);
	SET_VK_HASH(2);
	SET_VK_HASH(3);
	SET_VK_HASH(4);
	SET_VK_HASH(5);
	SET_VK_HASH(6);
	SET_VK_HASH(7);
	SET_VK_HASH(8);
	SET_VK_HASH(9);
	SET_VK_HASH(KP0);
	SET_VK_HASH(KP1);
	SET_VK_HASH(KP2);
	SET_VK_HASH(KP3);
	SET_VK_HASH(KP4);
	SET_VK_HASH(KP5);
	SET_VK_HASH(KP6);
	SET_VK_HASH(KP7);
	SET_VK_HASH(KP8);
	SET_VK_HASH(KP9);
	SET_VK_HASH(KPADD);
	SET_VK_HASH(KPSUB);
	SET_VK_HASH(KPDIV);
	SET_VK_HASH(KPMUL);
	SET_VK_HASH(KPDEC);
	SET_VK_HASH(KPENTER);
	SET_VK_HASH(F1);
	SET_VK_HASH(F2);
	SET_VK_HASH(F3);
	SET_VK_HASH(F4);
	SET_VK_HASH(F5);
	SET_VK_HASH(F6);
	SET_VK_HASH(F7);
	SET_VK_HASH(F8);
	SET_VK_HASH(F9);
	SET_VK_HASH(F10);
	SET_VK_HASH(F11);
	SET_VK_HASH(F12);
	SET_VK_HASH(NUMLOCK);
	SET_VK_HASH(SCROLLLOCK);
	SET_VK_HASH(SPACE);

	return hash;
}

void init_libtcod_keys()
{
#ifndef SCM_MAGIC_SNARFER
#include "libtcod/keys.x"
#endif
}
