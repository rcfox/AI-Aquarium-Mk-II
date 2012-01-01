#ifndef LIBTCOD_UTIL_H
#define LIBTCOD_UTIL_H

#define DEF_LIBTCOD_SCM(constant) SCM_VARIABLE_INIT(scm_tcod_##constant,#constant,scm_from_int(TCOD_##constant))

#endif
