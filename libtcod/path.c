#include <libguile.h>
#include <libtcod.h>
#include "scm_util.h"

static scm_t_bits path_tag;

static size_t scm_free_path(SCM path_smob)
{
	TCOD_path_t p = (TCOD_path_t)SCM_SMOB_DATA(path_smob);
	TCOD_path_delete(p);

	return 0;
}

SCM_DEFINE(scm_make_path, "make-libtcod-path", 1, 1, 0,
           (SCM map_smob, SCM diagonal_cost),
           "")
{
	SCM smob;
	float diag = 1.41f;

	if(!SCM_UNBNDP(diagonal_cost)) {
		diag = scm_to_double(diagonal_cost);
	}
	
	TCOD_map_t map = (TCOD_map_t)SCM_SMOB_DATA(map_smob);
	TCOD_path_t path = (TCOD_path_t)TCOD_path_new_using_map(map,diag);

	SCM_NEWSMOB(smob,path_tag,path);
	return smob;
}

SCM_DEFINE(scm_path_to_list, "path->list", 1, 0, 0,
           (SCM path_smob),
           "")
{
	TCOD_path_t path = (TCOD_path_t)SCM_SMOB_DATA(path_smob);
	SCM list = SCM_EOL;
	int x,y;
	for(int i = 0; i < TCOD_path_size(path); ++i) {
		TCOD_path_get(path,i,&x,&y);
		list = scm_cons(scm_cons(scm_from_int(x),scm_from_int(y)), list);
	}

	return list;
}

SCM_DEFINE(scm_path_compute, "libtcod-path-compute", 3, 0, 0,
           (SCM path_smob, SCM origin, SCM destination),
           "")
{
	SCM_ASSERT_PAIR(1,origin);
	SCM_ASSERT_PAIR(2,destination);
	TCOD_path_t path = (TCOD_path_t)SCM_SMOB_DATA(path_smob);

	int ox = scm_to_int(scm_car(origin));
	int oy = scm_to_int(scm_cdr(origin));
	int dx = scm_to_int(scm_car(destination));
	int dy = scm_to_int(scm_cdr(destination));

	TCOD_path_compute(path,ox,oy,dx,dy);

	return scm_path_to_list(path_smob);
}

SCM_DEFINE(scm_path_walk, "libtcod-path-walk", 1, 1, 0,
           (SCM path_smob, SCM recalculate),
           "")
{
	TCOD_path_t path = (TCOD_path_t)SCM_SMOB_DATA(path_smob);
	bool r = false;
	if(!SCM_UNBNDP(recalculate)) {
		r = scm_to_bool(recalculate);
	}
	int x,y;
	if(TCOD_path_walk(path,&x,&y,r)) {
		return scm_cons(scm_from_int(x),scm_from_int(y));
	} else {
		return scm_from_bool(0);
	}
}

void init_libtcod_path()
{
	path_tag = scm_make_smob_type("libtcod-path", sizeof(TCOD_path_t));

	scm_set_smob_free(path_tag, scm_free_path);
	
#ifndef SCM_MAGIC_SNARFER
#include "libtcod/path.x"
#endif
}
