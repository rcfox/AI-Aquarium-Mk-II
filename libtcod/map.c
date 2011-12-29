#include <libguile.h>
#include <libtcod.h>
#include "scm_util.h"

static scm_t_bits map_tag;

static size_t scm_free_map(SCM map_smob)
{
	TCOD_map_t m = (TCOD_map_t)SCM_SMOB_DATA(map_smob);
	TCOD_map_delete(m);

	return 0;
}

SCM_DEFINE(scm_make_map, "make-libtcod-map", 2, 0, 0,
           (SCM width, SCM height),
           "")
{
	SCM smob;
	TCOD_map_t map = (TCOD_map_t)TCOD_map_new(scm_to_int(width),scm_to_int(height));

	SCM_NEWSMOB(smob,map_tag,map);
	return smob;
}

SCM_DEFINE(scm_map_set, "libtcod-map-set!", 5, 0, 0,
           (SCM map_smob, SCM x, SCM y, SCM transparent, SCM walkable),
           "")
{
	TCOD_map_t map = (TCOD_map_t)SCM_SMOB_DATA(map_smob);

	TCOD_map_set_properties(map,scm_to_int(x),scm_to_int(y),scm_to_bool(transparent),scm_to_bool(walkable));

	return SCM_UNSPECIFIED;
}

SCM_DEFINE(scm_map_ref, "libtcod-map-ref", 3, 0, 0,
           (SCM map_smob, SCM x, SCM y),
           "")
{
	TCOD_map_t map = (TCOD_map_t)SCM_SMOB_DATA(map_smob);
	int _x = scm_to_int(x);
	int _y = scm_to_int(y);
	bool transparent = TCOD_map_is_transparent(map,_x,_y);
	bool walkable = TCOD_map_is_walkable(map,_x,_y);

	return scm_cons(scm_from_bool(walkable),scm_from_bool(transparent));
}

SCM_DEFINE(scm_map_dims, "libtcod-map-dimensions", 1, 0, 0,
           (SCM map_smob),
           "")
{
	TCOD_map_t map = (TCOD_map_t)SCM_SMOB_DATA(map_smob);
	int w = TCOD_map_get_width(map);
	int h = TCOD_map_get_height(map);
	return scm_cons(scm_from_int(w),scm_from_int(h));
}

SCM_DEFINE(scm_map_fov, "libtcod-map-fov", 6, 0, 0,
           (SCM map_smob, SCM center_x, SCM center_y, SCM radius, SCM light_walls, SCM fov_type),
           "")
{
	TCOD_map_t map = (TCOD_map_t)SCM_SMOB_DATA(map_smob);
	int x = scm_to_int(center_x);
	int y = scm_to_int(center_y);
	int r = scm_to_int(radius);
	bool walls = scm_to_bool(light_walls);
	TCOD_fov_algorithm_t fov = (TCOD_fov_algorithm_t)scm_to_int(fov_type);
	TCOD_map_compute_fov(map, x, y, r, walls, fov);

	int w = TCOD_map_get_width(map);
	int h = TCOD_map_get_height(map);

	if(r == 0) {
		r = MAX(w,h);
	}

	SCM fov_list = SCM_EOL;
	for(int i = -r; i < r; ++i) {
		for (int j = -r; j < r; ++j) {
			int a = i+x;
			int b = j+y;
			if(a >= 0 && a < w && b >= 0 && b < h) {
				if(TCOD_map_is_in_fov(map,a,b)) {
					SCM coords = scm_cons(scm_from_int(a),scm_from_int(b));
					fov_list = scm_cons(coords,fov_list);
				}
			}
		}
	}

	return fov_list;
}

void init_libtcod_map()
{
	map_tag = scm_make_smob_type("libtcod-map", sizeof(TCOD_map_t));

	scm_set_smob_free(map_tag, scm_free_map);
	
#ifndef SCM_MAGIC_SNARFER
#include "libtcod/map.x"
#endif
}
