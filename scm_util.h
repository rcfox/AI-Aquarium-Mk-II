#ifndef SCM_UTIL_H
#define SCM_UTIL_H

#define SCM_ASSERT_PAIR(argnum,arg) ({if(!scm_is_pair((arg))) {	  \
				const char* scm_assert_msg = "Expected pair"; \
				scm_wrong_type_arg(scm_assert_msg,(argnum),(arg)); \
			}})

#define SCM_ASSERT_LIST(argnum,arg) ({if(!scm_to_bool(scm_list_p((arg)))) { \
				const char* scm_assert_msg = "Expected list"; \
				scm_wrong_type_arg(scm_assert_msg,(argnum),(arg)); \
			}})

#endif
