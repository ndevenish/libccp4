#ifndef __GUARD_MAPLIB_HEADER
#define __GUARD_MAPLIB_HEADER

#ifdef __cplusplus
extern "C" {
#endif

int parse_mapheader(CMMFile *mfile);

int write_mapheader(CMMFile *mfile);

#ifdef __cplusplus
}
#endif

#endif  /* __GUARD_MAPLIB_HEADER */
