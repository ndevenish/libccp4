#ifndef __GUARD_MAPLIB_LABEL
#define __GUARD_MAPLIB_LABEL

#ifdef __cplusplus
extern "C" {
#endif

int parse_maplabels(CMMFile *mfile);
int write_maplabels(const CMMFile *mfile);


#ifdef __cplusplus
}
#endif

#endif    /* __GUARD_MAPLIB_LABEL */
