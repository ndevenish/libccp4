#ifndef __GUARD_MAPLIB_STATS
#define __GUARD_MAPLIB_STATS

#ifdef __cplusplus
extern "C" {
#endif

int stats_update(CMMFile_Stats *stats, void *section_begin,
                         void *section_end);

#ifdef __cplusplus
}
#endif

#endif   /* __GUARD_MAPLIB_STATS */
