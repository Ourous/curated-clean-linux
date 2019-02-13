
#ifdef KARBON
	struct vd_id {
		FSVolumeRefNum volume_id;
		long directory_id;
	};
#endif

extern void cache_dcl_path (char *file_name,
#ifdef KARBON
	struct vd_id vd_id,struct vd_id clean_system_files_vd_id,
#else
	short wd_ref_num,short clean_system_files_wd_ref_num,
#endif
	FileTime file_time,char *path);

struct search_dcl_path_in_cache_result {
#ifdef KARBON
#else
	short			wd_ref_num;
	short			clean_system_files_wd_ref_num;
#endif
	FileTime	file_time;
	char *			path;
};

extern int search_dcl_path_in_cache (char *file_name,struct search_dcl_path_in_cache_result *r);
extern struct file_block **get_file_blocks_p_of_dcl_file (char *file_name);
#if WRITE_DCL_MODIFICATION_TIME
extern struct file_block **get_file_blocks_p_and_time_of_dcl_file (char *file_name,FileTime *file_time_p);
#endif

extern void clear_path_cache (void);
