
#include "compiledefines.h"

#ifdef KARBON
#define TARGET_API_MAC_CARBON 1
#endif

#include "types.t"
#include "system.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <Files.h>
#include "path_cache.h"

struct path_cache_list {
	char *						pcache_path;
#ifdef KARBON
	struct vd_id				pcache_vd_id;
	struct vd_id				pcache_clean_system_files_vd_id;
#else
	short 						pcache_wd_ref_num;
	short						pcache_clean_system_files_wd_ref_num;
#endif
	FileTime					pcache_dcl_time;
	struct path_cache_list *	pcache_next;
	struct file_block *			pcache_file_blocks;
#if defined (__MWERKS__) || defined (__MRC__)
	char						pcache_file_name[];
#else
	char						pcache_file_name[0];
#endif
};

#define BUFFER_SIZE 1024

struct file_block {
	int					file_block_size;
	struct file_block *	file_block_next;
	char				file_block_data[BUFFER_SIZE];
};

static struct path_cache_list *path_cache [32]={
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL
};

static int simple_hash (char *name)
{
	int sum;
	
	sum=0;

	while (*name)
		sum += *name++;

	return sum & 31;
}

void cache_dcl_path (char *file_name,
#ifdef KARBON
	struct vd_id vd_id,struct vd_id clean_system_files_vd_id,
#else
	short wd_ref_num,short clean_system_files_wd_ref_num,
#endif
	FileTime file_time,char *path)
{
	int hash_value,file_name_length;
	struct path_cache_list **pcache_elem_p,*new_pcache_elem;
	
	hash_value=simple_hash (file_name);
	
	for (pcache_elem_p=&path_cache[hash_value]; *pcache_elem_p; 
		pcache_elem_p=&(*pcache_elem_p)->pcache_next)
	{
		if (!strcmp ((*pcache_elem_p)->pcache_file_name,file_name))
			return;
	}
	
	file_name_length=strlen (file_name);
	
	new_pcache_elem=(struct path_cache_list*)Alloc (sizeof (struct path_cache_list)+file_name_length+1,1);

	if (new_pcache_elem!=NULL){
		strcpy (new_pcache_elem->pcache_file_name,file_name);
		new_pcache_elem->pcache_path=path;
#ifdef KARBON
		new_pcache_elem->pcache_vd_id=vd_id;
		new_pcache_elem->pcache_clean_system_files_vd_id=clean_system_files_vd_id;
#else
		new_pcache_elem->pcache_wd_ref_num=wd_ref_num;
		new_pcache_elem->pcache_clean_system_files_wd_ref_num=clean_system_files_wd_ref_num;
#endif
		new_pcache_elem->pcache_dcl_time=file_time;
		new_pcache_elem->pcache_next=NULL;
		new_pcache_elem->pcache_file_blocks=NULL;

		*pcache_elem_p=new_pcache_elem;
	}
}

int search_dcl_path_in_cache (char *file_name,struct search_dcl_path_in_cache_result *r)
{
	int hash_value;
	struct path_cache_list **pcache_elem_p;
	
	hash_value=simple_hash (file_name);
	
	for (pcache_elem_p=&path_cache[hash_value]; *pcache_elem_p; 
		pcache_elem_p=&(*pcache_elem_p)->pcache_next)
	{
		if (!strcmp ((*pcache_elem_p)->pcache_file_name,file_name)){
			struct path_cache_list *pcache_elem;
			
			pcache_elem=*pcache_elem_p;
#ifdef KARBON
/*
			r->fs_spec=pcache_elem->pcache_vd_id;
			r->clean_system_files_fs_spec=pcache_elem->pcache_clean_system_files_fs_spec;
*/
#else
			r->wd_ref_num=pcache_elem->pcache_wd_ref_num;
			r->clean_system_files_wd_ref_num=pcache_elem->pcache_clean_system_files_wd_ref_num;
#endif
			r->file_time=pcache_elem->pcache_dcl_time;
			r->path=pcache_elem->pcache_path;

			return 1;
		}
	}
	
	return 0;
}

#if WRITE_DCL_MODIFICATION_TIME
struct file_block **get_file_blocks_p_and_time_of_dcl_file (char *file_name,FileTime *file_time_p)
{
	int hash_value;
	struct path_cache_list **pcache_elem_p;

	hash_value=simple_hash (file_name);
	
	for (pcache_elem_p=&path_cache[hash_value]; *pcache_elem_p; 
		pcache_elem_p=&(*pcache_elem_p)->pcache_next)
	{
		if (!strcmp ((*pcache_elem_p)->pcache_file_name,file_name)){
			struct path_cache_list *pcache_elem;
			
			pcache_elem=*pcache_elem_p;
			*file_time_p=pcache_elem->pcache_dcl_time;
			return &pcache_elem->pcache_file_blocks;
		}
	}
	
	return NULL;
}
#endif

struct file_block **get_file_blocks_p_of_dcl_file (char *file_name)
{
	int hash_value;
	struct path_cache_list **pcache_elem_p;

	hash_value=simple_hash (file_name);
	
	for (pcache_elem_p=&path_cache[hash_value]; *pcache_elem_p; 
		pcache_elem_p=&(*pcache_elem_p)->pcache_next)
	{
		if (!strcmp ((*pcache_elem_p)->pcache_file_name,file_name))
			return &(*pcache_elem_p)->pcache_file_blocks;
	}
	
	return NULL;
}

extern void clear_inline_cache (void);

void clear_cache (void)
{
	clear_path_cache();
	clear_inline_cache();
	FreePathList();
}

void clear_path_cache (void)
{
	int n;
	
	for (n=0; n<32; ++n){
		struct path_cache_list *pcache_elem,*next_pcache_elem;

		pcache_elem=path_cache[n];
		path_cache[n]=NULL;
		
		while (pcache_elem!=NULL){
			struct file_block *pcache_file_blocks,*next_pcache_file_block;
			
			next_pcache_elem=pcache_elem->pcache_next;
			pcache_file_blocks=pcache_elem->pcache_file_blocks;
			
			Free (pcache_elem);
			
			while (pcache_file_blocks!=NULL){
				next_pcache_file_block=pcache_file_blocks->file_block_next;
				Free (pcache_file_blocks);
				pcache_file_blocks=next_pcache_file_block;
			}
			
			pcache_elem=next_pcache_elem;
		}		
	}
}