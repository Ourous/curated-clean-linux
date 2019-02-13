
extern Bool USE_ABCOPTIONS;
extern char ROOTNAME[FileNameMax];
extern Bool ROOTSET;

extern Bool MakeVerbose;

extern void AddDependency (char *fname);
extern void AddABCInfo (unsigned nr_instr, unsigned nr_desc, unsigned nr_lab);
extern void AddVersionAndOptions (unsigned version, CompilerOptions options);
extern void AddStartLabel (char *label);

extern void PrintProjectInfo (File f);
extern void TouchProjectFile (char *fname);
extern void TouchDependentProjectFiles (char *fname);
extern void SetOptionsOfProjectNode (char *fname, CompilerOptions options);
extern void SetCurrentProjectNode (char *fname);
extern void SetRootOfProject (char *fname);
extern void ConvertOptionsToString (CompilerOptions options, char *optstring);
extern void ConvertOptionStringToOptions (char *optstring, CompilerOptions *options);
extern void MakeOptionsFromCurrentOptions (CompilerOptions *options);

extern void InitProject (void);

extern Bool ProjectIsUpToDate (void);
/* extern Bool BringProjectUpToDate (target_machine_type target_machine); */
extern Bool BuildApplication (target_machine_type target_machine, int cg_flags,
	long h_size, long ab_size, long c_size, long app_size, int link_flags,Bool uptodatemsg);
extern Bool CompileModule (char *icl_file_name);
extern Bool GenerateAssemblyFileForModule  (char *file_name,target_machine_type target_machine,int cg_flags);
extern void ResetProject (void);
extern void FreeProject (void);

/* extern int MakeMirandaToClean (void); */
