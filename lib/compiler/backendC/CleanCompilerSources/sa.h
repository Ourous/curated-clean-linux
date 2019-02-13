
extern Bool DoStrictExportChecks;
extern Bool DoStrictRelated;

extern void StrictnessAnalysis (ImpMod imod);
extern int init_strictness_analysis (ImpMod imod);
extern void do_strictness_analysis (void);
extern void finish_strictness_analysis (void);
extern int StrictnessAnalysisConvertRules (ImpRuleS *rules);
extern void StrictnessAnalysisForRule (SymbDef sdef);
extern void free_unused_sa_blocks (void);

