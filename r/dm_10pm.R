## dm_10pm
## Create long and wide data for 10pm analysis
## Pick all participants but only use other contacts
## Inputs: wfh_10pm.rds
## Outputs: 10pm.rds, 10pm_wide.rds

tenpm <- readRDS('data/wfh_10pm.rds')


## 10pm rule only contacts outside of the house, work, or school
## Reshape wide for analysis
tenpm_wide <- dcast(tenpm, part_id + survey_type  ~ wfh_10pm, value.var = c("n_cnt_other"))
tenpm_wide[, diff:= after - before]
tenpm_wide[, signchange := sign(diff)]
tenpm_wide$signchange_lab <- factor(tenpm_wide$signchange, 
                                    levels = c("-1", "0", "1"), 
                                    labels = c("Decreased", "Same", "Increased"))
## Check
table(tenpm_wide$signchange, tenpm_wide$signchange_lab)

## Merge the increase and decrease onto the long dataset.
merge_diff <- tenpm_wide[,.(part_id, diff, signchange, signchange_lab)]

nrow(tenpm)
tenpm <- merge(tenpm, merge_diff, by = "part_id", all.x = TRUE)
nrow(tenpm)

# Save data
saveRDS(tenpm, "data/10pm.rds")
saveRDS(tenpm_wide, "data/10pm_wide.rds")


