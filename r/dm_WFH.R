## dm_10pm
## Onl keep employed individuaks and create long and wide data for analysis
## Pick all participants but only use other contacts
## Inputs: wfh_10pm.rds
## Outputs: wfh.rds, wfh_wide.rds

wfh_10pm <- readRDS('data/wfh_10pm.rds')

## WFH
#######
## Filter to only employed people
wfh <- wfh_10pm[part_employed == "Yes"]

## Some are not in both now so remove them. 
wfh[, n_wfh := .N, by = part_id]
wfh <- wfh[n_wfh == 2]


## Reshape wide for analysis
wfh_wide <- dcast(wfh, part_id + survey_type  ~ wfh_10pm, value.var = c("n_cnt_work"))
wfh_wide[, diff:= after - before]
wfh_wide[, signchange := sign(diff)]
wfh_wide$signchange_lab <- factor(wfh_wide$signchange, 
                                  levels = c("-1", "0", "1"), 
                                  labels = c("Decreased", "Same", "Increased"))
table(wfh_wide$signchange, wfh_wide$signchange_lab)

## Merge the increase and decrease onto the long dataset.
merge_diff <- wfh_wide[,.(part_id, diff, signchange, signchange_lab)]

nrow(wfh)
wfh <- merge(wfh, merge_diff, by = "part_id", all.x = TRUE)
nrow(wfh)


# Save data
saveRDS(wfh, "data/wfh.rds")
saveRDS(wfh_wide, "data/wfh_wide.rds")
