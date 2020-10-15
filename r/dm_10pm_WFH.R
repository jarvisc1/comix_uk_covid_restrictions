## 10pm and WFH on 22nd of September
## Tag data for closest record before and after restriction
## Then save together for further scripts as different populations used for 10pm and WFH analysis
## Inputs: part_with_ld.RDS
## Outputs: wfh_10pm.rds

library(data.table)

## Read in participant data
part <- readRDS('data/part_with_ld.RDS')

## Remove lockdown participants
part <- part[local_ld != 1 ]

## Date when 10pm and work from home came in
wfh_10pm_date <- as.Date('2020-09-24')

# Filter data to be +/- 2 weeks
wfh_10pm <- part[between(date, wfh_10pm_date  - 14, wfh_10pm_date  + 14)]

# Tag before and after the rule of six
wfh_10pm$wfh_10pm <- ifelse(wfh_10pm$date < wfh_10pm_date, "before", "after")

## Choose the closest time before and after the wfh and 10pm window

# Get min date and maximum date by person by wfh and 10pm window
wfh_10pm[, min := min(date), by = .(part_id, wfh_10pm)]
wfh_10pm[, max := max(date), by = .(part_id, wfh_10pm)]
# Count how many times a person appear in each wfh and 10pm windowwindow
wfh_10pm[, N_wfh_10pm :=.N , by = .(part_id, wfh_10pm)]

# If before rule of six then choose max date if after choose min date
wfh_10pm$wfh_10pm_close_date <- fifelse(wfh_10pm$wfh_10pm == "before", wfh_10pm$max, wfh_10pm$min)


## Check that the filter works for those with more than one record
head(wfh_10pm[N_wfh_10pm == 2 & wfh_10pm == "before",
              .(part_id, date, wfh_10pm_close_date, wfh_10pm, min, max)][order(part_id, wfh_10pm)])
head(wfh_10pm[N_wfh_10pm == 2 & wfh_10pm == "after",
              .(part_id, date, wfh_10pm_close_date, wfh_10pm, min, max)][order(part_id, wfh_10pm)])

## Keep one record per window per person. 
wfh_10pm_onerec <-  wfh_10pm[date == wfh_10pm_close_date]

## Check that only one record per person per windows
wfh_10pm_onerec[, N_wfh_10pm_onerec := .N, by = .(part_id, wfh_10pm)]
table(wfh_10pm_onerec$N_wfh_10pm, wfh_10pm_onerec$N_wfh_10pm_onerec)

# Only keep those with two records in total
wfh_10pm_overall <- wfh_10pm_onerec[, N_overall := .N, by = .(part_id)]

## Check
table(wfh_10pm_overall$N_overall)

## Drop unpaired data. 
wfh_10pm_paired <- wfh_10pm_overall[N_overall == 2]

## Check
table(wfh_10pm_paired$N_overall)

head(wfh_10pm_paired[order(part_id), .(part_id, wfh_10pm, date)])
tail(wfh_10pm_paired[order(part_id), .(part_id, wfh_10pm, date)])

## Save next step is to split for WFH and 10pm analysis
saveRDS(wfh_10pm_paired, "data/wfh_10pm.rds")

