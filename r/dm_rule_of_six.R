## Rule of six 14th of September
## Remove lockdown participants and reduce data to +/- 2 weeks to rule of six
## and before and after for participants. Outputs wide and long datasets for analysis
## Inputs: part_with_ld.RDS
## Outputs: rule_of_six.rds, rule_of_six_wide.rds

library(data.table)

## Read in participant data
part <- readRDS('data/part_with_ld.RDS')

## Remove lockdown participants
part <- part[local_ld != 1 ]

## Date when rule of six came in 
rule_of_six_date <- as.Date('2020-09-14')

# Filter data to be +/- 2 weeks
ros <- part[between(date, rule_of_six_date - 14, rule_of_six_date + 14)]

# Tag before and after the rule of six
ros$rule_of_six <- ifelse(ros$date < rule_of_six_date, "before", "after")

## Choose the closest time before and after the rule of six
# Get min date and maximum date by person by rule of six window
ros[, min := min(date), by = .(part_id, rule_of_six)]
ros[, max := max(date), by = .(part_id, rule_of_six)]
# Count how many times a person appear in each rule of six window
ros[, N_ros :=.N , by = .(part_id, rule_of_six)]

# If before rule of six then choose max date if after choose min date
ros$ros_close_date <- fifelse(ros$rule_of_six == "before", ros$max, ros$min)


## Check that the filter works for those with more than one record
head(ros[N_ros == 2 & rule_of_six == "before",
         .(part_id, date, ros_close_date, rule_of_six, min, max)][order(part_id, rule_of_six)])
head(ros[N_ros == 2 & rule_of_six == "after",
         .(part_id, date, ros_close_date, rule_of_six, min, max)][order(part_id, rule_of_six)])

## Keep one record per window per person. 
ros_onerec <-  ros[date == ros_close_date]

## Check that only one record per person per windows
ros_onerec[, N_ros_onerec := .N, by = .(part_id, rule_of_six)]
## Check
table(ros_onerec$N_ros, ros_onerec$N_ros_onerec)

# Only keep those with two records in total
ros_overall <- ros_onerec[, N_overall := .N, by = .(part_id)]

## Check
table(ros_overall$N_overall)

## Drop unpaired data. 
ros_paired <- ros_overall[N_overall == 2]

## Check
table(ros_paired$N_overall)

head(ros_paired[order(part_id), .(part_id, rule_of_six, date)])
tail(ros_paired[order(part_id), .(part_id, rule_of_six, date)])


# Reshape for analysis
ros_paired_wide <- dcast(ros_paired, part_id + survey_type  ~ rule_of_six, value.var = c("n_cnt_not_work_not_school"))
ros_paired_wide[, diff := after - before]
ros_paired_wide[, signchange := sign(diff)]
ros_paired_wide$signchange_lab <- factor(ros_paired_wide$signchange, levels = c("-1", "0", "1"), labels = c("Decreased", "Same", "Increased"))
table(ros_paired_wide$signchange, ros_paired_wide$signchange_lab)

## Merge the increase and decrease onto the long dataset.
merge_diff <- ros_paired_wide[,.(part_id, diff, signchange, signchange_lab)]

nrow(ros_paired)
ros_paired <- merge(ros_paired, merge_diff, by = "part_id", all.x = TRUE)
nrow(ros_paired)

# Save data
saveRDS(ros_paired, "data/rule_of_six.rds")
saveRDS(ros_paired_wide, "data/rule_of_six_wide.rds")
