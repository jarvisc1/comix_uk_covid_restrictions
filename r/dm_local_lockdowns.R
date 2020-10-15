## Local lockdown restrictions
# Identify the areas that went into local lockdowns and when.
## Tag participants before and after the lockdowns date
## Inputs: Local_Lockdown_List.csv, part_with_ld.RDS
## Outputs: ld.rds, ld_wide.rds

library(data.table)

# Read in lockdown info
lldown <- read.csv("data/raw/Local_Lockdown_List.csv")

## Read in participant data
part <- readRDS('data/part_with_ld.RDS')

table(part$start_date_ld)

# There are at most two local lockdowns in each area.
start_dates <- part[local_ld == 1, .(first_sd = min(start_date_ld), second_sd = max(start_date_ld)), by = .(part_id)]
start_dates <- start_dates[!is.na(first_sd)]

# Check
nrow(part)
part <- merge(part, start_dates, by = "part_id", all = TRUE)
nrow(part)
part$first_ld <- fifelse(part$date < part$first_sd, "before", "after")
part$second_ld <- fifelse(part$date < part$second_sd, "before", "after")

table(part$first_ld, useNA = "ifany")
table(part$second_ld, useNA = "ifany")

# Filter data to be +/- 2 weeks of start date of a lockdown
ld <- part[date >= first_sd - 14]

## Choose the closest time before and after the first lockdown
# Get min date and maximum date by person by lockdown
ld[, min_first := min(date), by = .(part_id, first_ld)]
ld[, max_first := max(date), by = .(part_id, first_ld)]
# Count how many times a person appear in lockdown phase
ld[, N_first_ld :=.N , by = .(part_id, first_ld)]

table(ld$N_first_ld)

# If before rule of restriction then choose max date if after choose min date
ld$ld_close_date <- fifelse(ld$first_ld == "before", ld$max_first, ld$min_first)

head(ld[N_first_ld >= 2 & first_ld == "before",
         .(part_id, date, ld_close_date, first_ld, min_first, max_first, first_sd, local_ld)][order(part_id, first_ld)])
head(ld[N_first_ld >= 2 & first_ld == "after",
         .(part_id, date, ld_close_date, first_ld, min_first, max_first, first_sd, local_ld)][order(part_id, first_ld)])

## filter to closest date
ld_onerec <-  ld[date == ld_close_date]

## Check that only one record per person per windows
ld_onerec[, N_ld_onerec := .N, by = .(part_id, first_ld)]
## Check
table(ld_onerec$N_first_ld, ld_onerec$N_ld_onerec)

# Only keep those with two records in total
ld_overall <- ld_onerec[, N_overall := .N, by = .(part_id)]

## Check
table(ld_overall$N_overall)

## Drop unpaired data. 
ld_paired <- ld_overall[N_overall == 2]

## Checks
## Check only 2 records per person
table(ld_paired$N_overall)

## Check that it is
head(ld_paired[order(part_id), .(part_id, first_ld, date, local_ld)])
tail(ld_paired[order(part_id), .(part_id, first_ld, date, local_ld)])

## Check that before is a zero for lockdown and after is a one
table(ld_paired$first_ld, ld_paired$local_ld)


# Reshape for analysis
ld_paired_wide <- dcast(ld_paired, part_id + survey_type  ~ first_ld, value.var = c("n_cnt_not_work_not_school"))
ld_paired_wide[, diff := after - before]
ld_paired_wide[, signchange := sign(diff)]
ld_paired_wide$signchange_lab <- factor(ld_paired_wide$signchange, levels = c("-1", "0", "1"), labels = c("Decreased", "Same", "Increased")) 

## Check
table(ld_paired_wide$signchange, ld_paired_wide$signchange_lab)

## Merge the increase and decrease onto the long dataset.
merge_diff <- ld_paired_wide[,.(part_id, diff, signchange, signchange_lab)]

nrow(ld_paired)
ld_paired <- merge(ld_paired, merge_diff, by = "part_id", all.x = TRUE)
nrow(ld_paired)

# Save data
saveRDS(ld_paired, "data/ld.rds")
saveRDS(ld_paired_wide, "data/ld_wide.rds")
