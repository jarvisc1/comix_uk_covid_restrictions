library(data.table)

source('r/functions/table_creator_signchange.R')

ld <- readRDS('data/ld.rds')
tenpm <- readRDS('data/10pm.rds')
wfh <- readRDS('data/wfh.rds')
ros <- readRDS('data/rule_of_six.rds')


ld_bf <- ld[first_ld == "before"]
tenpm_bf <- tenpm[wfh_10pm == "before"]
wfh_bf <- wfh[wfh_10pm == "before"]
ros_bf <- ros[rule_of_six == "before"]


ld_tab <- table_creator_signchange(ld_bf, restriction = "Local Lockdown", display = "both")
tenpm_tab <- table_creator_signchange(tenpm_bf, restriction = "10pm closure", display = "both")
wfh_tab <- table_creator_signchange(wfh_bf, restriction = "Work from home", display = "both")
ros_tab <- table_creator_signchange(ros_bf, restriction = "Rule of Six", display = "both")


signchange_tab <- rbind(ld_tab, tenpm_tab, wfh_tab, ros_tab)

write.csv(signchange_tab, "outputs/signchange_characteristics.csv")

