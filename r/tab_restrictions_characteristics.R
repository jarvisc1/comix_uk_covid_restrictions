## Create tables for the restrictions


library(data.table)

source('r/functions/table_creator_res.R')

ld <- readRDS('data/ld.rds')
tenpm <- readRDS('data/10pm.rds')
wfh <- readRDS('data/wfh.rds')
ros <- readRDS('data/rule_of_six.rds')

ld_bf <- ld[first_ld == "before"]
tenpm_bf <- tenpm[wfh_10pm == "before"]
wfh_bf <- wfh[wfh_10pm == "before"]
ros_bf <- ros[rule_of_six == "before"]

ld_bf$total <- "Total"
tenpm_bf$total <- "Total" 
wfh_bf$total <- "Total" 
ros_bf$total <- "Total" 

ld_tab <- table_creator_res(ld_bf, restriction = "Local Lockdown", display = "both")
tenpm_tab <- table_creator_res(tenpm_bf, restriction = "10pm closure", display = "both")
wfh_tab <- table_creator_res(wfh_bf, restriction = "Work from home", display = "both")
ros_tab <- table_creator_res(ros_bf, restriction = "Rule of Six", display = "both")

## WFH has only yes for employment but N come before Y 
## Therefore need to change this for the WFH data to get the correct order
wfh_tab[labnum==4 & valnum == 1]$valnum <- 2


restrictions_tab <- rbind(ld_tab, tenpm_tab, wfh_tab, ros_tab)


res_tab <- dcast(restrictions_tab, labnum + valnum + Label + Value ~ Restriction, value.var = "N", fill = 0)
names(res_tab)
## Order columns
res_tab <- res_tab[,.(labnum, valnum, Label, Value, `Rule of Six`, `10pm closure`, `Work from home`, `Local Lockdown`)]

write.csv(res_tab, "outputs/restrictions_characteristics.csv")


