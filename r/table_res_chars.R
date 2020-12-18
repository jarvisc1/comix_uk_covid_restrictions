## Restrictions


## Create tables for the restrictions


library(data.table)

source('r/functions/table_creator_res.R')

ld <- qs::qread('data/clean/loc.qs')
tenpm <- qs::qread('data/clean/tenpm.qs')
wfh <- qs::qread('data/clean/wfh.qs')
ros <- qs::qread('data/clean/ros.qs')
t1 <- qs::qread('data/clean/t1.qs')
t2 <- qs::qread('data/clean/t2.qs')
t3 <- qs::qread('data/clean/t3.qs')
t1_ld <- qs::qread('data/clean/t1_ld.qs')
t2_ld <- qs::qread('data/clean/t2_ld.qs')
t3_ld <- qs::qread('data/clean/t3_ld.qs')

ld_bf   <- ld[resby == "before"]
tenpm_bf <- tenpm[resby == "before"]
wfh_bf   <- wfh[resby == "before"]
ros_bf   <- ros[resby == "before"]
t1_bf    <- t1[resby == "before"]
t2_bf    <- t2[resby == "before"]
t3_bf    <- t3[resby == "before"]
t1_ld_bf <- t1_ld[resby == "before"]
t2_ld_bf <- t2_ld[resby == "before"]
t3_ld_bf <- t3_ld[resby == "before"]

ld_bf$total <- "Total" 
tenpm_bf$total <- "Total" 
wfh_bf$total <- "Total" 
ros_bf$total <- "Total" 
t1_bf$total <- "Total" 
t2_bf$total <- "Total" 
t3_bf$total <- "Total" 
t1_ld_bf$total <- "Total"
t2_ld_bf$total <- "Total"
t3_ld_bf$total <- "Total"


ld_tab <- table_creator_res(ld_bf, restriction = "Local lockdown", display = "both")
tenpm_tab <- table_creator_res(tenpm_bf, restriction = "10pm closure", display = "both")
wfh_tab <- table_creator_res(wfh_bf, restriction = "Work from home", display = "both")
ros_tab <- table_creator_res(ros_bf, restriction = "Rule of Six", display = "both")
t1_tab <- table_creator_res(t1_bf, restriction = "Entry to T1", display = "both")
t2_tab <- table_creator_res(t2_bf, restriction = "Entry to T2", display = "both")
t3_tab <- table_creator_res(t3_bf, restriction = "Entry to T3", display = "both")
t1_ld_tab <- table_creator_res(t1_ld_bf, restriction = "T1 Exit", display = "both")
t2_ld_tab <- table_creator_res(t2_ld_bf, restriction = "T2 Exit", display = "both")
t3_ld_tab <- table_creator_res(t3_ld_bf, restriction = "T3 Exit", display = "both")



tenpm_tab
wfh_tab
## WFH has only yes for employment but N come before Y 
## Therefore need to change this for the WFH data to get the correct order
wfh_tab[labnum==2]$valnum <- wfh_tab[labnum==2]$valnum + 2
wfh_tab[labnum==2 & valnum > 5]$valnum <- wfh_tab[labnum==2 & valnum > 5]$valnum + 1
wfh_tab[labnum==4 & valnum == 1]$valnum <- 2


restrictions_tab <- rbind(ld_tab, tenpm_tab, wfh_tab, ros_tab, 
                          t1_tab, t2_tab, t3_tab, 
                          t1_ld_tab, t2_ld_tab, t3_ld_tab
                          )


res_tab <- dcast(restrictions_tab, labnum + valnum + Label + Value ~ Restriction, value.var = "N", fill = 0)
names(res_tab)
## Order columns
res_tab <- res_tab[,.(labnum, valnum, Label, Value, `Rule of Six`, `10pm closure`, `Work from home`, `Local lockdown`,
                      `Entry to T1`, `Entry to T2`, `Entry to T3`, `T1 Exit`,  `T2 Exit`, `T3 Exit`)]


write.csv(res_tab, "outputs/restrictions_characteristics.csv")

ld_tab <- table_creator_res(ld_bf, restriction = "Local lockdown", display = "count", missing_ = TRUE)
tenpm_tab <- table_creator_res(tenpm_bf, restriction = "10pm closure", display = "count", missing_ = TRUE)
wfh_tab <- table_creator_res(wfh_bf, restriction = "Work from home", display = "count", missing_ = TRUE)
ros_tab <- table_creator_res(ros_bf, restriction = "Rule of Six", display = "count", missing_ = TRUE)
t1_tab <- table_creator_res(t1_bf, restriction = "Entry to T1", display = "count", missing_ = TRUE)
t2_tab <- table_creator_res(t2_bf, restriction = "Entry to T2", display = "count", missing_ = TRUE)
t3_tab <- table_creator_res(t3_bf, restriction = "Entry to T3", display = "count", missing_ = TRUE)
t1_ld_tab <- table_creator_res(t1_ld_bf, restriction = "T1 Exit", display = "count", missing_ = TRUE)
t2_ld_tab <- table_creator_res(t2_ld_bf, restriction = "T2 Exit", display = "count", missing_ = TRUE)
t3_ld_tab <- table_creator_res(t3_ld_bf, restriction = "T3 Exit", display = "count", missing_ = TRUE)



restrictions_tab <- rbind(ld_tab, tenpm_tab, wfh_tab, ros_tab, 
                          t1_tab, t2_tab, t3_tab, 
                          t1_ld_tab, t2_ld_tab, t3_ld_tab
                          )
names(restrictions_tab)
res_tab <- dcast(restrictions_tab, labnum + valnum + label + values ~ restriction, value.var = "N", fill = 0)
names(res_tab)
## Order columns
res_tab <- res_tab[,.(labnum, valnum, label, values, `Rule of Six`, `10pm closure`, `Work from home`, `Local lockdown`,
                      `Entry to T1`, `Entry to T2`, `Entry to T3`, `T1 Exit`,  `T2 Exit`, `T3 Exit`)]
res_tab

write.csv(res_tab, "outputs/restrictions_characteristics_missing.csv")
