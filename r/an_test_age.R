## an permtests for 10pm and ros by age group


library(data.table)


# User written functions --------------------------------------------------
source('r/functions/perm_tests.R')
source('r/functions/summary_dt.R')
source('r/functions/bs_means.R')

# Read in wide data -------------------------------------------------------

ros_wide_age <- qs::qread('data/clean/ros_wide_age.qs')
tenpm_wide_age <- qs::qread('data/clean/tenpm_wide_age.qs')
t1_ld_wide_age <- qs::qread('data/clean/t1_ld_wide_age.qs')

cap_mean <- function(dt, var, n = 200) {
  dt$before <- pmin(dt$before, n)
  dt$after <- pmin(dt$after, n)
  dt$diff <-  dt$after - dt$before 
  dt
}

ros_wide_age <- cap_mean(ros_wide_age)
tenpm_wide_age <- cap_mean(tenpm_wide_age)
t1_ld_wide_age <- cap_mean(tenpm_wide_age)


# Read in long data -------------------------------------------------------
ros_wide_age_05_17 <- ros_wide_age[part_age_group == "5-17" & !is.na(diff)]
ros_wide_age_18_39 <- ros_wide_age[part_age_group == "18-39" & !is.na(diff)]
ros_wide_age_40_59 <- ros_wide_age[part_age_group == "40-59" & !is.na(diff)]
ros_wide_age_60plus <- ros_wide_age[part_age_group == "60+" & !is.na(diff)]
tenpm_wide_age_05_17 <-  tenpm_wide_age[part_age_group == "5-17" & !is.na(diff)]
tenpm_wide_age_18_39 <-  tenpm_wide_age[part_age_group == "18-39" & !is.na(diff)]
tenpm_wide_age_40_59 <-  tenpm_wide_age[part_age_group == "40-59" & !is.na(diff)]
tenpm_wide_age_60plus <- tenpm_wide_age[part_age_group == "60+" & !is.na(diff)]
t1_ld_wide_age_05_17 <-  t1_ld_wide_age[part_age_group == "5-17" & !is.na(diff)]
t1_ld_wide_age_18_39 <-  t1_ld_wide_age[part_age_group == "18-39" & !is.na(diff)]
t1_ld_wide_age_40_59 <-  t1_ld_wide_age[part_age_group == "40-59" & !is.na(diff)]
t1_ld_wide_age_60plus <- t1_ld_wide_age[part_age_group == "60+" & !is.na(diff)]
# Perm_tests --------------------------------------------------------------
ros_perm_05_17 <-    perm_tests(ros_wide_age_05_17, contacts_   = "exclude work and school", label_ = "ROS - 5-17")
ros_perm_18_39 <-    perm_tests(ros_wide_age_18_39, contacts_   = "exclude work and school", label_ = "ROS - 18-39")
ros_perm_40_59 <-    perm_tests(ros_wide_age_40_59, contacts_   = "exclude work and school", label_ = "ROS - 40-59")
ros_perm_60plus <-   perm_tests(ros_wide_age_60plus, contacts_   = "exclude work and school", label_ = "ROS - 60+")
tenpm_perm_05_17 <-    perm_tests(tenpm_wide_age_05_17, contacts_   = "exclude work and school", label_ =  "10pm - 5-17")
tenpm_perm_18_39 <-    perm_tests(tenpm_wide_age_18_39, contacts_   = "exclude work and school", label_ =  "10pm - 18-39")
tenpm_perm_40_59 <-    perm_tests(tenpm_wide_age_40_59, contacts_   = "exclude work and school", label_ =  "10pm - 40-59")
tenpm_perm_60plus <-   perm_tests(tenpm_wide_age_60plus, contacts_   = "exclude work and school", label_ = "10pm - 60+")
t1_ld_perm_05_17 <-    perm_tests(t1_ld_wide_age_05_17, contacts_   =  "exclude school", label_ =  "T1 to LD - 5-17")
t1_ld_perm_18_39 <-    perm_tests(t1_ld_wide_age_18_39, contacts_   =  "exclude school", label_ =  "T1 to LD - 18-39")
t1_ld_perm_40_59 <-    perm_tests(t1_ld_wide_age_40_59, contacts_   =  "exclude school", label_ =  "T1 to LD - 40-59")
t1_ld_perm_60plus <-   perm_tests(t1_ld_wide_age_60plus, contacts_   = "exclude school", label_ =  "T1 to LD - 60+")


ros_age <- rbind(
  ros_perm_05_17, 
  ros_perm_18_39, 
  ros_perm_40_59, 
  ros_perm_60plus)
tenpm_age <- rbind(
  tenpm_perm_05_17, 
  tenpm_perm_18_39, 
  tenpm_perm_40_59, 
  tenpm_perm_60plus)
t1_ld_perm_age <- rbind(
  t1_ld_perm_05_17, 
  t1_ld_perm_18_39, 
  t1_ld_perm_40_59, 
  t1_ld_perm_60plus)

sub_perm <- rbind(ros_age,tenpm_age, t1_ld_perm_age)

write.csv(sub_perm, file = "outputs/sub_test_summary.csv")

