## an permtests


library(data.table)


# User written functions --------------------------------------------------
source('r/functions/perm_tests.R')
source('r/functions/summary_dt.R')
source('r/functions/bs_means.R')

# Read in wide data -------------------------------------------------------

loc_wide <- qs::qread('data/clean/loc_wide.qs')
ros_wide <- qs::qread('data/clean/ros_wide.qs')
tenpm_wide <- qs::qread('data/clean/tenpm_wide.qs')
wfh_wide <- qs::qread('data/clean/wfh_wide.qs')
t1_wide <- qs::qread('data/clean/t1_wide.qs')
t2_wide <- qs::qread('data/clean/t2_wide.qs')
t3_wide <- qs::qread('data/clean/t3_wide.qs')
t1_ld_wide <- qs::qread('data/clean/t1_ld_wide.qs')
t2_ld_wide <- qs::qread('data/clean/t2_ld_wide.qs')
t3_ld_wide <- qs::qread('data/clean/t3_ld_wide.qs')

check_10 <- function(x) sum(abs(x$diff) <50)/nrow(x)
check_10 <- function(x) sum(abs(x$diff) >50)

check_10(loc_wide)
check_10(ros_wide)
check_10(tenpm_wide)
check_10(wfh_wide)
check_10(t1_wide)
check_10(t2_wide)
check_10(t3_wide)
check_10(t1_ld_wide)
check_10(t2_ld_wide)
check_10(t3_ld_wide)


cap_mean <- function(dt, var, n = 200) {
  dt$before <- pmin(dt$before, n)
  dt$after <- pmin(dt$after, n)
  dt$diff <-  dt$after - dt$before 
  dt
}

loc_wide <- cap_mean(loc_wide)
ros_wide <- cap_mean(ros_wide)
tenpm_wide <- cap_mean(tenpm_wide)
wfh_wide <- cap_mean(wfh_wide)
t1_wide <- cap_mean(t1_wide)
t2_wide <- cap_mean(t2_wide)
t3_wide <- cap_mean(t3_wide)
t1_ld_wide <- cap_mean(t1_ld_wide)
t2_ld_wide <- cap_mean(t2_ld_wide)
t3_ld_wide <- cap_mean(t3_ld_wide)


# This is now setup to do perm tests for the new data. --------------------

loc_perm <-   perm_tests(loc_wide, contacts_   = "exclude work and school", label_ = "Local")
ros_perm <-   perm_tests(ros_wide, contacts_   = "exclude work and school", label_ = "ROS")
tenpm_perm <-   perm_tests(tenpm_wide, contacts_   = "other", label_ = "10pm")
wfh_perm <-   perm_tests(wfh_wide, contacts_   = "work", label_ = "WFH")
t1_perm <-   perm_tests(t1_wide, contacts_   = "exclude work and school", label_ = "T1 entry")
t2_perm <-   perm_tests(t2_wide, contacts_   = "exclude work and school", label_ = "T2 entry")
t3_perm <-   perm_tests(t3_wide, contacts_   = "exclude work and school", label_ = "T3 entry")
t1_ld_perm <-   perm_tests(t1_ld_wide, contacts_   = "exclude school", label_ = "T1 exit to LD")
t2_ld_perm <-   perm_tests(t2_ld_wide, contacts_   = "exclude school", label_ = "T2 exit to LD")
t3_ld_perm <-   perm_tests(t3_ld_wide, contacts_   = "exclude school", label_ = "T3 exit to LD")


nat_res <- rbind(ros_perm, tenpm_perm, wfh_perm)
tier_entry <- rbind(t1_perm, t2_perm, t3_perm)
tier_exit_ld <- rbind(t1_ld_perm, t2_ld_perm, t3_ld_perm)

all_tests <- rbind(loc_perm, nat_res, tier_entry, tier_exit_ld)


all_tests[ ,.(Restriction, Decreased - Increased), ]

all_tests[,1:9]

all_tests[,c(1, 10:15)]

all_tests[,c(16:21)]


qs::qsave(all_tests, file = "outputs/all_test_summary.qs")
