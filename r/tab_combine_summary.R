## an_combine_summaries


ros <- readRDS('outputs/ros_summary.RDS')
tenpm <- readRDS('outputs/10pm_summary.RDS')
wfh <- readRDS('outputs/wfh_summary.RDS')
ld <- readRDS('outputs/ld_summary.RDS')
ld_sens <- readRDS('outputs/ld_sens_summary.RDS')
summary_combined <- rbind(ros, tenpm, wfh, ld, ld_sens)

write.csv(summary_combined, "outputs/summary_combined.csv")

