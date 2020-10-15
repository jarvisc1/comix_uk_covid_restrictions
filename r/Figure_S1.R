## fig1a

library(ggplot2)
library(data.table)
library(ggthemr)
library(patchwork)
ggthemr("dust")

## Read in lockdown data
ld <- readRDS('data/ld.rds')
ros <- readRDS('data/rule_of_six.rds')
wfh <- readRDS('data/wfh.rds')
tenpm <- readRDS('data/10pm.rds')


ld_ <- ld[, .(part_id, n_cnt_all, n_cnt_analysis =  n_cnt_not_work_not_school, period = first_ld, restriction = "Local", signchange_lab)]
ros_ <- ros[, .(part_id, n_cnt_all, n_cnt_analysis =  n_cnt_not_work_not_school, period = rule_of_six, restriction = "Rule of six", signchange_lab )]
wfh_ <- wfh[part_employed == "Yes", .(part_id, n_cnt_all, n_cnt_analysis = n_cnt_work, period = wfh_10pm, restriction = "WFH", signchange_lab)]
tenpm_ <- tenpm[, .(part_id, n_cnt_all, n_cnt_analysis= n_cnt_other, period = wfh_10pm, restriction = "10pm", signchange_lab)]


ggdt <- rbind(ld_, ros_, wfh_, tenpm_)



ggdt[, n_cnt_analysis_trim := fifelse(n_cnt_analysis < 10, as.character(n_cnt_analysis), "10+")]


ggdt$n_cnt_analysis_trim <- factor(ggdt$n_cnt_analysis_trim, levels = c(as.character(0:9), "10+"))
ggdt$period <- factor(ggdt$period, levels = c("before", "after"), labels = c("Before", "After"))
ggdt$restriction <- factor(ggdt$restriction, levels = c("Local", "Rule of six", "10pm", "WFH"))


a_dist <- ggplot(ggdt[period == "Before"]) +
  geom_bar(aes(n_cnt_analysis, fill = signchange_lab)) + 
  facet_wrap(restriction~., scales =  "free", nrow = 4) +
  ggtitle("A: Before") +
  ylab("Number of participants") +
  xlab("Number of contacts") +
  labs(fill = "Change in contacts") 
aa_dist <- ggplot(ggdt[period == "After"]) +
  geom_bar(aes(n_cnt_analysis, fill = signchange_lab)) + 
  facet_wrap(restriction~., scales =  "free", nrow = 4) +
  ggtitle("After") +
  ylab("Number of participants") +
  xlab("Number of contacts") +
  labs(fill = "Change in contacts") 


all_dist <- (a_dist | aa_dist)

ld_wide <- readRDS('data/ld_wide.rds')
ros_wide <- readRDS('data/rule_of_six_wide.rds')
wfh_wide <- readRDS('data/wfh_wide.rds')
tenpm_wide <- readRDS('data/10pm_wide.rds')

ld_wide <-       ld_wide[, .(part_id, diff, period = "Difference", restriction = "Local", signchange_lab)] 
ros_wide <-     ros_wide[, .(part_id, diff, period = "Difference", restriction = "Rule of six", signchange_lab)] 
wfh_wide <-     wfh_wide[, .(part_id, diff, period = "Difference", restriction = "WFH", signchange_lab)] 
tenpm_wide <- tenpm_wide[, .(part_id, diff, period = "Difference", restriction = "10pm", signchange_lab)] 

ggdt_wide <- rbind(ld_wide, ros_wide, wfh_wide, tenpm_wide)

ggdt_wide$restriction <- factor(ggdt_wide$restriction, levels = c("Local", "Rule of six", "10pm", "WFH"))


b_diff <- ggplot(ggdt_wide) +
  geom_bar(aes(diff, fill = signchange_lab)) + 
  facet_wrap(restriction~., scales =  "free", ncol = 1) +
  guides(fill="none") +
  ggtitle("B") +
  xlab("Change in contacts") +
  ylab("") 


ggdt_prop <- ggdt_wide[, .(num = .N), by = .(restriction, signchange_lab)]
ggdt_prop[, denom := sum(num), by = .(restriction)]
ggdt_prop[, prop := num/denom,]

ggdt_prop$restriction <- factor(ggdt_prop$restriction, levels = c("WFH", "10pm", "Rule of six", "Local"))

c_sign <- ggplot(ggdt_prop) +
  geom_col(aes(x = restriction, y = prop, fill = signchange_lab)) + 
  guides(fill="none") +
  ggtitle("C") +
  ylab("Proportion changed") +
  xlab("") +
  coord_flip()

c_sign

figS1 <- ((a_dist | b_diff ) / c_sign )+  plot_layout(guides = "collect") + plot_layout(heights = c(5, 1))
figS1

ggsave("outputs/Figure_S1A.png",plot = all_dist, dpi = 500, width = 10, height = 10) 
ggsave("outputs/Figure_S1B.png",plot = b_diff, dpi = 500, width = 10, height = 10) 




