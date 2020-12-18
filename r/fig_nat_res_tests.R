## fig1a

library(ggplot2)
library(data.table)
library(ggthemr)
library(patchwork)
ggthemr("dust")

## Read in lockdown data
ld <-    qs::qread('data/clean/loc.qs')
ros <-   qs::qread('data/clean/ros.qs')
wfh <-   qs::qread('data/clean/wfh.qs')
tenpm <- qs::qread('data/clean/tenpm.qs')

ld_ <- ld[,       .(part_id, n_cnt, n_cnt_analysis = n_cnt_not_work_not_school, period = resby, restriction = "Local",       signchange_lab)]
ros_ <- ros[,     .(part_id, n_cnt, n_cnt_analysis = n_cnt_not_work_not_school, period = resby, restriction = "Rule of six", signchange_lab )]
wfh_ <- wfh[,     .(part_id, n_cnt, n_cnt_analysis = n_cnt_work,                period = resby, restriction = "WFH",         signchange_lab)]
tenpm_ <- tenpm[, .(part_id, n_cnt, n_cnt_analysis = n_cnt_other,               period = resby, restriction = "10pm",        signchange_lab)]


ggdt <- rbind(ld_, ros_, wfh_, tenpm_)



ggdt[, n_cnt_analysis_trim := fifelse(n_cnt_analysis < 10, as.character(n_cnt_analysis), "10+")]

ggdt[, table(n_cnt_analysis_trim, restriction)]

ggdt$n_cnt_analysis_trim <- factor(ggdt$n_cnt_analysis_trim, levels = c(as.character(0:9), "10+"))
ggdt$period <- factor(ggdt$period, levels = c("before", "after"), labels = c("Before", "After"))
ggdt$restriction <- factor(ggdt$restriction, levels = c("Local", "Rule of six", "10pm", "WFH"))


a_dist <- ggplot(ggdt) +
  geom_bar(aes(n_cnt_analysis, fill = signchange_lab)) + 
  scale_y_continuous(expand = expansion(0)) +
  facet_grid(restriction ~ period, scales =  "free_y") +
  ggtitle("A") +
  ylab("Number of participants") +
  xlab("Number of contacts") +
  labs(fill = "Change in contacts") +
  scale_x_continuous(breaks = c(0,5,10), limits = c(-1,10), expand = expansion(0))

#a_dist

ld_wide <-    qs::qread('data/clean/loc_wide.qs')
ros_wide <-   qs::qread('data/clean/ros_wide.qs')
wfh_wide <-   qs::qread('data/clean/wfh_wide.qs')
tenpm_wide <- qs::qread('data/clean/tenpm_wide.qs')

ld_wide <-       ld_wide[, .(part_id, diff, period = "Difference", restriction = "Local", signchange_lab)] 
ros_wide <-     ros_wide[, .(part_id, diff, period = "Difference", restriction = "Rule of six", signchange_lab)] 
wfh_wide <-     wfh_wide[, .(part_id, diff, period = "Difference", restriction = "WFH", signchange_lab)] 
tenpm_wide <- tenpm_wide[, .(part_id, diff, period = "Difference", restriction = "10pm", signchange_lab)] 

ggdt_wide <- rbind(ld_wide, ros_wide, wfh_wide, tenpm_wide)

ggdt_wide$restriction <- factor(ggdt_wide$restriction, levels = c("Local", "Rule of six", "10pm", "WFH"))


b_diff <- ggplot(ggdt_wide[diff != 0]) +
  geom_bar(aes(diff, fill = signchange_lab)) + 
  scale_y_continuous(expand = expansion(0)) +
  scale_x_continuous(expand = expansion(0), limits = c(-10,10)) +
  facet_grid(restriction~., scales =  "free_y") +
  guides(fill="none") +
  ggtitle("B*") +
  xlab("Change in contacts") +
  ylab("") +
  labs(caption = "*Zero values removed") +
  scale_fill_manual(values = c("#db735c", "#9A8A76"))

#b_diff
ggdt_prop <- ggdt_wide[, .(num = .N), by = .(restriction, signchange_lab)]
ggdt_prop[, denom := sum(num), by = .(restriction)]
ggdt_prop[, prop := num/denom,]

ggdt_prop$restriction <- factor(ggdt_prop$restriction, levels = c("WFH", "10pm", "Rule of six", "Local"))
  
c_sign <- ggplot(ggdt_prop) +
  geom_col(aes(x = restriction, y = prop, fill = signchange_lab)) + 
  scale_y_continuous(expand = expansion(0)) +
  scale_x_discrete(expand = expansion(0)) +
  guides(fill="none") +
  ggtitle("C") +
  ylab("Proportion changed") +
  xlab("") +
  coord_flip()
  
#c_sign

fig2 <- ((a_dist | b_diff ) / c_sign )+  plot_layout(guides = "collect") + plot_layout(heights = c(5, 1))
ggsave("outputs/Figure_2.png", dpi = 500, width = 10, height = 10) 



# Without trim ------------------------------------------------------------


a_dist_s <- ggplot(ggdt) +
  geom_bar(aes(n_cnt_analysis, fill = signchange_lab)) + 
  scale_y_continuous(expand = expansion(0)) +
  facet_grid(restriction ~ period, scales =  "free") +
  ggtitle("A") +
  ylab("Number of participants") +
  xlab("Number of contacts") +
  labs(fill = "Change in contacts") +
  scale_x_continuous(expand = expansion(0), limits = c(-1, NA))

#a_dist


b_diff_s <- ggplot(ggdt_wide[]) +
  geom_bar(aes(diff, fill = signchange_lab)) + 
  scale_y_continuous(expand = expansion(0)) +
  scale_x_continuous(expand = expansion(0)) +
  facet_grid(restriction~., scales =  "free") +
  guides(fill="none") +
  ggtitle("B*") +
  xlab("Change in contacts") +
  ylab("") +
  labs() 



#c_sign

figS1 <- ((a_dist_s | b_diff_s ))+  plot_layout(guides = "collect") 
figS1

ggsave("outputs/Figure_S1.png", dpi = 500, width = 10, height = 10) 

