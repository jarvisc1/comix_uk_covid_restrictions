## fig1a

library(ggplot2)
library(data.table)
library(ggthemr)
library(patchwork)
ggthemr("dust")



t1_wide <- qs::qread('data/clean/t1_wide.qs')
t2_wide <- qs::qread('data/clean/t2_wide.qs')
t3_wide <- qs::qread('data/clean/t3_wide.qs')
t1_ld_wide <- qs::qread('data/clean/t1_ld_wide.qs')
t2_ld_wide <- qs::qread('data/clean/t2_ld_wide.qs')
t3_ld_wide <- qs::qread('data/clean/t3_ld_wide.qs')

t1_wide <-  t1_wide[, .(part_id, diff, period = "Difference", restriction = "Tier 1", signchange_lab)] 
t2_wide <-  t2_wide[, .(part_id, diff, period = "Difference", restriction = "Tier 2", signchange_lab)] 
t3_wide <-  t3_wide[, .(part_id, diff, period = "Difference", restriction = "Tier 3", signchange_lab)] 

ggdt_wide <- rbind(t1_wide, t2_wide, t3_wide)

ggdt_wide$restriction <- factor(ggdt_wide$restriction, levels = c("Tier 1", "Tier 2", "Tier 3"))


a_diff <- ggplot(ggdt_wide[diff != 0]) +
  geom_bar(aes(diff, fill = signchange_lab)) + 
  scale_y_continuous(expand = expansion(0)) +
  scale_x_continuous(expand = expansion(0), limits =c(-10,10)) +
  facet_grid(restriction~., scales =  "free_y") +
  guides(fill="none") +
  ggtitle("A: Enter Tier") +
  xlab("Change in contacts") +
  ylab("") +
  labs(caption = "*Zero values removed") +
  scale_fill_manual(values = c("#db735c", "#9A8A76"))
a_diff

t1_ld_wide <-  t1_ld_wide[, .(part_id, diff, period = "Difference", restriction = "Tier 1", signchange_lab)] 
t2_ld_wide <-  t2_ld_wide[, .(part_id, diff, period = "Difference", restriction = "Tier 2", signchange_lab)] 
t3_ld_wide <-  t3_ld_wide[, .(part_id, diff, period = "Difference", restriction = "Tier 3", signchange_lab)] 

ggdt_ld_wide <- rbind(t1_ld_wide, t2_ld_wide, t3_ld_wide)

ggdt_ld_wide$restriction <- factor(ggdt_ld_wide$restriction, levels = c("Tier 1", "Tier 2", "Tier 3"))


b_diff <- ggplot(ggdt_ld_wide[diff != 0]) +
  geom_bar(aes(diff, fill = signchange_lab)) + 
  scale_y_continuous(expand = expansion(0)) +
  scale_x_continuous(expand = expansion(0), limits =c(-10,10)) +
  facet_grid(restriction~., scales =  "free_y") +
  guides(fill="none") +
  ggtitle("B: Exit Tier to lockdown") +
  xlab("Change in contacts") +
  ylab("") +
  labs(caption = "*Zero values removed") +
  scale_fill_manual(values = c("#db735c", "#9A8A76"))
b_diff

#b_diff
ggdt_prop <- ggdt_wide[, .(num = .N), by = .(restriction, signchange_lab)]
ggdt_prop[, denom := sum(num), by = .(restriction)]
ggdt_prop[, prop := num/denom,]

ggdt_prop$restriction <- factor(ggdt_prop$restriction, levels = c("Tier 3", "Tier 2", "Tier 1"))
  
c_sign <- ggplot(ggdt_prop) +
  geom_col(aes(x = restriction, y = prop, fill = signchange_lab)) + 
  scale_y_continuous(expand = expansion(0)) +
  scale_x_discrete(expand = expansion(0)) +
  guides() +
  labs(fill = "Change in contacts") +
  ggtitle("C") +
  ylab("Proportion changed") +
  xlab("") +
  coord_flip()
  
c_sign
ggdt_ld_prop <- ggdt_ld_wide[, .(num = .N), by = .(restriction, signchange_lab)]
ggdt_ld_prop[, denom := sum(num), by = .(restriction)]
ggdt_ld_prop[, prop := num/denom,]

ggdt_ld_prop$restriction <- factor(ggdt_ld_prop$restriction, levels = c("Tier 3", "Tier 2", "Tier 1"))
  
d_sign <- ggplot(ggdt_ld_prop) +
  geom_col(aes(x = restriction, y = prop, fill = signchange_lab)) + 
  scale_y_continuous(expand = expansion(0)) +
  scale_x_discrete(expand = expansion(0)) +
  guides(fill="none") +
  ggtitle("D") +
  ylab("Proportion changed") +
  xlab("") +
  coord_flip()

fig3 <- ((a_diff | b_diff ) / (c_sign | d_sign ))+  plot_layout(guides = "collect") + plot_layout(heights = c(5, 1))
fig3

ggsave("outputs/Figure_3.png", dpi = 500, width = 10, height = 10) 


# Without trim ------------------------------------------------------------


a_dist_s <- ggplot(ggdt_wide[]) +
  geom_bar(aes(diff, fill = signchange_lab)) + 
  scale_y_continuous(expand = expansion(0)) +
  scale_x_continuous(expand = expansion(0)) +
  facet_grid(restriction~., scales =  "free_y") +
  ggtitle("A: Enter Tier") +
  ylab("Number of participants") +
  xlab("Change in contacts") +
  labs(fill = "Change in contacts")

#a_dist


b_diff_s <- ggplot(ggdt_ld_wide[]) +
  geom_bar(aes(diff, fill = signchange_lab)) + 
  scale_y_continuous(expand = expansion(0)) +
  scale_x_continuous(expand = expansion(0)) +
  facet_grid(restriction~., scales =  "free_y") +
  guides(fill="none") +
  ggtitle("B: Exit Tier to lockdown") +
  xlab("Change in contacts") +
  ylab("") +
  labs() 



#c_sign

figS2 <- ((a_dist_s | b_diff_s ))+  plot_layout(guides = "collect") 
figS2

ggsave("outputs/Figure_S2.png", dpi = 500, width = 10, height = 10) 


