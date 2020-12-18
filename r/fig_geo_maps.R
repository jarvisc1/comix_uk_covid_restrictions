# fig_map

## Create a heatmap of restricitonsLocal lockdown restrictions
# Identify the areas that went into local lockdowns and when.
## Tag participants before and after the lockdowns date
## Inputs: res_by_week.qs, 
## Functions: No user written
## Outputs: res_by_week.qs

library(data.table)
library(ggplot2)
library(sf)
library(lubridate)
library(cowplot)

theme_set(cowplot::theme_cowplot(font_size = 18) + theme(strip.background = element_blank()))

# Geo map -----------------------------------------------------------------


shp <- read_sf('data/raw/geo/Local_Authority_Districts__May_2020__Boundaries_UK_BGC.shp')

res <- qs::qread("data/clean/res_by_ltla_month.qs")

shp_res <- base::merge(shp, res, by.x =  "LAD20CD", by.y = "ltlacd", all.y = TRUE)

shp_res$Ncat =cut(shp_res$N, breaks = c(-1, 0, 1, 5,10,18,25), labels = c("None", "1", "2-5","6-10", "10+", "10+"), right = TRUE  )

shp_res <- shp_res[shp_res$month >= 3 & shp_res$month <12,]

shp_res$month_cat <- factor(shp_res$month, levels = 6:11, label = c("24th March-June", "July", "August", "September", "October", "November - December 5th"))
shp_res$month_catall <- month(shp_res$month, label = TRUE, abbr = TRUE)

cols <- c("#feedde", "#fdbe85", "#fd8d3c", "#e6550d", "#a63603")
cols <- c('#eff3ff','#bdd7e7','#6baed6','#3182bd','#08519c')


map_res <- ggplot(shp_res) +
  geom_sf(aes(fill = Ncat), size = 0.001, col =NA) +
  facet_wrap(.~month_catall, nrow = 1) +
  scale_fill_manual(values = cols) +
  scale_y_continuous(expan = c(0,0)) +
  scale_x_continuous(expan = c(0,0)) +
  theme(panel.grid.major = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line.y.left = element_blank(),
        axis.line.x.bottom = element_blank(),
        panel.spacing = unit(0, "cm"),
        
        ) +
  labs(fill = "Maximum \nNumber of restrictions", title = "A")

xint <- c(as.Date("2020-03-02"), 
          as.Date("2020-04-06"), 
          as.Date("2020-05-04"), 
          as.Date("2020-06-01"), 
          as.Date("2020-07-08"),
          as.Date("2020-08-02"),
          as.Date("2020-09-07"),
          as.Date("2020-10-05"),
          as.Date("2020-11-02"),
          as.Date("2020-12-07"))

ydf <- data.table(month = month(xint), xint = xint)

xdf <- data.table(expand.grid(date = seq(as.Date("2020-03-02"), 
                                         as.Date("2020-12-07"), by = "1 week"), y = 0:6))

xdf[, xmin := date-7]
xdf[, xmax := date]
xdf[, ymin := y]
xdf[, ymax := y+1]
xdf[, month := month(xmin)]

xdf <- merge(ydf, xdf, by = "month")

# Load average contacts ---------------------------------------------------
avg_cnts <- qs::qread('data/clean/avg_cnts.qs')

min(avg_cnts$start_date)
avg_contacts <- ggplot(xdf) +
  geom_rect(aes(ymin = ymin, ymax = ymax,xmin = xmin, xmax = xmax), fill = "white") + 
  scale_x_date(breaks = seq(as.Date("2020-03-02"), 
                            as.Date("2020-12-07"), by = "1 week"), expand = c(0,0), date_labels = "%d-%b") +
  #geom_vline(aes(xintercept = xint), linetype = 2, col = "grey", alpha = 0.5) +
  geom_hline(aes(yintercept = ymin), linetype = 2, col = "grey", alpha = 0.5) +
  scale_y_continuous(expand = expansion(0), breaks = c(0,2,4,6)) +
  geom_ribbon(data = avg_cnts, aes(x = start_date, ymin = lci, ymax = uci), alpha = 0.2) +
  geom_line(data = avg_cnts,  aes(start_date, y = med)) +
  geom_point(data = avg_cnts,  aes(start_date, y = med)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.y.left = element_line(),
        axis.line.x.bottom = element_line(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 2)) +
  labs(y = "Mean number of contacts", title = "B", x = "") 

avg_contacts
# Heat map ----------------------------------------------------------------
res <- qs::qread("data/clean/res_by_week.qs")


res[, Ncat := cut(N, breaks = c(-1,1,  10, 20, 50, 101, 300, 320), labels = c("None", "1-10", "11-20", "21-50", "51-100","101-300",
                                                                              "National"), right = TRUE  )]
res_levs <- c("National lockdown",
              "Work from home",
              "Rule of six", 
              "10pm closure",
              "Travel",
              "Non-essential closures",
              "Events cancelled",
              "Table service only",
              "Restaurants closed",
              "Schools closed",
              "No indoor mixing",
              "Discouraged overnight stays",
              "Tier 1", "Tier 2", "Tier 3"
)
res_labs <- c("National lockdown",
              "Work from home",
              "Rule of six*", 
              "10pm closure*",
              "Travel",
              "Non-essential closures",
              "Events cancelled",
              "Table service only*",
              "Restaurants closed",
              "Schools closed",
              "No indoor mixing",
              "Discouraged overnight stays",
              "Tier 1", "Tier 2", "Tier 3"
)
res[, res_grp := factor(res_grp, levels = rev(res_levs), labels = rev(res_labs))]

cols <- c('#f2f0f7','#dadaeb','#bcbddc','#9e9ac8','#807dba','#6a51a3','#4a1486')

breaks_dates <- unique(res$week)
res$lab_dates <- as.Date(paste(2020, res$week, 1, sep="-"), "%Y-%U-%u")
res[, .(lab_dates, week)]

res[, month := month(lab_dates)]
res <- res[month > 2]
res$x <- as.integer(as.factor(res$lab_dates))
x_tick <- c(0, unique(res$x))
len <- length(x_tick)

res[N < 10, Nlab := paste0("  ", N)]
res[N >= 10 & N < 100, Nlab := paste0(" ", N)]
res[N >= 100, Nlab := paste0(N)]
res[N == 0 | N > 300, Nlab := "   "]

## Close need to make lab_dates and x_tick same size. 
res[, month := month(lab_dates)]

res[, ymin := as.numeric(res_grp)-0.5]
res[, ymax := as.numeric(res_grp)+0.5]
res[, y := as.numeric(res_grp)]
res[, x := lab_dates + 3]
res[, xmin := lab_dates]
res[, xmax := lab_dates+7]

hmap <- ggplot(res, aes(ymin = ymin, ymax = ymax,xmin = xmin, xmax = xmax)) +
  geom_rect(aes(fill = Ncat)) + 
  geom_text(aes(label = Nlab, x = x, y = y), col = "black", size = 2.5) +
  scale_fill_manual(values = cols) + 
  scale_x_date(breaks = seq(as.Date("2020-03-02"), 
                            as.Date("2020-12-07"), by = "1 week"), expand = c(0,0), date_labels = "%d-%b") +
  geom_vline(aes(xintercept = xmin), col = "white") +
  geom_hline(aes(yintercept = ymin), col = "white") +
  scale_y_continuous(breaks = 1:15, labels =  levels(res$res_grp),
                     expand = expansion(0)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 2)
        
  ) +
  labs(fill = "Maximum \nNumber of Areas (LTLA)", y = "Restriction",
       x = "Week starting", title = "C") 





# Combine together --------------------------------------------------------

#map_res
library(patchwork)
fig1 <- (map_res / avg_contacts / hmap  ) + plot_layout(heights = c(1.5, 1, 1.5))
ggsave(filename = "outputs/Figure_1.png", fig1, width = 20, height = 12)
