
## Uncomment to instal packages required for analyses.
# install.packages('ggplot2')
# install.packages('patchwork')
# install.packages('remotes')
#install.packages("data.table")
#data.table::update.dev.pkg() # latest development version:
#remotes::install_github('cttobin/ggthemr')

## Clean data
source('r/dm_combine_data.R')
source('r/dm_local_lockdowns.R')
source('r/dm_rule_of_six.R')
source('r/dm_10pm_WFH.R')
source('r/dm_10pm.R')
source('r/dm_WFH.R')

## Run analyses
set.seed(700)

par(mfrow = c(2,1))
source('r/an_local_lockdowns.R')
source('r/an_local_lockdowns_sens.R')
par(mfrow = c(3,1))
source('r/an_rule_of_six.R')
source('r/an_10pm.R')
source('r/an_WFH.R')

# Create tables
## Table 1
source('r/tab_restrictions_characteristics.R')
## Table 2
source('r/tab_combine_summary.R')
## Table 3
source('r/tab_signchange_characteristics.R')

# Create Figure
source('r/Figure_1.R')
source('r/Figure_S1.R')
