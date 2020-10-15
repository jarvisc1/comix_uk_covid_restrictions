
## Combine participant and lockdown data to identify where local lockdowns take place.
# Identify the areas that went into local lockdowns and when.
## Input: Local_Lockdown_List.csv and participant.rds data
## Outputs: part_with_ld.rds - Combined data source

library(data.table)

# Read in lockdown info
lldown <- read.csv("data/raw/Local_Lockdown_List.csv")

# Read in participants
part <- readRDS("data/raw/participants.rds")

# Clean the dates
lldown$Start.Date <- as.Date(lldown$Start.Date, format = "%d/%m/%Y")
lldown$End.Date <- as.Date(lldown$End.Date, format = "%d/%m/%Y")

lldown$area_1_name <- lldown$LTLA
lldown <- as.data.table(lldown)

# Create an end date that is 30th Sept
lldown$End.Date[is.na(lldown$End.Date)] <- as.Date("2020-09-30")
min(lldown$Start.Date)

# Merge the lockdown time onto the data
part_local_ld <- part[lldown, on = .(area_1_name)][between(date, Start.Date, End.Date)]

## Create a binary variable
part$local_ld <- 0
part$local_ld[part$personweek %in% part_local_ld$personweek] <- 1

## Rename the local lockdown data for a merge
local_ld_merge <- part_local_ld[,.(personweek, 
                                   LTLA, 
                                   start_date_ld = Start.Date,
                                   end_date_ld = End.Date,
                                   ld_restrictions = `Level.of.Restrictions..H..M..L.`
)]

## Merge on using personweek
part_ld <- merge(part, local_ld_merge, on = "personweek", all.x = TRUE)

## checks
nrow(part)
nrow(part_local_ld)
nrow(part_ld)

saveRDS(part_ld, "data/part_with_ld.RDS")

      