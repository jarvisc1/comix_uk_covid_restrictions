## Function to create summary tables for restrictions

## Function to create counts

tab_display_res_count <- function(dt, var, lab, restriction, labnum, missing = FALSE){
  
  if(missing){
    tab <- table(dt[[var]], useNA = "always")
  } else{
    tab <- table(dt[[var]])
  }
    
  tab_count <- as.data.table(tab)
  names(tab_count) = c("values",  "N")
  tab_count$label = lab
  tab_count$restriction <- restriction
  tab_count$valnum <- 1:(nrow(tab_count))
  tab_count$labnum <- labnum
  tab_count
}

## Function to create percentages
tab_display_res_perc <- function(dt, var, lab, restriction, labnum, missing = FALSE) {
  if(missing){
    tab <- table(dt[[var]], useNA = "always")
  } else{
    tab <- table(dt[[var]])
  }
  tab_perc <- as.data.table(prop.table(tab))
  names(tab_perc) = c("values", "N")
  tab_perc$label = lab
  tab_perc$restriction <- restriction
  tab_perc$valnum <- 1:(nrow(tab_perc))
  tab_perc$labnum <- labnum
  tab_perc
}


## Function to create table for each characteristic
table_creator_res <- function(dt, restriction, display = c("count", "perc", "both"), missing_ = FALSE) {
  res_count <- rbind(
    tab_display_res_count(dt, "total", "Total", restriction, labnum = 1, missing = missing_),
    tab_display_res_count(dt, "part_age_group_labs", "Age groups", restriction, labnum = 2, missing = missing_),
    tab_display_res_count(dt, "part_gender", "Gender", restriction, labnum = 3, missing = missing_),
    tab_display_res_count(dt, "part_employed", "Employed", restriction, labnum = 4, missing = missing_),
    tab_display_res_count(dt, "part_social_group", "Socio-economic status", restriction, labnum = 5, missing = missing_)
  )
  
  res_perc <- rbind(
    tab_display_res_perc(dt, "total", "Total", restriction, labnum = 1, missing = missing_),
    tab_display_res_perc(dt, "part_age_group_labs", "Age groups", restriction, labnum = 2, missing = missing_),
    tab_display_res_perc(dt, "part_gender", "Gender", restriction, labnum = 3, missing = missing_),
    tab_display_res_perc(dt, "part_employed", "Employed", restriction, labnum = 4, missing = missing_),
    tab_display_res_perc(dt, "part_social_group", "Socio-economic status", restriction, labnum = 5, missing = missing_)
  )
  
  res_perc$perc <- round(res_perc$N*100,1)
  res_perc$N <- NULL
  
  if(display == "count"){
    res_count
  }else if(display == "perc"){
    res_perc
  }else if(display == "both"){
    data.table(
      labnum = res_count$labnum,
      valnum = res_count$valnum,
      Restriction = res_count$restriction,
      Label = res_count$label,
      Value = res_count$values,
      N = fifelse(res_count$label == "Total" | res_count$N == 0,
                  as.character(res_count$N),
                  paste0(res_count$N, " (", format(round(res_perc$perc,1), nsmall = 1),"%)")
      )
    )
  }
}


