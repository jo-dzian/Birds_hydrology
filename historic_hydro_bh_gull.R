# SCRIPT 6A

# calculating IHA indicators for the Historical Period (1971-2000) so then it case be used
# as a baseline for calculating changes in the future scenarios

#extract data for full year for subbasins of interest
# use fun_sub_allyear from future_hydro script
gc() 
rm()

####### GROUP 1 ################################################################################
####### IHA group 1 Mean or median value for each calendar month replaced with means for periods of
#laying eggs, incubating and rearing chicks


######### incubating 20.04 - 31.05 ###################################

# narrow down the period to incubation and calculate mean
# fun_bh.gull_vp_incub 

#result in a list of lists
bh.gull_ref_incub_gr.1_list  <- lapply( bh.gull_vp_ref_list, lapply, fun_bh.gull_vp_incub)

#obtain results for the reaches as a mean from 9 models
bh.gull_ref_incub_gr.1_unlist <- bind_rows(list("mod.1" = bind_rows(bh.gull_ref_incub_gr.1_list[[1]], .id = "id"), 
                                                   "mod.2" = bind_rows(bh.gull_ref_incub_gr.1_list[[2]], .id = "id"),
                                                   "mod.3" = bind_rows(bh.gull_ref_incub_gr.1_list[[3]], .id = "id"),
                                                   "mod.4" = bind_rows(bh.gull_ref_incub_gr.1_list[[4]], .id = "id"),
                                                   "mod.5" = bind_rows(bh.gull_ref_incub_gr.1_list[[5]], .id = "id"),
                                                   "mod.6" = bind_rows(bh.gull_ref_incub_gr.1_list[[6]], .id = "id"),
                                                   "mod.7" = bind_rows(bh.gull_ref_incub_gr.1_list[[7]], .id = "id"),
                                                   "mod.8" = bind_rows(bh.gull_ref_incub_gr.1_list[[8]], .id = "id"),
                                                   "mod.9" = bind_rows(bh.gull_ref_incub_gr.1_list[[9]], .id = "id")), 
                                              .id = "model") 

bh.gull_ref_incub_gr.1_p1 <- aggregate(bh.gull_ref_incub_gr.1_unlist$mean_incub, 
                                          by=list(bh.gull_ref_incub_gr.1_unlist$subbasin), 
                                          FUN=mean)

write.csv(bh.gull_ref_incub_gr.1_p1, "D:/Ptaki_hydro/Obliczenia/R/Results/reference_hist/bh.gull_ref_incub_gr.1.csv")

#########
####
#### Standardized for carastrophy analysis
#### 
#########

fun_bh.gull_vp_incub_st <- function(x){
  month <- as.integer(format(x$date2, '%m'))
  day <- as.integer(format(x$date2, '%d'))
  in_data1 <- filter(x, month == 4 & day >= 20 | month == 5 & day <= 31) #select incubation period
  in_data2 <- aggregate(in_data1$st_Q, list(in_data1$subbasin), mean) #calculate mean for that period
  setNames(in_data2, c("subbasin", "mean_incub")) #rename columns
}

#standardized result in a list of lists
bh.gull_ref_incub_gr.1_list_st  <- lapply( data_ref_st, lapply, fun_bh.gull_vp_incub_st)

#obtain standardized results for the reaches as a mean from 9 models
bh.gull_ref_incub_gr.1_unlist_st <- bind_rows(list("mod.1" = bind_rows(bh.gull_ref_incub_gr.1_list_st[[1]], .id = "id"), 
                                                "mod.2" = bind_rows(bh.gull_ref_incub_gr.1_list_st[[2]], .id = "id"),
                                                "mod.3" = bind_rows(bh.gull_ref_incub_gr.1_list_st[[3]], .id = "id"),
                                                "mod.4" = bind_rows(bh.gull_ref_incub_gr.1_list_st[[4]], .id = "id"),
                                                "mod.5" = bind_rows(bh.gull_ref_incub_gr.1_list_st[[5]], .id = "id"),
                                                "mod.6" = bind_rows(bh.gull_ref_incub_gr.1_list_st[[6]], .id = "id"),
                                                "mod.7" = bind_rows(bh.gull_ref_incub_gr.1_list_st[[7]], .id = "id"),
                                                "mod.8" = bind_rows(bh.gull_ref_incub_gr.1_list_st[[8]], .id = "id"),
                                                "mod.9" = bind_rows(bh.gull_ref_incub_gr.1_list_st[[9]], .id = "id")), 
                                           .id = "model") 

bh.gull_ref_incub_gr.1_p1_st <- aggregate(bh.gull_ref_incub_gr.1_unlist_st$mean_incub, 
                                       by=list(bh.gull_ref_incub_gr.1_unlist_st$subbasin), 
                                       FUN=mean)



#######^^^^^^^^^^^for catastrophic years

data_ref_st_y <- lapply(data_ref_st, lapply, fun_add_year) # fun_add_year in line 382 below

# narrow down the period to incubation and calculate mean

fun_bh.gull_vp_incub_st_cat1 <- function(x){
  month <- as.integer(format(x$date2, '%m'))
  day <- as.integer(format(x$date2, '%d'))
  in_data1 <- filter(x, month == 4 & day >= 20 | month == 5 & day <= 31) #select incubation period
  in_data2 <- aggregate(in_data1$st_Q, list(in_data1$Year), mean) #calculate mean for that period
  setNames(in_data2, c("Year", "mean_incub")) #rename columns
}


#result in a list of lists
bh.gull_ref_incub_gr.1_list_st_cat  <- lapply( data_ref_st_y, lapply, fun_bh.gull_vp_incub_st_cat1)

bh.gull_ref_incub_gr.1_list_st_cat1 <- map_df(bh.gull_ref_incub_gr.1_list_st_cat, ~bind_rows(., .id = 'subbasin'), .id = 'model')

write.csv(bh.gull_ref_incub_gr.1_list_st_cat1, "D:/Ptaki_hydro/Obliczenia/R/Results/reg_eq/new/bh.gull_ref_incub_gr.1_list_st_cat1.csv")

######### rearing chicks 1.05 - 10.06 #################################

# narrow down the period to incubation and calculate mean
#fun_bh.gull_vp_rear 

bh.gull_ref_rear_gr.1_list  <- lapply( bh.gull_vp_ref_list, lapply, fun_bh.gull_vp_rear)

#obtain results for the reaches as a mean from 9 models
bh.gull_ref_rear_gr.1_unlist <- bind_rows(list("mod.1" = bind_rows(bh.gull_ref_rear_gr.1_list[[1]], .id = "id"), 
                                                  "mod.2" = bind_rows(bh.gull_ref_rear_gr.1_list[[2]], .id = "id"),
                                                  "mod.3" = bind_rows(bh.gull_ref_rear_gr.1_list[[3]], .id = "id"),
                                                  "mod.4" = bind_rows(bh.gull_ref_rear_gr.1_list[[4]], .id = "id"),
                                                  "mod.5" = bind_rows(bh.gull_ref_rear_gr.1_list[[5]], .id = "id"),
                                                  "mod.6" = bind_rows(bh.gull_ref_rear_gr.1_list[[6]], .id = "id"),
                                                  "mod.7" = bind_rows(bh.gull_ref_rear_gr.1_list[[7]], .id = "id"),
                                                  "mod.8" = bind_rows(bh.gull_ref_rear_gr.1_list[[8]], .id = "id"),
                                                  "mod.9" = bind_rows(bh.gull_ref_rear_gr.1_list[[9]], .id = "id")), 
                                             .id = "model") 

bh.gull_ref_rear_gr.1_p1 <- aggregate(bh.gull_ref_rear_gr.1_unlist$mean_rear, 
                                         by=list(bh.gull_ref_rear_gr.1_unlist$subbasin), 
                                         FUN=mean)

write.csv(bh.gull_ref_rear_gr.1_p1, "D:/Ptaki_hydro/Obliczenia/R/Results/reference_hist/bh.gull_ref_rear_gr.1.csv")


##############################################################################################
####### GROUP 2 ################################################################################
####### IHA group 2 is Magnitude  and duration of annual extreme  water condition 
# (Annual maxima, 1-day mean) 
# 1,3,7 day max
library("zoo")

#calculating rolling 1,3 an 7 day mean on list
# fun_bh.gull_data_roll_list

#apply to list
bh.gull_data_ref_roll_list  <- lapply( data_ref, lapply, fun_bh.gull_data_roll_list)

# calculate the minimum and maximum per year per RCH
# fun_data_roll_list_max 
library(plyr)

bh.gull_ref_list_gr.2  <- lapply( bh.gull_data_ref_roll_list, lapply, fun_data_roll_list_max)

#obtain results for the reaches as a mean from 9 models
bh.gull_ref_gr.2_unlist <- bind_rows(list("mod.1" = bind_rows(bh.gull_ref_list_gr.2[[1]], .id = "id"), 
                                             "mod.2" = bind_rows(bh.gull_ref_list_gr.2[[2]], .id = "id"),
                                             "mod.3" = bind_rows(bh.gull_ref_list_gr.2[[3]], .id = "id"),
                                             "mod.4" = bind_rows(bh.gull_ref_list_gr.2[[4]], .id = "id"),
                                             "mod.5" = bind_rows(bh.gull_ref_list_gr.2[[5]], .id = "id"),
                                             "mod.6" = bind_rows(bh.gull_ref_list_gr.2[[6]], .id = "id"),
                                             "mod.7" = bind_rows(bh.gull_ref_list_gr.2[[7]], .id = "id"),
                                             "mod.8" = bind_rows(bh.gull_ref_list_gr.2[[8]], .id = "id"),
                                             "mod.9" = bind_rows(bh.gull_ref_list_gr.2[[9]], .id = "id")), 
                                        .id = "model") 

bh.gull_ref_gr.2_unlist_p1 <- cbind(setNames(aggregate(bh.gull_ref_gr.2_unlist$day01_max, 
                                                          by=list(bh.gull_ref_gr.2_unlist$subbasin), 
                                                          FUN=mean), c("subbasin", "day01_mean")),
                                       setNames(aggregate(bh.gull_ref_gr.2_unlist$day03_max, 
                                                          by=list(bh.gull_ref_gr.2_unlist$subbasin), 
                                                          FUN=mean), c("subbasin", "day03_mean")),
                                       setNames(aggregate(bh.gull_ref_gr.2_unlist$day07_max, 
                                                          by=list(bh.gull_ref_gr.2_unlist$subbasin), 
                                                          FUN=mean), c("subbasin", "day07_mean")))

write.csv(bh.gull_ref_gr.2_unlist_p1, "D:/Ptaki_hydro/Obliczenia/R/Results/reference_hist/bh.gull_ref_gr.2.csv")
##### model/subbasin/value
bh.gull_ref_gr.2_unlist_p2 <- cbind(setNames(aggregate(bh.gull_ref_gr.2_unlist$day01_max, 
                                                      by=list(bh.gull_ref_gr.2_unlist$model,
                                                              bh.gull_ref_gr.2_unlist$subbasin), 
                                                      FUN=mean), c("model","subbasin", "day01_mean")),
                                   setNames(aggregate(bh.gull_ref_gr.2_unlist$day03_max, 
                                                      by=list(bh.gull_ref_gr.2_unlist$model,
                                                              bh.gull_ref_gr.2_unlist$subbasin), 
                                                      FUN=mean), c("model","subbasin", "day03_mean")),
                                   setNames(aggregate(bh.gull_ref_gr.2_unlist$day07_max, 
                                                      by=list(bh.gull_ref_gr.2_unlist$model,
                                                              bh.gull_ref_gr.2_unlist$subbasin), 
                                                      FUN=mean), c("model", "subbasin", "day07_mean")))

#########
####
#### Standardized for carastrophy analysis
#### 
#########

fun_bh.gull_data_roll_list_st <- function(x){
  step1 <- dplyr::mutate(x, day01_mean = zoo::rollmean (x$st_Q, k = 1, fill = NA), #we have 1 measurment per day so this is acctually not necessary to calculate
                         day03_mean = zoo::rollmean (x$st_Q, k = 3, fill = NA),
                         day07_mean = zoo::rollmean (x$st_Q, k = 7, fill = NA))
  step1$Year <- format(as.Date(step1$date, format="%Y-%m-%d"),"%Y")
  step1$date2 <- as.POSIXct(step1$date, format="%Y-%m-%d")
  month <- as.integer(format(step1$date2, '%m'))
  day <- as.integer(format(step1$date2, '%d'))
  in_data3 <- filter(step1, month == 4 & day >= 11 | month == 5 | month == 6 & day <= 10)}

gc()

bh.gull_data_ref_roll_list_st  <- lapply( data_ref_st, lapply, fun_bh.gull_data_roll_list_st)

bh.gull_ref_list_gr.2_st  <- lapply( bh.gull_data_ref_roll_list_st, lapply, fun_data_roll_list_max)

bh.gull_ref_gr.2_unlist_st <- bind_rows(list("mod.1" = bind_rows(bh.gull_ref_list_gr.2_st[[1]], .id = "id"), 
                                            "mod.2" = bind_rows(bh.gull_ref_list_gr.2_st[[2]], .id = "id"),
                                            "mod.3" = bind_rows(bh.gull_ref_list_gr.2_st[[3]], .id = "id"),
                                            "mod.4" = bind_rows(bh.gull_ref_list_gr.2_st[[4]], .id = "id"),
                                            "mod.5" = bind_rows(bh.gull_ref_list_gr.2_st[[5]], .id = "id"),
                                            "mod.6" = bind_rows(bh.gull_ref_list_gr.2_st[[6]], .id = "id"),
                                            "mod.7" = bind_rows(bh.gull_ref_list_gr.2_st[[7]], .id = "id"),
                                            "mod.8" = bind_rows(bh.gull_ref_list_gr.2_st[[8]], .id = "id"),
                                            "mod.9" = bind_rows(bh.gull_ref_list_gr.2_st[[9]], .id = "id")), 
                                       .id = "model") 

bh.gull_ref_gr.2_unlist_p1_st <- cbind(setNames(aggregate(bh.gull_ref_gr.2_unlist_st$day01_max, 
                                                         by=list(bh.gull_ref_gr.2_unlist_st$subbasin), 
                                                         FUN=mean), c("subbasin", "day01_mean")),
                                      setNames(aggregate(bh.gull_ref_gr.2_unlist_st$day03_max, 
                                                         by=list(bh.gull_ref_gr.2_unlist_st$subbasin), 
                                                         FUN=mean), c("subbasin", "day03_mean")),
                                      setNames(aggregate(bh.gull_ref_gr.2_unlist_st$day07_max, 
                                                         by=list(bh.gull_ref_gr.2_unlist_st$subbasin), 
                                                         FUN=mean), c("subbasin", "day07_mean")))

bh.gull_ref_gr.2_unlist_p2_st <- cbind(setNames(aggregate(bh.gull_ref_gr.2_unlist_st$day01_max, 
                                                         by=list(bh.gull_ref_gr.2_unlist_st$model,
                                                                 bh.gull_ref_gr.2_unlist_st$subbasin), 
                                                         FUN=mean), c("model","subbasin", "day01_mean")),
                                      setNames(aggregate(bh.gull_ref_gr.2_unlist_st$day03_max, 
                                                         by=list(bh.gull_ref_gr.2_unlist_st$model,
                                                                 bh.gull_ref_gr.2_unlist_st$subbasin), 
                                                         FUN=mean), c("model","subbasin", "day03_mean")),
                                      setNames(aggregate(bh.gull_ref_gr.2_unlist_st$day07_max, 
                                                         by=list(bh.gull_ref_gr.2_unlist_st$model,
                                                                 bh.gull_ref_gr.2_unlist_st$subbasin), 
                                                         FUN=mean), c("model", "subbasin", "day07_mean")))


# summary for analysis od regression equation
bh.gull_ref_list_gr.2_st_reg <- map_df(bh.gull_ref_list_gr.2_st, ~bind_rows(., .id = 'subbasin'), .id = 'model')

bh.gull_ref_list_gr.2_st_reg_out <- select (bh.gull_ref_list_gr.2_st_reg,-c(day01_max, day07_max))

write.csv(bh.gull_ref_list_gr.2_st_reg_out, "D:/Ptaki_hydro/Obliczenia/R/Results/reg_eq/new2/bh.gull_ref_list_gr.2_st_reg_out.csv")



##############################################################################################
####### GROUP 3 ################################################################################
####### IHA group 3 is  Timing of annual extreme water conditions,
#Julian date of each annual 1-day maximum 

#bh.gull vp:
#  11.04 is 101 or 102 (Leap) julian day
#  10.06 is 161 or 162 (Leap) julian day
# Leap years: 2004, 2008, 2012, 2016

#add julian day
#fun_julian 

gc()


#chech if julian date is within vulnerability period range (101 and 162 days) and count as 1 if yes, 0 as no.
#fun_bh.gull_list_gr.3 
        
bh.gull_ref_list_gr.3  <- lapply( data_ref_julian, lapply, fun_bh.gull_list_gr.3)

#obtain results for the reaches as a mean from 9 models
bh.gull_ref_list_gr.3_unlist <- bind_rows(list("mod.1" = bind_rows(bh.gull_ref_list_gr.3[[1]], .id = "id"), 
                                                  "mod.2" = bind_rows(bh.gull_ref_list_gr.3[[2]], .id = "id"),
                                                  "mod.3" = bind_rows(bh.gull_ref_list_gr.3[[3]], .id = "id"),
                                                  "mod.4" = bind_rows(bh.gull_ref_list_gr.3[[4]], .id = "id"),
                                                  "mod.5" = bind_rows(bh.gull_ref_list_gr.3[[5]], .id = "id"),
                                                  "mod.6" = bind_rows(bh.gull_ref_list_gr.3[[6]], .id = "id"),
                                                  "mod.7" = bind_rows(bh.gull_ref_list_gr.3[[7]], .id = "id"),
                                                  "mod.8" = bind_rows(bh.gull_ref_list_gr.3[[8]], .id = "id"),
                                                  "mod.9" = bind_rows(bh.gull_ref_list_gr.3[[9]], .id = "id")), 
                                             .id = "model") 

#calculate sum of years (out of 29) where the peak flow occured during the vulnerability period (over_vp_max) 
# in each model
bh.gull_ref_list_gr.3_unlist_p1 <- setNames (aggregate(bh.gull_ref_list_gr.3_unlist$vp_max, 
                                                          by=list(bh.gull_ref_list_gr.3_unlist$model, 
                                                                  bh.gull_ref_list_gr.3_unlist$subbasin), 
                                                          FUN=sum), c("model", "subbasin","above_vp_max"))

bh.gull_ref_list_gr.3_unlist_p1$above_vp_max_period <- bh.gull_ref_list_gr.3_unlist_p1$above_vp_max/29


# find a mean value from all 9 models
bh.gull_ref_list_gr.3_unlist_p2 <- setNames (aggregate(bh.gull_ref_list_gr.3_unlist_p1$above_vp_max, 
                                                          by=list(bh.gull_ref_list_gr.3_unlist_p1$subbasin), 
                                                          FUN=mean), c("subbasin","mean_above_vp_max"))

# per the 29 year period
bh.gull_ref_list_gr.3_unlist_p2$mean_above_vp_max_period <- bh.gull_ref_list_gr.3_unlist_p2$mean_above_vp_max/29



write.csv(bh.gull_ref_list_gr.3_unlist_p2, "D:/Ptaki_hydro/Obliczenia/R/Results/reference_hist/bh.gull_ref_gr.3.csv")


##############################################################################################
####### GROUP 4 ################################################################################
####### IHA group 4 is  Frequency and duration of high and low pulses
# Number of low pulses within each water year, Mean or median duration of low pulses (days),
# Number of high pulses within each water year, Mean or median duration of high pulses (days)
# above 0.95

gc()

# fun_add_year 

bh.gull_vp_ref_list_y <- lapply(bh.gull_vp_ref_list, lapply, fun_add_year)


bh.gull_vp_ref_list_a <- bind_rows(list("mod.1" = bind_rows( bh.gull_vp_ref_list_y[[1]] , .id = "id"), 
                                           "mod.2" = bind_rows( bh.gull_vp_ref_list_y[[2]], .id = "id"),
                                           "mod.3" = bind_rows( bh.gull_vp_ref_list_y[[3]], .id = "id"),
                                           "mod.4" = bind_rows( bh.gull_vp_ref_list_y[[4]], .id = "id"),
                                           "mod.5" = bind_rows( bh.gull_vp_ref_list_y[[5]], .id = "id"),
                                           "mod.6" = bind_rows( bh.gull_vp_ref_list_y[[6]], .id = "id"),
                                           "mod.7" = bind_rows( bh.gull_vp_ref_list_y[[7]], .id = "id"),
                                           "mod.8" = bind_rows( bh.gull_vp_ref_list_y[[8]], .id = "id"),
                                           "mod.9" = bind_rows( bh.gull_vp_ref_list_y[[9]], .id = "id")),
                                      .id = "model") 
#count how many days during the vulnerability period are higher than 95% quartile 
#
bh.gull_ref_gr.4_p1 <- full_join(bh.gull_vp_ref_list_a, data_ref_q0.95_period_list_a, 
                                    by=c("model","subbasin")) %>% 
                                   mutate(condition = (flow > P0.95))  

bh.gull_ref_gr.4_p1$condition2 <- as.integer(bh.gull_ref_gr.4_p1$condition)

bh.gull_ref_gr.4_p2 <- aggregate(bh.gull_ref_gr.4_p1$condition2, 
                                    by=list(bh.gull_ref_gr.4_p1$model, bh.gull_ref_gr.4_p1$subbasin), FUN=sum)

#mean number of days above Q3 per the 29 year period 1971-2000
bh.gull_ref_gr.4_p2$yearly <- bh.gull_ref_gr.4_p2$x/29

# calculate a mean out of all the 9 models per subbasin
bh.gull_ref_gr.4_p3 <- aggregate(bh.gull_ref_gr.4_p2$yearly, 
                                    by=list(bh.gull_ref_gr.4_p2$Group.2), FUN=mean)

# write gr.4 to a file
write.csv(bh.gull_ref_gr.4_p3, "D:/Ptaki_hydro/Obliczenia/R/Results/reference_hist/bh.gull_ref_gr.4.csv")

