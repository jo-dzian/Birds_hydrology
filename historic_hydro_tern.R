# SCRIPT 6C


# calculating IHA indicators for the Historical Period (1971-2000) so then it case be used
# as a baseline for calculating changes in the future scenarios

# data_ref <- file contains all the data for18 subbasins during the reference period 
# extract data for vulnerability period for tern for subbasins of interest

#fun_tern_vp 

tern_vp_ref_cm1 <- fun_tern_vp("D:/Ptaki_hydro/Obliczenia/4TU/RawModelOutputs/ref_cm01_1974_2000_reach.csv")
tern_vp_ref_cm2 <- fun_tern_vp("D:/Ptaki_hydro/Obliczenia/4TU/RawModelOutputs/ref_cm02_1974_2000_reach.csv")
tern_vp_ref_cm3 <- fun_tern_vp("D:/Ptaki_hydro/Obliczenia/4TU/RawModelOutputs/ref_cm03_1974_2000_reach.csv")
tern_vp_ref_cm4 <- fun_tern_vp("D:/Ptaki_hydro/Obliczenia/4TU/RawModelOutputs/ref_cm04_1974_2000_reach.csv")
tern_vp_ref_cm5 <- fun_tern_vp("D:/Ptaki_hydro/Obliczenia/4TU/RawModelOutputs/ref_cm05_1974_2000_reach.csv")
tern_vp_ref_cm6 <- fun_tern_vp("D:/Ptaki_hydro/Obliczenia/4TU/RawModelOutputs/ref_cm06_1974_2000_reach.csv")
tern_vp_ref_cm7 <- fun_tern_vp("D:/Ptaki_hydro/Obliczenia/4TU/RawModelOutputs/ref_cm07_1974_2000_reach.csv")
tern_vp_ref_cm8 <- fun_tern_vp("D:/Ptaki_hydro/Obliczenia/4TU/RawModelOutputs/ref_cm08_1974_2000_reach.csv")
tern_vp_ref_cm9 <- fun_tern_vp("D:/Ptaki_hydro/Obliczenia/4TU/RawModelOutputs/ref_cm09_1974_2000_reach.csv")


tern_vp_ref_list <- list (tern_vp_ref_cm1, tern_vp_ref_cm2, tern_vp_ref_cm3,
                            tern_vp_ref_cm4, tern_vp_ref_cm5, tern_vp_ref_cm6,
                            tern_vp_ref_cm7, tern_vp_ref_cm8, tern_vp_ref_cm9)


####### GROUP 1 ################################################################################
####### IHA group 1 Mean or median value for each calendar month replaced with means for periods of
#laying eggs, incubating and rearing chicks

######### laying eggs 11.05 - 20.06 ###################################

# narrow down the period to incubation and calculate mean
#fun_tern_vp_le 

#result in a list of lists
tern_ref_le_gr.1_list  <- lapply( tern_vp_ref_list, lapply, fun_tern_vp_le)

#obtain results for the reaches as a mean from 9 models
tern_ref_le_gr.1_unlist <- bind_rows(list("mod.1" = bind_rows(tern_ref_le_gr.1_list[[1]], .id = "id"), 
                                             "mod.2" = bind_rows(tern_ref_le_gr.1_list[[2]], .id = "id"),
                                             "mod.3" = bind_rows(tern_ref_le_gr.1_list[[3]], .id = "id"),
                                             "mod.4" = bind_rows(tern_ref_le_gr.1_list[[4]], .id = "id"),
                                             "mod.5" = bind_rows(tern_ref_le_gr.1_list[[5]], .id = "id"),
                                             "mod.6" = bind_rows(tern_ref_le_gr.1_list[[6]], .id = "id"),
                                             "mod.7" = bind_rows(tern_ref_le_gr.1_list[[7]], .id = "id"),
                                             "mod.8" = bind_rows(tern_ref_le_gr.1_list[[8]], .id = "id"),
                                             "mod.9" = bind_rows(tern_ref_le_gr.1_list[[9]], .id = "id")), 
                                        .id = "model") 

tern_ref_le_gr.1_p1 <- aggregate(tern_ref_le_gr.1_unlist$mean_le, 
                                    by=list(tern_ref_le_gr.1_unlist$subbasin), 
                                    FUN=mean)

write.csv(tern_ref_le_gr.1_p1, "D:/Ptaki_hydro/Obliczenia/R/Results/reference_hist/tern_ref_le_gr.1.csv")


######### incubating 21.05 - 30.06 ###################################

# narrow down the period to incubation and calculate mean
# fun_tern_vp_incub 

#result in a list of lists
tern_ref_incub_gr.1_list  <- lapply( tern_vp_ref_list, lapply, fun_tern_vp_incub)

#obtain results for the reaches as a mean from 9 models
tern_ref_incub_gr.1_unlist <- bind_rows(list("mod.1" = bind_rows(tern_ref_incub_gr.1_list[[1]], .id = "id"), 
                                                "mod.2" = bind_rows(tern_ref_incub_gr.1_list[[2]], .id = "id"),
                                                "mod.3" = bind_rows(tern_ref_incub_gr.1_list[[3]], .id = "id"),
                                                "mod.4" = bind_rows(tern_ref_incub_gr.1_list[[4]], .id = "id"),
                                                "mod.5" = bind_rows(tern_ref_incub_gr.1_list[[5]], .id = "id"),
                                                "mod.6" = bind_rows(tern_ref_incub_gr.1_list[[6]], .id = "id"),
                                                "mod.7" = bind_rows(tern_ref_incub_gr.1_list[[7]], .id = "id"),
                                                "mod.8" = bind_rows(tern_ref_incub_gr.1_list[[8]], .id = "id"),
                                                "mod.9" = bind_rows(tern_ref_incub_gr.1_list[[9]], .id = "id")), 
                                           .id = "model") 

tern_ref_incub_gr.1_p1 <- aggregate(tern_ref_incub_gr.1_unlist$mean_incub, 
                                       by=list(tern_ref_incub_gr.1_unlist$subbasin), 
                                       FUN=mean)

write.csv(tern_ref_incub_gr.1_p1, "D:/Ptaki_hydro/Obliczenia/R/Results/reference_hist/tern_ref_incub_gr.1.csv")

######### rearing chicks 11.06 - 10.07 #################################

# narrow down the period to incubation and calculate mean
# fun_tern_vp_rear 

tern_ref_rear_gr.1_list  <- lapply( tern_vp_ref_list, lapply, fun_tern_vp_rear)

#obtain results for the reaches as a mean from 9 models
tern_ref_rear_gr.1_unlist <- bind_rows(list("mod.1" = bind_rows(tern_ref_rear_gr.1_list[[1]], .id = "id"), 
                                               "mod.2" = bind_rows(tern_ref_rear_gr.1_list[[2]], .id = "id"),
                                               "mod.3" = bind_rows(tern_ref_rear_gr.1_list[[3]], .id = "id"),
                                               "mod.4" = bind_rows(tern_ref_rear_gr.1_list[[4]], .id = "id"),
                                               "mod.5" = bind_rows(tern_ref_rear_gr.1_list[[5]], .id = "id"),
                                               "mod.6" = bind_rows(tern_ref_rear_gr.1_list[[6]], .id = "id"),
                                               "mod.7" = bind_rows(tern_ref_rear_gr.1_list[[7]], .id = "id"),
                                               "mod.8" = bind_rows(tern_ref_rear_gr.1_list[[8]], .id = "id"),
                                               "mod.9" = bind_rows(tern_ref_rear_gr.1_list[[9]], .id = "id")), 
                                          .id = "model") 

tern_ref_rear_gr.1_p1 <- aggregate(tern_ref_rear_gr.1_unlist$mean_rear, 
                                      by=list(tern_ref_rear_gr.1_unlist$subbasin), 
                                      FUN=mean)

write.csv(tern_ref_rear_gr.1_p1, "D:/Ptaki_hydro/Obliczenia/R/Results/reference_hist/tern_ref_rear_gr.1.csv")


##############################################################################################
####### GROUP 2 ################################################################################
####### IHA group 2 is Magnitude  and duration of annual extreme  water condition 
# (Annual maxima, 1-day mean) 
# 1,3,7 day max

library("zoo")

#calculating rolling 1,3 an 7 day mean on list
#fun_tern_data_roll_list 

#apply to list
tern_data_ref_roll_list  <- lapply( data_ref, lapply, fun_tern_data_roll_list)

# calculate the minimum and maximum per year per RCH

library(plyr)
fun_data_roll_list_max 

tern_ref_list_gr.2  <- lapply( tern_data_ref_roll_list, lapply, fun_data_roll_list_max)

#obtain results for the reaches as a mean from 9 models
tern_ref_gr.2_unlist <- bind_rows(list("mod.1" = bind_rows(tern_ref_list_gr.2[[1]], .id = "id"), 
                                          "mod.2" = bind_rows(tern_ref_list_gr.2[[2]], .id = "id"),
                                          "mod.3" = bind_rows(tern_ref_list_gr.2[[3]], .id = "id"),
                                          "mod.4" = bind_rows(tern_ref_list_gr.2[[4]], .id = "id"),
                                          "mod.5" = bind_rows(tern_ref_list_gr.2[[5]], .id = "id"),
                                          "mod.6" = bind_rows(tern_ref_list_gr.2[[6]], .id = "id"),
                                          "mod.7" = bind_rows(tern_ref_list_gr.2[[7]], .id = "id"),
                                          "mod.8" = bind_rows(tern_ref_list_gr.2[[8]], .id = "id"),
                                          "mod.9" = bind_rows(tern_ref_list_gr.2[[9]], .id = "id")), 
                                     .id = "model") 

tern_ref_gr.2_unlist_p1 <- cbind(setNames(aggregate(tern_ref_gr.2_unlist$day01_max, 
                                                       by=list(tern_ref_gr.2_unlist$subbasin), 
                                                       FUN=mean), c("subbasin", "day01_mean")),
                                    setNames(aggregate(tern_ref_gr.2_unlist$day03_max, 
                                                       by=list(tern_ref_gr.2_unlist$subbasin), 
                                                       FUN=mean), c("subbasin", "day03_mean")),
                                    setNames(aggregate(tern_ref_gr.2_unlist$day07_max, 
                                                       by=list(tern_ref_gr.2_unlist$subbasin), 
                                                       FUN=mean), c("subbasin", "day07_mean")))

write.csv(tern_ref_gr.2_unlist_p1, "D:/Ptaki_hydro/Obliczenia/R/Results/reference_hist/tern_ref_gr.2.csv")
##### model/subbasin/value
tern_ref_gr.2_unlist_p2 <- cbind(setNames(aggregate(tern_ref_gr.2_unlist$day01_max, 
                                                       by=list(tern_ref_gr.2_unlist$model,
                                                               tern_ref_gr.2_unlist$subbasin), 
                                                       FUN=mean), c("model","subbasin", "day01_mean")),
                                    setNames(aggregate(tern_ref_gr.2_unlist$day03_max, 
                                                       by=list(tern_ref_gr.2_unlist$model,
                                                               tern_ref_gr.2_unlist$subbasin), 
                                                       FUN=mean), c("model","subbasin", "day03_mean")),
                                    setNames(aggregate(tern_ref_gr.2_unlist$day07_max, 
                                                       by=list(tern_ref_gr.2_unlist$model,
                                                               tern_ref_gr.2_unlist$subbasin), 
                                                       FUN=mean), c("model", "subbasin", "day07_mean")))

#########
####
#### Standardized for carastrophy analysis
#### 
#########

fun_tern_data_roll_list_st <- function(x){
  step1 <- dplyr::mutate(x, day01_mean = zoo::rollmean (x$st_Q, k = 1, fill = NA), #we have 1 measurment per day so this is acctually not necessary to calculate
                         day03_mean = zoo::rollmean (x$st_Q, k = 3, fill = NA),
                         day07_mean = zoo::rollmean (x$st_Q, k = 7, fill = NA))
  step1$Year <- format(as.Date(step1$date, format="%Y-%m-%d"),"%Y")
  step1$date2 <- as.POSIXct(step1$date, format="%Y-%m-%d")
  month <- as.integer(format(step1$date2, '%m'))
  day <- as.integer(format(step1$date2, '%d'))
  in_data3 <- filter(step1, month == 5 & day >= 11 | month == 6 | month == 7 & day <= 10)}

gc()

tern_data_ref_roll_list_st  <- lapply( data_ref_st, lapply, fun_tern_data_roll_list_st)

tern_ref_list_gr.2_st  <- lapply( tern_data_ref_roll_list_st, lapply, fun_data_roll_list_max)

tern_ref_gr.2_unlist_st <- bind_rows(list("mod.1" = bind_rows(tern_ref_list_gr.2_st[[1]], .id = "id"), 
                                             "mod.2" = bind_rows(tern_ref_list_gr.2_st[[2]], .id = "id"),
                                             "mod.3" = bind_rows(tern_ref_list_gr.2_st[[3]], .id = "id"),
                                             "mod.4" = bind_rows(tern_ref_list_gr.2_st[[4]], .id = "id"),
                                             "mod.5" = bind_rows(tern_ref_list_gr.2_st[[5]], .id = "id"),
                                             "mod.6" = bind_rows(tern_ref_list_gr.2_st[[6]], .id = "id"),
                                             "mod.7" = bind_rows(tern_ref_list_gr.2_st[[7]], .id = "id"),
                                             "mod.8" = bind_rows(tern_ref_list_gr.2_st[[8]], .id = "id"),
                                             "mod.9" = bind_rows(tern_ref_list_gr.2_st[[9]], .id = "id")), 
                                        .id = "model") 

tern_ref_gr.2_unlist_p1_st <- cbind(setNames(aggregate(tern_ref_gr.2_unlist_st$day01_max, 
                                                          by=list(tern_ref_gr.2_unlist_st$subbasin), 
                                                          FUN=mean), c("subbasin", "day01_mean")),
                                       setNames(aggregate(tern_ref_gr.2_unlist_st$day03_max, 
                                                          by=list(tern_ref_gr.2_unlist_st$subbasin), 
                                                          FUN=mean), c("subbasin", "day03_mean")),
                                       setNames(aggregate(tern_ref_gr.2_unlist_st$day07_max, 
                                                          by=list(tern_ref_gr.2_unlist_st$subbasin), 
                                                          FUN=mean), c("subbasin", "day07_mean")))

tern_ref_gr.2_unlist_p2_st <- cbind(setNames(aggregate(tern_ref_gr.2_unlist_st$day01_max, 
                                                          by=list(tern_ref_gr.2_unlist_st$model,
                                                                  tern_ref_gr.2_unlist_st$subbasin), 
                                                          FUN=mean), c("model","subbasin", "day01_mean")),
                                       setNames(aggregate(tern_ref_gr.2_unlist_st$day03_max, 
                                                          by=list(tern_ref_gr.2_unlist_st$model,
                                                                  tern_ref_gr.2_unlist_st$subbasin), 
                                                          FUN=mean), c("model","subbasin", "day03_mean")),
                                       setNames(aggregate(tern_ref_gr.2_unlist_st$day07_max, 
                                                          by=list(tern_ref_gr.2_unlist_st$model,
                                                                  tern_ref_gr.2_unlist_st$subbasin), 
                                                          FUN=mean), c("model", "subbasin", "day07_mean")))


# summary for analysis od regression equation
tern_ref_list_gr.2_st_reg <- map_df(tern_ref_list_gr.2_st, ~bind_rows(., .id = 'subbasin'), .id = 'model')

tern_ref_list_gr.2_st_reg_out <- select (tern_ref_list_gr.2_st_reg,-c(day01_max, day07_max))

write.csv(tern_ref_list_gr.2_st_reg_out, "D:/Ptaki_hydro/Obliczenia/R/Results/reg_eq/new2/tern_ref_list_gr.2_st_reg_out.csv")


##############################################################################################
####### GROUP 4 ################################################################################
####### IHA group 4 is  Frequency and duration of high and low pulses
# Number of low pulses within each water year, Mean or median duration of low pulses (days),
# Number of high pulses within each water year, Mean or median duration of high pulses (days)
# above Q3

gc()
# calculate percentiles and quartiles
fun_data_q3_period <- function(x) {
  ddply(x,.(subbasin), summarize,
        Q3=quantile(flow, 0.75))#find the 75% percentile for the period 1971-2000
}

data_ref_q3_period_in <- lapply(data_ref_julian, lapply, fun_data_q3_period)

fun_add_year <- function (x){
  x$Year <- format(as.Date(x$date, format="%Y-%m-%d"),"%Y");return(x)}

tern_vp_ref_list_y <- lapply(tern_vp_ref_list, lapply, fun_add_year)


tern_vp_ref_list_a <- bind_rows(list("mod.1" = bind_rows( tern_vp_ref_list_y[[1]] , .id = "id"), 
                                        "mod.2" = bind_rows( tern_vp_ref_list_y[[2]], .id = "id"),
                                        "mod.3" = bind_rows( tern_vp_ref_list_y[[3]], .id = "id"),
                                        "mod.4" = bind_rows( tern_vp_ref_list_y[[4]], .id = "id"),
                                        "mod.5" = bind_rows( tern_vp_ref_list_y[[5]], .id = "id"),
                                        "mod.6" = bind_rows( tern_vp_ref_list_y[[6]], .id = "id"),
                                        "mod.7" = bind_rows( tern_vp_ref_list_y[[7]], .id = "id"),
                                        "mod.8" = bind_rows( tern_vp_ref_list_y[[8]], .id = "id"),
                                        "mod.9" = bind_rows( tern_vp_ref_list_y[[9]], .id = "id")),
                                   .id = "model") 

data_ref_q3_period_list_a <- bind_rows(list("mod.1" = bind_rows(data_ref_q3_period_in[[1]], .id = "id"), 
                                               "mod.2" = bind_rows(data_ref_q3_period_in[[2]], .id = "id"),
                                               "mod.3" = bind_rows(data_ref_q3_period_in[[3]], .id = "id"),
                                               "mod.4" = bind_rows(data_ref_q3_period_in[[4]], .id = "id"),
                                               "mod.5" = bind_rows(data_ref_q3_period_in[[5]], .id = "id"),
                                               "mod.6" = bind_rows(data_ref_q3_period_in[[6]], .id = "id"),
                                               "mod.7" = bind_rows(data_ref_q3_period_in[[7]], .id = "id"),
                                               "mod.8" = bind_rows(data_ref_q3_period_in[[8]], .id = "id"),
                                               "mod.9" = bind_rows(data_ref_q3_period_in[[9]], .id = "id")), 
                                          .id = "model") 

#count how many days during the vulnerability period are higher than 75% quartile 
#
tern_ref_gr.4_p1 <- full_join(tern_vp_ref_list_a, data_ref_q3_period_list_a, 
                                 by=c("model","subbasin")) %>% 
  mutate(condition = (flow > Q3))  

tern_ref_gr.4_p1$condition2 <- as.integer(tern_ref_gr.4_p1$condition)


tern_ref_gr.4_p2 <- aggregate(tern_ref_gr.4_p1$condition2, 
                                 by=list(tern_ref_gr.4_p1$model, tern_ref_gr.4_p1$subbasin), FUN=sum)

#mean number of days above Q3 per the 27 year period 2024-2050
tern_ref_gr.4_p2$yearly <- tern_ref_gr.4_p2$x/27

# calculate a mean out of all the 9 models per subbasin
tern_ref_gr.4_p3 <- aggregate(tern_ref_gr.4_p2$yearly, 
                                 by=list(tern_ref_gr.4_p2$Group.2), FUN=mean)

# write gr.4 to a file
write.csv(tern_ref_gr.4_p3, "D:/Ptaki_hydro/Obliczenia/R/Results/reference_hist/tern_ref_gr.4.csv")

#############################################################
#############################################################
# summary for analysis od regression equation
#############################################################

tern_ref_gr.4_p4 <- aggregate(tern_ref_gr.4_p1$condition2, 
                              by=list(tern_ref_gr.4_p1$model, tern_ref_gr.4_p1$subbasin,
                                      tern_ref_gr.4_p1$Year), FUN=sum)

write.csv(tern_ref_gr.4_p4, "D:/Ptaki_hydro/Obliczenia/R/Results/reg_eq/new/tern_ref_gr.4_reg_eq.csv")
