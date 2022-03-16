# SCRIPT NO. 4 A

install.packages("dplyr")
install.packages("plyr")
install.packages("RColorBrewer")
install.packages("lubridate")
install.packages("tidyverse")
install.packages("IHA", repos="http://R-Forge.R-project.org")
install.packages("packrat")
install.packages("tibble")
install.packages("ggplot2")
install.packages("ggplot")
install.packages("readr")
install.packages("purrr")

library(readr)
library("tidyverse")
library("plyr")
library("dplyr")
#packrat - package that manages all the differences between versions of packages and R
library("packrat")
library("tibble")
#library("EflowStats")
library("ggplot2")
library("lubridate")
library("purrr")
library("ggforce")
library("reshape2")

setwd ("D:/Ptaki_hydro/Obliczenia/R/IHA")

#island locations paired with subbasins
island_reach <- data.frame( read.csv("wyspa_subbasin.csv"))

#Narrow down the Q data to subbasins with islands
#there are 19 subbasins as 3 describe double bird islands

Q_mod_Wisla <- data.frame( read.csv("Q_mod_Wisla.csv"))

#because data was read from a csv file the dates were converted to factor, now they need to be converted to date again
Q_mod_Wisla$date <- as.Date(Q_mod_Wisla$mdate, format="%Y-%m-%d")
#yyyy-mm-dd

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Data with vulnerability period for Śmieszka : 11.04 - 10.06
# bh.gull_Q_mod - data with modelled streamflow for vulnerability period for BHG

#black-headed gull #Śmieszka #(Chroicocephalus ridibundus) nesting success data
# bh_gull_NS

library(hrbrthemes)
library(scales)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
############################ graph for comparing simulated and observed streamflow in each year
############################ during vulnerability period

# COMAPRE BY YEAR ######
# narrow down the subbasins to 5 that also have a gauging station
extract_gauge <- function (x) {
  subset(x, x$RCH == 1087|
           x$RCH ==1264|
           x$RCH ==1501|
           x$RCH ==1545|
           x$RCH ==1875)
}

#apply the function to a list
bh.gull_sub_list <- lapply(bh.gull_Q_mod_list , extract_gauge)

#a function to create and save a plot
obs_vs_sim <- function(x, y) {  
  plot_type1 <- ggplot()+
    geom_line(data=x, aes(x=date, y=FLOW_OUTcms), color = "blue")+ #simulated
    geom_line(data=y, aes(x=date, y=Q), color="red")+ #observed
    facet_wrap(~RCH, ncol=1)+
    labs(x = "year", y="Streamflow") +
    ggtitle(y$Year) + 
    scale_x_date(date_breaks = "10 days")+
    theme(plot.title = element_text(hjust = 0.5))+#positioning the title in the center
    coord_cartesian( ylim = c(0, 6000))
  print(plot_type1)
  ggsave(plot_type1, file = paste0("D:/Ptaki_hydro/Obliczenia/R/Kalibracja_Q_PL/Q_mod_obs/bh_gull/",
                                   y$Year, ".jpg"), #saving the plot into a folder
         device = "jpg",
         width = 11,
         height = 16)
}

#apply the function to two lists and produce .jpg plots into the folder
obs_vs_sim_list <- mapply(obs_vs_sim, bh.gull_sub_list, bh.gull_Q_obs_list, SIMPLIFY = FALSE)

# COMPARE BY LOCATION ######
###### for Puławy

bh.gull_mod_Pulawy_vp <- subset(bh.gull_Q_mod, bh.gull_Q_mod$RCH == "1545") # only for the vulnerability period
#bh.gull_mod_Pulawy_vp$date = as.Date(Smieszka_mod_Pulawy_vp$date, format="%Y-%m-%d")
#bh.gull_mod_Pulawy_vp$YEAR <- format(as.Date(Smieszka_mod_Pulawy_vp$date, format="%Y-%m-%d"),"%Y")

bh.gull_obs_Pulawy_vp <- subset(bh.gull_Q_obs, bh.gull_Q_obs$RCH =="1545")#checked subbasin with Puławy
#to make sure months are displayed in English

Sys.setlocale("LC_ALL", "English")
Sys.setenv("LANGUAGE"="En")

bimonthly <- function(x) {
  x_range <- range(x, na.rm = TRUE)
  date_range <- c(floor_date(x_range[1], "month"),
                ceiling_date(x_range[2], "month"))
  monthly <- seq(date_range[1], date_range[2], by = "1 month")
  sort(c(monthly, monthly + days(14)))
}


bh.gull_obs_vs_mod_location <- ggplot()+
    geom_line(data=bh.gull_mod_Pulawy_vp, aes(x=date, y=FLOW_OUTcms), color = "blue")+ #simulated
    geom_line(data=bh.gull_obs_Pulawy_vp, aes(x=date, y=Q), color="red")+ #observed
    facet_wrap( ~ format(date, "%Y"), scales = "free_x", ncol=1)+ # free_x makes the plot narrow down to vulnerability period
    labs(x = "year", y="Q m3/s") +
    ggtitle("Black-headed gull") + 
    scale_x_date(date_labels = "%b %d", breaks = bh.gull_breaks_in)+
    theme(plot.title = element_text(hjust = 0.5), #positioning the title in the center
          text = element_text(size = 20))+
    coord_cartesian( ylim = c(0, 6000)) 
    


print(bh.gull_obs_vs_mod_location)
  
ggsave(bh.gull_obs_vs_mod_location, 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Kalibracja_Q_PL/Q_mod_obs/bh_gull/Smieszka_Pulawy.jpg", 
                     ".jpg"), #saving the plot into a folder
      device = "jpg",
      width = 8,
      height = 25)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
############### graph with ribbon from min to max of all locations per year with line showing mean

#set function to find the min and max for the ribbon
min_max <- function (x){
  dat <- data.frame(y = mean(x),
                    ymin = min(x),
                    ymax = max(x))
  return(dat)
  }
# try for year 2004 (1st object in the list)
graph_bh.gull_2004 <- ggplot(bh.gull_Q_mod_list[[1]], aes(x=date, y=FLOW_OUTcms))+ 
  stat_summary(geom = "line", fun= mean, color="olivedrab", size=1.1)+ 
  stat_summary(geom = "line", fun= min, color="grey")+ 
  stat_summary(geom = "line", fun= max, color="grey")+
  stat_summary(geom = "ribbon", fun.data = min_max, alpha= 0.3, fill ="green")+
  scale_x_date(date_breaks = "10 days")+
  coord_cartesian( ylim = c(0, 4000))

print(graph_bh.gull_2004)

# try for year 2010 (7th object in the list)
graph_bh.gull_2010 <- ggplot(bh.gull_Q_mod_list[[7]], aes(x=date, y=FLOW_OUTcms))+ 
  stat_summary(geom = "line", fun= mean, color="olivedrab", size=1.1)+ 
  stat_summary(geom = "line", fun= min, color="grey")+ 
  stat_summary(geom = "line", fun= max, color="grey")+
  stat_summary(geom = "ribbon", fun.data = min_max, alpha= 0.3, fill ="green")+
  scale_x_date(date_breaks = "10 days")+
  coord_cartesian( ylim = c(0, 4000))

print(graph_bh.gull_2010)

########### Q for all locations in single line plot ###############################

# function that was changed to a single data frame
#bh.gull_line_plot <- function (x) {
#  ggplot(x, aes(date, FLOW_OUTcms, group = RCH)) +
#    geom_line()+ 
#    facet_wrap( ~ format(date, "%Y"),  ncol=1)+
#    labs(y="Q m3/s")+
#    scale_x_date(date_labels = "%b %d", limits = as.Date(format (date, "%m-%d"), "%m-%d"))+
#    theme (panel.grid.minor = element_blank()) +
#    coord_cartesian( ylim = c(0, 5000))
#}
## apply it to a list
#bh.gull_line_plot_list <- lapply(bh.gull_Q_mod_list , bh.gull_line_plot)

# display the first graph from the list which is for year 2004
#plot(bh.gull_line_plot_list[[1]] )

bh.gull_breaks <- c("0411", "0520", "0420", "0531", "0510", "0610") 
years <- 2004:2018; 
bh.gull_breaks_in <- tibble(crossing(years, bh.gull_breaks), 
                            date = ymd(paste0(years,bh.gull_breaks))) %>% pull(date) 

bh.gull_line_plot_df <- bh.gull_Q_mod_list %>%
  # Make a single data.frame from the list of data.frames
  bind_rows() %>% 
  # Create variable year
  mutate(year = year(date)) %>% 
  ggplot(aes(date, FLOW_OUTcms, group = RCH)) +
  geom_line() + 
  # Using year for facet, with free scales and a single column
  facet_wrap(~year, scales = "free", ncol = 1)+
  ylab("Q m3/s")+
  theme (panel.grid.minor = element_blank(),
         text = element_text(size = 20)) +
  scale_x_date(date_labels = "%b %d", 
               breaks = bh.gull_breaks_in)+
  coord_cartesian( ylim = c(0, 5000))



########### GHANT CHART VULNERABILITY PERIOD

library("reshape2")
#11.04 - 10.06
task1_bh.gull <- c('Laying eggs', '2004-04-11', '2004-05-20')
task2_bh.gull <- c('Incubating', '2004-04-20', '2004-05-31')
task3_bh.gull <- c('Rearing chicks', '2004-05-10', '2004-06-10')

# vp - vulnerability period
vp_bh.gull <- as.data.frame(rbind(task3_bh.gull, task2_bh.gull, task1_bh.gull))
names(vp_bh.gull) <- c('task', 'start', 'end')
vp_bh.gull$task <- factor(vp_bh.gull$task, levels = vp_bh.gull$task)
vp_bh.gull$start <- as.Date(vp_bh.gull$start)
vp_bh.gull$end <- as.Date(vp_bh.gull$end)
vp_bh.gull_melted <- melt(vp_bh.gull, measure.vars = c('start', 'end'))

# starting date to begin plot (earlier start_date)
bh.gull_start_date <- as.Date('2004-04-11')


bh.gull_vp_plot <- ggplot(vp_bh.gull_melted, aes(value, task)) + 
  geom_line(size = 7, colour="seagreen4") +
  labs(x = '', y = '', title = 'Vulnerability period') +
  theme_bw(base_size = 10) +
  theme(text = element_text(size=20))+
  theme(plot.title = element_text(hjust = 0),
        panel.grid.major.x = element_line(colour="black", linetype = "dashed"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
        #axis.text.x = element_text(angle = 0, size = 10),
        #axis.text.y = element_text(angle = 0, size = 10)) +
  scale_x_date(date_labels = "%b %d", limits = c(bh.gull_start_date, NA), 
               breaks = (as.Date(c("2004-04-11", "2004-05-20",
                                   '2004-04-20', '2004-05-31',
                                   '2004-05-10', '2004-06-10'))))

print(bh.gull_vp_plot)    

################# COMBINING GRAPHS ON SINGLE PAGE

#install.packages("ggpubr")
library("ggpubr")

#ggarrange(plotlist = Smieszka_line_plot_list,nrow = 5,ncol = ceiling(length(Smieszka_line_plot_list)/2))

#bh.gull_line_plot_1 <- ggarrange(plotlist = bh.gull_line_plot_list,nrow = 15,ncol = 1)

bh.gull_box_plot_2 <- ggarrange(plotlist = bh.gull_box_sin_list,nrow = 15, ncol = 1)

#install.packages("patchwork")
library("patchwork")

#arrangement of plots with patchwork package
bh.gull_layout <- ((bh.gull_line_plot_df  + bh.gull_box_plot_2))+
                  bh.gull_vp_plot +
                  plot_layout(width = c(1,0.5), height = c(3.5,0.3)) #width of 1st and 2nd column
                                                                    # hight of 1st row and 2nd row

### ??? aline the dates in the vulnerability period graph and hydrogrpahs
### ??? change the labels on the y axis to title of the columns

print(bh.gull_layout) 
#save as width=700 and height=2000

ggsave(bh.gull_layout, 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Kalibracja_Q_PL/Q_mod_obs/bh_gull/bh.gull_layout1.jpg", 
                     ".jpg"), #saving the plot into a folder
       device = "jpg",
       width = 10,
       height = 29)

# other packages for arrangement of plots
#install.packages("grid")
library("grid")
#install.packages("gridExtra")
library("gridExtra")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  IHA  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


################ Create data frames for each subbasin for the whole year
RCH_split <- split(Q_mod_Wisla, Q_mod_Wisla$RCH)

#add a year column
RCH_split <- lapply(RCH_split, function(x) {
  x$Year <- format(as.Date(x$date, format="%Y-%m-%d"),"%Y")
  x
})
################ Create data frames for each subbasin for vulnerability period
RCH_split_bh.gull <- split(bh.gull_Q_mod, bh.gull_Q_mod$RCH)

################ Calculate Q mean for 2004-2018 for standardisation

#RCH_split_mean <-lapply (RCH_split, function(x) list('Q'=mean(x[["FLOW_OUTcms"]])))

#change to 3S list of data frames
RCH_split_mean <- lapply (RCH_split, function(x) {
  x$Q_mean <- mean(x$FLOW_OUTcms)
  x})
  
#RCH_split_bh.gull_st1 <- Map( function(x, y) c(list(standQ=x$FLOW_OUTcms/y$Q_mean), x),
#                             RCH_split_bh.gull,RCH_split_mean)

#standardise data for the whole year
RCH_split_st1 <- lapply (RCH_split_mean, function(x) 
  { x$standQ <- x$FLOW_OUTcms/x$Q_mean
  x})

####### Altering IHA functions ###############################################################

##############################################################################################
####### GROUP 1 ################################################################################
####### IHA group 1 Mean or median value for each calendar month replaced with means for periods of
#laying eggs, incubating and rearing chicks

######### laying eggs 11.04 - 20.05 ################################
bh.gull_interv_le1 <- as.interval(ddays(40), start = ISOdate(2004:2018, 4, 11, 0, tz = 'Europe/Warsaw'))
bh.gull_interv_le2 <- bh.gull_Q_mod$date %within% as.list(bh.gull_interv_le1)
bh.gull_interv_le3 <- (bh.gull_Q_mod$date)[which(bh.gull_interv_le2)] # values date format
##Narrow down the Q data to vulnerability period
# unlisted
#bh.gull_interv_le <- bh.gull_Q_mod[bh.gull_Q_mod$date %in% bh.gull_interv_le3, ] 
# for list
bh.gull_interv_le_list <- lapply(RCH_split_bh.gull, function(x) {
  x[x$date %in% bh.gull_interv_le3, ]  })

#standardised
bh.gull_interv_le_list_st <- lapply(RCH_split_st1, function(x) {
  x[x$date %in% bh.gull_interv_le3, ]  })

##calculate mean streamflow Q during the bh.gull vulnerability period for laying eggs 
# unlisted
#bh.gull_le_gr.1 <- aggregate(bh.gull_interv_le[, 7], list(bh.gull_interv_le$Year), mean) %>%
 # remove_rownames %>% 
#  column_to_rownames(var="Group.1")

# for list
bh.gull_le_gr.1_list <- lapply(bh.gull_interv_le_list, function(x) {
  aggregate(x$FLOW_OUTcms, list(x$Year), mean) %>%
  remove_rownames %>% 
  column_to_rownames(var="Group.1")
  })
#standardised
bh.gull_le_gr.1_list_st <- lapply(bh.gull_interv_le_list_st, function(x) {
  aggregate(x$standQ, list(x$Year), mean) %>%
    remove_rownames %>% 
    column_to_rownames(var="Group.1")
})

##change names of columns
# unlisted
#names(bh.gull_le_gr.1)[names(bh.gull_le_gr.1) == 'x'] <- 'gr.1_mean_LE'

######### incubating 20.04 - 31.05 ###################################
bh.gull_interv_i1 <- as.interval(ddays(42), start = ISOdate(2004:2018, 4, 20, 0, tz = 'Europe/Warsaw'))
bh.gull_interv_i2 <- bh.gull_Q_mod$date %within% as.list(bh.gull_interv_i1)
bh.gull_interv_i3 <- (bh.gull_Q_mod$date)[which(bh.gull_interv_i2)]
#Narrow down the Q data to vulnerability period
## unlisted
#bh.gull_interv_i <- bh.gull_Q_mod[bh.gull_Q_mod$date %in% bh.gull_interv_i3, ] 
#for list
bh.gull_interv_i_list <- lapply(RCH_split_bh.gull, function(x) {
  x[x$date %in% bh.gull_interv_i3, ]  })

#standardised
bh.gull_interv_i_list_st <- lapply(RCH_split_st1, function(x) {
  x[x$date %in% bh.gull_interv_i3, ]  })

#calculate mean streamflow Q during the bh.gull vulnerability period for incubation 
# unlisted
#bh.gull_i_gr.1 <- aggregate(bh.gull_interv_i[, 7], list(bh.gull_interv_i$Year), mean) %>%
#  remove_rownames %>% 
#  column_to_rownames(var="Group.1")

# for list
bh.gull_i_gr.1_list <- lapply(bh.gull_interv_i_list, function(x) {
  aggregate(x$FLOW_OUTcms, list(x$Year), mean) %>%
    remove_rownames %>% 
    column_to_rownames(var="Group.1")})

#standardised
bh.gull_i_gr.1_list_st <- lapply(bh.gull_interv_i_list_st, function(x) {
  aggregate(x$standQ, list(x$Year), mean) %>%
    remove_rownames %>% 
    column_to_rownames(var="Group.1")
})

#change names of columns
# unlisted
#names(bh.gull_i_gr.1)[names(bh.gull_i_gr.1) == 'x'] <- 'gr.1 mean Incub'

### rearing chicks 10.05 - 10.06 #################################
bh.gull_interv_rc1 <- as.interval(ddays(32), start = ISOdate(2004:2018, 5, 10, 0, tz = 'Europe/Warsaw'))
bh.gull_interv_rc2 <- bh.gull_Q_mod$date %within% as.list(bh.gull_interv_rc1)
bh.gull_interv_rc3 <- (bh.gull_Q_mod$date)[which(bh.gull_interv_rc2)]
#Narrow down the Q data to vulnerability period
#unlisted
#bh.gull_interv_rc <- bh.gull_Q_mod[bh.gull_Q_mod$date %in% bh.gull_interv_rc3, ] 
#for list
bh.gull_interv_rc_list <- lapply(RCH_split_bh.gull, function(x) {
  x[x$date %in% bh.gull_interv_rc3, ]  })

#standardised
bh.gull_interv_rc_list_st <- lapply(RCH_split_st1, function(x) {
  x[x$date %in% bh.gull_interv_rc3, ]  })


#calculate mean streamflow Q during the bh.gull vulnerability period for rearing chicks 
#unlisted
#bh.gull_rc_gr.1 <- aggregate(bh.gull_interv_rc[, 7], list(bh.gull_interv_rc$Year), mean) %>%
#  remove_rownames %>% 
#  column_to_rownames(var="Group.1")

#for list
bh.gull_rc_gr.1_list <- lapply(bh.gull_interv_rc_list, function(x) {
  aggregate(x$FLOW_OUTcms, list(x$Year), mean) %>%
    remove_rownames %>% 
    column_to_rownames(var="Group.1")})

#standardised
bh.gull_rc_gr.1_list_st <- lapply(bh.gull_interv_rc_list_st, function(x) {
  aggregate(x$standQ, list(x$Year), mean) %>%
    remove_rownames %>% 
    column_to_rownames(var="Group.1")
})

#change names of columns
#unlisted
#names(bh.gull_rc_gr.1)[names(bh.gull_rc_gr.1) == 'x'] <- 'gr.1 mean RC'

#### joining results into single dataframe
#unlisted
#bh.gull_gr.1 <- cbind(bh.gull_le_gr.1, bh.gull_i_gr.1, bh.gull_rc_gr.1)

#for list
bh.gull_list_gr.1 <- mapply(cbind, bh.gull_le_gr.1_list,bh.gull_i_gr.1_list, bh.gull_rc_gr.1_list, SIMPLIFY=FALSE)

##change names of columns in the list
colnames.gr1 = c("gr.1_mean_LE","gr.1_mean_Incub", "gr.1_mean_RC") 
bh.gull_list_gr.1 <- lapply(bh.gull_list_gr.1, setNames, colnames.gr1)


#standardised
bh.gull_list_gr.1_st <- mapply(cbind, bh.gull_le_gr.1_list_st, 
                               bh.gull_i_gr.1_list_st, bh.gull_rc_gr.1_list_st, 
                               SIMPLIFY=FALSE)

bh.gull_list_gr.1_st <- lapply(bh.gull_list_gr.1_st, setNames, colnames.gr1)


##############################################################################################
####### GROUP 2 ################################################################################
####### IHA group 2 is Magnitude  and duration of annual extreme  water condition 
# (Annual minima, 1-day mean & Annual maxima, 1-day mean) 

library("zoo")

#calculating rolling 1,3 an 7 day mean on list (DOESN'T WORK, I'm using the code for data frame below)
Q_mod_Wisla_roll_list <- RCH_split %>%
  lapply(dplyr::mutate(day01_mean = zoo::rollmean (FLOW_OUTcms, k = 1, fill = NA), #we have 1 measurment per day so this is acctually not necessary to calculate
                day03_mean = zoo::rollmean (FLOW_OUTcms, k = 3, fill = NA),
                day07_mean = zoo::rollmean (FLOW_OUTcms, k = 7, fill = NA)))

#calculating rolling 1,3 an 7 day mean on data frame
Q_mod_Wisla_roll <- Q_mod_Wisla %>%
dplyr::arrange(desc(date)) %>% 
  dplyr::group_by(RCH) %>% 
  dplyr::mutate(day01_mean = zoo::rollmean (FLOW_OUTcms, k = 1, fill = NA), #we have 1 measurment per day so this is acctually not necessary to calculate
                day03_mean = zoo::rollmean (FLOW_OUTcms, k = 3, fill = NA),
                day07_mean = zoo::rollmean (FLOW_OUTcms, k = 7, fill = NA)) %>% 
  dplyr::ungroup()

#standardised
Q_mod_Wisla_roll_st <- lapply(RCH_split_st1, function (x) {
   list(day01_mean = zoo::rollmean (x$standQ, k = 1, fill = NA), #we have 1 measurment per day so this is acctually not necessary to calculate
        day03_mean = zoo::rollmean (x$standQ, k = 3, fill = NA),
        day07_mean = zoo::rollmean (x$standQ, k = 7, fill = NA),
        date= as.Date(x$date),
        RCH= x$RCH,
        Year = x$Year,
        standQ = x$standQ)
  })
 
# narrow it down to the vulnerability period of bh.gull
Q_mod_Wisla_roll_vp_bh.gull <- Q_mod_Wisla_roll[Q_mod_Wisla_roll$date %in% bh.gull_dat, ] 

#standardised
Q_mod_Wisla_roll_vp_bh.gull_st_in <- lapply(Q_mod_Wisla_roll_st, function(x) 
  Map(`[`, x, list(x$date %in% bh.gull_dat)))

Q_mod_Wisla_roll_vp_bh.gull_st <- lapply(Q_mod_Wisla_roll_vp_bh.gull_st_in, bind_rows)


#add a year column
Q_mod_Wisla_roll_vp_bh.gull$Year <- 
  format(as.Date(Q_mod_Wisla_roll_vp_bh.gull$date, format="%Y-%m-%d"),"%Y")

#Create lists according to RCH
RCH_names <- c(910,950,1012,1087,1134,1240,1264,1289,1329,1358,1501,1545,1565,1601,1629,1727,1748,1875)

Q_mod_Wisla_roll_vp_bh.gull_list <- 
  Q_mod_Wisla_roll_vp_bh.gull %>% group_split(Q_mod_Wisla_roll_vp_bh.gull$RCH) %>% setNames(RCH_names)

# calculate the minimum and maximum per year per RCH
 
library(plyr)
bh.gull_list_gr.2 <- lapply(Q_mod_Wisla_roll_vp_bh.gull_list,function(x) 
  ddply(x,.(RCH,Year), summarize,
        day01_min=min(day01_mean), day01_max=max(day01_mean),
        day03_min=min(day03_mean), day03_max=max(day03_mean),
        day07_min=min(day07_mean), day07_max=max(day07_mean) 
        ))

#standardised
bh.gull_list_gr.2_st <- lapply(Q_mod_Wisla_roll_vp_bh.gull_st, function(x) 
  ddply(x,.(RCH,Year), summarize,
        day01_min=min(day01_mean), day01_max=max(day01_mean),
        day03_min=min(day03_mean), day03_max=max(day03_mean),
        day07_min=min(day07_mean), day07_max=max(day07_mean) 
  ))


##############################################################################################
####### GROUP 3 ################################################################################
####### IHA group 3 is  Timing of annual extreme water conditions,
#Julian date of each annual 1-day maximum and 1-day minimum 


#bh.gull vp:
#  11.04 is 101 or 102 (Leap) julian day
#  10.06 is 161 or 162 (Leap) julian day
# Leap years: 2004, 2008, 2012, 2016

#add julian day
RCH_split <- lapply(RCH_split, function(x) {
  x$julian <- yday(x$date);return(x)})

bh.gull_list_gr.3 <- lapply(RCH_split,function(x) 
  ddply(x,.(RCH,Year), summarize,
        min=min(FLOW_OUTcms), max=max(FLOW_OUTcms),
        julian_min= which.min(FLOW_OUTcms),
        julian_max= which.max(FLOW_OUTcms),#gives julian day of the min/max 
        # i dont understand how this above works as it doesn't specify i want the value from the julian column
        # I sppose it counted the order of the value occurence in each year without using the julian column
        #chech if julian date is within vulnerability period range (101 and 162 days) and count as 1 if yes, 0 as no.
        vp_min = case_when(julian_min >= 101 & julian_min <= 162 ~ 1, TRUE ~ 0),
        vp_max = case_when(julian_max >= 101 & julian_max <= 162 ~ 1, TRUE ~ 0)))


##############################################################################################
####### GROUP 4 ################################################################################
####### IHA group 4 is  Frequency and duration of high and low pulses
# Number of low pulses within each water year, Mean or median duration of low pulses (days),
# Number of high pulses within each water year, Mean or median duration of high pulses (days)

# calculate percentiles and quartiles for the 15 year period
RCH_split_q13_period <- lapply(RCH_split,function(x) 
  ddply(x,.(RCH), summarize,
        P0.05=quantile(FLOW_OUTcms, 0.05),#find the 10% percentile for 15 year period
        Q1=quantile(FLOW_OUTcms, 0.25),#find the 25% quartile for 15 year period
        Q3=quantile(FLOW_OUTcms, 0.75),#find the 75% quartile for 15 year period
        P0.95=quantile(FLOW_OUTcms, 0.95)#find the 90% percentile for 15 year period
        ))

# reorganize list so it matches the structure of the other datasets        
bh.gull_Q_mod_list_RCH <- bh.gull_Q_mod %>% group_split(bh.gull_Q_mod$RCH) %>% setNames(RCH_names)

#count how many days during the vulnerability period are higher than 75% quartile and lower than 25%
# I need to use the bh.gull_Q_mod_list_RCH
bh.gull.gr4_part1 <- Map(function(x, y) aggregate(FLOW_OUTcms > cbind(Q3, P0.95)~Year, merge(x, y, all = TRUE,
                            na.action = 'na.pass'), sum, na.rm = TRUE, na.action = 'na.pass'), 
            bh.gull_Q_mod_list_RCH, RCH_split_q13_period)

bh.gull.gr4_part2 <- Map(function(x, y) aggregate(FLOW_OUTcms < cbind(P0.05, Q1)~Year, merge(x, y, all = TRUE,
                             na.action = 'na.pass'), sum, na.rm = TRUE, na.action = 'na.pass'), 
             bh.gull_Q_mod_list_RCH, RCH_split_q13_period)

# cbind the two parts
bh.gull_gr4_parts <- Map(cbind, bh.gull.gr4_part1, bh.gull.gr4_part2) 
# remove Year appearing twice  
bh.gull_list_gr.4 <-  lapply(bh.gull_gr4_parts , "[", -c(4))

#calculate % of time above Q3 etc.

######################################################################
######## ??? how to draw regression plots between IHA values and Nesting success for all locations together on single plot

# use only b_gull_list

# We plot the independent variable on the x axis and the dependent variable on the y axis.
# Regression analysis also gives us a value called R^2, R squared. This tells us how much of the variation 
# in the y axis variable's values is accounted for by the variation in the x axis variable's values.

#### ???? I'm not sure if I asigned x and y correctly

#combine all 4 groups of indicators with NS

bh.gull_list_parts <- Map(cbind,  bh.gull_list_gr.2, bh.gull_list_gr.1, bh.gull_list_gr.3, bh.gull_list_gr.4,
                          bh_gull_NS_list) 

#standardised
bh.gull_list_parts_st <- Map(cbind,  bh.gull_list_gr.2_st, bh.gull_list_gr.1_st, bh.gull_list_gr.3, bh.gull_list_gr.4,
                          bh_gull_NS_list) 



# remove columns appearing twice or no longer needed
bh.gull_list_gr.all_NS <-  lapply(bh.gull_list_parts , "[", -c(12:17,20,27))

#standardised
bh.gull_list_gr.all_NS_st <-  lapply(bh.gull_list_parts_st , "[", -c(12:17,20,27))

library(data.table)
bh.gull_iha_foredit <- rbindlist(bh.gull_list_gr.all_NS,  fill=TRUE)
## standardised
bh.gull_iha_foredit_st <- rbindlist(bh.gull_list_gr.all_NS_st,  fill=TRUE)

#export in order to combine several NS columns into one manually
write.csv(bh.gull_iha_foredit, "bh.gull_iha_foredit.csv")
## standardised
write.csv(bh.gull_iha_foredit_st, "bh.gull_iha_foredit_st.csv")

#import edited file
bh.gull_iha_edit <-read.csv("bh.gull_iha_edit.csv")
## standardised
bh.gull_iha_edit_st <-read.csv("bh.gull_iha_edit_st.csv")

library("ggpubr")
# GROUP 1
# values
bh.gull_graph_mean_LE <- ggscatter(bh.gull_iha_edit, x = "gr.1.mean.LE", y = "NS", use="complete.obs", 
                              add = "reg.line", conf.int = TRUE, 
                              cor.coef = TRUE, cor.method = "pearson",
                              xlab = "gr.1_mean_LE", ylab = "NS",
                              cor.coeff.args = 
                                list(method = "pearson", label.x.npc = 0.7, label.y.npc = 1, label.sep = "\n"),
                              ylim = c(0, 2))
print(bh.gull_graph_mean_LE)

  ## standardised
  bh.gull_graph_mean_LE_st <- ggscatter(bh.gull_iha_edit_st, x = "gr.1_mean_LE", y = "NS", use="complete.obs", 
                                     add = "reg.line", conf.int = TRUE, 
                                     cor.coef = TRUE, cor.method = "pearson",
                                     xlab = "gr.1_mean_LE", ylab = "NS", 
                                     cor.coeff.args = 
                                       list(method = "pearson", label.x = 3, label.y = 1.5, label.sep = "\n"),
                                     cor.coef.size = c(6), 
                                      ylim = c(0, 2),
                                      xlim = c(0, 4.5)) %>%
                                       ggpar(font.x=c(20), font.y=c(20),font.tickslab=c(20))

                                     
  print(bh.gull_graph_mean_LE_st)  
  
# values
bh.gull_graph_mean_Incub <- ggscatter(bh.gull_iha_edit, x = "gr.1.mean.Incub", y = "NS", use="complete.obs", 
                           add = "reg.line", conf.int = TRUE, 
                           cor.coef = TRUE, cor.method = "pearson",
                           xlab = "gr.1_mean_Incub", ylab = "NS",
                           cor.coeff.args = 
                             list(method = "pearson", label.x.npc = 0.6, label.y.npc = 1, label.sep = "\n"),
                           ylim = c(0, 2))
  print(bh.gull_graph_mean_Incub)
  ## standardised
  bh.gull_graph_mean_Incub_st <- ggscatter(bh.gull_iha_edit_st, x = "gr.1_mean_Incub", y = "NS", use="complete.obs", 
                                        add = "reg.line", conf.int = TRUE, 
                                        cor.coef = TRUE, cor.method = "pearson",
                                        xlab = "gr.1_mean_Incub", ylab = "NS",
                                        cor.coeff.args = 
                                          list(method = "pearson", label.x = 3, label.y = 1.5, label.sep = "\n"),
                                        cor.coef.size = c(13),
                                        ylim = c(0, 2),
                                        xlim = c(0, 4.5))%>%
                                        ggpar(font.x=c(40), font.y=c(40),font.tickslab=c(40))
  
  print(bh.gull_graph_mean_Incub_st)  
  
# values
bh.gull_graph_mean_RC <- ggscatter(bh.gull_iha_edit, x = "gr.1.mean.RC", y = "NS", use="complete.obs", 
                          add = "reg.line", conf.int = TRUE, 
                          cor.coef = TRUE, cor.method = "pearson",
                          xlab = "gr.1_mean_RC", ylab = "NS",
                          cor.coeff.args = 
                            list(method = "pearson", label.x.npc = 0.6, label.y.npc = 1, label.sep = "\n"),
                          ylim = c(0, 2))
  print(bh.gull_graph_mean_RC)
  ## standardised
  bh.gull_graph_mean_RC_st <- ggscatter(bh.gull_iha_edit_st, x = "gr.1_mean_RC", y = "NS", use="complete.obs", 
                                     add = "reg.line", conf.int = TRUE, 
                                     cor.coef = TRUE, cor.method = "pearson",
                                     xlab = "gr.1_mean_RC", ylab = "NS",
                                     cor.coeff.args = 
                                       list(method = "pearson", label.x = 3.5, label.y = 1.5, label.sep = "\n"),
                                     cor.coef.size = c(13),
                                     ylim = c(0, 2),
                                     xlim = c(0, 5))%>%
                                      ggpar(font.x=c(40), font.y=c(40),font.tickslab=c(40))
  
  print(bh.gull_graph_mean_RC_st)
  
  
# GROUP 2
# values
bh.gull_graph_day01_min <- ggscatter(bh.gull_iha_edit, x = "day01_min", y = "NS", use="complete.obs", 
                             add = "reg.line", conf.int = TRUE, 
                             cor.coef = TRUE, cor.method = "pearson",
                             xlab = "gr.2_day01_min", ylab = "NS",
                             cor.coeff.args = 
                               list(method = "pearson", label.x.npc = 0.8, label.y.npc = 1, label.sep = "\n"),
                             ylim = c(0, 2))
  print(bh.gull_graph_day01_min) 

## standardised
  
  bh.gull_graph_day01_min_st <- ggscatter(bh.gull_iha_edit_st, x = "day01_min", y = "NS", use="complete.obs", 
                                       add = "reg.line", conf.int = TRUE, 
                                       cor.coef = TRUE, cor.method = "pearson",
                                       xlab = "gr.2_day01_min", ylab = "NS",
                                       cor.coeff.args = 
                                         list(method = "pearson", label.x = 1, label.y = 1.5, label.sep = "\n"),
                                       cor.coef.size = c(6),
                                       ylim = c(0, 2),
                                       xlim = c(0.4, 1.4))%>%
                                        ggpar(font.x=c(20), font.y=c(20),font.tickslab=c(20))
  
  print(bh.gull_graph_day01_min_st) 

# values  
bh.gull_graph_day01_max <- ggscatter(bh.gull_iha_edit, x = "day01_max", y = "NS", use="complete.obs", 
                               add = "reg.line", conf.int = TRUE, 
                               cor.coef = TRUE, cor.method = "pearson",
                               xlab = "gr.2_day01_max", ylab = "NS",
                               cor.coeff.args = 
                                 list(method = "pearson", label.x.npc = 0.6, label.y.npc = 1, label.sep = "\n"),
                               ylim = c(0, 2))
  print(bh.gull_graph_day01_max) 
## standardised
  bh.gull_graph_day01_max_st <- ggscatter(bh.gull_iha_edit_st, x = "day01_max", y = "NS", use="complete.obs", 
                                       add = "reg.line", conf.int = TRUE, 
                                       cor.coef = TRUE, cor.method = "pearson",
                                       xlab = "gr.2_day01_max", ylab = "NS",
                                       cor.coeff.args = 
                                         list(method = "pearson", label.x = 6, label.y = 1.5, label.sep = "\n"),
                                       cor.coef.size = c(6),
                                       ylim = c(0, 2),
                                       xlim = c(0, 9))%>%
                                        ggpar(font.x=c(20), font.y=c(20),font.tickslab=c(20))
  
  print(bh.gull_graph_day01_max_st) 
  
# values  
bh.gull_graph_day03_min <- ggscatter(bh.gull_iha_edit, x = "day03_min", y = "NS", use="complete.obs", 
                               add = "reg.line", conf.int = TRUE, 
                               cor.coef = TRUE, cor.method = "pearson",
                               xlab = "gr.2_day03_min", ylab = "NS",
                               cor.coeff.args = 
                                 list(method = "pearson", label.x.npc = 0.6, label.y.npc = 1, label.sep = "\n"),
                               ylim = c(0, 2))
  print(bh.gull_graph_day03_min) 
## standardised
  bh.gull_graph_day03_min_st <- ggscatter(bh.gull_iha_edit_st, x = "day03_min", y = "NS", use="complete.obs", 
                                       add = "reg.line", conf.int = TRUE, 
                                       cor.coef = TRUE, cor.method = "pearson",
                                       xlab = "gr.2_day03_min", ylab = "NS",
                                       cor.coeff.args = 
                                         list(method = "pearson", label.x = 1, label.y = 1.5, label.sep = "\n"),
                                       cor.coef.size = c(6),
                                       ylim = c(0, 2),
                                       xlim = c(0.4, 1.4))%>%
                                        ggpar(font.x=c(20), font.y=c(20),font.tickslab=c(20))
  
  print(bh.gull_graph_day03_min_st) 
  
   
# values  
bh.gull_graph_day03_max <- ggscatter(bh.gull_iha_edit, x = "day03_max", y = "NS", use="complete.obs", 
                               add = "reg.line", conf.int = TRUE, 
                               cor.coef = TRUE, cor.method = "pearson",
                               xlab = "gr.2_day03_max", ylab = "NS",
                               cor.coeff.args = 
                                 list(method = "pearson", label.x.npc = 0.6, label.y.npc = 1, label.sep = "\n"),
                               ylim = c(0, 2))
  print(bh.gull_graph_day03_max)

## standardised  
  bh.gull_graph_day03_max_st <- ggscatter(bh.gull_iha_edit_st, x = "day03_max", y = "NS", use="complete.obs", 
                                       add = "reg.line", conf.int = TRUE, 
                                       cor.coef = TRUE, cor.method = "pearson",
                                       xlab = "gr.2_day03_max", ylab = "NS",
                                       cor.coeff.args = 
                                         list(method = "pearson", label.x = 6, label.y = 1.6, label.sep = "\n"),
                                       cor.coef.size = c(13),
                                       ylim = c(0, 2),
                                       xlim = c(0, 9))%>%
                                        ggpar(font.x=c(40), font.y=c(40),font.tickslab=c(40))
  
  print(bh.gull_graph_day03_max_st)
  
# values
bh.gull_graph_day07_min <- ggscatter(bh.gull_iha_edit, x = "day07_min", y = "NS", use="complete.obs", 
                               add = "reg.line", conf.int = TRUE, 
                               cor.coef = TRUE, cor.method = "pearson",
                               xlab = "gr.2_day07_min", ylab = "NS",
                               cor.coeff.args = 
                                 list(method = "pearson", label.x.npc = 0.6, label.y.npc = 1, label.sep = "\n"),
                               ylim = c(0, 2))
  print(bh.gull_graph_day07_min) 

## standardised  
  bh.gull_graph_day07_min_st <- ggscatter(bh.gull_iha_edit_st, x = "day07_min", y = "NS", use="complete.obs", 
                                       add = "reg.line", conf.int = TRUE, 
                                       cor.coef = TRUE, cor.method = "pearson",
                                       xlab = "gr.2_day07_min", ylab = "NS",
                                       cor.coeff.args = 
                                         list(method = "pearson", label.x = 1, label.y = 1.5, label.sep = "\n"),
                                       cor.coef.size = c(6),
                                       ylim = c(0, 2),
                                       xlim = c(0.4, 1.4))%>%
                                         ggpar(font.x=c(20), font.y=c(20),font.tickslab=c(20))
  
  print(bh.gull_graph_day07_min_st) 
  
# values  
bh.gull_graph_day07_max <- ggscatter(bh.gull_iha_edit, x = "day07_max", y = "NS", use="complete.obs", 
                               add = "reg.line", conf.int = TRUE, 
                               cor.coef = TRUE, cor.method = "pearson",
                               xlab = "gr.2_day07_max", ylab = "NS",
                               cor.coeff.args = 
                                 list(method = "pearson", label.x.npc = 0.6, label.y.npc = 1, label.sep = "\n"),
                               ylim = c(0, 2))
  print(bh.gull_graph_day07_max) 

## standardised  
  bh.gull_graph_day07_max_st <- ggscatter(bh.gull_iha_edit_st, x = "day07_max", y = "NS", use="complete.obs", 
                                       add = "reg.line", conf.int = TRUE, 
                                       cor.coef = TRUE, cor.method = "pearson",
                                       xlab = "gr.2_day07_max", ylab = "NS",
                                       cor.coeff.args = 
                                         list(method = "pearson", label.x = 6, label.y = 1.5, label.sep = "\n"),
                                       cor.coef.size = c(6),
                                       ylim = c(0, 2),
                                       xlim = c(0, 9))%>%
                                        ggpar(font.x=c(20), font.y=c(20),font.tickslab=c(20))
  
  print(bh.gull_graph_day07_max_st) 
  
# GROUP 3
bh.gull_graph_vp_max <- ggscatter(bh.gull_iha_edit, x = "vp_max", y = "NS", use="complete.obs", 
                      add = "reg.line", conf.int = TRUE, 
                      cor.coef = TRUE, cor.method = "pearson",
                      xlab = "gr.3_vp_max", ylab = "NS",
                      cor.coeff.args = 
                        list(method = "pearson", label.x = 0.65, label.y = 1.7, label.sep = "\n"),
                      cor.coef.size = c(13),
                      ylim = c(0, 2),
                      xlim = c(0, 1))%>%
                      ggpar(font.x=c(40), font.y=c(40),font.tickslab=c(40))

  print(bh.gull_graph_vp_max)

# GROUP 4
  
bh.gull_graph_Q3 <- ggscatter(bh.gull_iha_edit, x = "Q3", y = "NS", use="complete.obs", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "gr.4_P0.75", ylab = "NS",
          cor.coeff.args = 
            list(method = "pearson", label.x = 50, label.y = 1.5, label.sep = "\n"),
          cor.coef.size = c(6),
          ylim = c(0, 2),
          xlim = c(0, 70))%>%
          ggpar(font.x=c(20), font.y=c(20),font.tickslab=c(20))


  print(bh.gull_graph_Q3)

bh.gull_graph_P0.95 <- ggscatter(bh.gull_iha_edit, x = "P0.95", y = "NS", use="complete.obs", 
                      add = "reg.line", conf.int = TRUE, 
                      cor.coef = TRUE, cor.method = "pearson",
                      xlab = "gr.4_P0.95", ylab = "NS",
                      cor.coeff.args = 
                        list(method = "pearson", label.x = 25, label.y = 1.5, label.sep = "\n"),
                      cor.coef.size = c(13),
                      ylim = c(0, 2),
                      xlim = c(0, 35))%>%
                      ggpar(font.x=c(40), font.y=c(40),font.tickslab=c(40))

  print(bh.gull_graph_P0.95)

bh.gull_graph_Q1 <- ggscatter(bh.gull_iha_edit, x = "Q1", y = "NS", use="complete.obs", 
                      add = "reg.line", conf.int = TRUE, 
                      cor.coef = TRUE, cor.method = "pearson",
                      xlab = "gr.4_P0.25", ylab = "NS",
                      cor.coeff.args = 
                        list(method = "pearson", label.x = 6, label.y = 1.5, label.sep = "\n"),
                      cor.coef.size = c(6),
                      ylim = c(0, 2),
                      xlim = c(0, 9))%>%
                      ggpar(font.x=c(20), font.y=c(20),font.tickslab=c(20))

  print(bh.gull_graph_Q1)

  #obtaining regression equation
  test_st <- lm(NS ~ gr.1_mean_Incub, data=bh.gull_iha_edit_st)
  coef(test_st) # a + b*x
  print(test_st)
  summary (test_st)
  
  #obtaining regression equation
  test_st <- lm(NS ~ day03_max, data=bh.gull_iha_edit_st)
  coef(test_st)
  print(test_st)
  summary (test_st)
  
  ##### correlation matrix between IHA 2004-2018
  
  # correlation with NS matches the earlier calclations
  install.packages("corrplot")
  library("corrplot")
  
  bh.gull_iha_edit_st_df <- data.frame(bh.gull_iha_edit_st)
  bh.gull_iha_edit_st_df$gr.2_day03_max <- as.numeric(bh.gull_iha_edit_st$day03_max)
  bh.gull_iha_edit_st_df$vp_min <- as.numeric(bh.gull_iha_edit_st$vp_min)
  bh.gull_iha_edit_st_df$gr.3_vp_max <- as.numeric(bh.gull_iha_edit_st$vp_max)
  bh.gull_iha_edit_st_df$Q3 <- as.numeric(bh.gull_iha_edit_st$Q3)
  bh.gull_iha_edit_st_df$gr.4_P0.95 <- as.numeric(bh.gull_iha_edit_st$P0.95)
  bh.gull_iha_edit_st_df$P0.05 <- as.numeric(bh.gull_iha_edit_st$P0.05)
  bh.gull_iha_edit_st_df$Q1 <- as.numeric(bh.gull_iha_edit_st$Q1)
  bh.gull_iha_edit_st_df$NS <- as.numeric(bh.gull_iha_edit_st$NS)
  #bh.gull_iha_edit_st_df <- bh.gull_iha_edit_st_df[ -c(1:3, 19) ]
  
  str(bh.gull_iha_edit_st_df)
  
  matrix_bh.gull_iha <- cor((bh.gull_iha_edit_st_df %>%
                            select(19,11:12,20:22)), use="complete.obs")
  
  write.csv(matrix_bh.gull_iha, "D:/Ptaki_hydro/Obliczenia/R/Results/reg_eq/graphs/bh.gull_matrix.csv")
  
  plot_matrix_bh.gull_iha <- corrplot(matrix_bh.gull_iha, method="number", type = "upper",  
           tl.col = "black", tl.srt = 45, title = "\n Correlation matrix \n Black-headed gull")
  
  
  png(height=600, width=600, file="D:/Ptaki_hydro/Obliczenia/R/Results/reg_eq/graphs/bh.gull_matrix.png", type = "cairo")
  corrplot(matrix_bh.gull_iha, method="number", type = "upper",  
           tl.col = "black", tl.srt = 45, title = "\n\n\nCorrelation matrix \n Black-headed gull")
  dev.off()
  
 
# Gather the Graphs
  library(gridExtra)
  library(grid)
  library(ggplot2)
  library(lattice)
  
bh.gull_cor_graph <- grid.arrange(
                bh.gull_graph_mean_LE, bh.gull_graph_mean_Incub, bh.gull_graph_mean_RC,
                bh.gull_graph_day01_min, bh.gull_graph_day01_max, 
                bh.gull_graph_day03_min, bh.gull_graph_day03_max, 
                bh.gull_graph_day07_min, bh.gull_graph_day07_max,
                bh.gull_graph_vp_max,
                bh.gull_graph_Q3, bh.gull_graph_P0.95, bh.gull_graph_Q1,
                ncol=3)


bh.gull_cor_graph_st <- grid.arrange(
  bh.gull_graph_mean_LE_st, bh.gull_graph_mean_Incub_st, bh.gull_graph_mean_RC_st,
  bh.gull_graph_day01_min_st, bh.gull_graph_day01_max_st, 
  bh.gull_graph_day03_min_st, bh.gull_graph_day03_max_st, 
  bh.gull_graph_day07_min_st, bh.gull_graph_day07_max_st,
  bh.gull_graph_vp_max,
  bh.gull_graph_Q3, bh.gull_graph_P0.95, bh.gull_graph_Q1,
  ncol=1)

ggsave(bh.gull_cor_graph_st, 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/IHA/bh.gull_cor_graph_st4", 
                     ".jpg"), #saving the plot into a folder
       device = "jpg",
       width = 5,
       height = 33)

# graphs for 5 chosen IHAs (for the main text of the article)
bh.gull_cor_graph_st_main <- grid.arrange(
  plot_spacer(), bh.gull_graph_mean_Incub_st, bh.gull_graph_mean_RC_st,
  bh.gull_graph_day03_max_st, bh.gull_graph_vp_max, bh.gull_graph_P0.95, 
  plot_spacer(), ncol=1)

ggsave(bh.gull_cor_graph_st_main, 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/IHA/bh.gull_cor_graph_st4_main", 
                     ".jpg"), #saving the plot into a folder
       device = "jpg",
       width = 10,
       height = 35)


# graphs for the IHAs not chosen (for the supplement)
bh.gull_cor_graph_st_supl <- grid.arrange(
  bh.gull_graph_mean_LE_st, 
  bh.gull_graph_day01_min_st, bh.gull_graph_day01_max_st, 
  bh.gull_graph_day03_min_st, 
  bh.gull_graph_day07_min_st, bh.gull_graph_day07_max_st, plot_spacer(),
  bh.gull_graph_Q3, plot_spacer(), bh.gull_graph_Q1,
  ncol=1)

ggsave(bh.gull_cor_graph_st_supl, 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/IHA/bh.gull_cor_graph_st4_supl", 
                     ".jpg"), #saving the plot into a folder
       device = "jpg",
       width = 5,
       height = 33)

# width 1800 height 2000 (3 columns)
# width 400 height 3000 (1 column)
print(bh.gull_cor_graph)

# collinearity diagnostics
install.packages('olsrr')
library('olsrr')

model_bh.gull_st <- lm(NS ~ gr.1_mean_Incub + gr.1_mean_RC + day03_max + vp_max + P0.95,
            data = bh.gull_iha_edit_st)

collinearity_bh.gull_st <- ols_coll_diag(model_bh.gull_st)

#####******#####******#####******#####
##### SUMMARIZING IHA INDICATORS
#####******#####******#####******#####gr.3_vp_max

#putting all the indicators in one list
bh.gull_list_all_in <- mapply(cbind, bh.gull_list_gr.1, bh.gull_list_gr.2, bh.gull_list_gr.3, bh.gull_list_gr.4
                              , SIMPLIFY=FALSE)
# standardized
bh.gull_list_all_in_st <- mapply(cbind, bh.gull_list_gr.1_st, bh.gull_list_gr.2_st, bh.gull_list_gr.3, bh.gull_list_gr.4
                              , SIMPLIFY=FALSE)

# leave only the columns with IHA
bh.gull_list_all <- lapply(bh.gull_list_all_in, "[", c(5, 1:3, 6:11, 18:19, 21:24))

# standardized
bh.gull_list_all_st <- lapply(bh.gull_list_all_in_st, "[", c(5, 1:3, 6:11, 18:19, 21:24))

#********
#calculate means for each IHA (without spliting into RCH or years) for the baseline period

library(plyr)
library(dplyr)

# bind list to a tibble and make list names to a column
bh.gull_list_all_df <- plyr::ldply(bh.gull_list_all, data.frame) %>% 
  dplyr::rename(RCH = 1)
#standardized
bh.gull_list_all_df_st <- plyr::ldply(bh.gull_list_all_st, data.frame) %>% 
  dplyr::rename(RCH = 1)

# mean
bh.gull_list_all_mean <- bh.gull_list_all_df %>% 
  dplyr::summarise(gr.1_mean_LE = mean(gr.1_mean_LE),
                   gr.1_mean_Incub = mean(gr.1_mean_Incub),
                   gr.1_mean_RC = mean(gr.1_mean_RC),
                   gr.2_day01_min = mean(day01_min),
                   gr.2_day01_max = mean(day01_max),
                   gr.2_day03_min = mean(day03_min),
                   gr.2_day03_max = mean(day03_max),
                   gr.2_day07_min = mean(day07_min),
                   gr.2_day07_max = mean(day07_max),
                   gr.3_vp_min = mean(vp_min),
                   gr.3_vp_max = mean(vp_max),
                   gr.4_P0.95 = mean(P0.95),
                   gr.4_Q3 = mean(Q3),
                   gr.4_Q1 = mean(Q1),
                   gr.4_P0.05 = mean(P0.05))

#standardized
bh.gull_list_all_mean_st <- bh.gull_list_all_df_st %>% 
  dplyr::summarise(gr.1_mean_LE = mean(gr.1_mean_LE),
                   gr.1_mean_Incub = mean(gr.1_mean_Incub),
                   gr.1_mean_RC = mean(gr.1_mean_RC),
                   gr.2_day01_min = mean(day01_min),
                   gr.2_day01_max = mean(day01_max),
                   gr.2_day03_min = mean(day03_min),
                   gr.2_day03_max = mean(day03_max),
                   gr.2_day07_min = mean(day07_min),
                   gr.2_day07_max = mean(day07_max),
                   gr.3_vp_min = mean(vp_min),
                   gr.3_vp_max = mean(vp_max),
                   gr.4_P0.95 = mean(P0.95),
                   gr.4_Q3 = mean(Q3),
                   gr.4_Q1 = mean(Q1),
                   gr.4_P0.05 = mean(P0.05))


write.csv(bh.gull_list_all_mean, "D:/Ptaki_hydro/Obliczenia/R/Results/bh.gull_list_all_mean.csv")
#standardized
write.csv(bh.gull_list_all_mean_st, "D:/Ptaki_hydro/Obliczenia/R/Results/bh.gull_list_all_mean_st.csv")

#********
#calculate means for each IHA (with spliting into RCH but without spliting into years) 
# for the baseline period

# RCH mean
bh.gull_list_rch_mean <- bh.gull_list_all_df %>%  
  dplyr::group_by(RCH) %>% 
  dplyr::summarise(gr.1_mean_LE = mean(gr.1_mean_LE),
                   gr.1_mean_Incub = mean(gr.1_mean_Incub),
                   gr.1_mean_RC = mean(gr.1_mean_RC),
                   gr.2_day01_min = mean(day01_min),
                   gr.2_day01_max = mean(day01_max),
                   gr.2_day03_min = mean(day03_min),
                   gr.2_day03_max = mean(day03_max),
                   gr.2_day07_min = mean(day07_min),
                   gr.2_day07_max = mean(day07_max),
                   gr.3_vp_min = mean(vp_min),
                   gr.3_vp_max = mean(vp_max),
                   gr.4_P0.95 = mean(P0.95),
                   gr.4_Q3 = mean(Q3),
                   gr.4_Q1 = mean(Q1),
                   gr.4_P0.05 = mean(P0.05))

bh.gull_list_rch_mean_st <- bh.gull_list_all_df_st %>%  
  dplyr::group_by(RCH) %>% 
  dplyr::summarise(gr.1_mean_LE = mean(gr.1_mean_LE),
                   gr.1_mean_Incub = mean(gr.1_mean_Incub),
                   gr.1_mean_RC = mean(gr.1_mean_RC),
                   gr.2_day01_min = mean(day01_min),
                   gr.2_day01_max = mean(day01_max),
                   gr.2_day03_min = mean(day03_min),
                   gr.2_day03_max = mean(day03_max),
                   gr.2_day07_min = mean(day07_min),
                   gr.2_day07_max = mean(day07_max),
                   gr.3_vp_min = mean(vp_min),
                   gr.3_vp_max = mean(vp_max),
                   gr.4_P0.95 = mean(P0.95),
                   gr.4_Q3 = mean(Q3),
                   gr.4_Q1 = mean(Q1),
                   gr.4_P0.05 = mean(P0.05))


write.csv(bh.gull_list_rch_mean, "D:/Ptaki_hydro/Obliczenia/R/Results/bh.gull_list_rch_mean.csv")
write.csv(bh.gull_list_rch_mean_st, "D:/Ptaki_hydro/Obliczenia/R/Results/bh.gull_list_rch_mean_st.csv")

#********
#calculate means for each IHA (with spliting into Years but without spliting into RCH) 
# for the baseline period


# year mean
bh.gull_list_year_mean<- bh.gull_list_all_df %>% 
  dplyr::group_by(Year) %>% 
  dplyr::summarise(gr.1_mean_LE = mean(gr.1_mean_LE),
                   gr.1_mean_Incub = mean(gr.1_mean_Incub),
                   gr.1_mean_RC = mean(gr.1_mean_RC),
                   gr.2_day01_min = mean(day01_min),
                   gr.2_day01_max = mean(day01_max),
                   gr.2_day03_min = mean(day03_min),
                   gr.2_day03_max = mean(day03_max),
                   gr.2_day07_min = mean(day07_min),
                   gr.2_day07_max = mean(day07_max),
                   gr.3_vp_min = mean(vp_min),
                   gr.3_vp_max = mean(vp_max),
                   gr.4_P0.95 = mean(P0.95),
                   gr.4_Q3 = mean(Q3),
                   gr.4_Q1 = mean(Q1),
                   gr.4_P0.05 = mean(P0.05))

write.csv(bh.gull_list_year_mean, "D:/Ptaki_hydro/Obliczenia/R/Results/bh.gull_list_year_mean.csv")

