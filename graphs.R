# SCRIPT 7

# bar plots showing IHA indicator values during 2004-2018
#bh.gull_list_year_mean z Q_IHA_b.gull
#m.gull_list_year_mean z Q_IHA_m.gull
#tern_list_year_mean z Q_IHA_tern


library(data.table)
library(ggplot2)

all_unlist_year_mean <- bind_rows("Mew gull" = m.gull_list_year_mean[ ,1:16],
                   "Black-headed gull" = bh.gull_list_year_mean[ ,1:16],  
                    "Little tern" = tern_list_year_mean[ ,1:16], 
                         .id = "bird")


all_unlist_year_mean_1 <- ggplot(data=all_unlist_year_mean, aes(x= Year, y=gr.1_mean_LE, fill = bird)) +
  geom_bar( position="dodge", stat="identity", width=.6) +
  ggtitle("gr.1_mean_LE") +
  ylab ("Q m3/s")+
  scale_fill_manual(values=c("seagreen4", "orchid4", "orangered2"))
all_unlist_year_mean_1a <-all_unlist_year_mean_1 + theme(legend.position="none") ### remove legend

plot(all_unlist_year_mean_1)

ggsave(all_unlist_year_mean_1 , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/",
                       "all_unlist_year_mean_1.jpg"),device = "jpg", width = 10, height = 5)


all_unlist_year_mean_2 <- ggplot(data=all_unlist_year_mean, aes(x= Year, y=gr.1_mean_Incub, fill = bird)) +
  geom_bar( position="dodge", stat="identity", width=.6) +
  ggtitle("gr.1_mean_Incub") +
  ylab ("Q m3/s")+
  scale_fill_manual(values=c("seagreen4", "orchid4", "orangered2"))
all_unlist_year_mean_2a <-all_unlist_year_mean_2 + theme(legend.position="none") ### remove legend
ggsave(all_unlist_year_mean_2 , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/",
                     "all_unlist_year_mean_2.jpg"),device = "jpg", width = 10, height = 5)
                

all_unlist_year_mean_3 <- ggplot(data=all_unlist_year_mean, aes(x= Year, y=gr.1_mean_RC, fill = bird)) +
  geom_bar( position="dodge", stat="identity", width=.6) +
  ggtitle("gr.1_mean_RC") +
  ylab ("Q m3/s")+
  scale_fill_manual(values=c("seagreen4", "orchid4", "orangered2"))
all_unlist_year_mean_3a <-all_unlist_year_mean_3 + theme(legend.position="none") ### remove legend
ggsave(all_unlist_year_mean_3 , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/",
                     "all_unlist_year_mean_3.jpg"),device = "jpg", width = 10, height = 5)


all_unlist_year_mean_4 <- ggplot(data=all_unlist_year_mean, aes(x= Year, y=gr.2_day01_min, fill = bird)) +
  geom_bar( position="dodge", stat="identity", width=.6) +
  ggtitle("gr.2_day01_min") +
  ylab ("Q m3/s")+
  scale_fill_manual(values=c("seagreen4", "orchid4", "orangered2"))
all_unlist_year_mean_4a <-all_unlist_year_mean_4 + theme(legend.position="none") ### remove legend
ggsave(all_unlist_year_mean_4 , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/",
                     "all_unlist_year_mean_4.jpg"),device = "jpg", width = 10, height = 5)


all_unlist_year_mean_5 <- ggplot(data=all_unlist_year_mean, aes(x= Year, y=gr.2_day03_min, fill = bird)) +
  geom_bar( position="dodge", stat="identity", width=.6) +
  ggtitle("gr.2_day03_min") +
  ylab ("Q m3/s")+
  scale_y_continuous(#breaks = seq(from = 0, to = 60, by = 10), 
    limits=c(0, 4000))+
  scale_fill_manual(values=c("seagreen4", "orchid4", "orangered2"))
all_unlist_year_mean_5a <-all_unlist_year_mean_5 + theme(legend.position="none") ### remove legend
ggsave(all_unlist_year_mean_5 , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/",
                     "all_unlist_year_mean_5.jpg"),device = "jpg", width = 10, height = 5)


all_unlist_year_mean_6 <- ggplot(data=all_unlist_year_mean, aes(x= Year, y=gr.2_day07_min, fill = bird)) +
  geom_bar( position="dodge", stat="identity", width=.6) +
  ggtitle("gr.2_day07_min") +
  ylab ("Q m3/s")+
  scale_fill_manual(values=c("seagreen4", "orchid4", "orangered2"))
all_unlist_year_mean_6a <-all_unlist_year_mean_6 + theme(legend.position="none") ### remove legend
ggsave(all_unlist_year_mean_6 , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/",
                     "all_unlist_year_mean_6.jpg"),device = "jpg", width = 10, height = 5)


all_unlist_year_mean_7 <- ggplot(data=all_unlist_year_mean, aes(x= Year, y=gr.2_day01_max, fill = bird)) +
  geom_bar( position="dodge", stat="identity", width=.6) +
  ggtitle("gr.2_day01_max") +
  ylab ("Q m3/s")+
  scale_fill_manual(values=c("seagreen4", "orchid4", "orangered2"))
all_unlist_year_mean_7a <-all_unlist_year_mean_7 + theme(legend.position="none") ### remove legend
ggsave(all_unlist_year_mean_7 , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/",
                     "all_unlist_year_mean_7.jpg"),device = "jpg", width = 10, height = 5)


all_unlist_year_mean_8 <- ggplot(data=all_unlist_year_mean, aes(x= Year, y=gr.2_day03_max, fill = bird)) +
  geom_bar( position="dodge", stat="identity", width=.6) +
  ggtitle("gr.2_day03_max") +
  ylab ("Q m3/s")+
  scale_fill_manual(values=c("seagreen4", "orchid4", "orangered2"))
all_unlist_year_mean_8a <-all_unlist_year_mean_8 + theme(legend.position="none") ### remove legend
ggsave(all_unlist_year_mean_8 , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/",
                     "all_unlist_year_mean_8.jpg"),device = "jpg", width = 10, height = 5)


all_unlist_year_mean_9 <- ggplot(data=all_unlist_year_mean, aes(x= Year, y=gr.2_day07_max, fill = bird)) +
  geom_bar( position="dodge", stat="identity", width=.6) +
  ggtitle("gr.2_day07_max") +
  ylab ("Q m3/s")+
  scale_fill_manual(values=c("seagreen4", "orchid4", "orangered2"))
all_unlist_year_mean_9a <-all_unlist_year_mean_9 + theme(legend.position="none") ### remove legend
ggsave(all_unlist_year_mean_9 , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/",
                     "all_unlist_year_mean_9.jpg"),device = "jpg", width = 10, height = 5)


## same 000
all_unlist_year_mean_10 <- ggplot(data=all_unlist_year_mean, aes(x= Year, y=gr.3_vp_min, fill = bird)) +
  geom_bar( position="dodge", stat="identity", width=.6) +
  ggtitle("gr.3_vp_min") +
  ylab ("1=Yes, 0=No")+
  scale_fill_manual(values=c("seagreen4", "orchid4", "orangered2"))+ 
  scale_y_continuous(breaks = seq(from = 0, to = 1, by = 1))
ggsave(all_unlist_year_mean_10 , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/",
                     "all_unlist_year_mean_10.jpg"),device = "jpg", width = 10, height = 5)

all_unlist_year_mean_11 <- ggplot(data=all_unlist_year_mean, aes(x= Year, y=gr.3_vp_max, fill = bird)) +
  geom_bar( position="dodge", stat="identity", width=.6) +
  ggtitle("gr.3_vp_max") +
  ylab ("1=Yes, 0=No")+
  scale_fill_manual(values=c("seagreen4", "orchid4", "orangered2"))+ 
  scale_y_continuous(breaks = seq(from = 0, to = 1, by = 1))
all_unlist_year_mean_11a <-all_unlist_year_mean_11 + theme(legend.position="none") ### remove legend
ggsave(all_unlist_year_mean_11 , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/",
                     "all_unlist_year_mean_11.jpg"),device = "jpg", width = 10, height = 5)


all_unlist_year_mean_12 <- ggplot(data=all_unlist_year_mean, aes(x= Year, y=gr.4_P0.95, fill = bird)) +
  geom_bar( position="dodge", stat="identity", width=.6) +
  ggtitle("gr.4_P0.95") +
  ylab ("no. of days")+
  scale_fill_manual(values=c("seagreen4", "orchid4", "orangered2"))+ 
  scale_y_continuous(#breaks = seq(from = 0, to = 60, by = 10), 
    limits=c(0, 70))
all_unlist_year_mean_12a <-all_unlist_year_mean_12 + theme(legend.position="none") ### remove legend
ggsave(all_unlist_year_mean_12 , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/",
                     "all_unlist_year_mean_12.jpg"),device = "jpg", width = 10, height = 5)


all_unlist_year_mean_13 <- ggplot(data=all_unlist_year_mean, aes(x= Year, y=gr.4_Q3, fill = bird)) +
  geom_bar( position="dodge", stat="identity", width=.6) +
  ggtitle("gr.4_P0.75") +
  ylab ("no. of days")+
  scale_fill_manual(values=c("seagreen4", "orchid4", "orangered2"))+ 
  scale_y_continuous(#breaks = seq(from = 0, to = 60, by = 10), 
    limits=c(0, 70))
all_unlist_year_mean_13a <-all_unlist_year_mean_13 + theme(legend.position="none") ### remove legend
ggsave(all_unlist_year_mean_13 , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/",
                     "all_unlist_year_mean_13.jpg"),device = "jpg", width = 10, height = 5)



all_unlist_year_mean_14 <- ggplot(data=all_unlist_year_mean, aes(x= Year, y=gr.4_Q1, fill = bird)) +
  geom_bar( position="dodge", stat="identity", width=.6) +
  ggtitle("gr.4_P0.25") +
  ylab ("no. of days")+
  scale_fill_manual(values=c("seagreen4", "orchid4", "orangered2"))+ 
  scale_y_continuous(#breaks = seq(from = 0, to = 60, by = 10), 
    limits=c(0, 70))
all_unlist_year_mean_14a <-all_unlist_year_mean_14 + theme(legend.position="none") ### remove legend
ggsave(all_unlist_year_mean_14 , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/",
                     "all_unlist_year_mean_14.jpg"),device = "jpg", width = 10, height = 5)


## same 000
all_unlist_year_mean_15 <- ggplot(data=all_unlist_year_mean, aes(x= Year, y=gr.4_P0.05, fill = bird)) +
  geom_bar( position="dodge", stat="identity", width=.6) +
  ggtitle("gr.4_P0.05")+
  ylab ("no. of days")+
  scale_fill_manual(values=c("seagreen4", "orchid4", "orangered2"))+ 
  scale_y_continuous(breaks = seq(from = 0, to = 10, by = 1))
ggsave(all_unlist_year_mean_15 , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/",
                     "all_unlist_year_mean_15.jpg"),device = "jpg", width = 10, height = 5)

# join all the graphs together without no.10 and 15 as they have only 000 values
library("ggpubr")
all_unlist_year_mean_plot <- ggarrange(
              all_unlist_year_mean_1a, all_unlist_year_mean_2a, all_unlist_year_mean_3a,
              #all_unlist_year_mean_4a, 
              all_unlist_year_mean_5a, 
              #all_unlist_year_mean_6a,
              #all_unlist_year_mean_7a, 
              all_unlist_year_mean_8a, 
              #all_unlist_year_mean_9a,
              all_unlist_year_mean_11a, all_unlist_year_mean_12a,
              all_unlist_year_mean_13a, all_unlist_year_mean_14, 
             nrow = 3, ncol = 3, common.legend = TRUE, legend="bottom") 

ggsave(all_unlist_year_mean_plot , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/",
                     "all_unlist_year_mean_plot_short.jpg"),device = "jpg", width = 20, height = 10)




############################################
############################################
#*******   Absolute values ***********#
#*******    BOX PLOTS      ***********#
###########################################
############################################
library("data.table")

############################################
#*******   Mew gull   ***********#
###########################################

#######%%%%%%%%%%
# Mew-Gull absolute values box_plot GR.1 Laying eggs

m_gull_gr.1_le_absolute_list <- list(m.gull_ref_le_gr.1_unlist, m.gull_nf_4.5_le_gr.1_unlist, m.gull_ff_4.5_le_gr.1_unlist,
                                        m.gull_nf_8.5_le_gr.1_unlist, m.gull_ff_8.5_le_gr.1_unlist)

names(m_gull_gr.1_le_absolute_list) <- c("ref","nf_4.5", "ff_4.5", "nf_8.5", "ff_8.5")

m_gull_gr.1_le_absolute <- rbindlist(m_gull_gr.1_le_absolute_list, id="id_scen")

m_gull_gr.1_le_absolute$id_scen <- factor(m_gull_gr.1_le_absolute$id_scen , levels=c("ref","nf_4.5", "ff_4.5", "nf_8.5", "ff_8.5"))

m_gull_gr.1_le_absolute_graph <- ggplot(m_gull_gr.1_le_absolute, aes(x=id_scen, y=mean_le)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="Q m3/s") +
  ggtitle("gr.1_mean_LE")+
  coord_cartesian( ylim = c(400, 1200))+
  theme(axis.title.x = element_blank())+
  theme(panel.background = element_rect(fill = "white", colour = 'black'),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "lightgrey"),
        panel.grid.minor = element_line(size = 0.3, linetype = 'solid', colour = "lightgrey"),
        plot.title = element_text(size = 20),
        text = element_text(size=18))



#######%%%%%%%%%%
# Mew-Gull absolute values box_plot GR.1 Incub

m_gull_gr.1_incub_absolute_list <- list(m.gull_ref_incub_gr.1_unlist, m.gull_nf_4.5_incub_gr.1_unlist, m.gull_ff_4.5_incub_gr.1_unlist,
                                         m.gull_nf_8.5_incub_gr.1_unlist, m.gull_ff_8.5_incub_gr.1_unlist)

names(m_gull_gr.1_incub_absolute_list) <- c("ref","nf_4.5", "ff_4.5", "nf_8.5", "ff_8.5")

m_gull_gr.1_incub_absolute <- rbindlist(m_gull_gr.1_incub_absolute_list, id="id_scen")

m_gull_gr.1_incub_absolute$id_scen <- factor(m_gull_gr.1_incub_absolute$id_scen , levels=c("ref","nf_4.5", "ff_4.5", "nf_8.5", "ff_8.5"))
RCH_split_m.gull
m_gull_gr.1_incub_absolute_graph <- ggplot(m_gull_gr.1_incub_absolute, aes(x=id_scen, y=mean_incub)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="Q m3/s") +
  ggtitle("gr.1_mean_Incub")+
  coord_cartesian( ylim = c(400, 1200))+
  theme(axis.title.x = element_blank())+
  theme(panel.background = element_rect(fill = "white", colour = 'black'),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "lightgrey"),
        panel.grid.minor = element_line(size = 0.3, linetype = 'solid', colour = "lightgrey"),
        plot.title = element_text(size = 20),
        text = element_text(size=18))


#######%%%%%%%%%%
# Mew-Gull absolute values box_plot GR.1 Rear

m_gull_gr.1_rear_absolute_list <- list(m.gull_ref_rear_gr.1_unlist, m.gull_nf_4.5_rear_gr.1_unlist, m.gull_ff_4.5_rear_gr.1_unlist,
                                        m.gull_nf_8.5_rear_gr.1_unlist, m.gull_ff_8.5_rear_gr.1_unlist)

names(m_gull_gr.1_rear_absolute_list) <- c("ref","nf_4.5", "ff_4.5", "nf_8.5", "ff_8.5")

m_gull_gr.1_rear_absolute <- rbindlist(m_gull_gr.1_rear_absolute_list, id="id_scen")

m_gull_gr.1_rear_absolute$id_scen <- factor(m_gull_gr.1_rear_absolute$id_scen , levels=c("ref","nf_4.5", "ff_4.5", "nf_8.5", "ff_8.5"))

m_gull_gr.1_rear_absolute_graph <- ggplot(m_gull_gr.1_rear_absolute, aes(x=id_scen, y=mean_rear)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="Q m3/s") +
  ggtitle("gr.1_mean_RC")+
  coord_cartesian( ylim = c(400, 1200))+
  theme(axis.title.x = element_blank())+
  theme(panel.background = element_rect(fill = "white", colour = 'black'),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "lightgrey"),
        panel.grid.minor = element_line(size = 0.3, linetype = 'solid', colour = "lightgrey"),
        plot.title = element_text(size = 20),
        text = element_text(size=18))

#######%%%%%%%%%%
# Mew-Gull absolute values box_plot GR.2

m_gull_gr.2_absolute_list <- list(m.gull_ref_gr.2_unlist_p2, m.gull_nf_4.5_gr.2_unlist_p2,  m.gull_ff_4.5_gr.2_unlist_p2, 
                                   m.gull_nf_8.5_gr.2_unlist_p2, m.gull_ff_8.5_gr.2_unlist_p2)

names(m_gull_gr.2_absolute_list) <- c("ref","nf_4.5", "ff_4.5", "nf_8.5", "ff_8.5")

m_gull_gr.2_absolute <- rbindlist(m_gull_gr.2_absolute_list, id="id_scen")

m_gull_gr.2_absolute$id_scen <- factor(m_gull_gr.2_absolute$id_scen , levels=c("ref","nf_4.5", "ff_4.5", "nf_8.5", "ff_8.5"))

m_gull_gr.2_absolute <- subset(m_gull_gr.2_absolute, select=-c(5,6,8,9,11,12))

# m_gull_gr.2_01max_absolute_graph <- ggplot(m_gull_gr.2_absolute, aes(x=id_scen, y=day01_mean)) + 
#  geom_boxplot( alpha =0.8, width = 0.9)+ 
#  labs(y="mean streamflow m3/s") +
#  ggtitle("m.gull gr.2_day01_max")+
#  coord_cartesian( ylim = c(900, 2200))+
#  theme(axis.title.x = element_blank())

m_gull_gr.2_03max_absolute_graph <- ggplot(m_gull_gr.2_absolute, aes(x=id_scen, y=day03_mean)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="Q m3/s") +
  ggtitle("gr.2_day03_max")+
  coord_cartesian( ylim = c(900, 2200))+
  theme(axis.title.x = element_blank())+
  theme(panel.background = element_rect(fill = "white", colour = 'black'),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "lightgrey"),
        panel.grid.minor = element_line(size = 0.3, linetype = 'solid', colour = "lightgrey"),
        plot.title = element_text(size = 20),
        text = element_text(size=18))

# m_gull_gr.2_07max_absolute_graph <- ggplot(m_gull_gr.2_absolute, aes(x=id_scen, y=day07_mean)) + 
#  geom_boxplot( alpha =0.8, width = 0.9)+ 
#  labs(y="mean streamflow m3/s") +
#  ggtitle("m.gull gr.2_day07_max")+
#  coord_cartesian( ylim = c(900, 2200))+
#  theme(axis.title.x = element_blank())

#######%%%%%%%%%%
# Mew-Gull absolute values box_plot GR.4

m_gull_gr.4_absolute_list <- list(m.gull_ref_gr.4_p2, m.gull_nf_4.5_gr.4_p2, m.gull_ff_4.5_gr.4_p2,
                                   m.gull_nf_8.5_gr.4_p2, m.gull_ff_8.5_gr.4_p2)

names(m_gull_gr.4_absolute_list) <- c("ref","nf_4.5", "ff_4.5", "nf_8.5", "ff_8.5")

m_gull_gr.4_absolute <- rbindlist(m_gull_gr.4_absolute_list, id="id")

m_gull_gr.4_absolute$id <- factor(m_gull_gr.4_absolute$id , levels=c("ref","nf_4.5", "ff_4.5", "nf_8.5", "ff_8.5"))

m_gull_gr.4_absolute_graph <- ggplot(m_gull_gr.4_absolute, aes(x=id, y=yearly)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="no. of days") +
  ggtitle("gr.4_P0.75")+
  theme(axis.title.x = element_blank())+
  coord_cartesian( ylim = c(10, 50))+
  theme(panel.background = element_rect(fill = "white", colour = 'black'),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "lightgrey"),
        panel.grid.minor = element_line(size = 0.3, linetype = 'solid', colour = "lightgrey"),
        plot.title = element_text(size = 20),
        text = element_text(size=18))



# join all the graphs together 

library("ggpubr")
#model/subbasin/value 
# 162 values (9 models * 18 subbasins)

m_gull_box_plot_absolute <- ggarrange(m_gull_gr.1_le_absolute_graph, m_gull_gr.1_incub_absolute_graph, 
                                      m_gull_gr.1_rear_absolute_graph, 
                                      #m_gull_gr.2_01max_absolute_graph, m_gull_gr.2_07max_absolute_graph,
                                      m_gull_gr.2_03max_absolute_graph, plot_spacer(), plot_spacer(), 
                                      m_gull_gr.4_absolute_graph,
                                       
                                       nrow = 7, ncol = 1)

plot(m_gull_box_plot_absolute)

ggsave(m_gull_box_plot_absolute , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/",
                     "m_gull_box_plot_absolute.jpg"),device = "jpg", width = 20, height = 10)




############################################
#*******   Black- headed gull   ***********#
###########################################

#######%%%%%%%%%%
# BH-Gull absolute values box_plot GR.1 Incub

bh_gull_gr.1_incub_absolute_list <- list(bh.gull_ref_incub_gr.1_unlist, bh.gull_nf_4.5_incub_gr.1_unlist, bh.gull_ff_4.5_incub_gr.1_unlist,
                                   bh.gull_nf_8.5_incub_gr.1_unlist, bh.gull_ff_8.5_incub_gr.1_unlist)

names(bh_gull_gr.1_incub_absolute_list) <- c("ref","nf_4.5", "ff_4.5", "nf_8.5", "ff_8.5")

bh_gull_gr.1_incub_absolute <- rbindlist(bh_gull_gr.1_incub_absolute_list, id="id_scen")

bh_gull_gr.1_incub_absolute$id_scen <- factor(bh_gull_gr.1_incub_absolute$id_scen , levels=c("ref","nf_4.5", "ff_4.5", "nf_8.5", "ff_8.5"))

bh_gull_gr.1_incub_absolute_graph <- ggplot(bh_gull_gr.1_incub_absolute, aes(x=id_scen, y=mean_incub)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="Q m3/s") +
  ggtitle("gr.1_mean_Incub")+
  coord_cartesian( ylim = c(400, 1200))+
  theme(axis.title.x = element_blank())+
  theme(panel.background = element_rect(fill = "white", colour = 'black'),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "lightgrey"),
        panel.grid.minor = element_line(size = 0.3, linetype = 'solid', colour = "lightgrey"),
        plot.title = element_text(size = 20),
        text = element_text(size=18))

#######%%%%%%%%%%
# BH-Gull absolute values box_plot GR.1 Rear

bh_gull_gr.1_rear_absolute_list <- list(bh.gull_ref_rear_gr.1_unlist, bh.gull_nf_4.5_rear_gr.1_unlist, bh.gull_ff_4.5_rear_gr.1_unlist,
                                         bh.gull_nf_8.5_rear_gr.1_unlist, bh.gull_ff_8.5_rear_gr.1_unlist)

names(bh_gull_gr.1_rear_absolute_list) <- c("ref","nf_4.5", "ff_4.5", "nf_8.5", "ff_8.5")

bh_gull_gr.1_rear_absolute <- rbindlist(bh_gull_gr.1_rear_absolute_list, id="id_scen")

bh_gull_gr.1_rear_absolute$id_scen <- factor(bh_gull_gr.1_rear_absolute$id_scen , levels=c("ref","nf_4.5", "ff_4.5", "nf_8.5", "ff_8.5"))

bh_gull_gr.1_rear_absolute_graph <- ggplot(bh_gull_gr.1_rear_absolute, aes(x=id_scen, y=mean_rear)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="Q m3/s") +
  ggtitle("gr.1_mean_RC") +
  coord_cartesian( ylim = c(400, 1200))+
  theme(axis.title.x = element_blank())+
  theme(panel.background = element_rect(fill = "white", colour = 'black'),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "lightgrey"),
        panel.grid.minor = element_line(size = 0.3, linetype = 'solid', colour = "lightgrey"),
        plot.title = element_text(size = 20),
        text = element_text(size=18))

#######%%%%%%%%%%
# BH-Gull absolute values box_plot GR.2

bh_gull_gr.2_absolute_list <- list(bh.gull_ref_gr.2_unlist_p2, bh.gull_nf_4.5_gr.2_unlist_p2,  bh.gull_ff_4.5_gr.2_unlist_p2, 
                                        bh.gull_nf_8.5_gr.2_unlist_p2, bh.gull_ff_8.5_gr.2_unlist_p2)

names(bh_gull_gr.2_absolute_list) <- c("ref","nf_4.5", "ff_4.5", "nf_8.5", "ff_8.5")

bh_gull_gr.2_absolute <- rbindlist(bh_gull_gr.2_absolute_list, id="id_scen")

bh_gull_gr.2_absolute$id_scen <- factor(bh_gull_gr.2_absolute$id_scen , levels=c("ref","nf_4.5", "ff_4.5", "nf_8.5", "ff_8.5"))

bh_gull_gr.2_absolute <- subset(bh_gull_gr.2_absolute, select=-c(5,6,8,9,11,12))

# bh_gull_gr.2_01max_absolute_graph <- ggplot(bh_gull_gr.2_absolute, aes(x=id_scen, y=day01_mean)) + 
#  geom_boxplot( alpha =0.8, width = 0.9)+ 
#  labs(y="mean streamflow m3/s") +
#  ggtitle("bh.gull gr.2_day01_max") +
#  coord_cartesian( ylim = c(900, 2200))+
#  theme(axis.title.x = element_blank())

bh_gull_gr.2_03max_absolute_graph <- ggplot(bh_gull_gr.2_absolute, aes(x=id_scen, y=day03_mean)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="Q m3/s") +
  ggtitle("gr.2_day03_max") +
  coord_cartesian( ylim = c(900, 2200))+
  theme(axis.title.x = element_blank())+
  theme(panel.background = element_rect(fill = "white", colour = 'black'),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "lightgrey"),
        panel.grid.minor = element_line(size = 0.3, linetype = 'solid', colour = "lightgrey"),
        plot.title = element_text(size = 20),
        text = element_text(size=18))

# bh_gull_gr.2_07max_absolute_graph <- ggplot(bh_gull_gr.2_absolute, aes(x=id_scen, y=day07_mean)) + 
#  geom_boxplot( alpha =0.8, width = 0.9)+ 
#  labs(y="mean streamflow m3/s") +
#  ggtitle("bh.gull gr.2_day07_max")+
#  coord_cartesian( ylim = c(900, 2200))+
#  theme(axis.title.x = element_blank())


#######%%%%%%%%%%
# BH-Gull absolute values box_plot GR.3

bh_gull_gr.3_absolute_list <- list(bh.gull_ref_list_gr.3_unlist_p1, bh.gull_nf_4.5_list_gr.3_unlist_p1,
                                   bh.gull_ff_4.5_list_gr.3_unlist_p1, bh.gull_nf_8.5_list_gr.3_unlist_p1,
                                   bh.gull_ff_8.5_list_gr.3_unlist_p1)

names(bh_gull_gr.3_absolute_list) <- c("ref","nf_4.5", "ff_4.5", "nf_8.5", "ff_8.5")

bh_gull_gr.3_absolute <- rbindlist(bh_gull_gr.3_absolute_list, id="id")

bh_gull_gr.3_absolute$id <- factor(bh_gull_gr.3_absolute$id , levels=c("ref","nf_4.5", "ff_4.5", "nf_8.5", "ff_8.5"))

bh_gull_gr.3_absolute_graph <- ggplot(bh_gull_gr.3_absolute, aes(x=id, y=above_vp_max_period)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y=" 1=Yes 0=No") +
  ggtitle("gr.3_vp_max")+
  theme(axis.title.x = element_blank())+
  theme(panel.background = element_rect(fill = "white", colour = 'black'),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "lightgrey"),
        panel.grid.minor = element_line(size = 0.3, linetype = 'solid', colour = "lightgrey"),
        plot.title = element_text(size = 20),
        text = element_text(size=18))

#######%%%%%%%%%%
# BH-Gull absolute values box_plot GR.4

bh_gull_gr.4_absolute_list <- list(bh.gull_ref_gr.4_p2, bh.gull_nf_4.5_gr.4_p2, bh.gull_ff_4.5_gr.4_p2,
                              bh.gull_nf_8.5_gr.4_p2, bh.gull_ff_8.5_gr.4_p2)

names(bh_gull_gr.4_absolute_list) <- c("ref","nf_4.5", "ff_4.5", "nf_8.5", "ff_8.5")

bh_gull_gr.4_absolute <- rbindlist(bh_gull_gr.4_absolute_list, id="id")

bh_gull_gr.4_absolute$id <- factor(bh_gull_gr.4_absolute$id , levels=c("ref","nf_4.5", "ff_4.5", "nf_8.5", "ff_8.5"))

bh_gull_gr.4_absolute_graph <- ggplot(bh_gull_gr.4_absolute, aes(x=id, y=yearly)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="no. of days") +
  ggtitle("gr.4_P0.95")+
  theme(axis.title.x = element_blank())+
  theme(panel.background = element_rect(fill = "white", colour = 'black'),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "lightgrey"),
        panel.grid.minor = element_line(size = 0.3, linetype = 'solid', colour = "lightgrey"),
        plot.title = element_text(size = 20),
        text = element_text(size=18))

plot(bh_gull_gr.4_absolute_graph)

# join all the graphs together 

library("ggpubr")
#model/subbasin/value 
# 162 values (9 models * 18 subbasins)

bh_gull_box_plot_absolute <- ggarrange(plot_spacer(), bh_gull_gr.1_incub_absolute_graph, bh_gull_gr.1_rear_absolute_graph,
                                       #bh_gull_gr.2_01max_absolute_graph, bh_gull_gr.2_07max_absolute_graph,
                                       bh_gull_gr.2_03max_absolute_graph, bh_gull_gr.3_absolute_graph, 
                                       bh_gull_gr.4_absolute_graph,plot_spacer(), 
  
  nrow = 7, ncol = 1)

plot(bh_gull_box_plot_absolute)

ggsave(bh_gull_box_plot_absolute , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/",
                     "bh_gull_box_plot_absolute1.jpg"),device = "jpg", width = 20, height = 10)


############################################
#*******   Little Tern   ***********#
###########################################

#######%%%%%%%%%%
# Little tern absolute values box_plot GR.1 Laying eggs

tern_gr.1_le_absolute_list <- list(tern_ref_le_gr.1_unlist, tern_nf_4.5_le_gr.1_unlist, tern_ff_4.5_le_gr.1_unlist,
                                     tern_nf_8.5_le_gr.1_unlist, tern_ff_8.5_le_gr.1_unlist)

names(tern_gr.1_le_absolute_list) <- c("ref","nf_4.5", "ff_4.5", "nf_8.5", "ff_8.5")

tern_gr.1_le_absolute <- rbindlist(tern_gr.1_le_absolute_list, id="id_scen")

tern_gr.1_le_absolute$id_scen <- factor(tern_gr.1_le_absolute$id_scen , levels=c("ref","nf_4.5", "ff_4.5", "nf_8.5", "ff_8.5"))

tern_gr.1_le_absolute_graph <- ggplot(tern_gr.1_le_absolute, aes(x=id_scen, y=mean_le)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="Q m3/s") +
  ggtitle("gr.1_mean_LE")+
  coord_cartesian( ylim = c(400, 1200))+
  theme(axis.title.x = element_blank())+
  theme(panel.background = element_rect(fill = "white", colour = 'black'),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "lightgrey"),
        panel.grid.minor = element_line(size = 0.3, linetype = 'solid', colour = "lightgrey"),
        plot.title = element_text(size = 20),
        text = element_text(size=18))



#######%%%%%%%%%%
# Little tern absolute values box_plot GR.1 Incub

tern_gr.1_incub_absolute_list <- list(tern_ref_incub_gr.1_unlist, tern_nf_4.5_incub_gr.1_unlist, tern_ff_4.5_incub_gr.1_unlist,
                                        tern_nf_8.5_incub_gr.1_unlist, tern_ff_8.5_incub_gr.1_unlist)

names(tern_gr.1_incub_absolute_list) <- c("ref","nf_4.5", "ff_4.5", "nf_8.5", "ff_8.5")

tern_gr.1_incub_absolute <- rbindlist(tern_gr.1_incub_absolute_list, id="id_scen")

tern_gr.1_incub_absolute$id_scen <- factor(tern_gr.1_incub_absolute$id_scen , levels=c("ref","nf_4.5", "ff_4.5", "nf_8.5", "ff_8.5"))

tern_gr.1_incub_absolute_graph <- ggplot(tern_gr.1_incub_absolute, aes(x=id_scen, y=mean_incub)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="Q m3/s") +
  ggtitle("gr.1_mean_Incub")+
  coord_cartesian( ylim = c(400, 1200))+
  theme(axis.title.x = element_blank())+
  theme(panel.background = element_rect(fill = "white", colour = 'black'),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "lightgrey"),
        panel.grid.minor = element_line(size = 0.3, linetype = 'solid', colour = "lightgrey"),
        plot.title = element_text(size = 20),
        text = element_text(size=18))


#######%%%%%%%%%%
# Little tern absolute values box_plot GR.1 Rear

tern_gr.1_rear_absolute_list <- list(tern_ref_rear_gr.1_unlist, tern_nf_4.5_rear_gr.1_unlist, tern_ff_4.5_rear_gr.1_unlist,
                                       tern_nf_8.5_rear_gr.1_unlist, tern_ff_8.5_rear_gr.1_unlist)

names(tern_gr.1_rear_absolute_list) <- c("ref","nf_4.5", "ff_4.5", "nf_8.5", "ff_8.5")

tern_gr.1_rear_absolute <- rbindlist(tern_gr.1_rear_absolute_list, id="id_scen")

tern_gr.1_rear_absolute$id_scen <- factor(tern_gr.1_rear_absolute$id_scen , levels=c("ref","nf_4.5", "ff_4.5", "nf_8.5", "ff_8.5"))

tern_gr.1_rear_absolute_graph <- ggplot(tern_gr.1_rear_absolute, aes(x=id_scen, y=mean_rear)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="Q m3/s") +
  ggtitle("gr.1_mean_RC")+
  coord_cartesian( ylim = c(400, 1200))+
  theme(axis.title.x = element_blank())+
  theme(panel.background = element_rect(fill = "white", colour = 'black'),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "lightgrey"),
        panel.grid.minor = element_line(size = 0.3, linetype = 'solid', colour = "lightgrey"),
        plot.title = element_text(size = 20),
        text = element_text(size=18))

#######%%%%%%%%%%
# Little tern absolute values box_plot GR.2

tern_gr.2_absolute_list <- list(tern_ref_gr.2_unlist_p2, tern_nf_4.5_gr.2_unlist_p2,  tern_ff_4.5_gr.2_unlist_p2, 
                                  tern_nf_8.5_gr.2_unlist_p2, tern_ff_8.5_gr.2_unlist_p2)

names(tern_gr.2_absolute_list) <- c("ref","nf_4.5", "ff_4.5", "nf_8.5", "ff_8.5")

tern_gr.2_absolute <- rbindlist(tern_gr.2_absolute_list, id="id_scen")

tern_gr.2_absolute$id_scen <- factor(tern_gr.2_absolute$id_scen , levels=c("ref","nf_4.5", "ff_4.5", "nf_8.5", "ff_8.5"))

tern_gr.2_absolute <- subset(tern_gr.2_absolute, select=-c(5,6,8,9,11,12))

# tern_gr.2_01max_absolute_graph <- ggplot(tern_gr.2_absolute, aes(x=id_scen, y=day01_mean)) + 
#  geom_boxplot( alpha =0.8, width = 0.9)+ 
#  labs(y="mean streamflow m3/s") +
#  ggtitle("tern gr.2_day01_max") +
#  coord_cartesian( ylim = c(900, 2200))+
#  theme(axis.title.x = element_blank())

tern_gr.2_03max_absolute_graph <- ggplot(tern_gr.2_absolute, aes(x=id_scen, y=day03_mean)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="Q m3/s") +
  ggtitle("gr.2_day03_max") +
  theme(axis.title.x = element_blank())+
  coord_cartesian( ylim = c(900, 2200))+ 
  theme(panel.background = element_rect(fill = "white", colour = 'black'),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "lightgrey"),
        panel.grid.minor = element_line(size = 0.3, linetype = 'solid', colour = "lightgrey"),
        plot.title = element_text(size = 20),
        text = element_text(size=18))

# tern_gr.2_07max_absolute_graph <- ggplot(tern_gr.2_absolute, aes(x=id_scen, y=day07_mean)) + 
#  geom_boxplot( alpha =0.8, width = 0.9)+ 
#  labs(y="mean streamflow m3/s") +
#  ggtitle("tern gr.2_day07_max")+
#  coord_cartesian( ylim = c(900, 2200))+
#  theme(axis.title.x = element_blank())

#######%%%%%%%%%%
# Little tern absolute values box_plot GR.4

tern_gr.4_absolute_list <- list(tern_ref_gr.4_p2, tern_nf_4.5_gr.4_p2, tern_ff_4.5_gr.4_p2,
                                  tern_nf_8.5_gr.4_p2, tern_ff_8.5_gr.4_p2)

names(tern_gr.4_absolute_list) <- c("ref","nf_4.5", "ff_4.5", "nf_8.5", "ff_8.5")

tern_gr.4_absolute <- rbindlist(tern_gr.4_absolute_list, id="id")

tern_gr.4_absolute$id <- factor(tern_gr.4_absolute$id , levels=c("ref","nf_4.5", "ff_4.5", "nf_8.5", "ff_8.5"))

tern_gr.4_absolute_graph <- ggplot(tern_gr.4_absolute, aes(x=id, y=yearly)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="no. of days") +
  ggtitle("gr.4_P0.75")+
  theme(axis.title.x = element_blank())+
  coord_cartesian( ylim = c(10, 50))+
  theme(panel.background = element_rect(fill = "white", colour = 'black'),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "lightgrey"),
        panel.grid.minor = element_line(size = 0.3, linetype = 'solid', colour = "lightgrey"),
        plot.title = element_text(size = 20),
        text = element_text(size=18))

plot(tern_gr.4_absolute_graph)

# join all the graphs together 

library("ggpubr")
#model/subbasin/value 
# 162 values (9 models * 18 subbasins)

tern_box_plot_absolute <- ggarrange(tern_gr.1_le_absolute_graph, tern_gr.1_incub_absolute_graph, 
                                      tern_gr.1_rear_absolute_graph, 
                                      # tern_gr.2_01max_absolute_graph, tern_gr.2_07max_absolute_graph, 
                                      tern_gr.2_03max_absolute_graph, plot_spacer(), plot_spacer(),
                                      tern_gr.4_absolute_graph,  
                                      
                                      nrow = 7, ncol = 1)


ggsave(tern_box_plot_absolute , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/",
                     "tern_box_plot_absolute.jpg"),device = "jpg", width = 20, height = 10)

######## ALL 3 BIRDS ########
#model/subbasin/value 
# 162 values (9 models * 18 subbasins)

all_box_plot_absolute <- ggarrange(m_gull_gr.1_le_absolute_graph, m_gull_gr.1_incub_absolute_graph, 
                                   m_gull_gr.1_rear_absolute_graph, m_gull_gr.2_03max_absolute_graph,  
                                   m_gull_gr.4_absolute_graph,
                                   bh_gull_gr.1_incub_absolute_graph, bh_gull_gr.1_rear_absolute_graph,
                                   bh_gull_gr.3_absolute_graph, bh_gull_gr.2_03max_absolute_graph, 
                                   bh_gull_gr.4_absolute_graph,
                                    tern_gr.1_le_absolute_graph, tern_gr.1_incub_absolute_graph, 
                                    tern_gr.1_rear_absolute_graph, tern_gr.2_03max_absolute_graph,  
                                    tern_gr.4_absolute_graph,
                                    
                                   nrow = 3, ncol = 5)
plot(all_box_plot_absolute)

ggsave(all_box_plot_absolute , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/",
                     "all_box_plot_absolute.jpg"),device = "jpg", width = 20, height = 10)

library("patchwork")

all_box_plot_absolute1 <- (m_gull_gr.1_le_absolute_graph | m_gull_gr.1_incub_absolute_graph | m_gull_gr.1_rear_absolute_graph | m_gull_gr.2_03max_absolute_graph |plot_spacer()|plot_spacer()| m_gull_gr.4_absolute_graph) /
                          (plot_spacer() | bh_gull_gr.1_incub_absolute_graph | bh_gull_gr.1_rear_absolute_graph | bh_gull_gr.2_03max_absolute_graph | bh_gull_gr.3_absolute_graph | bh_gull_gr.4_absolute_graph | plot_spacer()) /
                          (tern_gr.1_le_absolute_graph | tern_gr.1_incub_absolute_graph | tern_gr.1_rear_absolute_graph | tern_gr.2_03max_absolute_graph | plot_spacer() | plot_spacer()| tern_gr.4_absolute_graph)
                        

plot(all_box_plot_absolute1)
ggsave(all_box_plot_absolute1 , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/",
                     "all_box_plot_absolute1.jpg"),device = "jpg", width = 30, height = 10)

all_box_plot_absolute2 <- ggarrange(m_gull_box_plot_absolute, 
                                    bh_gull_box_plot_absolute, 
                                    tern_box_plot_absolute,
                                    nrow = 1, ncol = 3)

plot(all_box_plot_absolute2)

ggsave(all_box_plot_absolute2 , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/",
                     "all_box_plot_absolute2.jpg"),device = "jpg", width = 14, height = 20)

############################################
############################################
#*******   % CHANGE        ***********#
#*******    BOX PLOTS      ***********#
###########################################
# % increase = (Future/Original Number - 1) × 100
############################################



############################################
#*******   Mew gull   ***********#
###########################################

#######%%%%%%%%%%
# Mew-Gull percentage change in values box_plot GR.1 Laying eggs

m_gull_gr.1_le_perc_chang_in <- m.gull_ref_le_gr.1_unlist %>% 
  right_join(m.gull_nf_4.5_le_gr.1_unlist, by=c("model","subbasin")) %>%
  right_join(m.gull_ff_4.5_le_gr.1_unlist, by=c("model","subbasin")) %>%
  right_join(m.gull_nf_8.5_le_gr.1_unlist, by=c("model","subbasin")) %>%
  right_join(m.gull_ff_8.5_le_gr.1_unlist, by=c("model","subbasin"))

#remove repeating columns
m_gull_gr.1_le_perc_chang_in <- m_gull_gr.1_le_perc_chang_in[-c(2,5,7,9,11)]

names(m_gull_gr.1_le_perc_chang_in) <- c("model", "subbasin", "ref",
                                             "abs_nf_4.5",  "abs_ff_4.5", 
                                             "abs_nf_8.5",  "abs_ff_8.5")


m_gull_gr.1_le_perc_chang_in$NF_4.5 <- (m_gull_gr.1_le_perc_chang_in$abs_nf_4.5/m_gull_gr.1_le_perc_chang_in$ref - 1)*100
m_gull_gr.1_le_perc_chang_in$FF_4.5 <- (m_gull_gr.1_le_perc_chang_in$abs_ff_4.5/m_gull_gr.1_le_perc_chang_in$ref - 1)*100
m_gull_gr.1_le_perc_chang_in$NF_8.5 <- (m_gull_gr.1_le_perc_chang_in$abs_nf_8.5/m_gull_gr.1_le_perc_chang_in$ref - 1)*100
m_gull_gr.1_le_perc_chang_in$FF_8.5 <- (m_gull_gr.1_le_perc_chang_in$abs_ff_8.5/m_gull_gr.1_le_perc_chang_in$ref - 1)*100


m_gull_gr.1_le_perc_chang <- pivot_longer(m_gull_gr.1_le_perc_chang_in, cols=8:11, names_to = "scenario", values_to = "perc_change")
m_gull_gr.1_le_perc_chang <- m_gull_gr.1_le_perc_chang[-c(3:7)]

# establish an order
m_gull_gr.1_le_perc_chang$scenario <- factor(m_gull_gr.1_le_perc_chang$scenario , 
                                                 levels=c("NF_4.5", "FF_4.5", "NF_8.5", "FF_8.5"))

#test for making a pivot table in excel
write.csv(m_gull_gr.1_le_perc_chang, "D:/Ptaki_hydro/Obliczenia/R/Results/m_gull_gr.1_le_perc_chang.csv")

m_gull_gr.1_le_perc_graph <- ggplot(m_gull_gr.1_le_perc_chang, aes(x=scenario, y=perc_change)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="% change in Q m3/s") +
  ggtitle("gr.1_mean_LE")+
  theme(axis.title.x = element_blank())+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  coord_cartesian( ylim = c(-30, 70))+
  scale_y_continuous(breaks = seq(from = -100, to = 200, by = 10))

######TEST 21.01.2022
m_gull_gr.1_le_perc_graph <- ggplot(m_gull_gr.1_le_perc_chang, aes(x=scenario, y=perc_change)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="% change in Q m3/s") +
  ggtitle("gr.1_mean_LE")+
  theme(axis.title.x = element_blank(),
        panel.background = element_rect(fill = "white", colour = 'black'),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "lightgrey"),
        panel.grid.minor = element_line("NA"),
        plot.title = element_text(size = 20),
        text = element_text(size=18))+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  coord_cartesian( ylim = c(-30, 70))+
  scale_y_continuous(breaks = seq(from = -100, to = 200, by = 10))


plot(m_gull_gr.1_le_perc_graph)

# calculate median from boxplots
m_gull_gr.1_le_perc_chang_median <- aggregate( perc_change ~ scenario ,
                                                  data = m_gull_gr.1_le_perc_chang, 
                                                  median)

#######%%%%%%%%%%
# M-Gull percentage change in change in values box_plot GR.1 Incub

m_gull_gr.1_incub_perc_chang_in <- m.gull_ref_incub_gr.1_unlist %>% 
  right_join(m.gull_nf_4.5_incub_gr.1_unlist, by=c("model","subbasin")) %>%
  right_join(m.gull_ff_4.5_incub_gr.1_unlist, by=c("model","subbasin")) %>%
  right_join(m.gull_nf_8.5_incub_gr.1_unlist, by=c("model","subbasin")) %>%
  right_join(m.gull_ff_8.5_incub_gr.1_unlist, by=c("model","subbasin"))

#remove repeating columns
m_gull_gr.1_incub_perc_chang_in <- m_gull_gr.1_incub_perc_chang_in[-c(2,5,7,9,11)]

names(m_gull_gr.1_incub_perc_chang_in) <- c("model", "subbasin", "ref",
                                             "abs_nf_4.5",  "abs_ff_4.5", 
                                             "abs_nf_8.5",  "abs_ff_8.5")


m_gull_gr.1_incub_perc_chang_in$NF_4.5 <- (m_gull_gr.1_incub_perc_chang_in$abs_nf_4.5/m_gull_gr.1_incub_perc_chang_in$ref - 1)*100
m_gull_gr.1_incub_perc_chang_in$FF_4.5 <- (m_gull_gr.1_incub_perc_chang_in$abs_ff_4.5/m_gull_gr.1_incub_perc_chang_in$ref - 1)*100
m_gull_gr.1_incub_perc_chang_in$NF_8.5 <- (m_gull_gr.1_incub_perc_chang_in$abs_nf_8.5/m_gull_gr.1_incub_perc_chang_in$ref - 1)*100
m_gull_gr.1_incub_perc_chang_in$FF_8.5 <- (m_gull_gr.1_incub_perc_chang_in$abs_ff_8.5/m_gull_gr.1_incub_perc_chang_in$ref - 1)*100


m_gull_gr.1_incub_perc_chang <- pivot_longer(m_gull_gr.1_incub_perc_chang_in, cols=8:11, names_to = "scenario", values_to = "perc_change")
m_gull_gr.1_incub_perc_chang <- m_gull_gr.1_incub_perc_chang[-c(3:7)]

# establish an order
m_gull_gr.1_incub_perc_chang$scenario <- factor(m_gull_gr.1_incub_perc_chang$scenario , 
                                                 levels=c("NF_4.5", "FF_4.5", "NF_8.5", "FF_8.5"))


m_gull_gr.1_incub_perc_graph <- ggplot(m_gull_gr.1_incub_perc_chang, aes(x=scenario, y=perc_change)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="% change in Q m3/s") +
  ggtitle("gr.1_mean_Incub")+
  theme(axis.title.x = element_blank(),
        panel.background = element_rect(fill = "white", colour = 'black'),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "lightgrey"),
        panel.grid.minor = element_line("NA"),
        plot.title = element_text(size = 20),
        text = element_text(size=18))+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  coord_cartesian( ylim = c(-30, 70))+
  scale_y_continuous(breaks = seq(from = -100, to = 200, by = 10))

plot(m_gull_gr.1_incub_perc_graph)

# calculate median from boxplots
m_gull_gr.1_incub_perc_chang_median <- aggregate(perc_change ~ scenario ,
                                                 data = m_gull_gr.1_incub_perc_chang, 
                                                 median)


#######%%%%%%%%%%
# M-Gull percentage change in values box_plot GR.1 Rear

m_gull_gr.1_rear_perc_chang_in <- m.gull_ref_rear_gr.1_unlist %>% 
  right_join(m.gull_nf_4.5_rear_gr.1_unlist, by=c("model","subbasin")) %>%
  right_join(m.gull_ff_4.5_rear_gr.1_unlist, by=c("model","subbasin")) %>%
  right_join(m.gull_nf_8.5_rear_gr.1_unlist, by=c("model","subbasin")) %>%
  right_join(m.gull_ff_8.5_rear_gr.1_unlist, by=c("model","subbasin"))

#remove repeating columns
m_gull_gr.1_rear_perc_chang_in <- m_gull_gr.1_rear_perc_chang_in[-c(2,5,7,9,11)]

names(m_gull_gr.1_rear_perc_chang_in) <- c("model", "subbasin", "ref",
                                            "abs_nf_4.5",  "abs_ff_4.5", 
                                            "abs_nf_8.5",  "abs_ff_8.5")


m_gull_gr.1_rear_perc_chang_in$NF_4.5 <- (m_gull_gr.1_rear_perc_chang_in$abs_nf_4.5/m_gull_gr.1_rear_perc_chang_in$ref - 1)*100
m_gull_gr.1_rear_perc_chang_in$FF_4.5 <- (m_gull_gr.1_rear_perc_chang_in$abs_ff_4.5/m_gull_gr.1_rear_perc_chang_in$ref - 1)*100
m_gull_gr.1_rear_perc_chang_in$NF_8.5 <- (m_gull_gr.1_rear_perc_chang_in$abs_nf_8.5/m_gull_gr.1_rear_perc_chang_in$ref - 1)*100
m_gull_gr.1_rear_perc_chang_in$FF_8.5 <- (m_gull_gr.1_rear_perc_chang_in$abs_ff_8.5/m_gull_gr.1_rear_perc_chang_in$ref - 1)*100


m_gull_gr.1_rear_perc_chang <- pivot_longer(m_gull_gr.1_rear_perc_chang_in, cols=8:11, names_to = "scenario", values_to = "perc_change")
m_gull_gr.1_rear_perc_chang <- m_gull_gr.1_rear_perc_chang[-c(3:7)]

# establish an order
m_gull_gr.1_rear_perc_chang$scenario <- factor(m_gull_gr.1_rear_perc_chang$scenario , 
                                                levels=c("NF_4.5", "FF_4.5", "NF_8.5", "FF_8.5"))


m_gull_gr.1_rear_perc_graph <- ggplot(m_gull_gr.1_rear_perc_chang, aes(x=scenario, y=perc_change)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="% change in Q m3/s") +
  ggtitle("gr.1_mean_RC")+
  theme(axis.title.x = element_blank(),
        panel.background = element_rect(fill = "white", colour = 'black'),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "lightgrey"),
        panel.grid.minor = element_line("NA"),
        plot.title = element_text(size = 20),
        text = element_text(size=18))+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  coord_cartesian( ylim = c(-30, 70))+
  scale_y_continuous(breaks = seq(from = -100, to = 200, by = 10))

plot(m_gull_gr.1_rear_perc_graph)

# calculate median from boxplots
m_gull_gr.1_rear_perc_chang_median <- aggregate( perc_change ~ scenario ,
                                                  data = m_gull_gr.1_rear_perc_chang, 
                                                  median)

#######%%%%%%%%%%
# M-Gull percentage change in values box_plot GR.2

m.gull_ref_gr.2_unlist_p22 <- m.gull_ref_gr.2_unlist_p2[-c(3:5,7:9)]
m.gull_nf_4.5_gr.2_unlist_p22 <- m.gull_nf_4.5_gr.2_unlist_p2[-c(3:5,7:9)]
m.gull_ff_4.5_gr.2_unlist_p22 <- m.gull_ff_4.5_gr.2_unlist_p2[-c(3:5,7:9)]
m.gull_nf_8.5_gr.2_unlist_p22 <- m.gull_nf_8.5_gr.2_unlist_p2[-c(3:5,7:9)]
m.gull_ff_8.5_gr.2_unlist_p22 <- m.gull_ff_8.5_gr.2_unlist_p2[-c(3:5,7:9)]

m_gull_gr.2_perc_chang_in <- m.gull_ref_gr.2_unlist_p22 %>% 
  right_join(m.gull_nf_4.5_gr.2_unlist_p22, by=c("model","subbasin")) %>%
  right_join(m.gull_ff_4.5_gr.2_unlist_p22, by=c("model","subbasin")) %>%
  right_join(m.gull_nf_8.5_gr.2_unlist_p22, by=c("model","subbasin")) %>%
  right_join(m.gull_ff_8.5_gr.2_unlist_p22, by=c("model","subbasin"))

names(m_gull_gr.2_perc_chang_in) <- c("model", "subbasin", "ref",
                                       "abs_nf_4.5", "abs_ff_4.5", 
                                       "abs_nf_8.5", "abs_ff_8.5")


m_gull_gr.2_perc_chang_in$NF_4.5 <- (m_gull_gr.2_perc_chang_in$abs_nf_4.5/m_gull_gr.2_perc_chang_in$ref - 1)*100
m_gull_gr.2_perc_chang_in$FF_4.5 <- (m_gull_gr.2_perc_chang_in$abs_ff_4.5/m_gull_gr.2_perc_chang_in$ref - 1)*100
m_gull_gr.2_perc_chang_in$NF_8.5 <- (m_gull_gr.2_perc_chang_in$abs_nf_8.5/m_gull_gr.2_perc_chang_in$ref - 1)*100
m_gull_gr.2_perc_chang_in$FF_8.5 <- (m_gull_gr.2_perc_chang_in$abs_ff_8.5/m_gull_gr.2_perc_chang_in$ref - 1)*100


m_gull_gr.2_perc_chang <- pivot_longer(m_gull_gr.2_perc_chang_in, cols=8:11, names_to = "scenario", values_to = "perc_change")
m_gull_gr.2_perc_chang <- m_gull_gr.2_perc_chang[-c(3:7)]

m_gull_gr.2_perc_chang$scenario <- factor(m_gull_gr.2_perc_chang$scenario , 
                                           levels=c("NF_4.5", "FF_4.5", "NF_8.5", "FF_8.5"))

m_gull_gr.2_perc_graph <- ggplot(m_gull_gr.2_perc_chang, aes(x=scenario, y=perc_change)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="% change in Q m3/s") +
  ggtitle("gr.2_day03_max") +
  theme(axis.title.x = element_blank(),
        panel.background = element_rect(fill = "white", colour = 'black'),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "lightgrey"),
        panel.grid.minor = element_line("NA"),
        plot.title = element_text(size = 20),
        text = element_text(size=18))+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  coord_cartesian( ylim = c(-30, 70))+
  scale_y_continuous(breaks = seq(from = -100, to = 200, by = 10))

plot(m_gull_gr.2_perc_graph)

# calculate median from boxplots
m_gull_gr.2_perc_chang_median <- aggregate( perc_change ~ scenario ,
                                                  data = m_gull_gr.2_perc_chang, 
                                                  median)

#######%%%%%%%%%%
# Mew-Gull percentage change in values box_plot GR.4

m_gull_gr.4_perc_chang_in <- m.gull_ref_gr.4_p2 %>% right_join(m.gull_nf_4.5_gr.4_p2, by=c("Group.1","Group.2")) %>%
  right_join(m.gull_ff_4.5_gr.4_p2, by=c("Group.1","Group.2")) %>%
  right_join(m.gull_nf_8.5_gr.4_p2, by=c("Group.1","Group.2")) %>%
  right_join(m.gull_ff_8.5_gr.4_p2, by=c("Group.1","Group.2"))

names(m_gull_gr.4_perc_chang_in) <- c("model", "subbasin", "val1", "ref",
                                       "val.2", "abs_nf_4.5", "val.3", "abs_ff_4.5", 
                                       "val.4", "abs_nf_8.5", "val.5" , "abs_ff_8.5")


m_gull_gr.4_perc_chang_in$NF_4.5 <- (m_gull_gr.4_perc_chang_in$abs_nf_4.5/m_gull_gr.4_perc_chang_in$ref - 1)*100
m_gull_gr.4_perc_chang_in$FF_4.5 <- (m_gull_gr.4_perc_chang_in$abs_ff_4.5/m_gull_gr.4_perc_chang_in$ref - 1)*100
m_gull_gr.4_perc_chang_in$NF_8.5 <- (m_gull_gr.4_perc_chang_in$abs_nf_8.5/m_gull_gr.4_perc_chang_in$ref - 1)*100
m_gull_gr.4_perc_chang_in$FF_8.5 <- (m_gull_gr.4_perc_chang_in$abs_ff_8.5/m_gull_gr.4_perc_chang_in$ref - 1)*100


m_gull_gr.4_perc_chang <- pivot_longer(m_gull_gr.4_perc_chang_in, cols=13:16, names_to = "scenario", values_to = "perc_change")
m_gull_gr.4_perc_chang <- m_gull_gr.4_perc_chang[-c(3:12)]

m_gull_gr.4_perc_chang$scenario <- factor(m_gull_gr.4_perc_chang$scenario , 
                                           levels=c("NF_4.5", "FF_4.5", "NF_8.5", "FF_8.5"))

m_gull_gr.4_perc_graph <- ggplot(m_gull_gr.4_perc_chang, aes(x=scenario, y=perc_change)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="% change in no. of days ") +
  ggtitle("gr.4_P0.75")+
  theme(axis.title.x = element_blank(),
        panel.background = element_rect(fill = "white", colour = 'black'),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "lightgrey"),
        panel.grid.minor = element_line("NA"),
        plot.title = element_text(size = 20),
        text = element_text(size=18))+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  coord_cartesian( ylim = c(-40, 100))+
  scale_y_continuous(breaks = seq(from = -100, to = 200, by = 20))

plot(m_gull_gr.4_perc_graph)

# calculate median from boxplots
m_gull_gr.4_perc_chang_median <- aggregate( perc_change ~ scenario ,
                                            data = m_gull_gr.4_perc_chang, 
                                            median)


# join all the graphs together 

library("ggpubr")
#model/subbasin/value 
# 162 values (9 models * 18 subbasins)

m_gull_box_plot_perc <- ggarrange(m_gull_gr.1_le_perc_graph, m_gull_gr.1_incub_perc_graph, 
                                m_gull_gr.1_rear_perc_graph, m_gull_gr.2_perc_graph, 
                                plot_spacer(), plot_spacer(), m_gull_gr.4_perc_graph,
                                nrow = 7, ncol = 1)

plot(m_gull_box_plot_perc)

ggsave(m_gull_box_plot_perc , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/",
                     "m_gull_box_plot_perc1.jpg"),device = "jpg", width = 3, height = 15)


# combine median values in table
install.packages("gdata")
library(gdata)

m_gull_box_plot_median <- combine(m_gull_gr.1_le_perc_chang_median, m_gull_gr.1_incub_perc_chang_median,
                                  m_gull_gr.1_rear_perc_chang_median,
                                  m_gull_gr.2_perc_chang_median, m_gull_gr.4_perc_chang_median)

write.csv(m_gull_box_plot_median, "D:/Ptaki_hydro/Obliczenia/R/Results/median_boxplot/m_gull_box_plot_median.csv") 


############################################
#*******   Black- headed gull   ***********#
###########################################
#######%%%%%%%%%%
# BH-Gull percentage change in values box_plot GR.1 Incub

bh_gull_gr.1_incub_perc_chang_in <- bh.gull_ref_incub_gr.1_unlist %>% 
  right_join(bh.gull_nf_4.5_incub_gr.1_unlist, by=c("model","subbasin")) %>%
  right_join(bh.gull_ff_4.5_incub_gr.1_unlist, by=c("model","subbasin")) %>%
  right_join(bh.gull_nf_8.5_incub_gr.1_unlist, by=c("model","subbasin")) %>%
  right_join(bh.gull_ff_8.5_incub_gr.1_unlist, by=c("model","subbasin"))

#remove repeating columns
bh_gull_gr.1_incub_perc_chang_in <- bh_gull_gr.1_incub_perc_chang_in[-c(2,5,7,9,11)]

names(bh_gull_gr.1_incub_perc_chang_in) <- c("model", "subbasin", "ref",
                                       "abs_nf_4.5",  "abs_ff_4.5", 
                                       "abs_nf_8.5",  "abs_ff_8.5")


bh_gull_gr.1_incub_perc_chang_in$NF_4.5 <- (bh_gull_gr.1_incub_perc_chang_in$abs_nf_4.5/bh_gull_gr.1_incub_perc_chang_in$ref - 1)*100
bh_gull_gr.1_incub_perc_chang_in$FF_4.5 <- (bh_gull_gr.1_incub_perc_chang_in$abs_ff_4.5/bh_gull_gr.1_incub_perc_chang_in$ref - 1)*100
bh_gull_gr.1_incub_perc_chang_in$NF_8.5 <- (bh_gull_gr.1_incub_perc_chang_in$abs_nf_8.5/bh_gull_gr.1_incub_perc_chang_in$ref - 1)*100
bh_gull_gr.1_incub_perc_chang_in$FF_8.5 <- (bh_gull_gr.1_incub_perc_chang_in$abs_ff_8.5/bh_gull_gr.1_incub_perc_chang_in$ref - 1)*100


bh_gull_gr.1_incub_perc_chang <- pivot_longer(bh_gull_gr.1_incub_perc_chang_in, cols=8:11, names_to = "scenario", values_to = "perc_change")
bh_gull_gr.1_incub_perc_chang <- bh_gull_gr.1_incub_perc_chang[-c(3:7)]

# establish an order
bh_gull_gr.1_incub_perc_chang$scenario <- factor(bh_gull_gr.1_incub_perc_chang$scenario , 
                                           levels=c("NF_4.5", "FF_4.5", "NF_8.5", "FF_8.5"))


bh_gull_gr.1_incub_perc_graph <- ggplot(bh_gull_gr.1_incub_perc_chang, aes(x=scenario, y=perc_change)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="% change in Q m3/s") +
  ggtitle("gr.1_mean_Incub")+
  theme(axis.title.x = element_blank(),
        panel.background = element_rect(fill = "white", colour = 'black'),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "lightgrey"),
        panel.grid.minor = element_line("NA"),
        plot.title = element_text(size = 20),
        text = element_text(size=18))+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  coord_cartesian( ylim = c(-30, 70))+
  scale_y_continuous(breaks = seq(from = -100, to = 200, by = 10))

plot(bh_gull_gr.1_incub_perc_graph)

# calculate median from boxplots
bh_gull_gr.1_incub_perc_chang_median <- aggregate(perc_change ~ scenario ,
                                                 data = bh_gull_gr.1_incub_perc_chang, 
                                                 median)


#######%%%%%%%%%%
# BH-Gull percentage change in values box_plot GR.1 Rear

bh_gull_gr.1_rear_perc_chang_in <- bh.gull_ref_rear_gr.1_unlist %>% 
  right_join(bh.gull_nf_4.5_rear_gr.1_unlist, by=c("model","subbasin")) %>%
  right_join(bh.gull_ff_4.5_rear_gr.1_unlist, by=c("model","subbasin")) %>%
  right_join(bh.gull_nf_8.5_rear_gr.1_unlist, by=c("model","subbasin")) %>%
  right_join(bh.gull_ff_8.5_rear_gr.1_unlist, by=c("model","subbasin"))

#remove repeating columns
bh_gull_gr.1_rear_perc_chang_in <- bh_gull_gr.1_rear_perc_chang_in[-c(2,5,7,9,11)]

names(bh_gull_gr.1_rear_perc_chang_in) <- c("model", "subbasin", "ref",
                                             "abs_nf_4.5",  "abs_ff_4.5", 
                                             "abs_nf_8.5",  "abs_ff_8.5")


bh_gull_gr.1_rear_perc_chang_in$NF_4.5 <- (bh_gull_gr.1_rear_perc_chang_in$abs_nf_4.5/bh_gull_gr.1_rear_perc_chang_in$ref - 1)*100
bh_gull_gr.1_rear_perc_chang_in$FF_4.5 <- (bh_gull_gr.1_rear_perc_chang_in$abs_ff_4.5/bh_gull_gr.1_rear_perc_chang_in$ref - 1)*100
bh_gull_gr.1_rear_perc_chang_in$NF_8.5 <- (bh_gull_gr.1_rear_perc_chang_in$abs_nf_8.5/bh_gull_gr.1_rear_perc_chang_in$ref - 1)*100
bh_gull_gr.1_rear_perc_chang_in$FF_8.5 <- (bh_gull_gr.1_rear_perc_chang_in$abs_ff_8.5/bh_gull_gr.1_rear_perc_chang_in$ref - 1)*100


bh_gull_gr.1_rear_perc_chang <- pivot_longer(bh_gull_gr.1_rear_perc_chang_in, cols=8:11, names_to = "scenario", values_to = "perc_change")
bh_gull_gr.1_rear_perc_chang <- bh_gull_gr.1_rear_perc_chang[-c(3:7)]

# establish an order
bh_gull_gr.1_rear_perc_chang$scenario <- factor(bh_gull_gr.1_rear_perc_chang$scenario , 
                                                 levels=c("NF_4.5", "FF_4.5", "NF_8.5", "FF_8.5"))


bh_gull_gr.1_rear_perc_graph <- ggplot(bh_gull_gr.1_rear_perc_chang, aes(x=scenario, y=perc_change)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="% change in Q m3/s") +
  ggtitle("gr.1_mean_RC")+
  theme(axis.title.x = element_blank(),
        panel.background = element_rect(fill = "white", colour = 'black'),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "lightgrey"),
        panel.grid.minor = element_line("NA"),
        plot.title = element_text(size = 20),
        text = element_text(size=18))+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  coord_cartesian( ylim = c(-30, 70))+
  scale_y_continuous(breaks = seq(from = -100, to = 200, by = 10))

plot(bh_gull_gr.1_rear_perc_graph)

# calculate median from boxplots
bh_gull_gr.1_rear_perc_chang_median <- aggregate(perc_change ~ scenario ,
                                                  data = bh_gull_gr.1_rear_perc_chang, 
                                                  median)

#######%%%%%%%%%%
# BH-Gull percentage change in values box_plot GR.2

bh.gull_ref_gr.2_unlist_p22 <- bh.gull_ref_gr.2_unlist_p2[-c(3:5,7:9)]
bh.gull_nf_4.5_gr.2_unlist_p22 <- bh.gull_nf_4.5_gr.2_unlist_p2[-c(3:5,7:9)]
bh.gull_ff_4.5_gr.2_unlist_p22 <- bh.gull_ff_4.5_gr.2_unlist_p2[-c(3:5,7:9)]
bh.gull_nf_8.5_gr.2_unlist_p22 <- bh.gull_nf_8.5_gr.2_unlist_p2[-c(3:5,7:9)]
bh.gull_ff_8.5_gr.2_unlist_p22 <- bh.gull_ff_8.5_gr.2_unlist_p2[-c(3:5,7:9)]

bh_gull_gr.2_perc_chang_in <- bh.gull_ref_gr.2_unlist_p22 %>% 
  right_join(bh.gull_nf_4.5_gr.2_unlist_p22, by=c("model","subbasin")) %>%
  right_join(bh.gull_ff_4.5_gr.2_unlist_p22, by=c("model","subbasin")) %>%
  right_join(bh.gull_nf_8.5_gr.2_unlist_p22, by=c("model","subbasin")) %>%
  right_join(bh.gull_ff_8.5_gr.2_unlist_p22, by=c("model","subbasin"))

names(bh_gull_gr.2_perc_chang_in) <- c("model", "subbasin", "ref",
                                       "abs_nf_4.5", "abs_ff_4.5", 
                                       "abs_nf_8.5", "abs_ff_8.5")


bh_gull_gr.2_perc_chang_in$NF_4.5 <- (bh_gull_gr.2_perc_chang_in$abs_nf_4.5/bh_gull_gr.2_perc_chang_in$ref - 1)*100
bh_gull_gr.2_perc_chang_in$FF_4.5 <- (bh_gull_gr.2_perc_chang_in$abs_ff_4.5/bh_gull_gr.2_perc_chang_in$ref - 1)*100
bh_gull_gr.2_perc_chang_in$NF_8.5 <- (bh_gull_gr.2_perc_chang_in$abs_nf_8.5/bh_gull_gr.2_perc_chang_in$ref - 1)*100
bh_gull_gr.2_perc_chang_in$FF_8.5 <- (bh_gull_gr.2_perc_chang_in$abs_ff_8.5/bh_gull_gr.2_perc_chang_in$ref - 1)*100


bh_gull_gr.2_perc_chang <- pivot_longer(bh_gull_gr.2_perc_chang_in, cols=8:11, names_to = "scenario", values_to = "perc_change")
bh_gull_gr.2_perc_chang <- bh_gull_gr.2_perc_chang[-c(3:7)]

bh_gull_gr.2_perc_chang$scenario <- factor(bh_gull_gr.2_perc_chang$scenario , 
                                           levels=c("NF_4.5", "FF_4.5", "NF_8.5", "FF_8.5"))

bh_gull_gr.2_perc_graph <- ggplot(bh_gull_gr.2_perc_chang, aes(x=scenario, y=perc_change)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="% change in Q m3/s") +
  ggtitle("gr.2_day03_max") +
  theme(axis.title.x = element_blank(),
        panel.background = element_rect(fill = "white", colour = 'black'),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "lightgrey"),
        panel.grid.minor = element_line("NA"),
        plot.title = element_text(size = 20),
        text = element_text(size=18))+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  coord_cartesian( ylim = c(-30, 70))+
  scale_y_continuous(breaks = seq(from = -100, to = 200, by = 10))

plot(bh_gull_gr.2_perc_graph)

# calculate median from boxplots
bh_gull_gr.2_perc_chang_median <- aggregate(perc_change ~ scenario ,
                                                 data = bh_gull_gr.2_perc_chang, 
                                                 median)


#######%%%%%%%%%%
# BH-Gull percentage change in values box_plot GR.3

bh_gull_gr.3_perc_chang_in <- bh.gull_ref_list_gr.3_unlist_p1 %>% 
  right_join(bh.gull_nf_4.5_list_gr.3_unlist_p1, by=c("model","subbasin")) %>%
  right_join(bh.gull_ff_4.5_list_gr.3_unlist_p1, by=c("model","subbasin")) %>%
  right_join(bh.gull_nf_8.5_list_gr.3_unlist_p1, by=c("model","subbasin")) %>%
  right_join(bh.gull_ff_8.5_list_gr.3_unlist_p1, by=c("model","subbasin"))

bh_gull_gr.3_perc_chang_in <- bh_gull_gr.3_perc_chang_in[ -c(3,5,7,9,11)]

names(bh_gull_gr.3_perc_chang_in) <- c("model", "subbasin", "ref",
                                        "abs_nf_4.5", "abs_ff_4.5", 
                                        "abs_nf_8.5",  "abs_ff_8.5")


bh_gull_gr.3_perc_chang_in$NF_4.5 <- (bh_gull_gr.3_perc_chang_in$abs_nf_4.5/bh_gull_gr.3_perc_chang_in$ref - 1)*100
bh_gull_gr.3_perc_chang_in$FF_4.5 <- (bh_gull_gr.3_perc_chang_in$abs_ff_4.5/bh_gull_gr.3_perc_chang_in$ref - 1)*100
bh_gull_gr.3_perc_chang_in$NF_8.5 <- (bh_gull_gr.3_perc_chang_in$abs_nf_8.5/bh_gull_gr.3_perc_chang_in$ref - 1)*100
bh_gull_gr.3_perc_chang_in$FF_8.5 <- (bh_gull_gr.3_perc_chang_in$abs_ff_8.5/bh_gull_gr.3_perc_chang_in$ref - 1)*100


bh_gull_gr.3_perc_chang <- pivot_longer(bh_gull_gr.3_perc_chang_in, cols=8:11, names_to = "scenario", values_to = "perc_change")
bh_gull_gr.3_perc_chang <- bh_gull_gr.3_perc_chang[-c(3:7)]

bh_gull_gr.3_perc_chang$scenario <- factor(bh_gull_gr.3_perc_chang$scenario , 
                                           levels=c("NF_4.5", "FF_4.5", "NF_8.5", "FF_8.5"))

bh_gull_gr.3_perc_graph <- ggplot(bh_gull_gr.3_perc_chang, aes(x=scenario, y=perc_change)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="% change") +
  ggtitle("gr.3_vp_max")+
  theme(axis.title.x = element_blank(),
        panel.background = element_rect(fill = "white", colour = 'black'),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "lightgrey"),
        panel.grid.minor = element_line("NA"),
        plot.title = element_text(size = 20),
        text = element_text(size=18))+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  coord_cartesian( ylim = c(-100, 200))+
  scale_y_continuous(breaks = seq(from = -100, to = 200, by = 30))

plot(bh_gull_gr.3_perc_graph)

# calculate median from boxplots
bh_gull_gr.3_perc_chang_median <- aggregate(perc_change ~ scenario ,
                                            data = bh_gull_gr.3_perc_chang, 
                                            median)

#######%%%%%%%%%%
# BH-Gull percentage change box_plot GR.4

bh_gull_gr.4_perc_chang_in <- bh.gull_ref_gr.4_p2 %>% right_join(bh.gull_nf_4.5_gr.4_p2, by=c("Group.1","Group.2")) %>%
                                     right_join(bh.gull_ff_4.5_gr.4_p2, by=c("Group.1","Group.2")) %>%
                                      right_join(bh.gull_nf_8.5_gr.4_p2, by=c("Group.1","Group.2")) %>%
                                        right_join(bh.gull_ff_8.5_gr.4_p2, by=c("Group.1","Group.2"))

names(bh_gull_gr.4_perc_chang_in) <- c("model", "subbasin", "val1", "ref",
                                       "val.2", "abs_nf_4.5", "val.3", "abs_ff_4.5", 
                                       "val.4", "abs_nf_8.5", "val.5" , "abs_ff_8.5")


bh_gull_gr.4_perc_chang_in$NF_4.5 <- (bh_gull_gr.4_perc_chang_in$abs_nf_4.5/bh_gull_gr.4_perc_chang_in$ref - 1)*100
bh_gull_gr.4_perc_chang_in$FF_4.5 <- (bh_gull_gr.4_perc_chang_in$abs_ff_4.5/bh_gull_gr.4_perc_chang_in$ref - 1)*100
bh_gull_gr.4_perc_chang_in$NF_8.5 <- (bh_gull_gr.4_perc_chang_in$abs_nf_8.5/bh_gull_gr.4_perc_chang_in$ref - 1)*100
bh_gull_gr.4_perc_chang_in$FF_8.5 <- (bh_gull_gr.4_perc_chang_in$abs_ff_8.5/bh_gull_gr.4_perc_chang_in$ref - 1)*100


bh_gull_gr.4_perc_chang <- pivot_longer(bh_gull_gr.4_perc_chang_in, cols=13:16, names_to = "scenario", values_to = "perc_change")
bh_gull_gr.4_perc_chang <- bh_gull_gr.4_perc_chang[-c(3:12)]

bh_gull_gr.4_perc_chang$scenario <- factor(bh_gull_gr.4_perc_chang$scenario , 
                                             levels=c("NF_4.5", "FF_4.5", "NF_8.5", "FF_8.5"))

bh_gull_gr.4_perc_graph <- ggplot(bh_gull_gr.4_perc_chang, aes(x=scenario, y=perc_change)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="% change in no. of days") +
  ggtitle("gr.4_P0.95")+
  theme(axis.title.x = element_blank(),
        panel.background = element_rect(fill = "white", colour = 'black'),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "lightgrey"),
        panel.grid.minor = element_line("NA"),
        plot.title = element_text(size = 20),
        text = element_text(size=18))+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  coord_cartesian( ylim = c(-70, 170))+
  scale_y_continuous(breaks = seq(from = -100, to = 200, by = 30))
  
plot(bh_gull_gr.4_perc_graph)

# calculate median from boxplots
bh_gull_gr.4_perc_chang_median <- aggregate(perc_change ~ scenario ,
                                            data = bh_gull_gr.4_perc_chang, 
                                            median)

library("ggpubr")
library("patchwork")
#model/subbasin/value 
# 162 values (9 models * 18 subbasins)

bh_gull_box_plot_perc <- ggarrange( plot_spacer(), bh_gull_gr.1_incub_perc_graph, 
                                bh_gull_gr.1_rear_perc_graph, bh_gull_gr.2_perc_graph, 
                                bh_gull_gr.3_perc_graph, bh_gull_gr.4_perc_graph,plot_spacer(),
                                
                                nrow = 7, ncol = 1)

plot(bh_gull_box_plot_perc)

ggsave(bh_gull_box_plot_perc , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/",
                     "bh_gull_box_plot_perc1.jpg"),device = "jpg", width = 3, height = 15)

# combine median values in table

bh_gull_box_plot_median <- combine(bh_gull_gr.1_incub_perc_chang_median,bh_gull_gr.1_rear_perc_chang_median,
                                  bh_gull_gr.2_perc_chang_median, bh_gull_gr.3_perc_chang_median,
                                  bh_gull_gr.4_perc_chang_median)

write.csv(bh_gull_box_plot_median, "D:/Ptaki_hydro/Obliczenia/R/Results/median_boxplot/bh_gull_box_plot_median.csv") 


############################################
#*******   Little Tern   ***********#
###########################################


#######%%%%%%%%%%
# Tern percentage change in values box_plot GR.1 Laying eggs

tern_gr.1_le_perc_chang_in <- tern_ref_le_gr.1_unlist %>% 
  right_join(tern_nf_4.5_le_gr.1_unlist, by=c("model","subbasin")) %>%
  right_join(tern_ff_4.5_le_gr.1_unlist, by=c("model","subbasin")) %>%
  right_join(tern_nf_8.5_le_gr.1_unlist, by=c("model","subbasin")) %>%
  right_join(tern_ff_8.5_le_gr.1_unlist, by=c("model","subbasin"))

#remove repeating columns
tern_gr.1_le_perc_chang_in <- tern_gr.1_le_perc_chang_in[-c(2,5,7,9,11)]

names(tern_gr.1_le_perc_chang_in) <- c("model", "subbasin", "ref",
                                         "abs_nf_4.5",  "abs_ff_4.5", 
                                         "abs_nf_8.5",  "abs_ff_8.5")


tern_gr.1_le_perc_chang_in$NF_4.5 <- (tern_gr.1_le_perc_chang_in$abs_nf_4.5/tern_gr.1_le_perc_chang_in$ref - 1)*100
tern_gr.1_le_perc_chang_in$FF_4.5 <- (tern_gr.1_le_perc_chang_in$abs_ff_4.5/tern_gr.1_le_perc_chang_in$ref - 1)*100
tern_gr.1_le_perc_chang_in$NF_8.5 <- (tern_gr.1_le_perc_chang_in$abs_nf_8.5/tern_gr.1_le_perc_chang_in$ref - 1)*100
tern_gr.1_le_perc_chang_in$FF_8.5 <- (tern_gr.1_le_perc_chang_in$abs_ff_8.5/tern_gr.1_le_perc_chang_in$ref - 1)*100


tern_gr.1_le_perc_chang <- pivot_longer(tern_gr.1_le_perc_chang_in, cols=8:11, names_to = "scenario", values_to = "perc_change")
tern_gr.1_le_perc_chang <- tern_gr.1_le_perc_chang[-c(3:7)]

# establish an order
tern_gr.1_le_perc_chang$scenario <- factor(tern_gr.1_le_perc_chang$scenario , 
                                             levels=c("NF_4.5", "FF_4.5", "NF_8.5", "FF_8.5"))


tern_gr.1_le_perc_graph <- ggplot(tern_gr.1_le_perc_chang, aes(x=scenario, y=perc_change)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="% change in Q m3/s") +
  ggtitle("gr.1_mean_LE")+
  theme(axis.title.x = element_blank(),
        panel.background = element_rect(fill = "white", colour = 'black'),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "lightgrey"),
        panel.grid.minor = element_line("NA"),
        plot.title = element_text(size = 20),
        text = element_text(size=18))+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  coord_cartesian( ylim = c(-30, 70))+
  scale_y_continuous(breaks = seq(from = -100, to = 200, by = 10))

plot(tern_gr.1_le_perc_graph)

# calculate median from boxplots
tern_gr.1_le_perc_chang_median <- aggregate( perc_change ~ scenario ,
                                               data = tern_gr.1_le_perc_chang, 
                                               median)

#######%%%%%%%%%%
# Tern percentage change in change in values box_plot GR.1 Incub

tern_gr.1_incub_perc_chang_in <- tern_ref_incub_gr.1_unlist %>% 
  right_join(tern_nf_4.5_incub_gr.1_unlist, by=c("model","subbasin")) %>%
  right_join(tern_ff_4.5_incub_gr.1_unlist, by=c("model","subbasin")) %>%
  right_join(tern_nf_8.5_incub_gr.1_unlist, by=c("model","subbasin")) %>%
  right_join(tern_ff_8.5_incub_gr.1_unlist, by=c("model","subbasin"))

#remove repeating columns
tern_gr.1_incub_perc_chang_in <- tern_gr.1_incub_perc_chang_in[-c(2,5,7,9,11)]

names(tern_gr.1_incub_perc_chang_in) <- c("model", "subbasin", "ref",
                                            "abs_nf_4.5",  "abs_ff_4.5", 
                                            "abs_nf_8.5",  "abs_ff_8.5")


tern_gr.1_incub_perc_chang_in$NF_4.5 <- (tern_gr.1_incub_perc_chang_in$abs_nf_4.5/tern_gr.1_incub_perc_chang_in$ref - 1)*100
tern_gr.1_incub_perc_chang_in$FF_4.5 <- (tern_gr.1_incub_perc_chang_in$abs_ff_4.5/tern_gr.1_incub_perc_chang_in$ref - 1)*100
tern_gr.1_incub_perc_chang_in$NF_8.5 <- (tern_gr.1_incub_perc_chang_in$abs_nf_8.5/tern_gr.1_incub_perc_chang_in$ref - 1)*100
tern_gr.1_incub_perc_chang_in$FF_8.5 <- (tern_gr.1_incub_perc_chang_in$abs_ff_8.5/tern_gr.1_incub_perc_chang_in$ref - 1)*100


tern_gr.1_incub_perc_chang <- pivot_longer(tern_gr.1_incub_perc_chang_in, cols=8:11, names_to = "scenario", values_to = "perc_change")
tern_gr.1_incub_perc_chang <- tern_gr.1_incub_perc_chang[-c(3:7)]

# establish an order
tern_gr.1_incub_perc_chang$scenario <- factor(tern_gr.1_incub_perc_chang$scenario , 
                                                levels=c("NF_4.5", "FF_4.5", "NF_8.5", "FF_8.5"))


tern_gr.1_incub_perc_graph <- ggplot(tern_gr.1_incub_perc_chang, aes(x=scenario, y=perc_change)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="% change in Q m3/s") +
  ggtitle("gr.1_mean_Incub")+
  theme(axis.title.x = element_blank(),
        panel.background = element_rect(fill = "white", colour = 'black'),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "lightgrey"),
        panel.grid.minor = element_line("NA"),
        plot.title = element_text(size = 20),
        text = element_text(size=18))+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  coord_cartesian( ylim = c(-30, 70))+
  scale_y_continuous(breaks = seq(from = -100, to = 200, by = 10))

plot(tern_gr.1_incub_perc_graph)

# calculate median from boxplots
tern_gr.1_incub_perc_chang_median <- aggregate( perc_change ~ scenario ,
                                             data = tern_gr.1_incub_perc_chang, 
                                             median)

#######%%%%%%%%%%
# Tern percentage change in values box_plot GR.1 Rear

tern_gr.1_rear_perc_chang_in <- tern_ref_rear_gr.1_unlist %>% 
  right_join(tern_nf_4.5_rear_gr.1_unlist, by=c("model","subbasin")) %>%
  right_join(tern_ff_4.5_rear_gr.1_unlist, by=c("model","subbasin")) %>%
  right_join(tern_nf_8.5_rear_gr.1_unlist, by=c("model","subbasin")) %>%
  right_join(tern_ff_8.5_rear_gr.1_unlist, by=c("model","subbasin"))

#remove repeating columns
tern_gr.1_rear_perc_chang_in <- tern_gr.1_rear_perc_chang_in[-c(2,5,7,9,11)]

names(tern_gr.1_rear_perc_chang_in) <- c("model", "subbasin", "ref",
                                           "abs_nf_4.5",  "abs_ff_4.5", 
                                           "abs_nf_8.5",  "abs_ff_8.5")


tern_gr.1_rear_perc_chang_in$NF_4.5 <- (tern_gr.1_rear_perc_chang_in$abs_nf_4.5/tern_gr.1_rear_perc_chang_in$ref - 1)*100
tern_gr.1_rear_perc_chang_in$FF_4.5 <- (tern_gr.1_rear_perc_chang_in$abs_ff_4.5/tern_gr.1_rear_perc_chang_in$ref - 1)*100
tern_gr.1_rear_perc_chang_in$NF_8.5 <- (tern_gr.1_rear_perc_chang_in$abs_nf_8.5/tern_gr.1_rear_perc_chang_in$ref - 1)*100
tern_gr.1_rear_perc_chang_in$FF_8.5 <- (tern_gr.1_rear_perc_chang_in$abs_ff_8.5/tern_gr.1_rear_perc_chang_in$ref - 1)*100


tern_gr.1_rear_perc_chang <- pivot_longer(tern_gr.1_rear_perc_chang_in, cols=8:11, names_to = "scenario", values_to = "perc_change")
tern_gr.1_rear_perc_chang <- tern_gr.1_rear_perc_chang[-c(3:7)]

# establish an order
tern_gr.1_rear_perc_chang$scenario <- factor(tern_gr.1_rear_perc_chang$scenario , 
                                               levels=c("NF_4.5", "FF_4.5", "NF_8.5", "FF_8.5"))


tern_gr.1_rear_perc_graph <- ggplot(tern_gr.1_rear_perc_chang, aes(x=scenario, y=perc_change)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="% change in Q m3/s") +
  ggtitle("gr.1_mean_RC")+
  theme(axis.title.x = element_blank(),
        panel.background = element_rect(fill = "white", colour = 'black'),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "lightgrey"),
        panel.grid.minor = element_line("NA"),
        plot.title = element_text(size = 20),
        text = element_text(size=18))+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  coord_cartesian( ylim = c(-30, 70))+
  scale_y_continuous(breaks = seq(from = -100, to = 200, by = 10))

plot(tern_gr.1_rear_perc_graph)

# calculate median from boxplots
tern_gr.1_rear_perc_chang_median <- aggregate( perc_change ~ scenario ,
                                             data = tern_gr.1_rear_perc_chang, 
                                             median)

#######%%%%%%%%%%
# Tern percentage change in values box_plot GR.2

tern_ref_gr.2_unlist_p22 <- tern_ref_gr.2_unlist_p2[-c(3:5,7:9)]
tern_nf_4.5_gr.2_unlist_p22 <- tern_nf_4.5_gr.2_unlist_p2[-c(3:5,7:9)]
tern_ff_4.5_gr.2_unlist_p22 <- tern_ff_4.5_gr.2_unlist_p2[-c(3:5,7:9)]
tern_nf_8.5_gr.2_unlist_p22 <- tern_nf_8.5_gr.2_unlist_p2[-c(3:5,7:9)]
tern_ff_8.5_gr.2_unlist_p22 <- tern_ff_8.5_gr.2_unlist_p2[-c(3:5,7:9)]

tern_gr.2_perc_chang_in <- tern_ref_gr.2_unlist_p22 %>% 
  right_join(tern_nf_4.5_gr.2_unlist_p22, by=c("model","subbasin")) %>%
  right_join(tern_ff_4.5_gr.2_unlist_p22, by=c("model","subbasin")) %>%
  right_join(tern_nf_8.5_gr.2_unlist_p22, by=c("model","subbasin")) %>%
  right_join(tern_ff_8.5_gr.2_unlist_p22, by=c("model","subbasin"))

names(tern_gr.2_perc_chang_in) <- c("model", "subbasin", "ref",
                                      "abs_nf_4.5", "abs_ff_4.5", 
                                      "abs_nf_8.5", "abs_ff_8.5")


tern_gr.2_perc_chang_in$NF_4.5 <- (tern_gr.2_perc_chang_in$abs_nf_4.5/tern_gr.2_perc_chang_in$ref - 1)*100
tern_gr.2_perc_chang_in$FF_4.5 <- (tern_gr.2_perc_chang_in$abs_ff_4.5/tern_gr.2_perc_chang_in$ref - 1)*100
tern_gr.2_perc_chang_in$NF_8.5 <- (tern_gr.2_perc_chang_in$abs_nf_8.5/tern_gr.2_perc_chang_in$ref - 1)*100
tern_gr.2_perc_chang_in$FF_8.5 <- (tern_gr.2_perc_chang_in$abs_ff_8.5/tern_gr.2_perc_chang_in$ref - 1)*100


tern_gr.2_perc_chang <- pivot_longer(tern_gr.2_perc_chang_in, cols=8:11, names_to = "scenario", values_to = "perc_change")
tern_gr.2_perc_chang <- tern_gr.2_perc_chang[-c(3:7)]

tern_gr.2_perc_chang$scenario <- factor(tern_gr.2_perc_chang$scenario , 
                                          levels=c("NF_4.5", "FF_4.5", "NF_8.5", "FF_8.5"))

tern_gr.2_perc_graph <- ggplot(tern_gr.2_perc_chang, aes(x=scenario, y=perc_change)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="% change in Q m3/s") +
  ggtitle("gr.2_day03_max") +
  theme(axis.title.x = element_blank(),
        panel.background = element_rect(fill = "white", colour = 'black'),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "lightgrey"),
        panel.grid.minor = element_line("NA"),
        plot.title = element_text(size = 20),
        text = element_text(size=18))+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  coord_cartesian( ylim = c(-30, 70))+
  scale_y_continuous(breaks = seq(from = -100, to = 200, by = 10))

plot(tern_gr.2_perc_graph)

# calculate median from boxplots
tern_gr.2_perc_chang_median <- aggregate( perc_change ~ scenario ,
                                             data = tern_gr.2_perc_chang, 
                                             median)

#######%%%%%%%%%%
# Tern percentage change in values box_plot GR.4

tern_gr.4_perc_chang_in <- tern_ref_gr.4_p2 %>% right_join(tern_nf_4.5_gr.4_p2, by=c("Group.1","Group.2")) %>%
  right_join(tern_ff_4.5_gr.4_p2, by=c("Group.1","Group.2")) %>%
  right_join(tern_nf_8.5_gr.4_p2, by=c("Group.1","Group.2")) %>%
  right_join(tern_ff_8.5_gr.4_p2, by=c("Group.1","Group.2"))

names(tern_gr.4_perc_chang_in) <- c("model", "subbasin", "val1", "ref",
                                      "val.2", "abs_nf_4.5", "val.3", "abs_ff_4.5", 
                                      "val.4", "abs_nf_8.5", "val.5" , "abs_ff_8.5")


tern_gr.4_perc_chang_in$NF_4.5 <- (tern_gr.4_perc_chang_in$abs_nf_4.5/tern_gr.4_perc_chang_in$ref - 1)*100
tern_gr.4_perc_chang_in$FF_4.5 <- (tern_gr.4_perc_chang_in$abs_ff_4.5/tern_gr.4_perc_chang_in$ref - 1)*100
tern_gr.4_perc_chang_in$NF_8.5 <- (tern_gr.4_perc_chang_in$abs_nf_8.5/tern_gr.4_perc_chang_in$ref - 1)*100
tern_gr.4_perc_chang_in$FF_8.5 <- (tern_gr.4_perc_chang_in$abs_ff_8.5/tern_gr.4_perc_chang_in$ref - 1)*100


tern_gr.4_perc_chang <- pivot_longer(tern_gr.4_perc_chang_in, cols=13:16, names_to = "scenario", values_to = "perc_change")
tern_gr.4_perc_chang <- tern_gr.4_perc_chang[-c(3:12)]

tern_gr.4_perc_chang$scenario <- factor(tern_gr.4_perc_chang$scenario , 
                                          levels=c("NF_4.5", "FF_4.5", "NF_8.5", "FF_8.5"))

tern_gr.4_perc_graph <- ggplot(tern_gr.4_perc_chang, aes(x=scenario, y=perc_change)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="% change in no. of days") +
  ggtitle("gr.4_P0.75")+
  theme(axis.title.x = element_blank(),
        panel.background = element_rect(fill = "white", colour = 'black'),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "lightgrey"),
        panel.grid.minor = element_line("NA"),
        plot.title = element_text(size = 20),
        text = element_text(size=18))+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  coord_cartesian( ylim = c(-40, 100))+
  scale_y_continuous(breaks = seq(from = -100, to = 200, by = 20))

plot(tern_gr.4_perc_graph)

# calculate median from boxplots
tern_gr.4_perc_chang_median <- aggregate( perc_change ~ scenario ,
                                          data = tern_gr.4_perc_chang, 
                                          median)

# join all the graphs together 

library("ggpubr")
#model/subbasin/value 
# 162 values (9 models * 18 subbasins)

tern_box_plot_perc <- ggarrange(tern_gr.1_le_perc_graph, tern_gr.1_incub_perc_graph, 
                                    tern_gr.1_rear_perc_graph, tern_gr.2_perc_graph, 
                                    plot_spacer(), plot_spacer(), tern_gr.4_perc_graph,
                                    
                                    nrow = 7, ncol = 1)

plot(tern_box_plot_perc)

ggsave(tern_box_plot_perc , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/",
                     "tern_box_plot_perc.jpg"),device = "jpg", width = 3, height = 15)

# combine median values in table

tern_box_plot_median <- combine(tern_gr.1_le_perc_chang_median, tern_gr.1_incub_perc_chang_median,
                                tern_gr.1_rear_perc_chang_median,
                                tern_gr.2_perc_chang_median, tern_gr.4_perc_chang_median)

write.csv(tern_box_plot_median, "D:/Ptaki_hydro/Obliczenia/R/Results/median_boxplot/tern_box_plot_median.csv") 


######## ALL 3 BIRDS ########
#model/subbasin/value 
# 162 values (9 models * 18 subbasins)

#horizontal
all_box_plot_perc <- ggarrange(m_gull_gr.1_le_perc_graph, m_gull_gr.1_incub_perc_graph, 
                                   m_gull_gr.1_rear_perc_graph, m_gull_gr.2_perc_graph,  
                                   m_gull_gr.4_perc_graph,
                                   bh_gull_gr.1_incub_perc_graph, bh_gull_gr.1_rear_perc_graph,
                                   bh_gull_gr.3_perc_graph, bh_gull_gr.2_perc_graph, 
                                   bh_gull_gr.4_perc_graph,
                                   tern_gr.1_le_perc_graph, tern_gr.1_incub_perc_graph, 
                                   tern_gr.1_rear_perc_graph, tern_gr.2_perc_graph,  
                                   tern_gr.4_perc_graph,
                                   
                                   nrow = 3, ncol = 5)

plot(all_box_plot_perc)

ggsave(all_box_plot_perc , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/",
                     "all_box_plot_perc.jpg"),device = "jpg", width = 20, height = 10)

#vertical
all_box_plot_perc1 <- ggarrange(m_gull_box_plot_perc,
                                bh_gull_box_plot_perc,
                                tern_box_plot_perc,
                                nrow = 1, ncol = 3)
plot(all_box_plot_perc1)

ggsave(all_box_plot_perc1 , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/",
                     "all_box_plot_perc1.jpg"),device = "jpg", width = 10, height = 20)

#vertical2
all_box_plot_perc2 <- ggarrange(m_gull_box_plot_perc,
                                bh_gull_box_plot_perc,
                                tern_box_plot_perc,
                                nrow = 1, ncol = 3)
plot(all_box_plot_perc2)

ggsave(all_box_plot_perc2 , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/",
                     "all_box_plot_perc2.jpg"),device = "jpg", width = 14, height = 20)


# with empty spaces, iha lining up
library("patchwork")

all_box_plot_perc2 <- (m_gull_gr.1_le_perc_graph | m_gull_gr.1_incub_perc_graph | m_gull_gr.1_rear_perc_graph | m_gull_gr.2_perc_graph | plot_spacer() | plot_spacer()| m_gull_gr.4_perc_graph) /
  ( plot_spacer() | bh_gull_gr.1_incub_perc_graph | bh_gull_gr.1_rear_perc_graph | bh_gull_gr.2_perc_graph | bh_gull_gr.3_perc_graph | bh_gull_gr.4_perc_graph | plot_spacer()) /
  ( tern_gr.1_le_perc_graph | tern_gr.1_incub_perc_graph | tern_gr.1_rear_perc_graph | tern_gr.2_perc_graph | plot_spacer() | plot_spacer() | tern_gr.4_perc_graph)


plot(all_box_plot_perc2)

ggsave(all_box_plot_perc2 , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/",
                     "all_box_plot_perc2.jpg"),device = "jpg", width = 30, height = 10)


####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#######
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#######
#### GRAPHS FOR SUBBASINS #####
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#######
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#######
##### TERN
tern_gr.1_le_perc_graph_sub <- ggplot(tern_gr.1_le_perc_chang, 
                                      aes(y=perc_change, group=subbasin)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="% change in mean streamflow m3/s") +
  ggtitle("tern Gr.1 le")+
  theme(axis.title.x = element_blank())+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  scale_y_continuous(breaks = seq(from = -100, to = 200, by = 10))+
  facet_grid(scenario ~ subbasin)

plot(tern_gr.1_le_perc_graph_sub)

ggsave(tern_gr.1_le_perc_graph_sub , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/subbasin/",
                     "tern_gr.1_le_perc_graph_sub.jpg"),device = "jpg", width = 20, height = 10)


tern_gr.1_incub_perc_graph_sub <- ggplot(tern_gr.1_incub_perc_chang, aes(y=perc_change, group=subbasin)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="% change in mean streamflow m3/s") +
  ggtitle("tern Gr.1 Incub")+
  theme(axis.title.x = element_blank())+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  scale_y_continuous(breaks = seq(from = -100, to = 200, by = 10))+
  facet_grid(scenario ~ subbasin)

ggsave(tern_gr.1_incub_perc_graph_sub , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/subbasin/",
                     "tern_gr.1_incub_perc_graph_sub.jpg"),device = "jpg", width = 20, height = 10)

tern_gr.1_rear_perc_graph_sub <- ggplot(tern_gr.1_rear_perc_chang, aes(y=perc_change, group=subbasin)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="% change in mean streamflow m3/s") +
  ggtitle("tern Gr.1 Incub")+
  theme(axis.title.x = element_blank())+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  scale_y_continuous(breaks = seq(from = -100, to = 200, by = 10))+
  facet_grid(scenario ~ subbasin)

ggsave(tern_gr.1_rear_perc_graph_sub , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/subbasin/",
                     "tern_gr.1_rear_perc_graph_sub.jpg"),device = "jpg", width = 20, height = 10)


tern_gr.2_perc_graph_sub <- ggplot(tern_gr.2_perc_chang, aes(y=perc_change, group=subbasin)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="% change in mean streamflow m3/s") +
  ggtitle("tern Gr.2 day_03max") +
  theme(axis.title.x = element_blank())+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  scale_y_continuous(breaks = seq(from = -100, to = 200, by = 10))+
  facet_grid(scenario ~ subbasin)

ggsave(tern_gr.2_perc_graph_sub , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/subbasin/",
                     "tern_gr.2_perc_graph_sub.jpg"),device = "jpg", width = 20, height = 10)


tern_gr.4_perc_graph_sub <- ggplot(tern_gr.4_perc_chang, aes(y=perc_change, group=subbasin)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="% change in no. of days above 0.75 percentile") +
  ggtitle("% tern Gr.4 >0.75")+
  theme(axis.title.x = element_blank())+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  scale_y_continuous(breaks = seq(from = -100, to = 200, by = 10))+
  facet_grid(scenario ~ subbasin)

ggsave(tern_gr.4_perc_graph_sub , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/subbasin/",
                     "tern_gr.4_perc_graph_sub.jpg"),device = "jpg", width = 20, height = 10)

# all together
all_box_plot_tern_perc_graph_sub <- ggarrange(tern_gr.1_le_perc_graph_sub, tern_gr.1_incub_perc_graph_sub,
                               tern_gr.1_rear_perc_graph_sub, tern_gr.2_perc_graph_sub,
                               tern_gr.4_perc_graph_sub,
                               
                               nrow = 5, ncol = 1)

plot(all_box_plot_tern_perc_graph_sub)

ggsave(all_box_plot_tern_perc_graph_sub , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/subbasin/",
                     "all_box_plot_tern_perc_graph_sub.jpg"),device = "jpg", width = 20, height = 30)


##### Mew Gull
m_gull_gr.1_le_perc_graph_sub <- ggplot(m_gull_gr.1_le_perc_chang, 
                                      aes(y=perc_change, group=subbasin)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="% change in mean streamflow m3/s") +
  ggtitle("m_gull Gr.1 le")+
  theme(axis.title.x = element_blank())+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  scale_y_continuous(breaks = seq(from = -100, to = 200, by = 10))+
  facet_grid(scenario ~ subbasin)

plot(m_gull_gr.1_le_perc_graph_sub)

ggsave(m_gull_gr.1_le_perc_graph_sub , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/subbasin/",
                     "m_gull_gr.1_le_perc_graph_sub.jpg"),device = "jpg", width = 20, height = 10)


m_gull_gr.1_incub_perc_graph_sub <- ggplot(m_gull_gr.1_incub_perc_chang, aes(y=perc_change, group=subbasin)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="% change in mean streamflow m3/s") +
  ggtitle("m_gull Gr.1 Incub")+
  theme(axis.title.x = element_blank())+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  scale_y_continuous(breaks = seq(from = -100, to = 200, by = 10))+
  facet_grid(scenario ~ subbasin)

ggsave(m_gull_gr.1_incub_perc_graph_sub , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/subbasin/",
                     "m_gull_gr.1_incub_perc_graph_sub.jpg"),device = "jpg", width = 20, height = 10)

m_gull_gr.1_rear_perc_graph_sub <- ggplot(m_gull_gr.1_rear_perc_chang, aes(y=perc_change, group=subbasin)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="% change in mean streamflow m3/s") +
  ggtitle("m_gull Gr.1 Incub")+
  theme(axis.title.x = element_blank())+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  scale_y_continuous(breaks = seq(from = -100, to = 200, by = 10))+
  facet_grid(scenario ~ subbasin)

ggsave(m_gull_gr.1_rear_perc_graph_sub , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/subbasin/",
                     "m_gull_gr.1_rear_perc_graph_sub.jpg"),device = "jpg", width = 20, height = 10)


m_gull_gr.2_perc_graph_sub <- ggplot(m_gull_gr.2_perc_chang, aes(y=perc_change, group=subbasin)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="% change in mean streamflow m3/s") +
  ggtitle("m_gull Gr.2 day_03max") +
  theme(axis.title.x = element_blank())+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  scale_y_continuous(breaks = seq(from = -100, to = 200, by = 10))+
  facet_grid(scenario ~ subbasin)

ggsave(m_gull_gr.2_perc_graph_sub , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/subbasin/",
                     "m_gull_gr.2_perc_graph_sub.jpg"),device = "jpg", width = 20, height = 10)


m_gull_gr.4_perc_graph_sub <- ggplot(m_gull_gr.4_perc_chang, aes(y=perc_change, group=subbasin)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="% change in no. of days above 0.75 percentile") +
  ggtitle("% m_gull Gr.4 >0.75")+
  theme(axis.title.x = element_blank())+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  scale_y_continuous(breaks = seq(from = -100, to = 200, by = 10))+
  facet_grid(scenario ~ subbasin)

ggsave(m_gull_gr.4_perc_graph_sub , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/subbasin/",
                     "m_gull_gr.4_perc_graph_sub.jpg"),device = "jpg", width = 20, height = 10)

# all together
all_box_plot_m_gull_perc_graph_sub <- ggarrange(m_gull_gr.1_le_perc_graph_sub, m_gull_gr.1_incub_perc_graph_sub,
                                              m_gull_gr.1_rear_perc_graph_sub, m_gull_gr.2_perc_graph_sub,
                                              m_gull_gr.4_perc_graph_sub,
                                              
                                              nrow = 5, ncol = 1)

plot(all_box_plot_m_gull_perc_graph_sub)

ggsave(all_box_plot_m_gull_perc_graph_sub , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/subbasin/",
                     "all_box_plot_m_gull_perc_graph_sub.jpg"),device = "jpg", width = 20, height = 30)


##### Black Headed Gull

bh_gull_gr.1_incub_perc_graph_sub <- ggplot(bh_gull_gr.1_incub_perc_chang, aes(y=perc_change, group=subbasin)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="% change in mean streamflow m3/s") +
  ggtitle("bh_gull Gr.1 Incub")+
  theme(axis.title.x = element_blank())+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  scale_y_continuous(breaks = seq(from = -100, to = 200, by = 10))+
  facet_grid(scenario ~ subbasin)

ggsave(bh_gull_gr.1_incub_perc_graph_sub , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/subbasin/",
                     "bh_gull_gr.1_incub_perc_graph_sub.jpg"),device = "jpg", width = 20, height = 10)

bh_gull_gr.1_rear_perc_graph_sub <- ggplot(bh_gull_gr.1_rear_perc_chang, aes(y=perc_change, group=subbasin)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="% change in mean streamflow m3/s") +
  ggtitle("bh_gull Gr.1 Incub")+
  theme(axis.title.x = element_blank())+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  scale_y_continuous(breaks = seq(from = -100, to = 200, by = 10))+
  facet_grid(scenario ~ subbasin)

ggsave(bh_gull_gr.1_rear_perc_graph_sub , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/subbasin/",
                     "bh_gull_gr.1_rear_perc_graph_sub.jpg"),device = "jpg", width = 20, height = 10)


bh_gull_gr.2_perc_graph_sub <- ggplot(bh_gull_gr.2_perc_chang, aes(y=perc_change, group=subbasin)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="% change in mean streamflow m3/s") +
  ggtitle("bh_gull Gr.2 day_03max") +
  theme(axis.title.x = element_blank())+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  scale_y_continuous(breaks = seq(from = -100, to = 200, by = 10))+
  facet_grid(scenario ~ subbasin)

ggsave(bh_gull_gr.2_perc_graph_sub , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/subbasin/",
                     "bh_gull_gr.2_perc_graph_sub.jpg"),device = "jpg", width = 20, height = 10)


bh_gull_gr.3_perc_graph_sub <- ggplot(bh_gull_gr.3_perc_chang, aes(y=perc_change, group=subbasin)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="Max. falling within VP") +
  ggtitle("bh_gull Gr.3 day_03max") +
  theme(axis.title.x = element_blank())+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  scale_y_continuous(breaks = seq(from = -100, to = 200, by = 10))+
  facet_grid(scenario ~ subbasin)

ggsave(bh_gull_gr.3_perc_graph_sub , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/subbasin/",
                     "bh_gull_gr.3_perc_graph_sub.jpg"),device = "jpg", width = 20, height = 10)


bh_gull_gr.4_perc_graph_sub <- ggplot(bh_gull_gr.4_perc_chang, aes(y=perc_change, group=subbasin)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="% change in no. of days above 0.95 percentile") +
  ggtitle("% bh_gull Gr.4 >0.95")+
  theme(axis.title.x = element_blank())+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  scale_y_continuous(breaks = seq(from = -100, to = 200, by = 10))+
  facet_grid(scenario ~ subbasin)

ggsave(bh_gull_gr.4_perc_graph_sub , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/subbasin/",
                     "bh_gull_gr.4_perc_graph_sub.jpg"),device = "jpg", width = 20, height = 10)

# all together
all_box_plot_bh_gull_perc_graph_sub <- ggarrange(bh_gull_gr.1_incub_perc_graph_sub,
                                                bh_gull_gr.1_rear_perc_graph_sub, 
                                                bh_gull_gr.2_perc_graph_sub, bh_gull_gr.3_perc_graph_sub,
                                                bh_gull_gr.4_perc_graph_sub,
                                                
                                                nrow = 5, ncol = 1)

plot(all_box_plot_bh_gull_perc_graph_sub)

ggsave(all_box_plot_bh_gull_perc_graph_sub , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/subbasin/",
                     "all_box_plot_bh_gull_perc_graph_sub.jpg"),device = "jpg", width = 20, height = 30)
######
######
######
###### 3 selected subbasins

# 910 (location 22, down the river), 
# 1358 (location 9, middle of the river),
# 1875 (location 1, up the river)
######
######
######


##### Black Headed Gull

#^^^^^^^^^^ highest correlating IHA
bh_gull_gr.1_incub_perc_graph_sub <- ggplot(data=subset(bh_gull_gr.1_incub_perc_chang, 
                                            subbasin=="1875" | subbasin=="1358"|subbasin=="910"),
                                            aes(y=perc_change, group= subbasin)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="% change in Q m3/s") +
  ggtitle("Black-headed gull gr.1_mean_Incub")+
  theme(axis.title.x = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  scale_y_continuous(breaks = seq(from = -30, to = 100, by = 20), limits = c(-30,100))+
  #ylim(-30, 100)+
  facet_grid(scenario ~ dplyr::desc(subbasin))

plot(bh_gull_gr.1_incub_perc_graph_sub)

ggsave(bh_gull_gr.1_incub_perc_graph_sub , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/subbasin/",
                     "bh_gull_3_gr.1_incub_perc_graph_sub.jpg"),device = "jpg", width = 5, height = 10)

bh_gull_gr.1_rear_perc_graph_sub <- ggplot(data=subset(bh_gull_gr.1_rear_perc_chang, 
                                           subbasin=="910" | subbasin=="1358"|subbasin=="1875"),
                                            aes(y=perc_change, group=subbasin)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="% change in mean streamflow m3/s") +
  ggtitle("bh_gull Gr.1 rear")+
  theme(axis.title.x = element_blank())+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  scale_y_continuous(breaks = seq(from = -100, to = 200, by = 10))+
  facet_grid(scenario ~ dplyr::desc(subbasin))

ggsave(bh_gull_gr.1_rear_perc_graph_sub , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/subbasin/",
                     "bh_gull_3_gr.1_rear_perc_graph_sub.jpg"),device = "jpg", width = 5, height = 10)


bh_gull_gr.2_perc_graph_sub <- ggplot(data=subset(bh_gull_gr.2_perc_chang, 
                                      subbasin=="910" | subbasin=="1358"|subbasin=="1875"),
                                      aes(y=perc_change, group=subbasin)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="% change in mean streamflow m3/s") +
  ggtitle("bh_gull Gr.2 day_03max") +
  theme(axis.title.x = element_blank())+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  scale_y_continuous(breaks = seq(from = -100, to = 200, by = 10))+
  facet_grid(scenario ~ dplyr::desc(subbasin))

ggsave(bh_gull_gr.2_perc_graph_sub , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/subbasin/",
                     "bh_gull_3_gr.2_perc_graph_sub.jpg"),device = "jpg", width = 5, height = 10)


bh_gull_gr.3_perc_graph_sub <- ggplot(data=subset(bh_gull_gr.3_perc_chang, 
                                      subbasin=="910" | subbasin=="1358"|subbasin=="1875"),
                                      aes(y=perc_change, group=subbasin)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="Max. falling within VP") +
  ggtitle("bh_gull Gr.3 ") +
  theme(axis.title.x = element_blank())+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  scale_y_continuous(breaks = seq(from = -100, to = 200, by = 10))+
  facet_grid(scenario ~ dplyr::desc(subbasin))

ggsave(bh_gull_gr.3_perc_graph_sub , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/subbasin/",
                     "bh_gull_3_gr.3_perc_graph_sub.jpg"),device = "jpg", width = 5, height = 10)


bh_gull_gr.4_perc_graph_sub <- ggplot(dat=subset(bh_gull_gr.4_perc_chang, 
                                      subbasin=="910" | subbasin=="1358"|subbasin=="1875"),
                                      aes(y=perc_change, group=subbasin)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="% change in no. of days above 0.95 percentile") +
  ggtitle("% bh_gull Gr.4 >0.95")+
  theme(axis.title.x = element_blank())+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  scale_y_continuous(breaks = seq(from = -100, to = 200, by = 10))+
  facet_grid(scenario ~ dplyr::desc(subbasin))

ggsave(bh_gull_gr.4_perc_graph_sub , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/subbasin/",
                     "bh_gull_3_gr.4_perc_graph_sub.jpg"),device = "jpg", width = 5, height = 10)

# all together
all_box_plot_bh_gull_perc_graph_sub <- ggarrange(bh_gull_gr.1_incub_perc_graph_sub,
                                                 bh_gull_gr.1_rear_perc_graph_sub, 
                                                 bh_gull_gr.2_perc_graph_sub, bh_gull_gr.3_perc_graph_sub,
                                                 bh_gull_gr.4_perc_graph_sub,
                                                 
                                                 nrow = 5, ncol = 1)

plot(all_box_plot_bh_gull_perc_graph_sub)

ggsave(all_box_plot_bh_gull_perc_graph_sub , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/subbasin/",
                     "all_box_plot_bh_gull_3_perc_graph_sub.jpg"),device = "jpg", width = 3, height = 20)


##### Mew Gull
m_gull_gr.1_le_perc_graph_sub <- ggplot(data=subset(m_gull_gr.1_le_perc_chang, 
                                               subbasin=="910" | subbasin=="1358"|subbasin=="1875"),
                                        aes(y=perc_change, group=subbasin)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="% change in mean streamflow m3/s") +
  ggtitle("m_gull Gr.1 le")+
  theme(axis.title.x = element_blank())+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  scale_y_continuous(breaks = seq(from = -100, to = 200, by = 10))+
  facet_grid(scenario ~ subbasin)

plot(m_gull_gr.1_le_perc_graph_sub)

ggsave(m_gull_gr.1_le_perc_graph_sub , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/subbasin/",
                     "m_gull_3_gr.1_le_perc_graph_sub.jpg"),device = "jpg", width = 5, height = 10)


m_gull_gr.1_incub_perc_graph_sub <- ggplot(data=subset(m_gull_gr.1_incub_perc_chang, 
                                          subbasin=="910" | subbasin=="1358"|subbasin=="1875"),
                                           aes(y=perc_change, group=subbasin)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="% change in mean streamflow m3/s") +
  ggtitle("m_gull Gr.1 Incub")+
  theme(axis.title.x = element_blank())+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  scale_y_continuous(breaks = seq(from = -100, to = 200, by = 10))+
  facet_grid(scenario ~ subbasin)

ggsave(m_gull_gr.1_incub_perc_graph_sub , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/subbasin/",
                     "m_gull_3_gr.1_incub_perc_graph_sub.jpg"),device = "jpg", width = 5, height = 10)

m_gull_gr.1_rear_perc_graph_sub <- ggplot(data=subset(m_gull_gr.1_rear_perc_chang, 
                                          subbasin=="910" | subbasin=="1358"|subbasin=="1875"),
                                          aes(y=perc_change, group=subbasin)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="% change in mean streamflow m3/s") +
  ggtitle("m_gull Gr.1 Incub")+
  theme(axis.title.x = element_blank())+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  scale_y_continuous(breaks = seq(from = -100, to = 200, by = 10))+
  facet_grid(scenario ~ subbasin)

ggsave(m_gull_gr.1_rear_perc_graph_sub , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/subbasin/",
                     "m_gull_3_gr.1_rear_perc_graph_sub.jpg"),device = "jpg", width = 5, height = 10)

#^^^^^^^^^^ highest correlating IHA
m_gull_gr.2_perc_graph_sub <- ggplot(data=subset(m_gull_gr.2_perc_chang, 
                                                 subbasin=="910" | subbasin=="1358"|subbasin=="1875"),
                                     aes(y=perc_change, group=subbasin)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="% change in Q m3/s") +
  ggtitle("Mew gull gr.2_day03_max") +
  theme(axis.title.x = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  scale_y_continuous(breaks = seq(from = -30, to = 100, by = 20), limits = c(-30,100))+
  facet_grid(scenario ~ dplyr::desc(subbasin))

ggsave(m_gull_gr.2_perc_graph_sub , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/subbasin/",
                     "m_gull_3_gr.2_perc_graph_sub.jpg"),device = "jpg", width = 5, height = 10)


m_gull_gr.4_perc_graph_sub <- ggplot(data=subset(m_gull_gr.4_perc_chang, 
                                                 subbasin=="910" | subbasin=="1358"|subbasin=="1875"),
                                     aes(y=perc_change, group=subbasin)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="% change in no. of days above 0.75 percentile") +
  ggtitle("% m_gull Gr.4 >0.75")+
  theme(axis.title.x = element_blank())+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  scale_y_continuous(breaks = seq(from = -100, to = 200, by = 10))+
  facet_grid(scenario ~ subbasin)

ggsave(m_gull_gr.4_perc_graph_sub , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/subbasin/",
                     "m_gull_3_gr.4_perc_graph_sub.jpg"),device = "jpg", width = 5, height = 10)

# all together
all_box_plot_m_gull_perc_graph_sub <- ggarrange(m_gull_gr.1_le_perc_graph_sub, m_gull_gr.1_incub_perc_graph_sub,
                                                m_gull_gr.1_rear_perc_graph_sub, m_gull_gr.2_perc_graph_sub,
                                                m_gull_gr.4_perc_graph_sub,
                                                
                                                nrow = 5, ncol = 1)

plot(all_box_plot_m_gull_perc_graph_sub)

ggsave(all_box_plot_m_gull_perc_graph_sub , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/subbasin/",
                     "all_box_plot_m_gull_3_perc_graph_sub.jpg"),device = "jpg", width = 3, height = 20)


##### Tern
tern_gr.1_le_perc_graph_sub <- ggplot(data=subset(tern_gr.1_le_perc_chang, 
                                                    subbasin=="910" | subbasin=="1358"|subbasin=="1875"),
                                        aes(y=perc_change, group=subbasin)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="% change in mean streamflow m3/s") +
  ggtitle("tern Gr.1 le")+
  theme(axis.title.x = element_blank())+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  scale_y_continuous(breaks = seq(from = -100, to = 200, by = 10))+
  facet_grid(scenario ~ subbasin)

plot(tern_gr.1_le_perc_graph_sub)

ggsave(tern_gr.1_le_perc_graph_sub , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/subbasin/",
                     "tern_3_gr.1_le_perc_graph_sub.jpg"),device = "jpg", width = 5, height = 10)


tern_gr.1_incub_perc_graph_sub <- ggplot(data=subset(tern_gr.1_incub_perc_chang, 
                                                       subbasin=="910" | subbasin=="1358"|subbasin=="1875"),
                                           aes(y=perc_change, group=subbasin)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="% change in mean streamflow m3/s") +
  ggtitle("tern Gr.1 Incub")+
  theme(axis.title.x = element_blank())+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  scale_y_continuous(breaks = seq(from = -100, to = 200, by = 10))+
  facet_grid(scenario ~ subbasin)

ggsave(tern_gr.1_incub_perc_graph_sub , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/subbasin/",
                     "tern_3_gr.1_incub_perc_graph_sub.jpg"),device = "jpg", width = 5, height = 10)

tern_gr.1_rear_perc_graph_sub <- ggplot(data=subset(tern_gr.1_rear_perc_chang, 
                                                      subbasin=="910" | subbasin=="1358"|subbasin=="1875"),
                                          aes(y=perc_change, group=subbasin)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="% change in mean streamflow m3/s") +
  ggtitle("tern Gr.1 Incub")+
  theme(axis.title.x = element_blank())+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  scale_y_continuous(breaks = seq(from = -100, to = 200, by = 10))+
  facet_grid(scenario ~ subbasin)

ggsave(tern_gr.1_rear_perc_graph_sub , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/subbasin/",
                     "tern_3_gr.1_rear_perc_graph_sub.jpg"),device = "jpg", width = 5, height = 10)


tern_gr.2_perc_graph_sub <- ggplot(data=subset(tern_gr.2_perc_chang, 
                                                 subbasin=="910" | subbasin=="1358"|subbasin=="1875"),
                                     aes(y=perc_change, group=subbasin)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="% change in mean streamflow m3/s") +
  ggtitle("tern Gr.2 day_03max") +
  theme(axis.title.x = element_blank())+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  scale_y_continuous(breaks = seq(from = -100, to = 200, by = 10))+
  facet_grid(scenario ~ subbasin)

ggsave(tern_gr.2_perc_graph_sub , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/subbasin/",
                     "tern_3_gr.2_perc_graph_sub.jpg"),device = "jpg", width = 5, height = 10)

#^^^^^^^^^^ highest correlating IHA
tern_gr.4_perc_graph_sub <- ggplot(data=subset(tern_gr.4_perc_chang, 
                                                 subbasin=="910" | subbasin=="1358"|subbasin=="1875"),
                                     aes(y=perc_change, group=subbasin)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="% change in no. of days") +
  ggtitle("Little tern gr.4_P0.75")+
  theme(axis.title.x = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  scale_y_continuous(breaks = seq(from = -30, to = 100, by = 20), limits = c(-30,100))+
  facet_grid(scenario ~ dplyr::desc(subbasin))

ggsave(tern_gr.4_perc_graph_sub , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/subbasin/",
                     "tern_3_gr.4_perc_graph_sub.jpg"),device = "jpg", width = 5, height = 10)

# all together
all_box_plot_tern_perc_graph_sub <- ggarrange(tern_gr.1_le_perc_graph_sub, tern_gr.1_incub_perc_graph_sub,
                                                tern_gr.1_rear_perc_graph_sub, tern_gr.2_perc_graph_sub,
                                                tern_gr.4_perc_graph_sub,
                                                
                                                nrow = 5, ncol = 1)

plot(all_box_plot_tern_perc_graph_sub)

ggsave(all_box_plot_tern_perc_graph_sub , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/subbasin/",
                     "all_box_plot_tern_3_perc_graph_sub.jpg"),device = "jpg", width = 3, height = 20)



# PLot with the highest correlating IHA per bird / subbasin

graph_sub_all_birds <- ggarrange(m_gull_gr.2_perc_graph_sub,
                                 bh_gull_gr.1_incub_perc_graph_sub,
                                  tern_gr.4_perc_graph_sub,
                                             nrow = 1, ncol = 3)
plot(graph_sub_all_birds)  
ggsave(graph_sub_all_birds , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/subbasin/",
                     "graph_sub_all_birds.jpg"),device = "jpg", width = 13, height = 6)
#vertically: width 4, height 15

##########################################################################################
library("reshape2")

######### Catastrophic Years BOX_PLOTS
######### for NS=0


#### M.Gull #############################################################################

# 1 gr.2_day03_max-----> NS=0
# box plots for reversing the regression equation
# box plots for reversing the regression equation
m.gull_sum_reg_eq_in <- read.csv("D:/Ptaki_hydro/Obliczenia/R/Results/reg_eq/new/1_m.gull_sum_reg_eq.csv")
m.gull_sum_reg_eq_in.m <- melt(m.gull_sum_reg_eq_in , id.vars = "sc")
m.gull_sum_reg_eq_in.m$perc <- m.gull_sum_reg_eq_in.m$value/27*100

m.gull_sum_reg_eq_in.m$sc <- factor(m.gull_sum_reg_eq_in.m$sc , levels=c("ref", "nf 4.5", "ff 4.5", "nf 8.5", "ff 8.5"))

m.gull_catastophy_graph <- ggplot(m.gull_sum_reg_eq_in.m, aes(x=sc, y=perc)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+
  labs(y="% of years with CBS") +
  ggtitle("Mew gull\ngr.2_day03_max\nNS=0")+
  coord_cartesian( ylim = c(0, 40))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=13),
        axis.text.x = element_text(size=15),
        axis.text.y = element_text(size=15),
        plot.title = element_text(size = 18))

plot (m.gull_catastophy_graph)

ggsave(m.gull_catastophy_graph , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/reg_eq/graphs/",
                     "m.gull_catas_day03_max.jpg"),device = "jpg", width = 5, height = 3)

# median value
library(data.table)
m.gull_sum_reg_eq_0_in.median <- setDT(m.gull_sum_reg_eq_in.m)[, median(perc), by=sc]

# gr.2_day03_max ----> NS=0.1
# box plots for reversing the regression equation
m.gull_sum_reg_eq_01_in <- read.csv("D:/Ptaki_hydro/Obliczenia/R/Results/reg_eq/new/test_0.1/0.1_m.gull_sum_reg_eq.csv")
m.gull_sum_reg_eq_01_in.m <- melt(m.gull_sum_reg_eq_01_in , id.vars = "sc")
m.gull_sum_reg_eq_01_in.m$perc <- m.gull_sum_reg_eq_01_in.m$value/27*100

m.gull_sum_reg_eq_01_in.m$sc <- factor(m.gull_sum_reg_eq_01_in.m$sc , levels=c("ref", "nf 4.5", "ff 4.5", "nf 8.5", "ff 8.5"))

m.gull_catastophy_graph_01 <- ggplot(m.gull_sum_reg_eq_01_in.m, aes(x=sc, y=perc)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+
  labs(y="% of years with CBS") +
  ggtitle("Mew gull\ngr.2_day03_max\nNS=0.1")+
  coord_cartesian( ylim = c(0, 40))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=13),
        axis.text.x = element_text(size=15),
        axis.text.y = element_text(size=15),
        plot.title = element_text(size = 18))

plot (m.gull_catastophy_graph_01)

ggsave(m.gull_catastophy_graph_01 , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/reg_eq/graphs/",
                     "m.gull_catas_day03_max_0.1.jpg"),device = "jpg", width = 5, height = 3)

#### box plot with median displayed

library(data.table)
m.gull_sum_reg_eq_01_in.median <- setDT(m.gull_sum_reg_eq_01_in.m)[, median(perc), by=sc]

# gr.2_day03_max ----> 0.2
# box plots for reversing the regression equation
#m.gull_sum_reg_eq_02_in <- read.csv("D:/Ptaki_hydro/Obliczenia/R/Results/reg_eq/new/test_0.2/0.2_m.gull_sum_reg_eq.csv")
#m.gull_sum_reg_eq_02_in.m <- melt(m.gull_sum_reg_eq_02_in , id.vars = "sc")
#m.gull_sum_reg_eq_02_in.m$perc <- m.gull_sum_reg_eq_02_in.m$value/27*100

#m.gull_sum_reg_eq_02_in.m$sc <- factor(m.gull_sum_reg_eq_02_in.m$sc , levels=c("ref", "nf 4.5", "ff 4.5", "nf 8.5", "ff 8.5"))

#m.gull_catastophy_graph_02 <- ggplot(m.gull_sum_reg_eq_02_in.m, aes(x=sc, y=perc)) + 
#  geom_boxplot( alpha =0.8, width = 0.9)+
#  labs(y="% of years with CBS") +
#  ggtitle("Mew gull gr.2_day03_max NS=0.2")+
#  coord_cartesian( ylim = c(0, 80))+
#  theme(axis.title.x = element_blank())

#plot (m.gull_catastophy_graph_02)

#ggsave(m.gull_catastophy_graph_02 , 
#       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/reg_eq/graphs/",
#                     "m.gull_catas_day03_max_0.2.jpg"),device = "jpg", width = 5, height = 3)

#### box plot with median displayed
#m.gull_sum_reg_eq_02_in.median <- setDT(m.gull_sum_reg_eq_02_in.m)[, median(perc), by=sc]

# 2 gr.4_P0.75----> NS=0
# box plots for reversing the regression equation
m.gull_sum_reg_eq2_in <- read.csv("D:/Ptaki_hydro/Obliczenia/R/Results/reg_eq/new2/4_m.gull_sum_reg_eq.csv")
m.gull_sum_reg_eq2_in.m <- melt(m.gull_sum_reg_eq2_in , id.vars = "sc")
m.gull_sum_reg_eq2_in.m$perc <- m.gull_sum_reg_eq2_in.m$value/27*100

m.gull_sum_reg_eq2_in.m$sc <- factor(m.gull_sum_reg_eq2_in.m$sc , levels=c("ref", "nf 4.5", "ff 4.5", "nf 8.5", "ff 8.5"))
print(m.gull_sum_reg_eq2_in.m)

m.gull_catastophy_graph2 <- ggplot(m.gull_sum_reg_eq2_in.m, aes(x=sc, y=perc)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+
  labs(y="% of years with CBS") +
  ggtitle("Mew gull\ngr.4_P0.75\nNS=0")+
  coord_cartesian( ylim = c(0, 40))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=13),
        axis.text.x = element_text(size=15),
        axis.text.y = element_text(size=15),
        plot.title = element_text(size = 18))

plot (m.gull_catastophy_graph2)

ggsave(m.gull_catastophy_graph2 , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/reg_eq/graphs/",
                     "m.gull_catas_P0.75.jpg"),device = "jpg", width = 5, height = 3)
# median value
m.gull_sum_reg_eq2_in.median <- setDT(m.gull_sum_reg_eq2_in.m)[, median(perc), by=sc]

# 2 gr.4_P0.75----> NS=0.1
# box plots for reversing the regression equation
m.gull_sum_reg_eq2_0.1_in <- read.csv("D:/Ptaki_hydro/Obliczenia/R/Results/reg_eq/new2/4_m.gull_sum_reg_eq_0.1.csv")
m.gull_sum_reg_eq2_0.1_in.m <- melt(m.gull_sum_reg_eq2_0.1_in , id.vars = "sc")
m.gull_sum_reg_eq2_0.1_in.m$perc <- m.gull_sum_reg_eq2_0.1_in.m$value/27*100

m.gull_sum_reg_eq2_0.1_in.m$sc <- factor(m.gull_sum_reg_eq2_0.1_in.m$sc , levels=c("ref", "nf 4.5", "ff 4.5", "nf 8.5", "ff 8.5"))
print(m.gull_sum_reg_eq2_0.1_in.m)

m.gull_catastophy_graph2_0.1 <- ggplot(m.gull_sum_reg_eq2_0.1_in.m, aes(x=sc, y=perc)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+
  labs(y="% of years with CBS") +
  ggtitle("Mew gull\ngr.4_P0.75\nNS=0.1")+
  coord_cartesian( ylim = c(0, 60))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=13),
        axis.text.x = element_text(size=15),
        axis.text.y = element_text(size=15),
        plot.title = element_text(size = 18))

plot (m.gull_catastophy_graph2_0.1)

ggsave(m.gull_catastophy_graph2 , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/reg_eq/graphs/",
                     "m.gull_catas_P0.75_0.1.jpg"),device = "jpg", width = 5, height = 3)

# median value
m.gull_sum_reg_eq2_0.1_in.median <- setDT(m.gull_sum_reg_eq2_0.1_in.m)[, median(perc), by=sc]


#### BH.Gull
#### 1 mean_incub -----> NS=0
# box plots for reversing the regression equation
bh.gull_sum_reg_eq_in <- read.csv("D:/Ptaki_hydro/Obliczenia/R/Results/reg_eq/new/1_bh.gull_sum_reg_eq.csv")
bh.gull_sum_reg_eq_in.m <- melt(bh.gull_sum_reg_eq_in , id.vars = "sc")
bh.gull_sum_reg_eq_in.m$perc <- bh.gull_sum_reg_eq_in.m$value/27*100

bh.gull_sum_reg_eq_in.m$sc <- factor(bh.gull_sum_reg_eq_in.m$sc , levels=c("ref", "nf 4.5", "ff 4.5", "nf 8.5", "ff 8.5"))
print(bh.gull_sum_reg_eq_in.m)

bh.gull_catastophy_graph <- ggplot(bh.gull_sum_reg_eq_in.m, aes(x=sc, y=perc)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+
  labs(y="% of years with CBS") +
  ggtitle("Black-headed gull\ngr.1_mean_incub\nNS=0")+
  coord_cartesian( ylim = c(0, 40))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=13),
        axis.text.x = element_text(size=15),
        axis.text.y = element_text(size=15),
        plot.title = element_text(size = 18))

plot (bh.gull_catastophy_graph)


ggsave(bh.gull_catastophy_graph , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/reg_eq/graphs/",
                     "bh.gull_catas_mean_incub.jpg"),device = "jpg", width = 5, height = 3)
#### box plot with median displayed

bh.gull_sum_reg_eq_in.median <- setDT(bh.gull_sum_reg_eq_in.m)[, median(perc), by=sc]

#### 1 mean_incub -----> NS=0.1
# box plots for reversing the regression equation
bh.gull_sum_reg_eq_0.1_in <- read.csv("D:/Ptaki_hydro/Obliczenia/R/Results/reg_eq/new/test_0.1/0.1_bh.gull_sum_reg_eq.csv")
bh.gull_sum_reg_eq_0.1_in.m <- melt(bh.gull_sum_reg_eq_0.1_in , id.vars = "sc")
bh.gull_sum_reg_eq_0.1_in.m$perc <- bh.gull_sum_reg_eq_0.1_in.m$value/27*100

bh.gull_sum_reg_eq_0.1_in.m$sc <- factor(bh.gull_sum_reg_eq_0.1_in.m$sc , levels=c("ref", "nf 4.5", "ff 4.5", "nf 8.5", "ff 8.5"))
print(bh.gull_sum_reg_eq_0.1_in.m)

bh.gull_catastophy_graph_0.1 <- ggplot(bh.gull_sum_reg_eq_0.1_in.m, aes(x=sc, y=perc)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+
  labs(y="% of years with CBS") +
  ggtitle("Black-headed gull\ngr.1_mean_incub\nNS=0.1")+
  coord_cartesian( ylim = c(0, 40))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=13),
        axis.text.x = element_text(size=15),
        axis.text.y = element_text(size=15),
        plot.title = element_text(size = 18))

plot (bh.gull_catastophy_graph_0.1)


ggsave(bh.gull_catastophy_graph_0.1 , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/reg_eq/graphs/",
                     "bh.gull_catas_mean_incub_0.1.jpg"),device = "jpg", width = 5, height = 3)
#### box plot with median displayed

library(data.table)
bh.gull_sum_reg_eq_0.1_in.median <- setDT(bh.gull_sum_reg_eq_0.1_in.m)[, median(perc), by=sc]

## 0.2
#### 1 mean_incub
# box plots for reversing the regression equation
#bh.gull_sum_reg_eq_0.2_in <- read.csv("D:/Ptaki_hydro/Obliczenia/R/Results/reg_eq/new/test_0.2/0.2_bh.gull_sum_reg_eq.csv")
#bh.gull_sum_reg_eq_0.2_in.m <- melt(bh.gull_sum_reg_eq_0.2_in , id.vars = "sc")
#bh.gull_sum_reg_eq_0.2_in.m$perc <- bh.gull_sum_reg_eq_0.2_in.m$value/27*100

#bh.gull_sum_reg_eq_0.2_in.m$sc <- factor(bh.gull_sum_reg_eq_0.2_in.m$sc , levels=c("ref", "nf 4.5", "ff 4.5", "nf 8.5", "ff 8.5"))
#print(bh.gull_sum_reg_eq_0.2_in.m)

#bh.gull_catastophy_graph_0.2 <- ggplot(bh.gull_sum_reg_eq_0.2_in.m, aes(x=sc, y=perc)) + 
#  geom_boxplot( alpha =0.8, width = 0.9)+
#  labs(y="% of years with CBS") +
#  ggtitle("Black-headed gull gr.1_mean_incub NS=0.2")+
#  coord_cartesian( ylim = c(0, 80))+
#  theme(axis.title.x = element_blank())

#plot (bh.gull_catastophy_graph_0.2)


#ggsave(bh.gull_catastophy_graph_0.2 , 
#       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/reg_eq/graphs/",
#                     "bh.gull_catas_mean_incub_0.2.jpg"),device = "jpg", width = 5, height = 3)


#### 2 gr.2_day03_max -----> NS=0
# box plots for reversing the regression equation
bh.gull_sum_reg_eq2_in <- read.csv("D:/Ptaki_hydro/Obliczenia/R/Results/reg_eq/new2/4_bh.gull_sum_reg_eq.csv")
bh.gull_sum_reg_eq2_in.m <- melt(bh.gull_sum_reg_eq2_in , id.vars = "sc")
bh.gull_sum_reg_eq2_in.m$perc <- bh.gull_sum_reg_eq2_in.m$value/27*100

bh.gull_sum_reg_eq2_in.m$sc <- factor(bh.gull_sum_reg_eq2_in.m$sc , levels=c("ref", "nf 4.5", "ff 4.5", "nf 8.5", "ff 8.5"))

bh.gull_catastophy_graph2 <- ggplot(bh.gull_sum_reg_eq2_in.m, aes(x=sc, y=perc)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+
  labs(y="% of years with CBS") +
  ggtitle("Black-headed gull\n gr.2_day03_max\nNS=0")+
  coord_cartesian( ylim = c(0, 40))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=13),
        axis.text.x = element_text(size=15),
        axis.text.y = element_text(size=15),
        plot.title = element_text(size = 18))

plot (bh.gull_catastophy_graph2)

ggsave(bh.gull_catastophy_graph2 , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/reg_eq/graphs/",
                     "bh.gull_catas_day03_max.jpg"),device = "jpg", width = 5, height = 3)

#### box plot with median displayed
bh.gull_sum_reg_eq2_in.median <- setDT(bh.gull_sum_reg_eq2_in.m)[, median(perc), by=sc]

#### 2 gr.2_day03_max -----> NS=0.1
# box plots for reversing the regression equation
bh.gull_sum_reg_eq2_0.1_in <- read.csv("D:/Ptaki_hydro/Obliczenia/R/Results/reg_eq/new2/4_bh.gull_sum_reg_eq_0.1.csv")
bh.gull_sum_reg_eq2_0.1_in.m <- melt(bh.gull_sum_reg_eq2_0.1_in , id.vars = "sc")
bh.gull_sum_reg_eq2_0.1_in.m$perc <- bh.gull_sum_reg_eq2_0.1_in.m$value/27*100

bh.gull_sum_reg_eq2_0.1_in.m$sc <- factor(bh.gull_sum_reg_eq2_0.1_in.m$sc , levels=c("ref", "nf 4.5", "ff 4.5", "nf 8.5", "ff 8.5"))

bh.gull_catastophy_graph2_0.1 <- ggplot(bh.gull_sum_reg_eq2_0.1_in.m, aes(x=sc, y=perc)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+
  labs(y="% of years with CBS") +
  ggtitle("Black-headed gull\n gr.2_day03_max\nNS=0.1")+
  coord_cartesian( ylim = c(0, 60))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=13),
        axis.text.x = element_text(size=15),
        axis.text.y = element_text(size=15),
        plot.title = element_text(size = 18))

plot (bh.gull_catastophy_graph2_0.1)

ggsave(bh.gull_catastophy_graph2_0.1 , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/reg_eq/graphs/",
                     "bh.gull_catas_day03_max_0.1.jpg"),device = "jpg", width = 5, height = 3)

#### box plot with median displayed
bh.gull_sum_reg_eq2_0.1_in.median <- setDT(bh.gull_sum_reg_eq2_0.1_in.m)[, median(perc), by=sc]


### Tern #############################################################################
#### 1 gr.4_P0.75------> NS=0
# box plots for reversing the regression equation
tern_sum_reg_eq_in <- read.csv("D:/Ptaki_hydro/Obliczenia/R/Results/reg_eq/new/1_tern_sum_reg_eq.csv")
tern_sum_reg_eq_in.m <- melt(tern_sum_reg_eq_in , id.vars = "sc")
tern_sum_reg_eq_in.m$perc <- tern_sum_reg_eq_in.m$value/27*100

tern_sum_reg_eq_in.m$sc <- factor(tern_sum_reg_eq_in.m$sc , levels=c("ref", "nf 4.5", "ff 4.5", "nf 8.5", "ff 8.5"))
print(tern_sum_reg_eq_in.m)

tern_catastophy_graph <- ggplot(tern_sum_reg_eq_in.m, aes(x=sc, y=perc)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+
  labs(y="% of years with CBS") +
  ggtitle("Little tern\ngr.4_P0.75\nNS=0")+
  coord_cartesian( ylim = c(0, 40))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=13),
        axis.text.x = element_text(size=15),
        axis.text.y = element_text(size=15),
        plot.title = element_text(size = 18))

plot (tern_catastophy_graph)

ggsave(tern_catastophy_graph , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/reg_eq/graphs/",
                     "tern_catas_P0.75.jpg"),device = "jpg", width = 5, height = 3)
#### box plot with median displayed
library(data.table)
tern_sum_reg_eq_in.median <- setDT(tern_sum_reg_eq_in.m)[, median(perc), by=sc]

#### gr.4_P0.75 ----> NS=0.1
tern_sum_reg_eq_0.1_in <- read.csv("D:/Ptaki_hydro/Obliczenia/R/Results/reg_eq/new/test_0.1/0.1_tern_sum_reg_eq.csv")
tern_sum_reg_eq_0.1_in.m <- melt(tern_sum_reg_eq_0.1_in , id.vars = "sc")
tern_sum_reg_eq_0.1_in.m$perc <- tern_sum_reg_eq_0.1_in.m$value/27*100

tern_sum_reg_eq_0.1_in.m$sc <- factor(tern_sum_reg_eq_0.1_in.m$sc , levels=c("ref", "nf 4.5", "ff 4.5", "nf 8.5", "ff 8.5"))

tern_catastophy_graph_0.1 <- ggplot(tern_sum_reg_eq_0.1_in.m, aes(x=sc, y=perc)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+
  labs(y="% of years with CBS") +
  ggtitle("Little tern\ngr.4_P0.75\nNS=0.1")+
  coord_cartesian( ylim = c(0, 40))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=13),
        axis.text.x = element_text(size=15),
        axis.text.y = element_text(size=15),
        plot.title = element_text(size = 18))

plot (tern_catastophy_graph_0.1)

ggsave(tern_catastophy_graph_0.1 , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/reg_eq/graphs/",
                     "tern_catas_P0.75_0.1.jpg"),device = "jpg", width = 5, height = 3)
#### box plot with median displayed
library(data.table)
tern_sum_reg_eq_0.1_in.median <- setDT(tern_sum_reg_eq_0.1_in.m)[, median(perc), by=sc]
print(tern_sum_reg_eq_0.1_in.median)

#### gr.4_P0.75 ----> 0,2
#tern_sum_reg_eq_0.2_in <- read.csv("D:/Ptaki_hydro/Obliczenia/R/Results/reg_eq/new/test_0.2/0.2_tern_sum_reg_eq.csv")
#tern_sum_reg_eq_0.2_in.m <- melt(tern_sum_reg_eq_0.2_in , id.vars = "sc")
#tern_sum_reg_eq_0.2_in.m$perc <- tern_sum_reg_eq_0.2_in.m$value/27*100

#tern_sum_reg_eq_0.2_in.m$sc <- factor(tern_sum_reg_eq_0.2_in.m$sc , levels=c("ref", "nf 4.5", "ff 4.5", "nf 8.5", "ff 8.5"))

#tern_catastophy_graph_0.2 <- ggplot(tern_sum_reg_eq_0.2_in.m, aes(x=sc, y=perc)) + 
#  geom_boxplot( alpha =0.8, width = 0.9)+
#  labs(y="% of years with CBS") +
#  ggtitle("Little tern gr.4_P0.75 NS=0.2")+
#  coord_cartesian( ylim = c(0, 80))+
#  theme(axis.title.x = element_blank())

#plot (tern_catastophy_graph_0.2)

#ggsave(tern_catastophy_graph_0.2 , 
#      file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/reg_eq/graphs/",
#                   "tern_catas_P0.75_0.2.jpg"),device = "jpg", width = 5, height = 3)

#### 2 gr.2_day03_max -------> NS=0
# box plots for reversing the regression equation
tern_sum_reg_eq2_in <- read.csv("D:/Ptaki_hydro/Obliczenia/R/Results/reg_eq/new2/4_tern_sum_reg_eq.csv")
tern_sum_reg_eq2_in.m <- melt(tern_sum_reg_eq2_in , id.vars = "sc")
tern_sum_reg_eq2_in.m$perc <- tern_sum_reg_eq2_in.m$value/27*100

tern_sum_reg_eq2_in.m$sc <- factor(tern_sum_reg_eq2_in.m$sc , levels=c("ref", "nf 4.5", "ff 4.5", "nf 8.5", "ff 8.5"))
print(tern_sum_reg_eq2_in.m)

tern_catastophy_graph2 <- ggplot(tern_sum_reg_eq2_in.m, aes(x=sc, y=perc)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+
  labs(y="% of years with CBS") +
  ggtitle("Little tern\ngr.2_day03_max\nNS=0")+
  coord_cartesian( ylim = c(0, 40))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=13),
        axis.text.x = element_text(size=15),
        axis.text.y = element_text(size=15),
        plot.title = element_text(size = 18))

plot (tern_catastophy_graph2)

ggsave(tern_catastophy_graph2 , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/reg_eq/graphs/",
                     "tern_catas_day03_max.jpg"),device = "jpg", width = 5, height = 3)

#### box plot with median displayed
library(data.table)
tern_sum_reg_eq2_in.median <- setDT(tern_sum_reg_eq2_in.m)[, median(perc), by=sc]

#### 2 gr.2_day03_max -------> NS=0.1
# box plots for reversing the regression equation
tern_sum_reg_eq2_0.1_in <- read.csv("D:/Ptaki_hydro/Obliczenia/R/Results/reg_eq/new2/4_tern_sum_reg_eq_0.1.csv")
tern_sum_reg_eq2_0.1_in.m <- melt(tern_sum_reg_eq2_0.1_in , id.vars = "sc")
tern_sum_reg_eq2_0.1_in.m$perc <- tern_sum_reg_eq2_0.1_in.m$value/27*100

tern_sum_reg_eq2_0.1_in.m$sc <- factor(tern_sum_reg_eq2_0.1_in.m$sc , levels=c("ref", "nf 4.5", "ff 4.5", "nf 8.5", "ff 8.5"))
print(tern_sum_reg_eq2_0.1_in.m)

tern_catastophy_graph2_0.1 <- ggplot(tern_sum_reg_eq2_0.1_in.m, aes(x=sc, y=perc)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+
  labs(y="% of years with CBS") +
  ggtitle("Little tern\ngr.2_day03_max\nNS=0.1")+
  coord_cartesian( ylim = c(0, 60))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=13),
        axis.text.x = element_text(size=15),
        axis.text.y = element_text(size=15),
        plot.title = element_text(size = 18))

plot (tern_catastophy_graph2_0.1)

ggsave(tern_catastophy_graph2_0.1 , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/reg_eq/graphs/",
                     "tern_catas_day03_max_0.1.jpg"),device = "jpg", width = 5, height = 3)

#### box plot with median displayed
library(data.table)
tern_sum_reg_eq2_0.1_in.median <- setDT(tern_sum_reg_eq2_0.1_in.m)[, median(perc), by=sc]

# summary graph 3 birds NS=0.1
library("ggplot2")

catastophy_graph_NS0.1_all <- ggarrange(m.gull_catastophy_graph_01, 
                                        bh.gull_catastophy_graph_0.1,
                                        tern_catastophy_graph_0.1,
                                              nrow = 1, ncol = 3)

plot(catastophy_graph_NS0.1_all)

ggsave(catastophy_graph_NS0.1_all , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/reg_eq/graphs/",
                     "catastophy_graph_NS0.1_all.jpg"),device = "jpg", width = 13, height = 4)

# summary graphs 3 birds correlation matrix, NS=0, NS=0.2, 2nd highest
library("ggpubr")

catastophy_graph_appendix_all <- ggarrange(m.gull_catastophy_graph, 
                                        bh.gull_catastophy_graph,
                                        tern_catastophy_graph,
                                        #m.gull_catastophy_graph_02, 
                                        #bh.gull_catastophy_graph_0.2,
                                        #tern_catastophy_graph_0.2,
                                        m.gull_catastophy_graph2,
                                        bh.gull_catastophy_graph2,
                                        tern_catastophy_graph2,
                                        m.gull_catastophy_graph2_0.1,
                                        bh.gull_catastophy_graph2_0.1,
                                        tern_catastophy_graph2_0.1,
                                        nrow = 3, ncol = 3)

plot(catastophy_graph_appendix_all)

ggsave(catastophy_graph_appendix_all , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/reg_eq/graphs/",
                     "catastophy_graph_appendix_all.jpg"),device = "jpg", width = 18, height = 11)

install.packages("png")
library("png")
load_png_bh.gull <- readPNG("D:/Ptaki_hydro/Obliczenia/R/Results/reg_eq/graphs/bh.gull_matrix.png")
load_png_m.gull <- readPNG("D:/Ptaki_hydro/Obliczenia/R/Results/reg_eq/graphs/m.gull_matrix.png")
load_png_tern <- readPNG("D:/Ptaki_hydro/Obliczenia/R/Results/reg_eq/graphs/tern_matrix.png")

png_bh.gull <- ggplot() + 
  background_image(load_png_bh.gull) +
  # This ensures that the image leaves some space at the edges
  theme(plot.margin = margin(t=0.2, l=0.2, r=0.2, b=0.2, unit = "cm"))

png_m.gull <- ggplot() + 
  background_image(load_png_m.gull) +
  # This ensures that the image leaves some space at the edges
  theme(plot.margin = margin(t=0.2, l=0.2, r=0.2, b=0.2, unit = "cm"))

png_tern <- ggplot() + 
  background_image(load_png_tern) +
  # This ensures that the image leaves some space at the edges
  theme(plot.margin = margin(t=0.2, l=0.2, r=0.2, b=0.2, unit = "cm"))

png_all <- ggarrange(png_m.gull, png_bh.gull, png_tern, 
          ncol = 3, nrow = 1)
plot(png_all)

ggsave(png_all , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/reg_eq/graphs/",
                     "png_all.jpg"),device = "jpg", width = 15, height = 5)
