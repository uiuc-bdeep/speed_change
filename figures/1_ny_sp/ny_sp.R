#     ---------------------------------------------------------------------------------------- 
#   |  compare road fatalities in S?o Paulo and NY                                            |
#   |  Figure 1                                                                               |
#   |  By:  Renato Vieira                                                                     |
#   |       Big Data for Environmental Economics and Policy                                   |
#   |       University of Illinois at Urbana Chamapaign                                       |
#     ---------------------------------------------------------------------------------------- 

# Prelims -------------------------------------------------------------------------------------

# clear memory
rm(list=ls())
gc()

# required packages
library(ggplot2)
library(Cairo)
library(data.table)
library(scales)
library(lfe)

# inputs ----------------------------------------------------------------------

# output ----------------------------------------------------------------------
OP.p <- "figures/1_ny_sp/ny_sp.pdf"

# read data -----------------------------------------------------------------------------------

MD <- as.data.frame(matrix(nrow=11, ncol=0))
MD$city <- c("SP_tr","SP_tr","SP_tr",
             "SP_b",             "SP_v",             "_",
             "NY_tr","NY_tr","NY_tr",
             "NY_b",             "NY_v")
MD$var <- c("ped", "mot", "dri",
            "_",            "hom",            "_",
            "ped", "mot", "dri",
            "_",            "hom")
MD$fat <- c(36.2, 30.9, 15.5,
            0,            74.31,            0,
            17.6, 3.6, 7.0,
            0,            39.3)
MD$w <- c(1, 1, 1,
          0.1,          1,                     1,
          1, 1, 1, 0.1, 1)


TD <- as.data.frame(matrix(nrow=7, ncol=0))
TD$city <- c("SP","SP","SP",  "",  "NY","NY","NY")
TD$city_n <- c(1,1,1,  2,   3,3,3)
TD$var <- c("ped", "mot", "dri",  "_",  "ped", "mot", "dri")
TD$fat <- c(36.2, 30.9, 15.5,      0,  17.6, 3.6, 7.0)
fs1 <- 36.2 + 30.9 + 15.5
fs2 <- 17.6 + 3.6 + 7.0
TD$fat_p <- c(36.2/fs1, 30.9/fs1, 15.5/fs1,      0,  17.6/fs2, 3.6/fs2, 7.0/fs2)
TD$fat_p <- round(TD$fat_p*100,1)
TD$fat_c <- c(36.2/2, 36.2 + 30.9/2, 36.2 + 30.9 + 15.5/2,
              0,
              17.6/2, 17.6 + 1/2, 17.6 + 3.6 + 7.0/2)
TD$fat_t <- paste0(TD$fat, " (", TD$fat_p, "%)")
TD$fat_t[4] <- ""

FD <- as.data.frame(matrix(nrow=3, ncol=0))
FD$city <- c("SP",  "",  "NY")
FD$city_n <- c(1,  2,   3)
FD$var <- c("hom", "_",  "hom")
FD$fat <- c(74.31, 0, 39.3)
FD$fat_c <- c(0.65*74.31, 0, 0.4*39.3)
FD$fat_t <-c("74.31", "", "39.3")

ggplot() +
 geom_bar(data=TD, aes(x=city_n - 0.42, y=fat, fill=var),
          stat="identity", alpha = 0.5, width = 0.8)+
 geom_text(data=TD,aes(x=city_n - 0.42, y=fat_c ,label=fat_t),vjust=0, size = 2.5) +
 geom_bar(data=FD, aes(x=city_n + 0.42, y=fat, fill=var), stat="identity", alpha = 0.5, width = 0.8)+
 geom_text(data=FD,aes(x=city_n + 0.42, y=fat_c ,label=fat_t),vjust=0, size = 2.5) +
 annotate("text", x = 0.58, y = 90, label = "vehicle-related\nfatalities", size = 3) +
 annotate("text", x = 1.42, y = 80, label = "physical\nviolence", size = 3) +
 annotate("text", x = 2.58, y = 36, label = "vehicle-related\nfatalities", size = 3) +
 annotate("text", x = 3.42, y = 46, label = "physical\nviolence", size = 3) +
 scale_fill_manual(breaks = c("ped", "mot", "dri"),
                   labels = c("ped" = "pedestrians", "mot" = "motorcyclists",
                              "dri" = "drivers & passengers"),
                   values = c("ped" = "darkolivegreen2", "mot" = "gold",
                              "dri" = "darkorange", "hom" = "lightsteelblue", "_" = "red"),
                   name = "") +
 scale_x_discrete(limits = c(1,3), breaks = seq(1,3,1),
                  labels = c("São Paulo,\nBrazil", "", "New York,\nUSA")) +
 scale_y_continuous(limits = c(0,100), breaks = seq(0,100,20), expand = c(0,0)) +
 theme_bw() +
 theme(panel.grid = element_blank(), legend.position = "top") +
 labs(x = "", y = "Fatalities\n(per year per million residents)\n")

ggsave(filename = OP.p, w = 7, h = 4)