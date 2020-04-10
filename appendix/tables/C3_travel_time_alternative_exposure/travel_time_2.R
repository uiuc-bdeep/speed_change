# clear memory
rm(list=ls())
gc()

# required packages
library(lfe)
library(stargazer)
library(ggplot2)
library(cowplot)
library(scales)
library(Cairo)
library(stringr)

# inputs ----------------------------------------------------------------------
CD.p <- "data/crawler/travel_time_reg_data.rds"
VM.p <- "appendix/tables/C3_travel_time_alternative_exposure/2.1_simulation_data_MRs.rds"
# output
OP.p <-"appendix/tables/C3_travel_time_alternative_exposure/"


# main ----------------------------------------------------------------------------------------
# read data
CD <- readRDS(CD.p)
VM <- readRDS(VM.p)

VM$marg_ratio_0 <- VM$marg.ratio
VM$marg_ratio_0[is.na(VM$marg_ratio_0)] <- 0

VM <- VM[,c("ID_ORDEM", "marg_ratio_0","marg_ratio_1",
            "marg_ratio_2","marg_ratio_3",
            "ratio.marg_spill_1000_1","ratio.marg_spill_1000_2","ratio.marg_spill_1000_3",
            "ratio.marg_spill_3000_1","ratio.marg_spill_3000_2","ratio.marg_spill_3000_3",
            "ratio.marg_spill_5000_1","ratio.marg_spill_5000_2","ratio.marg_spill_5000_3")]

# fix trip hour ID label
CD$th_ID <- CD$th.ID

# recover trip ID
CD$ID_ORDEM <- as.numeric(str_split_fixed(as.character(CD$th.ID), " ", 2)[,1])

#merge alternative Marg ratios
CD <- merge(CD, VM, by="ID_ORDEM", all.x=T)


# convert length to km
CD$length <- CD$length/1000

# recover departure hour
CD$hour <- str_split_fixed(as.character(CD$th.ID), " ", 2)[,2]
# convert hour to numeric
CD$hour <- as.numeric(str_split_fixed(as.character(CD$hour), "_", 2)[,1])
# create dummy for peak
CD$p <- ifelse(CD$hour %in% c(7:9,17:19), "peak", "off-peak")


# get the hour of the motorcycle restriction adopted in May 2017
CD$moto <- ifelse(CD$hour %in% c(22,23,0,1,2,3,4) & 
                   CD$date >= as.Date("2017-05-13"), CD$marg.ratio, 0)

# define dat*hour FEs
CD$dh <- paste(CD$date, CD$hour)

CD$ratio.marg_spill_1000_0 <- CD$ratio.marg_spill_1000
CD$ratio.marg_spill_3000_0 <- CD$ratio.marg_spill_3000
CD$ratio.marg_spill_5000_0 <- CD$ratio.marg_spill_5000


################ estimate models
for(i in 0:4){# i <- 0
 CD[,"marg.ratio"] <- CD[,paste0("marg_ratio_",i)]
 CD[,"ratio.marg_spill_1000"] <- CD[,paste0("ratio.marg_spill_1000_",i)]
 CD[,"ratio.marg_spill_3000"] <- CD[,paste0("ratio.marg_spill_3000_",i)]
 CD[,"ratio.marg_spill_5000"] <- CD[,paste0("ratio.marg_spill_5000_",i)]
 
 
 m1 <- felm(log_tr.time ~ rain.dummy + Holiday + SLI + SLI:marg.ratio + moto |
                         th_ID | 0 | date, data = CD)
 s1 <- as.data.frame(summary(m1)$coefficients)


 m2 <- felm(log_tr.time ~ rain.dummy + Holiday + SLI + SLI:marg.ratio +
                          SLI:ratio.marg_spill_1000 + SLI:ratio.marg_spill_3000 + 
                          SLI:ratio.marg_spill_5000  + moto |
                         th_ID | 0 | date, data = CD)
 s2 <- as.data.frame(summary(m2)$coefficients)

 m3 <- felm(log_tr.time ~ SLI:marg.ratio +
                          SLI:ratio.marg_spill_1000 + SLI:ratio.marg_spill_3000 + 
                          SLI:ratio.marg_spill_5000  + moto |
                          th_ID + dh | 0 | date, data = CD)
 s3 <- as.data.frame(summary(m3)$coefficients)

 m4 <- felm(log_tr.time ~ marg.ratio:SLI:p +
                          SLI:ratio.marg_spill_1000 + SLI:ratio.marg_spill_3000 + 
                          SLI:ratio.marg_spill_5000  + moto |
                          th_ID + dh | 0 | date, data = CD)
 s4 <- as.data.frame(summary(m4)$coefficients)


 # add model name to summary
 s1$model <- "m1"
 s2$model <- "m2"
 s3$model <- "m3"
 s4$model <- "m4"


 # get row.names (variable of each coefficient)
 s1$var <- rownames(s1)
 s2$var <- rownames(s2)
 s3$var <- rownames(s3)
 s4$var <- rownames(s4)


 # get number of observations in each model
 s1$n <- m1$N
 s2$n <- m2$N
 s3$n <- m3$N
 s4$n <- m4$N


 # bind all results
 ss <- rbind(s1, s2, s3, s4)
 # get unique names (mode*variable)
 rownames(ss) <- paste(ss$var, ss$model)

 # get significance stars
 ss$star <- ifelse(ss$`Pr(>|t|)` < 0.001, "***",
            ifelse(ss$`Pr(>|t|)` < 0.01, "**",
            ifelse(ss$`Pr(>|t|)` < 0.05, "*"," ")))

 # save as csv
 write.csv(ss, paste0(OP.p,"alt_reg_",i,".csv"))
}

 

