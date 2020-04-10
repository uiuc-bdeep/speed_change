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
HH.p <- "data/trips/household_survey_2012.rds"

# output
OP.p <-"appendix/tables/B5_travel_time_actual_trips/travel_time_actual_trips.csv"


# main ----------------------------------------------------------------------------------------

# read data
HH <- readRDS(HH.p)
TH <- HH[!is.na(HH$ID_ORDEM),c("ID_ORDEM", "H_SAIDA")]

CD <- readRDS(CD.p)
CD$th_ID <- CD$th.ID
CD$ID_ORDEM <- as.numeric(do.call(rbind, strsplit(CD$th_ID, split = " "))[,1])

CD <- merge(CD, TH, by= "ID_ORDEM", all.x = T)
CD$length <- CD$length/1000

CD$hour <- str_split_fixed(as.character(CD$th.ID), " ", 2)[,2]
CD$hour <- as.numeric(str_split_fixed(as.character(CD$hour), "_", 2)[,1])

CD <- CD[CD$H_SAIDA == CD$hour,]


# create dummy for peak
CD$p <- ifelse(CD$hour %in% c(7:9,17:19), "peak", "off-peak")


# get the hour of the motorcycle restriction adopted in May 2017
CD$moto <- ifelse(CD$hour %in% c(22,23,0,1,2,3,4) & 
                   CD$date >= as.Date("2017-05-13"), CD$marg.ratio, 0)

# define dat*hour FEs
CD$dh <- paste(CD$date, CD$hour)




################ estimate models
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
write.csv(ss, OP.p)