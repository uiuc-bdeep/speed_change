
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
# output
OP.p <-"appendix/tables/B6_travel_time_variance/travel_time_variance.csv"

# main ----------------------------------------------------------------------------------------
# read data
CD <- readRDS("tables/4_travel_time_estimation/reg_out/reg_data.rds")

# variance of travel time before and after
CD$tt <- exp(CD$log_tr.time)*60

# marginais usage
CD$marg_g <- ifelse(CD$marg.ratio==0, "co",
             ifelse(CD$marg.ratio>0 & CD$marg.ratio<=0.10, "m0_10",
             ifelse(CD$marg.ratio>0.10 & CD$marg.ratio<=0.20, "m10_20",
             ifelse(CD$marg.ratio>0.20 & CD$marg.ratio<=0.50, "m20_50",
             ifelse(CD$marg.ratio>0.50, "m50_100",NA)))))

CD$id <- str_split_fixed(as.character(CD$th.ID), " ", 2)[,1]


CD$tt_sd <- ave(CD$tt, paste(CD$th_ID, CD$SLI), FUN=sd)
CD$tt_90 <- ave(CD$tt, paste(CD$th_ID, CD$SLI), FUN=function(x) quantile(x, 0.9))
CD$tt_50 <- ave(CD$tt, paste(CD$th_ID, CD$SLI), FUN=function(x) quantile(x, 0.5))
CD$tt_med <- ave(CD$tt, paste(CD$th_ID, CD$SLI), FUN=median)
CD$tt_9050 <- CD$tt_90/CD$tt_50

PP <- CD[!duplicated(paste(CD$th_ID, CD$SLI)),]

PP <- PP[!is.na(PP$tt_sd),]
PP <- PP[PP$tt_sd >0,]

PP$log_sd <- log(PP$tt_sd)
mm1 <- felm(log_sd~SLI+marg_g:SLI | th_ID| 0 |id, data=PP)
sm1 <- as.data.frame(summary(mm1)$coefficients)
sm1$m <- "m1"
sm1$s <- ifelse(sm1$`Pr(>|t|)`<0.001,"***",ifelse(sm1$`Pr(>|t|)`<0.01,"**",ifelse(sm1$`Pr(>|t|)`<0.05,"*"," ")))
rownames(sm1) <- paste(sm1$m, rownames(sm1))

PP$log_5090 <- log(PP$tt_9050)
mm2 <- felm(log_5090~SLI+marg_g:SLI | th_ID| 0 |id, data=PP)
sm2 <- as.data.frame(summary(mm2)$coefficients)
sm2$m <- "m2"
sm2$s <- ifelse(sm2$`Pr(>|t|)`<0.001,"***",ifelse(sm2$`Pr(>|t|)`<0.01,"**",ifelse(sm2$`Pr(>|t|)`<0.05,"*"," ")))
rownames(sm2) <- paste(sm2$m, rownames(sm2))

sm <- rbind(sm1, sm2)
write.csv(sm, OP.p, row.names=T)

