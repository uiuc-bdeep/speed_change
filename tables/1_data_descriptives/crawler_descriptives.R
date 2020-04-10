
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


# inputs ----------------------------------------------------------------------
CD.p <- "data/crawler/crawled_data_car.rds"

# output
OP.p <- "restat/tables/2_descriptives/crawler_descriptives.csv"


# main ----------------------------------------------------------------------------------------
# read data
CD <- readRDS(CD.p)


# create output table
mo <- as.data.frame(matrix(nrow = 8, ncol = 8))
# define output names
colnames(mo) <- c("Obs_all", "Share_all","mean_all", "sd_all",
                  "Obs_marg", "Share_marg", "mean_marg", "sd_marg")
rownames(mo) <- c("tot", "post", "peak", "rain", "marg", "time", "dist", "marg.share")


# fix OSRM NA length
CD <- CD[!is.na(CD$osrm.length),]
# convert length to km
CD$osrm.length <- CD$osrm.length/1000
# fix Marg ratio and length NAs 
CD$marg.ratio[is.na(CD$marg.ratio)] <- 0
CD$length.marg[is.na(CD$length.marg)] <- 0
# set trips with more than 400m of intercection as using Margs
CD$marg <- ifelse(CD$length.marg > 400, 1, 0)

# subset of trips that use Marginais
MD <- CD[CD$marg == 1,]

## All Trips (total, share of total, using marginais, share of the ones using marg) 
mo[1,1] <- sum(CD$obs)/(10^3)
mo[1,2] <- sum(CD$obs)/sum(CD$obs)
mo[1,5] <- sum(MD$obs)/(10^3)
mo[1,6] <- sum(MD$obs)/sum(MD$obs)


## Trips after speed change (total, share of total, using marginais, share of the ones using marg)
mo[2,1] <- sum(CD$obs[CD$speed_change == 1])/(10^3)
mo[2,2] <- sum(CD$obs[CD$speed_change == 1])/sum(CD$obs)
mo[2,5] <- sum(MD$obs[MD$speed_change == 1])/(10^3)
mo[2,6] <- sum(MD$obs[MD$speed_change == 1])/sum(MD$obs)

## Trips during peak periods  (total, share of total, using marginais, share of the ones using marg)
mo[3,1] <- sum(CD$obs[CD$hour %in% c(7,8,9,17,18,19)])/(10^3)
mo[3,2] <- sum(CD$obs[CD$hour %in% c(7,8,9,17,18,19)])/sum(CD$obs)
mo[3,5] <- sum(MD$obs[MD$hour %in% c(7,8,9,17,18,19)])/(10^3)
mo[3,6] <- sum(MD$obs[MD$hour %in% c(7,8,9,17,18,19)])/sum(MD$obs)

## Trips with rain  (total, share of total, using marginais, share of the ones using marg)
mo[4,1] <- sum(CD$obs[CD$rain.dummy %in% c(1)])/(10^3)
mo[4,2] <- sum(CD$obs[CD$rain.dummy %in% c(1)])/sum(CD$obs)
mo[4,5] <- sum(MD$obs[MD$rain.dummy %in% c(1)])/(10^3)
mo[4,6] <- sum(MD$obs[MD$rain.dummy %in% c(1)])/sum(MD$obs)

## Trips that use the Marginais  (total, share of total, using marginais, share of the ones using marg)
mo[5,1] <- sum(CD$obs[CD$marg %in% c(1)])/(10^3)
mo[5,2] <- sum(CD$obs[CD$marg %in% c(1)])/sum(CD$obs)
mo[5,5] <- sum(MD$obs[MD$marg %in% c(1)])/(10^3)
mo[5,6] <- sum(MD$obs[MD$marg %in% c(1)])/sum(MD$obs)

## travel time  (total mean, total s.d., mean using marginais, s.d. using marginais)
mo[6,3] <- mean(CD$tr.time)
mo[6,4] <- sd(CD$tr.time)
mo[6,7] <- mean(MD$tr.time)
mo[6,8] <- sd(MD$tr.time)

## length  (total mean, total s.d., mean using marginais, s.d. using marginais)
mo[7,3] <- mean(CD$osrm.length)
mo[7,4] <- sd(CD$osrm.length)
mo[7,7] <- mean(MD$osrm.length)
mo[7,8] <- sd(MD$osrm.length)

## length ratio at Marginais  (total mean, total s.d., mean using marginais, s.d. using marginais)
mo[8,3] <- mean(CD$marg.ratio)
mo[8,4] <- sd(CD$marg.ratio)
mo[8,7] <- mean(MD$marg.ratio)
mo[8,8] <- sd(MD$marg.ratio)

# fix output table NAs
mo[is.na(mo)] <- 0

# save
write.csv(mo, OP.p)