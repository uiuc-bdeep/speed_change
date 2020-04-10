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
MD.p <- "data/trips/household_survey_2012.rds"
ID.p <- "data/inflation/inflation.csv"
OS.p <- "data/osrm_simulations/osrm_partial_23.rds"


#regressions
CD1p <- "data/reg_out/reg_data.rds"
m4p <- "data/reg_out/reg_model_4.rds"

hours_month <- 1711/12
business_days <- 243

# output
OP.p <-"restat/tables/6_cba/B_cba_costs2.csv"


# main ----------------------------------------------------------------------------------------
# read data
CD <- readRDS(CD1p)
m4 <- readRDS(m4p)
s4 <- as.data.frame(summary(m4)$coefficients)

######################### compute counterfactuals
# inflation
ID <- read.csv(ID.p)
inflation <- ID$index[ID$date == as.character("2016-12-01")]/ID$index[ID$date == as.character("2012-12-01")]


# read household data
HH <- readRDS(MD.p)
CT <- HH[HH$TIPOVG %in% 2,]
CT <- CT[!is.na(CT$Time.car),]


# read OSRM simulations
OS <- readRDS(OS.p)

# define regression variables
CT$trip_id <- as.character(CT$ID_ORDEM)
CT$min <- ifelse(CT$MIN_SAIDA %in% c(0:19), "_00",
          ifelse(CT$MIN_SAIDA %in% c(20:39), "_20","_40"))
CT$th_ID <- paste0(CT$ID_ORDEM," " , CT$dep.hour, CT$min)
CT$month <- "-7"

CT$log_tr.time <- log(CT$Time.car)
CT$marg.ratio[is.na(CT$marg.ratio)] <- 0
CT$ratio.marg_spill_1000[is.na(CT$ratio.marg_spill_1000)] <- 0
CT$ratio.marg_spill_3000[is.na(CT$ratio.marg_spill_3000)] <- 0
CT$ratio.marg_spill_5000[is.na(CT$ratio.marg_spill_5000)] <- 0
CT$p <- ifelse(CT$dep.hour %in% c(7:9,17:19), 1, 0)


# add FEs to data
fe4 <- getfe(m4)
# th_ID FEs
TFE <- fe4[fe4$fe=="th_ID",c("idx","effect")]
colnames(TFE) <- c("th_ID", "th_ID_FE")
CT <- merge(CT, TFE, by="th_ID", all.x = T)
CT$th_ID_FE[is.na(CT$th_ID_FE)]<-mean(CT$th_ID_FE, na.rm=T)

# get model coefficients
CT$B_peak <- s4$Estimate[row.names(s4)=="marg.ratio:SLI:ppeak"]
CT$B_offpeak <- s4$Estimate[row.names(s4)=="marg.ratio:SLI:poff-peak"]
CT$B_sp1000 <- s4$Estimate[row.names(s4)=="SLI:ratio.marg_spill_1000"]
CT$B_sp3000 <- s4$Estimate[row.names(s4)=="SLI:ratio.marg_spill_3000"]
CT$B_sp5000 <- s4$Estimate[row.names(s4)=="SLI:ratio.marg_spill_5000"]
# remove spillover effect
CT$B_sp1000 <- 0
CT$B_sp3000 <- 0
CT$B_sp5000 <- 0


# assign treatment variables to model WI
CT$SLI <- 1
CT$marg.ratio_SLI_peak <- CT$marg.ratio*CT$p*CT$SLI
CT$marg.ratio_SLI_off <- CT$marg.ratio*(1-CT$p)*CT$SLI
CT$sp.ratio1000_SLI <- CT$ratio.marg_spill_1000*CT$SLI
CT$sp.ratio3000_SLI <- CT$ratio.marg_spill_3000*CT$SLI
CT$sp.ratio5000_SLI <- CT$ratio.marg_spill_5000*CT$SLI

# predict travel times
CT$pred_log.tt_WI <- CT$th_ID_FE +
                  CT$B_peak*CT$marg.ratio_SLI_peak +
                  CT$B_offpeak*CT$marg.ratio_SLI_off +
                  CT$B_sp1000*CT$sp.ratio1000_SLI +
                  CT$B_sp3000*CT$sp.ratio3000_SLI +
                  CT$B_sp5000*CT$sp.ratio5000_SLI
CT$pred_log.tt_WO <- CT$th_ID_FE


# merge simulation
CT <- merge(CT, OS, by="ID_ORDEM", all.x=T)


CT$pred_tt_WI <- exp(CT$pred_log.tt_WI)
CT$pred_tt_WIX <- exp(CT$pred_log.tt_WI)*(CT$dur_path3_sp3/CT$dur_path2_sp3)
CT$pred_tt_WO <- exp(CT$pred_log.tt_WO)
CT$hours_lost <- CT$pred_tt_WI - CT$pred_tt_WO

summary(AA$dur_path3_sp3/AA$dur_path2_sp3)


AA <- CT[CT$dur_path2_sp3!=CT$dur_path3_sp3,]
summary()

########## define VOTS
## Calculate hourly wages

# declared monthly wages
CT$wage_m <- ifelse(CT$CD_ATIVI %in% c(1,2) & CT$CO_REN_I %in% 1, CT$VL_REN_I,
             ifelse(CT$CD_ATIVI %in% c(1,2) & CT$CO_REN_I %in% 2, 0,
             ifelse(CT$CD_ATIVI %in% c(1,2) & CT$CO_REN_I %in% 3, CT$RENDA_FA/CT$NO_MORAF,
             0)))

# after tax wages
CT$nwage_m <- ifelse(CT$wage_m <= 1178.86, CT$wage_m*(1-0.08),
              ifelse(CT$wage_m <= 1637.11, CT$wage_m*(1-0.09),
              ifelse(CT$wage_m <= 1958.10, CT$wage_m*(1-0.165),
              ifelse(CT$wage_m <= 2453.50, CT$wage_m*(1-0.175),
              ifelse(CT$wage_m <= 3271.38, CT$wage_m*(1-0.25),
              ifelse(CT$wage_m <= 3916.29, CT$wage_m*(1-0.325),
              ifelse(CT$wage_m <= 4087.65, CT$wage_m*(1-0.225) - 430.78,
              CT$wage_m*(1-0.275) - 430.78)))))))

# hourly wage
CT$wage_h <- (CT$nwage_m/hours_month)*inflation

# VOTS
CT$motive_vtpi <- ifelse(CT$MOTIVO_O %in% c(1,2,3) & CT$MOTIVO_D %in% c(1,2,3), "business",
                  ifelse( (CT$MOTIVO_O %in% c(8) & CT$MOTIVO_D %in% c(1,2,3,9) ) |
                         (CT$MOTIVO_O %in% c(1,2,3,9) & CT$MOTIVO_D %in% c(8) ), "commute",
                  ifelse(CT$MOTIVO_O %in% c(4,5,6,10) | CT$MOTIVO_D %in% c(4,5,6,10), "personal", "leisure")))
CT$VTPI_mult <- ifelse(CT$motive_vtpi == "business", 1.5,
                ifelse(CT$motive_vtpi == "commute", 0.5,
                ifelse(CT$motive_vtpi == "personal", 0.25,0)))
CT$VOT_100 <- CT$wage_h
CT$VOT_50 <- CT$wage_h/2
CT$VOT_100_med <- median(CT$wage_h)
CT$VOT_50_med <- median(CT$wage_h)/2
CT$VOT_VTPI <- CT$wage_h*CT$VTPI_mult



############ calculate hours lost per day - with the policy
CT$hours_wi <- CT$pred_tt_WI
# calculate money lost per day
CT$value_wi_VOT_100 <- CT$hours_wi*CT$VOT_100
CT$value_wi_VOT_50 <- CT$hours_wi*CT$VOT_50
CT$value_wi_VOT_100_med <- CT$hours_wi*CT$VOT_100_med
CT$value_wi_VOT_50_med <- CT$hours_wi*CT$VOT_50_med
CT$value_wi_VOT_VTPI <- CT$hours_wi*CT$VOT_VTPI

############ calculate hours lost per day - without the policy
CT$hours_wo <- CT$pred_tt_WO
# calculate money lost per day
CT$value_wo_VOT_100 <- CT$hours_wo*CT$VOT_100
CT$value_wo_VOT_50 <- CT$hours_wo*CT$VOT_50
CT$value_wo_VOT_100_med <- CT$hours_wo*CT$VOT_100_med
CT$value_wo_VOT_50_med <- CT$hours_wo*CT$VOT_50_med
CT$value_wo_VOT_VTPI <- CT$hours_wo*CT$VOT_VTPI


############ calculate hours lost per day - DIFFERENCE WI-WO
CT$hours_lost <- CT$hours_wi - CT$hours_wo
# calculate money lost per day
CT$value_lost_VOT_100 <- CT$value_wi_VOT_100 - CT$value_wo_VOT_100
CT$value_lost_VOT_50 <- CT$value_wi_VOT_50 - CT$value_wo_VOT_50
CT$value_lost_VOT_100_med <- CT$value_wi_VOT_100_med - CT$value_wo_VOT_100_med
CT$value_lost_VOT_50_med <- CT$value_wi_VOT_50_med - CT$value_wo_VOT_50_med
CT$value_lost_VOT_VTPI <- CT$value_wi_VOT_VTPI - CT$value_wo_VOT_VTPI





############ calculate hours in traffic per year - with the policy
hours_wi_year <- sum(CT$hours_wi*CT$FE_VIA)*business_days/10^6
# calculate money lost per day
COST_VOT_100_wi <- sum(CT$value_wi_VOT_100*CT$FE_VIA)*business_days/10^6
COST_VOT_50_wi <- sum(CT$value_wi_VOT_50*CT$FE_VIA)*business_days/10^6
COST_VOT_100_med_wi <- sum(CT$value_wi_VOT_100_med*CT$FE_VIA)*business_days/10^6
COST_VOT_50_med_wi <- sum(CT$value_wi_VOT_50_med*CT$FE_VIA)*business_days/10^6
COST_VOT_VTPI_wi <- sum(CT$value_wi_VOT_VTPI*CT$FE_VIA)*business_days/10^6


############ calculate hours in traffic per year - wothout the policy
hours_wo_year <- sum(CT$hours_wo*CT$FE_VIA)*business_days/10^6
# calculate money lost per day
COST_VOT_100_wo <- sum(CT$value_wo_VOT_100*CT$FE_VIA)*business_days/10^6
COST_VOT_50_wo <- sum(CT$value_wo_VOT_50*CT$FE_VIA)*business_days/10^6
COST_VOT_100_med_wo <- sum(CT$value_wo_VOT_100_med*CT$FE_VIA)*business_days/10^6
COST_VOT_50_med_wo <- sum(CT$value_wo_VOT_50_med*CT$FE_VIA)*business_days/10^6
COST_VOT_VTPI_wo <- sum(CT$value_wo_VOT_VTPI*CT$FE_VIA)*business_days/10^6



############ calculate hours lost per year
hours_lost_year <- sum(CT$hours_lost*CT$FE_VIA)*business_days/10^6
# calculate money lost per day
COST_VOT_100 <- sum(CT$value_lost_VOT_100*CT$FE_VIA)*business_days/10^6
COST_VOT_50 <- sum(CT$value_lost_VOT_50*CT$FE_VIA)*business_days/10^6
COST_VOT_100_med <- sum(CT$value_lost_VOT_100_med*CT$FE_VIA)*business_days/10^6
COST_VOT_50_med <- sum(CT$value_lost_VOT_50_med*CT$FE_VIA)*business_days/10^6
COST_VOT_VTPI <- sum(CT$value_lost_VOT_VTPI*CT$FE_VIA)*business_days/10^6





MC <- as.data.frame(matrix(ncol=0,nrow=6))
MC$var <- c("hours lost (million)",
            "cost: 100% individual net-wages (R$ million)",
            "cost: 50% individual net-wages (R$ million)",
            "cost: 100% median net-wages (R$ million)",
            "cost: 50% median net-wages (R$ million)",
            "cost: VTPI individual VOT (R$ million)")
MC$wi <- c(hours_wi_year,
             COST_VOT_100_wi,
             COST_VOT_50_wi,
             COST_VOT_100_med_wi,
             COST_VOT_50_med_wi,
             COST_VOT_VTPI_wi)

MC$wo <- c(hours_wo_year,
           COST_VOT_100_wo,
           COST_VOT_50_wo,
           COST_VOT_100_med_wo,
           COST_VOT_50_med_wo,
           COST_VOT_VTPI_wo)


MC$lost <- c(hours_lost_year,
              COST_VOT_100,
              COST_VOT_50,
              COST_VOT_100_med,
              COST_VOT_50_med,
              COST_VOT_VTPI)

write.csv(MC, OP.p, row.names=F)
 

