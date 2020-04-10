#     ---------------------------------------------------------------------------------------- 
#   |  compare costs and benefits by education attainment group                               |
#   |                                                                                         |
#   |  By:  Renato Vieira                                                                     |
#   |       Big Data for Environmental Economics and Policy                                   |
#   |       University of Illinois at Urbana Chamapaign                                       |
#     ---------------------------------------------------------------------------------------- 

# Prelims -------------------------------------------------------------------------------------

# clear memory
rm(list=ls())
gc()

# required packages
library(lfe)
library(ggplot2)
library(Cairo)
library(cowplot)
library(stringr)
library(scales)

# inputs ----------------------------------------------------------------------
AD.p <- "data/accidents/accidents.rds"
VI.p <- "data/accidents/victims_cost.rds"
VE.p <- "data/accidents/vehicles_cost.rds"

ID.p <- "data/inflation/inflation.csv"
MD.p <- "data/crawler/master_paths2.rds"

HO.p <- "data/holidays/holidays_and_fare.csv"

TD.p <- "data/trips/household_survey_2012.rds"

# crawler data
CD.p <- "data/crawler/travel_time_reg_data.rds"



# output
OP.p <-"figures/6_distributional/distributional.pdf"

# treatment effects

# speed limit only
TE_marg1 <- 512/340 # (E[acc|no SLR])/(E[acc|SLR]) --- Sample (3)

hours_month <- 1711/12
business_days <- 243



# main ----------------------------------------------------------------------------------------
# read data
AD <- readRDS(AD.p)
VE <- readRDS(VE.p)
VI <- readRDS(VI.p)

HO <- read.csv(HO.p)
HO$date_d <- as.Date(as.character(HO$DATE))

# find accidents in the marginais
AM <- AD[AD$marg %in% 1,]
marg.acc <- AM$id_acidente

# inflation
ID <- read.csv(ID.p)
inflation <- ID$index[ID$date == as.character("2016-12-01")]/ID$index[ID$date == as.character("2014-12-01")]


VI$educ <- ifelse(VI$escolaridade %in% c(1,2), 2,
           ifelse(VI$escolaridade %in% c(3,4), 3,
           ifelse(VI$escolaridade %in% c(5,6), 4,
           ifelse(VI$escolaridade %in% c(7), 5,NA))))

# vehicle cost distribution (VCD)
VD <- VI[VI$tipo_vitima %in% "CD",]
VCD <- as.data.frame(as.matrix(table(VD$educ)/nrow(VD)))
colnames(VCD) <- "veh_cost_distr"
VCD$educ <- c(2:5)

# total vehicle damages in 2016 business days at marginais (tot_veh_dam_marg)
VE$year <- as.numeric(substr(VE$id_acidente,4,5))
tot_veh_dam_marg0 <- sum(VE$cost_vehicle[VE$year %in% 16 & VE$id_acidente %in% marg.acc])*inflation

# counterfactual
tot_veh_dam_marg1 <- tot_veh_dam_marg0*TE_marg1

VCD$veh_dam_0 <- tot_veh_dam_marg0*VCD$veh_cost_distr
VCD$veh_dam_1 <- tot_veh_dam_marg1*VCD$veh_cost_distr


# total victims costs in 2016 business days (tot_vic_cos)
# subset victims to baseline
VI$year <- as.numeric(substr(VI$id_acidente,4,5))
VI$month <- as.numeric(substr(VI$id_acidente,2,3))
VI$date_m <- as.Date(paste0("20", VI$year, "-", VI$month, "-01"))

VIM <- VI[VI$id_acidente %in% marg.acc & VI$date_m >= as.Date("2015-12-31") & VI$date_m < as.Date("2017-01-01"),]
VID <- VIM[VIM$classificacao %in% "M",]

#aggregate costs by educ
VIM$cost_victim_educ0 <- ave(VIM$cost_victim, VIM$educ, FUN =sum)*inflation
VIM_E <- VIM[!duplicated(VIM$educ), c("educ", "cost_victim_educ0")]
VIM_E$cost_victim_educ1 <- VIM_E$cost_victim_educ0*(TE_marg1)

VCD <- merge(VCD, VIM_E)

VCD$TC0 <- VCD$cost_victim_educ0 + VCD$veh_dam_0
VCD$TC1 <- VCD$cost_victim_educ1 + VCD$veh_dam_1

VCD$C <- (VCD$TC1 - VCD$TC0)/(10^6)







# main ----------------------------------------------------------------------------------------
# read data
CD <- readRDS(CD.p)
# fix trip hour ID label
CD$th_ID <- CD$th.ID

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



m4 <- felm(log_tr.time ~ marg.ratio:SLI:p +
            SLI:ratio.marg_spill_1000 + SLI:ratio.marg_spill_3000 + 
            SLI:ratio.marg_spill_5000  + moto |
            th_ID + dh | 0 | date, data = CD)
s4 <- as.data.frame(summary(m4)$coefficients)





######################### compute counterfactuals


# inflation
ID <- read.csv(ID.p)
inflation <- ID$index[ID$date == as.character("2016-12-01")]/ID$index[ID$date == as.character("2012-12-01")]


# read household data
HH <- readRDS(MD.p)


HH$educ <- HH$GRAU_INS
HH$educ[HH$educ %in% 1] <- 2


CT <- HH[HH$TIPOVG %in% 2,]
CT <- CT[!is.na(CT$Time.car),]

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


CT$pred_tt_WI <- exp(CT$pred_log.tt_WI)
CT$pred_tt_WO <- exp(CT$pred_log.tt_WO)
CT$hours_lost <- CT$pred_tt_WI - CT$pred_tt_WO


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


# calculate money lost per YEAR per educ level
CT$value_lost_VOT_50_med_YEAR <- CT$value_lost_VOT_50_med*CT$FE_VIA*business_days


CT$B <- ave(CT$value_lost_VOT_50_med_YEAR, CT$educ, FUN=sum)/(10^6)

PED <-CT[!duplicated(CT$educ),c("educ", "B")] 
VCD <- merge(VCD, PED, by = "educ", all.x=T)


#per capita Costs and benefits
# adult population
AP <- HH[HH$IDADE > 18,]
AP <- AP[!duplicated(AP$ID_PESS),]
AP$pop_educ <- ave(AP$FE_PESS, AP$educ, FUN=sum)
APE <- AP[!duplicated(AP$educ), c("educ", "pop_educ")]
VCD <- merge(VCD, APE, by = "educ")


VCD$CpC <- (VCD$C/VCD$pop_educ)*(10^6)
VCD$BpC <- -(VCD$B/VCD$pop_educ)*(10^6)

my_p <- list()

my_p[[1]] <- ggplot() +
 geom_col(aes(x = educ - 0.2, y = CpC, fill = "ben"), data = VCD, width = 0.4, alpha = 0.75) +
 geom_col(aes(x = educ + 0.2, y = BpC, fill = "cos"), data = VCD, width = 0.4, alpha = 0.75) +
 theme_bw() +
 theme(panel.grid = element_blank(), legend.position = "top") +
 scale_fill_manual(values = c("cos" = "firebrick", "ben" = "navy"),
                   labels = c("ben" = "Accident Reduction    ", "cos" = "Travel Time Costs    "),
                   name = "Legend:    ") +
 scale_y_continuous(breaks = seq(0,100,1)) +
 scale_x_discrete(limits = c(2:5), 
                  labels = c("no primary", "primary", "secondary", "college")) +
 labs(x = "\nEducational Attainment", y = "BRL per Capita per Year\n") +
 coord_cartesian(ylim=c(0,4))


# inputs ----------------------------------------------------------------------










# main ----------------------------------------------------------------------------------------
# read data
AD <- readRDS(AD.p)
VE <- readRDS(VE.p)
VI <- readRDS(VI.p)

# read data
HH <- readRDS(TD.p)
HH$trip_id <- as.character(HH$ID_ORDEM)
HH$marg.ratio[is.na(HH$marg.ratio)] <- 0
HH$ratio.marg_spill_1000[is.na(HH$ratio.marg_spill_1000)] <- 0
HH$ratio.marg_spill_3000[is.na(HH$ratio.marg_spill_3000)] <- 0
HH$educ <- HH$GRAU_INS
HH$educ[HH$educ %in% 1] <- 2


# count people above 18 within each educ group
HH <- HH[HH$IDADE > 18, ]
PP <- HH[!duplicated(HH$ID_PESS),]
PP$pop_educ <- ave(PP$FE_PESS, PP$educ, FUN=sum)
EG <- PP[!duplicated(PP$educ), c("educ", "pop_educ")]


# count people who travel by car within each educ group
CT <- HH[HH$TIPOVG %in% 2,]
#travelers
CTI <- CT[!duplicated(CT$ID_PESS),]
#by educ
CTI$car_educ <- ave(CTI$FE_PESS, CTI$educ, FUN=sum)
EG_CT <- CTI[!duplicated(CTI$educ), c("educ", "car_educ")]
EG <- merge(EG, EG_CT, by = "educ")

# count people who travel at Marginais by eachh educ group
CT$length.marg[is.na(CT$length.marg)] <- 0
MT <- CT[CT$length.marg > 200,]
#travelers
MTI <- MT[!duplicated(MT$ID_PESS),]
#by educ
MTI$marg_educ <- ave(MTI$FE_PESS, MTI$educ, FUN=sum)
EG_MT <- MTI[!duplicated(MTI$educ), c("educ", "marg_educ")]
EG <- merge(EG, EG_MT, by = "educ")




# find accidents by educ
VI$escolaridade[is.na(VI$escolaridade)] <- 8
VI$educ <- ifelse(VI$escolaridade %in% c(1,2), 2,
           ifelse(VI$escolaridade %in% c(3,4), 3,
           ifelse(VI$escolaridade %in% c(5,6), 4,
           ifelse(VI$escolaridade %in% c(7), 5,NA))))
VI$vic <-1
VI$vic_educ <- ave(VI$vic, VI$educ, FUN=sum)
VE <- VI[!duplicated(VI$educ),c("educ", "vic_educ")]
VE <- VE[!is.na(VE$educ),]
EG <- merge(EG, VE, by = "educ")

# find fatal victims by educ
VF <- VI[VI$victim_status %in% "dead",]
VF$fat_educ <- ave(VF$vic, VF$educ, FUN=sum)
FE <- VF[!duplicated(VF$educ),c("educ", "fat_educ")]
FE <- FE[!is.na(FE$educ),]
EG <- merge(EG, FE, by = "educ")

EG$pop_educ_share <- EG$pop_educ/sum(EG$pop_educ)
EG$car_educ_share <- EG$car_educ/sum(EG$car_educ)
EG$marg_educ_share <- EG$marg_educ/sum(EG$marg_educ)
EG$vic_educ_share <- EG$vic_educ/sum(EG$vic_educ)
EG$fat_educ_share <- EG$fat_educ/sum(EG$fat_educ)


EG$car_educ_p <- (EG$car_educ_share-EG$pop_educ_share)/EG$pop_educ_share
EG$marg_educ_p <- (EG$marg_educ_share-EG$pop_educ_share)/EG$pop_educ_share
EG$vic_educ_p <- (EG$vic_educ_share-EG$pop_educ_share)/EG$pop_educ_share
EG$fat_educ_p <- (EG$fat_educ_share-EG$pop_educ_share)/EG$pop_educ_share

pl <- list()

pl[[1]] <- 
ggplot() +
 geom_hline(aes(yintercept = 0), colour = "gray") +
 geom_col(aes(x = educ - 0.2, y = vic_educ_p, fill = "vic"), data = EG, width = 0.4, alpha = 0.95) +
 geom_col(aes(x = educ + 0.2, y = fat_educ_p, fill = "fat"), data = EG, width = 0.4, alpha = 0.95) +
 theme_bw() +
 theme(panel.grid = element_blank(), legend.position = "top",
       plot.title = element_text(hjust = 0.5),
       plot.subtitle = element_text(hjust = 0.5)) +
 scale_fill_manual(values = c("vic" = "lightblue", "fat" = "navy"),
                   labels = c("vic" = "all\nvictims\n", "fat" = "fatal\nvictims\n"),
                   name = "Type of\nVictim:    ") +
 scale_y_continuous(breaks = seq(-2,2,0.5), labels = percent) +
 scale_x_discrete(limits = c(2:5), 
                  labels = c("no primary", "primary", "secondary", "college")) +
 labs(x = "\nEducational Attainment", y = "Share of Type by Educ. Attainment\n Relative to Population Share by Educ. Attainment\n") +
 coord_cartesian(ylim = c(-1.5,1.5))



pl[[2]] <- 
 ggplot() +
 geom_hline(aes(yintercept = 0), colour = "gray") +
 geom_col(aes(x = educ - 0.2, y = car_educ_p, fill = "car"), data = EG, width = 0.4, alpha = 0.95) +
 geom_col(aes(x = educ + 0.2, y = marg_educ_p, fill = "marg"), data = EG, width = 0.4, alpha = 0.95) +
 theme_bw() +
 theme(panel.grid = element_blank(), legend.position = "top",
       plot.title = element_text(hjust = 0.5),
       plot.subtitle = element_text(hjust = 0.5)) +
 scale_fill_manual(values = c("car" = "lightpink", "marg" = "firebrick"),
                   labels = c("car" = "Uses\nPrivate\nVehicle", "marg" = "Uses Private\nVehicle at\nMarginais Highways"),
                   name = "Type of\nCommuter:    ") +
 scale_y_continuous(breaks = seq(-2,2,0.5), labels = percent) +
 scale_x_discrete(limits = c(2:5), 
                  labels = c("no primary", "primary", "secondary", "college")) +
 labs(x = "\nEducational Attainment", y = "\n\n") +
 coord_cartesian(ylim = c(-1.5,1.5))

pg <- plot_grid(pl[[1]], NULL, pl[[2]], ncol=3, nrow = 1, rel_widths = c(1,0.05,1))

t1 <- ggplot() + labs(title="Panel A: Costs and Benefits of Speed Limit Reduction by Educational Attainment")
t2 <- ggplot() + labs(title="Panel B: Private Vehicle Utilization and Accidents by Educational Attainment")


fp <- plot_grid(t1, my_p[[1]], NULL, t2, pg, ncol=1, nrow = 5,
                rel_heights = c(0.1,1,0.1,0.1,1))
ggsave(fp, filename = OP.p, w= 9, h = 9)
