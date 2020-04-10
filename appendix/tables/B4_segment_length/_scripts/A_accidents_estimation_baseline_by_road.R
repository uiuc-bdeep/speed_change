
# clear memory
rm(list=ls())
gc()

# required packages
library(lfe)
library(stargazer)
library(ggplot2)
library(FENmlm)
library(msm)
library(zoo)
library(data.table)
library(scales)
library(cowplot)
library(MatchIt)
library(optmatch)
source("tables/2_accidents_estimation/reg_functions_A.R")


# inputs ----------------------------------------------------------------------
PD.p <- "data/panel/monthly_panel_all_b1k.rds"


# output
OP.p <-"appendix/tables/B4_segment_length/_scripts/acc_base_id_byroad.csv"



# main ----------------------------------------------------------------------------------------
# read data
PD <- readRDS(PD.p)

# define tr and co groups
PD$tr_group <- ifelse(is.na(PD$ChangeDate), "co", "tr")
# exclude point NA
PD <- PD[!is.na(PD$point_id),]

### Create Relative Quarter Covariates (treated roads only)
# long term (>6 quarters after)
PD$Q5 <- ifelse(PD$q_dist >= 5 & PD$tr_group %in% "tr", 1, 0)
PD$Q5[is.na(PD$Q5)] <- 0
# post treatment
for(i in 0:4){PD[, paste0("Q",abs(i))] <- ifelse(PD[,"q_dist"] %in% i & PD[,"tr_group"] %in% "tr", 1, 0)}

# month as character
PD$mon <- as.character(substr(PD$date,1,7))

# Define cohorts
PD$cohort <- PD$change_month_num - 24186
PD$seg <- 1
PD$cohort_sum <- ave(PD$seg, PD$cohort, FUN=sum)
### Create Relative month Covariates per cohort
for(c in 1:6){
 for(m in 0:17){
  PD[,paste0("c",c,"m",m)] <- ifelse(PD[,"cohort"] %in% c & PD[,"tr_dist"]%in% m, 1, 0)
 }
}


# exclude the first quarter before the Speed Limit Change Date (due to anticipation effects)
PD <- PD[!(PD$q_dist %in% c(-1)),]

# drop data from after the 6th relative quarter (treatment only)
PD <- PD[PD$q_dist <= 5 | PD$tr_group %in% "co",]

## Subset to segments with at least 1 accidents in the whole period
PD$accidents_total <- ave(PD$accidents_m, PD$point_id, FUN=sum)
PD <- PD[PD$accidents_total >=1,]

# count accidents in the pre period by segment (used for matching)
PD$pre <- ifelse(PD$date < as.Date("2015-07-01"), 1, 0)
PD$acc_pre <- PD$pre*PD$accidents_m
PD$acc_pre_seg <- ave(PD$acc_pre, PD$point_id, FUN=sum)


# read id800
#I8 <- readRDS("restat/review/11_segment_length/id_800.rds")
# merge id
#PD <- merge(PD, I8, by="point_id", all.x=T)
# aggregate accidents by ID800 month
PD$month_id800 <- paste(PD$date_m, PD$StreetName)
PD$acc_m800 <- ave(PD$accidents_m, PD$month_id800, FUN=sum)
PD$camera <- ave(PD$camera, PD$month_id800, FUN=max)
PD$camera_D <- ave(PD$camera_D, PD$month_id800, FUN=max)
PD <- PD[!duplicated(PD$month_id800),]

# calculate weights
WW <- PD[!duplicated(PD$cohort),c("cohort", "cohort_sum")]
WW <- WW[!is.na(WW$cohort),]
WW$cohort_sum <- WW$cohort_sum/(3*sum(WW$cohort_sum))
colnames(WW) <- c("c","w")

# read VMT
VM <- readRDS("data/roads/vmt/vmt.rds")

### Create Data for Each Model ------------------------------------------------
# Model 0 - Treatment Only (Event Study)
ES <- PD[PD$tr_group %in% "tr",]

# Model 1 - all control segments (C1)
C1 <- PD[PD$tr_group %in% c("tr","co"),]

# MOdel 2 - with controls except segments within 1600m of treatment
C2 <- PD[PD$tr_group %in% "tr" | is.na(PD$buff1600),]

## MOdel 3 - with matched controls (also excludes segments within 1600m of treatment)
# segment level data
SD <- C2[!duplicated(C2$point_id),]
# treatment dummy
SD$tr <- ifelse(SD$tr_group %in% "tr",1,0)
# match based on accidents in the pre period
m.out <- matchit(tr ~ acc_pre_seg,
                 data = SD[,c("tr", "acc_pre_seg", "point_id")],
                 method = "nearest")
# get matching result
MS <- match.data(m.out)
# indentify matched segments
match_segs <- unique(MS$point_id)
C3 <- C2[C2$point_id %in% match_segs,]


# Regressions ----------------------------------------
c1m_t <- paste0("c1m",seq(0,17))
c2m_t <- paste0("c2m",seq(0,17))
c3m_t <- paste0("c3m",seq(0,17))
c4m_t <- paste0("c4m",seq(0,17))
c5m_t <- paste0("c5m",seq(0,17))
c6m_t <- paste0("c6m",seq(0,17))
f0c <- as.formula(paste("acc_m800 ~ ",
                       paste(c1m_t, collapse= "+"),"+", paste(c2m_t, collapse= "+"),"+", 
                       paste(c3m_t, collapse= "+"),"+", paste(c4m_t, collapse= "+"),"+",
                       paste(c5m_t, collapse= "+"),"+", paste(c6m_t, collapse= "+"),"+",
                       "camera + camera_D + t + log_fuel + log_cameras  | point_id"))

# Event Study
m0 <- femlm(acc_m800 ~ Q0 + Q1 + Q2 + Q3 + Q4 + Q5 +
                          camera + camera_D + t + log_fuel + log_cameras  | point_id,
            data = ES)

# Event Study CATT
m0c <- femlm(f0c,data = ES)


# With all controls
m1 <- femlm(acc_m800 ~ Q0 + Q1 + Q2 + Q3 + Q4 + Q5 +
                     camera + camera_D | point_id + mon,
           data = C1)

# With controls located more than 1.6km of treatment
m2 <- femlm(acc_m800 ~ Q0 + Q1 + Q2 + Q3 + Q4 + Q5 +
                          camera + camera_D  | point_id + mon,
           data = C2)

# With matched controls located more than 1.6km of treatment
m3 <- femlm(acc_m800 ~ Q0 + Q1 + Q2 + Q3 + Q4 + Q5 +
                          camera + camera_D  | point_id + mon,
           data = C3)


# Combine all results --------------------------------------------

# get outputs in relative terms and clustered s.e. 
a0 <- gentab(m0, 0, ES)
a1 <- gentab(m1, 1, C1)
a2 <- gentab(m2, 2, C2)
a3 <- gentab(m3, 3, C3)

a0c <- gentab(m0c,"0c",ES)
b0c <- agg_Q(m0c,0,ES,WW)
b0c <- rbind(b0c, a0c[109:113,])

# bind all results
a <- rbind(a0,a1,a2,a3,b0c)

# get unique row names (variable*model)
rownames(a) <- paste(a$v, a$m)

# get significance stars
a$star <- ifelse(a$p < 0.001, "***",
          ifelse(a$p < 0.01, "**",
          ifelse(a$p < 0.05, "*"," ")))
# save baseline output table
write.csv(a, OP.p)


