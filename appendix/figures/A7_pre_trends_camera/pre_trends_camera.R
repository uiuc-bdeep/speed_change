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
 source("figures/4_pre_trends/reg_functions.R")

 
 
 
 
# inputs ----------------------------------------------------------------------
PD.p <- "data/panel/monthly_panel_all_b1k.rds"


# output
OP.p <-"appendix/figures/A7_pre_trends_camera/pre_trends_camera.pdf"



# main ----------------------------------------------------------------------------------------
# read data
PD <- readRDS(PD.p)

# define min camera date in each segment
PD$date_c <- ifelse(PD$camera%in% 1, PD$date_m, Inf)
PD$min_c_date <- ave(PD$date_c, PD$point_id, FUN=min)

# define tr and co groups
PD$tr_group <- ifelse(is.na(PD$ChangeDate), "co", "tr")
# exclude point NA
PD <- PD[!is.na(PD$point_id),]

# drop anything above the 18th relative quarter in TR group
PD <- PD[PD$tr_dist <=17 | PD$tr_group %in% "co",]

### Create Relative Quarter Covariates (treated roads only)
# long term (>6 quarters after)
PD$Q5 <- ifelse(PD$q_dist >= 5 & PD$tr_group %in% "tr", 1, 0)
PD$Q5[is.na(PD$Q5)] <- 0
# post treatment
for(i in 0:4){PD[, paste0("Q",abs(i))] <- ifelse(PD[,"q_dist"] %in% i & PD[,"tr_group"] %in% "tr", 1, 0)}
# pre treatment (used in control model)
for(i in -16:-1){PD[, paste0("Q_",abs(i))] <- ifelse(PD[,"q_dist"] %in% i , 1, 0)}
 
# month as character
PD$mon <- as.character(substr(PD$date,1,7))

# Define cohorts
PD$cohort <- PD$change_month_num - 24186
PD$seg <- 1
PD$cohort_sum <- ave(PD$seg, PD$cohort, FUN=sum)
### Create Relative month Covariates per cohort
for(c in 1:6){
 for(q in 0:5){
  PD[,paste0("c",c,"q",q)] <- ifelse(PD[,"cohort"] %in% c & PD[,"q_dist"]%in% q, 1, 0)
 }
}
### Create pre-treatment Relative month Covariates per cohort
 for(c in 1:6){
  for(q in -16:-1){
   PD[,paste0("c",c,"q_",abs(q))] <- ifelse(PD[,"cohort"] %in% c & PD[,"q_dist"]%in% q, 1, 0)
  }
 }
 

# exclude the first quarter before the Speed Limit Change Date (due to anticipation effects)
#PD <- PD[!(PD$q_dist %in% c(-1)),]

# drop data from after the 6th relative quarter (treatment only)
PD <- PD[PD$q_dist <= 5 | PD$tr_group %in% "co",]

## Subset to segments with at least 1 accidents in the whole period
PD$accidents_total <- ave(PD$accidents_m, PD$point_id, FUN=sum)
PD <- PD[PD$accidents_total >=1,]

# count accidents in the pre period by segment (used for matching)
PD$pre <- ifelse(PD$date < as.Date("2015-07-01"), 1, 0)
PD$acc_pre <- PD$pre*PD$accidents_m
PD$acc_pre_seg <- ave(PD$acc_pre, PD$point_id, FUN=sum)

# calculate weights
WW <- PD[!duplicated(PD$cohort),c("cohort", "cohort_sum")]
WW <- WW[!is.na(WW$cohort),]
WW$cohort_sum <- WW$cohort_sum/(sum(WW$cohort_sum))
colnames(WW) <- c("c","w")


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
c1m_t <- paste0("c1q",seq(0,5))
c2m_t <- paste0("c2q",seq(0,5))
c3m_t <- paste0("c3q",seq(0,5))
c4m_t <- paste0("c4q",seq(0,5))
c5m_t <- paste0("c5q",seq(0,5))
c6m_t <- paste0("c6q",seq(0,5))
 
c1m_tn <- paste0("c1q_",c(1,3:12))
c2m_tn <- paste0("c2q_",c(1,3:12))
c3m_tn <- paste0("c3q_",c(1,3:12))
c4m_tn <- paste0("c4q_",c(1,3:12))
c5m_tn <- paste0("c5q_",c(1,3:12))
c6m_tn <- paste0("c6q_",c(1,3:12))
 
f0c <- as.formula(paste("accidents_m ~",
                        
                        paste(c1m_tn, collapse= "+"),"+", paste(c2m_tn, collapse= "+"),"+", 
                        paste(c3m_tn, collapse= "+"),"+", paste(c4m_tn, collapse= "+"),"+",
                        paste(c5m_tn, collapse= "+"),"+", paste(c6m_tn, collapse= "+"),"+",
                        
                        paste(c1m_t, collapse= "+"),"+", paste(c2m_t, collapse= "+"),"+", 
                        paste(c3m_t, collapse= "+"),"+", paste(c4m_t, collapse= "+"),"+",
                        paste(c5m_t, collapse= "+"),"+", paste(c6m_t, collapse= "+"),"+",
                        "camera + camera_D + t + log_fuel + log_cameras  | point_id"))

  m0c <- femlm(f0c,data = ES)

 # Event Study
 m0 <- femlm(accidents_m ~ Q_12 + Q_11 + Q_10 + Q_9 + 
              Q_8 + Q_7 + Q_6 + Q_5 + 
              Q_4 + Q_3 +  Q_1 + 
              Q0 + Q1 + Q2 + Q3 + Q4 + Q5 +
              camera + camera_D + t + log_fuel + log_cameras | point_id,
             data = ES)
 
 # With all controls
 m1 <- femlm(accidents_m ~ Q_12 + Q_11 + Q_10 + Q_9 + 
               Q_8 + Q_7 + Q_6 + Q_5 + 
               Q_4 + Q_3 + Q_2 + Q_1 + 
               Q0 + Q1 + Q2 + Q3 + Q4 + Q5 +
               camera + camera_D  | point_id + mon,
              data = C1)
 
 # With controls located more than 1.6km of treatment
 m2 <- femlm(accidents_m ~ Q_12 + Q_11 + Q_10 + Q_9 + 
              Q_8 + Q_7 + Q_6 + Q_5 + 
              Q_4 + Q_3 + Q_2 + Q_1 + 
              Q0 + Q1 + Q2 + Q3 + Q4 + Q5 +
              camera + camera_D  | point_id + mon,
             data = C2)
 
 # With matched controls located more than 1.6km of treatment
 m3 <- femlm(accidents_m ~ Q_12 + Q_11 + Q_10 + Q_9 + 
              Q_8 + Q_7 + Q_6 + Q_5 + 
              Q_4 + Q_3 + Q_2 + Q_1 + 
              Q0 + Q1 + Q2 + Q3 + Q4 + Q5 +
              camera + camera_D  | point_id + mon,
             data = C3)

bbq0c <- agg_Q(m0c,0,ES,WW)[,c("b","se","v","m")]
bbq0 <- gentab(m0,0,ES)[1:17,c("b","se","v","m")]
bbq1 <- gentab(m1,1,C1)[1:18,c("b","se","v","m")]
bbq1$b <- bbq1$b - bbq1$b[bbq1$v == "Q_2"]

bbq2 <- gentab(m2,2,C2)[1:18,c("b","se","v","m")]
bbq2$b <- bbq2$b - bbq2$b[bbq2$v == "Q_2"]

bbq3 <- gentab(m3,3,C3)[1:18,c("b","se","v","m")]
bbq3$b <- bbq3$b - bbq3$b[bbq3$v == "Q_2"]

bb <- rbind(bbq0c, bbq0,bbq1,bbq2,bbq3)
bb$q <- as.numeric(gsub("Q","",gsub("_","-",bb$v)))

bb$up <- bb$b + 1.96*bb$se
bb$do <- bb$b - 1.96*bb$se

bb$up[bb$q==-2] <- 0
bb$do[bb$q==-2] <- 0

bb$rq <- ifelse(bb$m == "m0", bb$q -0.2,
          ifelse(bb$m == "m0c", bb$q -0.1,
          ifelse(bb$m == "m2", bb$q +0.1,
          ifelse(bb$m == "m3", bb$q +0.2,bb$q))))






bb0 <- bb[1:2,]
bb0$b <- 0
bb0$se <- 0
bb0$m <- c("m0","m0c")
bb0$up <- 0
bb0$do <- 0
bb0$rq <- c(-2.2,-2.1)
bb <- rbind(bb,bb0)





# signs and banners
sb <- as.data.frame(matrix(nrow = 1, ncol = 0))
sb$x0 <- -1.5
sb$xf <- -0.5
sb$y0 <- -Inf
sb$yf <- Inf

ggplot() + 
 geom_hline(yintercept = 0, alpha = 0.15)+
 geom_point(aes(x=rq, y=b, colour = m), data = bb) +
 geom_errorbar(aes(x = rq, ymin = do, ymax = up, colour = m), data = bb, width = 0) +
 geom_vline(xintercept = -0.50, linetype = 2, colour ="red") +
 geom_rect(aes(xmin = x0, xmax = xf, ymin = y0, ymax = yf, fill = "sb"), alpha = 0.15, data = sb) +
 geom_rect(aes(xmin = 3, xmax = 3.75, ymin = 0.325, ymax = 0.4), fill = "purple", alpha = 0.15) +
 geom_text(aes(x=4, y=0.4, label ="signs and banners\non treated segm."),vjust=1,hjust = 0, size = 2.5) +
 coord_cartesian(ylim =c(-0.5,0.5)) +
 theme_bw() +
 theme(panel.grid = element_blank(), legend.position = "top") +
 labs(x = "quarters after speed limit reduction", y = "changes in accidents") +
 scale_y_continuous(labels = percent) +
 scale_colour_manual(values = c("m0" = "#777777",
                                "m0c" = "#222222",
                                "m1" = "#FF99CC",
                                "m2" = "#FF3333",
                                "m3" = "#990000"),
                     labels = c("m0" = "\n\n\n event study    \n unweighted\n\n",
                                "m0c" = "\n\n\n event study    \n CATT\n\n",
                                "m1" = "\n\n\n (1) event study w/ controls    \n      All non-treated ave.\n\n",
                                "m2" = "\n\n\n (2) event study w/ controls    \n      non-treated ave.\n      >1.6km from treatment\n",
                                "m3" = "\n\n\n (3) event study w/ controls    \n      non-treated ave.\n      >1.6km from treatment\n     >matched to treatment"),
                     name = "") +
 scale_fill_manual(values = c("sb" = "purple"),
                   labels = c("sb" = "signs and banners\non treated segm."),
                   name = "") +
 scale_x_continuous(breaks = seq(-99,8,1)) +
 guides(colour=guide_legend(ncol=5), fill =F)
ggsave(OP.p, w =10 , h =5)






