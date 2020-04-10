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


# inputs ----------------------------------------------------------------------
PD.p <- "data/panel/monthly_panel_all_b1k.rds"


# output
OP.p <-"appendix/figures/C4_accidents_on_controls_by_buffer/accidents_on_controls_by_buffer.pdf"



gentab <- function(my.model, model.number, my.data){
 
 MD <- my.data
 mm <- my.model
 ss <- summary(mm, se = "cluster", cluster = MD$StreetName)
 cov <- ss$cov.scaled
 bb <- exp(mm$coefficients) - 1
 se <- as.numeric(rep(0,length(mm$coefficients)))
 for(i in 1:length(mm$coefficients)){se[i] <- deltamethod(as.formula(paste0("~ exp(x",i,") - 1")), mm$coefficients, cov)}
 pp <- 2 * pnorm(abs(bb/se), lower.tail=FALSE)
 tt <- as.data.frame(cbind("b" = bb, "se" = se, "t"= abs(bb/se), "p" = pp))
 tt$m <- paste0("m", model.number)
 tt$v <- row.names(tt)
 tt$n <- mm$n
 return(tt)
}


# main ----------------------------------------------------------------------------------------
# read data
MD <- readRDS(PD.p)
MD <- as.data.frame(MD)

B <- as.data.frame(matrix(nrow = 14, ncol = 0))
B$buff <- seq(400,1600,200)
B$b <- 0
B$se <- 0


PD <- MD
PD$tr_group <- ifelse(is.na(PD$ChangeDate), "co", "tr")
PD$tr_group[is.na(PD$buff3000) & PD$tr_group %in% "co"] <- "b3000"
PD$tr_group[is.na(PD$buff2600) & PD$tr_group %in% "co"] <- "b2600"
PD$tr_group[is.na(PD$buff2200) & PD$tr_group %in% "co"] <- "b2200"
PD$tr_group[is.na(PD$buff1800) & PD$tr_group %in% "co"] <- "b1800"
PD$tr_group[is.na(PD$buff1400) & PD$tr_group %in% "co"] <- "b1400"
PD$tr_group[is.na(PD$buff1000) & PD$tr_group %in% "co"] <- "b1000"
PD$tr_group[is.na(PD$buff600) & PD$tr_group %in% "co"] <- "b600"
PD$tr_group[is.na(PD$buff200) & PD$tr_group %in% "co"] <- "b200"


PD$change_month_num <- (2015*12) + 7
PD$m_dist <- PD$month_num - PD$change_month_num
PD$q_dist <- floor(PD$m_dist/3)
 
# exclude point NA
PD <- PD[!is.na(PD$point_id),]
 
# Subset to segments with at least 2 accidents
PD <- PD[PD$accidents_total >=2,]
 
# count accidents in the pre period by segment
PD$pre <- ifelse(PD$date < as.Date("2015-07-01"), 1, 0)
PD$acc_pre <- PD$pre*PD$accidents_m
PD$acc_pre_seg <- ave(PD$acc_pre, PD$point_id, FUN=sum)
# Subset to segments with at least 1 accident in the pre
PD <- PD[PD$acc_pre_seg >=1,]
 
# month 
PD$mon <- as.character(substr(PD$date,1,7))

PP <- PD[!duplicated(PD$point_id),]


# Create Relative Quarter Covariates
PD$Qpos_tr <- ifelse(PD$q_dist >= 0 & PD$tr_group %in% "tr", 1, 0)
PD$Qpos_200 <- ifelse(PD$q_dist >= 0 & PD$tr_group %in% "b200", 1, 0)
PD$Qpos_600 <- ifelse(PD$q_dist >= 0 & PD$tr_group %in% "b600", 1, 0)
PD$Qpos_1000 <- ifelse(PD$q_dist >= 0 & PD$tr_group %in% "b1000", 1, 0)
PD$Qpos_1400 <- ifelse(PD$q_dist >= 0 & PD$tr_group %in% "b1400", 1, 0)
PD$Qpos_1800 <- ifelse(PD$q_dist >= 0 & PD$tr_group %in% "b1800", 1, 0)
PD$Qpos_2200 <- ifelse(PD$q_dist >= 0 & PD$tr_group %in% "b2200", 1, 0)
PD$Qpos_2600 <- ifelse(PD$q_dist >= 0 & PD$tr_group %in% "b2600", 1, 0)
PD$Qpos_3000 <- ifelse(PD$q_dist >= 0 & PD$tr_group %in% "b3000", 1, 0)

PD$Qpos_tr[is.na(PD$Qpos_tr)] <- 0
PD$Qpos_200[is.na(PD$Qpos_200)] <- 0
PD$Qpos_600[is.na(PD$Qpos_600)] <- 0
PD$Qpos_1000[is.na(PD$Qpos_1000)] <- 0
PD$Qpos_1400[is.na(PD$Qpos_1400)] <- 0
PD$Qpos_1800[is.na(PD$Qpos_1800)] <- 0
PD$Qpos_2200[is.na(PD$Qpos_2200)] <- 0
PD$Qpos_2600[is.na(PD$Qpos_2600)] <- 0
PD$Qpos_3000[is.na(PD$Qpos_3000)] <- 0

PD <- PD[PD$date_m < as.Date("2017-01-01"),]
# Regressions ----------------------------------------
 
 m1 <- femlm(accidents_m ~ -1 + Qpos_tr + 
                           Qpos_200 + Qpos_600 + Qpos_1000 +
                           Qpos_1400 + Qpos_1800 + Qpos_2200 +
                           Qpos_2600 + Qpos_3000 +
                           camera | point_id + mon ,
             data = PD)
 a1 <- gentab(m1, 1, PD)
 
a1$b <- a1$b - a1$b[9]
a1$up <- a1$b + 1.96*a1$se
a1$do <- a1$b - 1.96*a1$se

mo <- a1[1:9,]
mo$buff <- c(0,400,800,1200,1600,2000, 2400, 2800, 3200)



ggplot() + 
 geom_hline(yintercept = 0, colour ="gray") +
 geom_point(aes(x=buff+200, y=b), data = mo) +
 geom_errorbar(aes(x = buff+200, ymin = do, ymax = up), data = mo, width = 0) +
 theme_bw() +
 theme(panel.grid = element_blank(), legend.position = "top", plot.title = element_text(hjust = 0.5)) +
 labs(x = "distance to treated segments", y = "Change in accidents\nafter July 2015") +
 scale_y_continuous(labels = percent, breaks = seq(-1,1,0.1)) +
 scale_x_continuous(breaks = seq(200,3400,400),
                    labels = c(0,400,800,1200,1600,2000, 2400, 2800, ">3200") ) +
 coord_cartesian(ylim = c(-0.35,0.35), xlim = c(0,3000))
ggsave(OP.p, w = 7, h = 4)



 