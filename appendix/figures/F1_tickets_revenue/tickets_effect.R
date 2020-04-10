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
library(rgeos)
library(rgdal)


# inputs ----------------------------------------------------------------------
TD.p <- "data/tickets/tickets_over_segments_all.rds"
OP.p <-"tables/3_tickets_estimation/tickets_estimation.csv"
PD.p <- "data/panel/monthly_panel_all_b1k.rds"

#output
O1 <- "appendix/figures/F1_tickets_revenue/tickets_effect.pdf"

# main ----------------------------------------------------------------------------------------

# read tickets data
TI <- readRDS(TD.p)
# read panel
PD <- readRDS(PD.p)

HO <- read.csv("data/holidays/holidays_and_fare.csv")

# keep only speed tickets
SP <- TI[TI$ticket.speed %in% 1,]

# keep only weekdays
HO$date <- as.Date(as.character(HO$DATE))
HO <- HO[,c("date","Clean_weekday")]
SP <- merge(SP, HO, by="date", all.x=T)
SP <- SP[SP$Clean_weekday %in% 1,]
# keep Marginais only
SP$StreetName <- as.character(SP$StreetName)

SP$marg <- ifelse(SP$StreetName %in% c("AV MARGINAL DO RIO PINHEIROS",
                             "AV MARGINAL DO RIO TIETE"),1,0)

# count tickets per camera date
SP$loc_date <- paste(SP$Local, SP$date)

# tickets per day
SP$tics <- ave(SP$tickets, SP$loc_date,FUN=sum)

# aggregate by date
LM <- SP[!duplicated(SP$loc_date),]

# take log of tickets
LM$log_tic <- log(LM$tics)

# get month
LM$date_m <- as.Date(paste0(year(LM$date),"-",month(LM$date),"-01"))

# get panel covariates
PD2 <- PD[!duplicated(PD$date_m),
          c("date_m", "log_fuel", "log_cameras")]
LM <- merge(LM, PD2, by="date_m",all.x=T)

PD3 <- PD[!duplicated(PD$point_id),
          c("point_id", "ChangeDate")]
LM <- merge(LM, PD3, by="point_id",all.x=T)

MA <- LM[is.na(LM$ChangeDate) | LM$marg%in%1,]


#distance to treatment
MA$date_diff <- as.numeric(MA$date - as.Date("2015-07-20"))
MA$date_diff_q <- floor(MA$date_diff/90)

QQ <- MA[MA$date_diff_q %in% c(-5:7),]

for(y in 0:12){
  QQ[,paste0("ymr_",y)] <- ifelse(QQ[,"date_diff_q"] == y-5,
                                  QQ[,"marg"],0)
}

# weights
QQ$w <- ave(QQ$tics, QQ$Local, FUN=mean)

# tickets data
mx <- felm(log_tic~ymr_0+ymr_1+ymr_2+ymr_3+ymr_5+
                   ymr_6+ymr_7+ymr_8+ymr_9+ymr_10+ymr_11+ymr_12|
             Local+date|0|date_m,data=QQ)
summary(mx)
Sx <- as.data.frame(summary(mx)$coefficients)

sx <- Sx[1:12,]
sx$m <- as.numeric(substr(row.names(sx),5,6))-5
ssx <- sx[1,]
ssx$Estimate <- 0
ssx$`Cluster s.e.` <- 0
ssx$m <- -1
sx <- rbind(sx,ssx)
sx$mod <- "sx"


sx$b <- exp(sx$Estimate) - 1

sx$upx <- sx$Estimate + 1.96*sx$`Cluster s.e.`
sx$dox <- sx$Estimate - 1.96*sx$`Cluster s.e.`

sx$up <- exp(sx$upx) - 1
sx$do <- exp(sx$dox ) - 1


ggplot() +
 geom_hline(yintercept = 0, alpha=0.5) +
 geom_vline(aes(linetype="slrx",xintercept = 0),alpha=0.5) +
 geom_vline(aes(linetype="slre",xintercept = 6),alpha=0.5) +
 geom_point(aes(x=m+0.5,y=b),data=sx) +
 geom_errorbar(aes(x=m+0.5,ymin=do,ymax=up),data=sx, width=0) +
 theme_bw() +
 theme(panel.grid = element_blank()) +
 scale_y_continuous(label=percent, breaks = seq(-1,3,0.5)) +
 coord_cartesian(ylim=c(-1,3)) +
 labs(y = "changes in speeding tickets\n(Marginais Highways)",
      x = "quarters relative to speed limit reduction") +
 scale_linetype_manual(limits = c("slrx","slre"),
                       values = c("slrx" = 1,"slre"=2),
                       labels = c("slrx" = "\nspeed limit reduction\n",
                                  "slre" = "\nspeed limit increase\n"),
                       name = "") + 
 theme(legend.position = "top", legend.direction = "horizontal",
       legend.key = element_rect(size = 12),
       legend.key.height = unit(1.5, "lines"),
       legend.title = element_text(hjust = 0.5)) +
 scale_x_continuous(breaks = seq(-100,100,1))

ggsave(O1,w=12,h=6)

# estimate tickets in the baseline with and without the policy
BL <- QQ[QQ$ymr_4%in%1,]

BLT <- sum(BL$tics)
BLT1 <- sum(exp(BL$log_tic + sx[5,1]))

#tickets due to the policy
NT <- (BLT1 - BLT)*4

# ticket value
TI_L <- sum(SP$tickets[SP$marg %in%1 & SP$ticket.speed_0 %in% 1 &
            SP$year %in% 2015 & SP$month %in% c("04","05","06")])
TI_M <- sum(SP$tickets[SP$marg %in%1 & SP$ticket.speed_20 %in% 1 &
            SP$year %in% 2015 & SP$month %in% c("04","05","06")])
TI_H <- sum(SP$tickets[SP$marg %in%1 & SP$ticket.speed_50 %in% 1 &
            SP$year %in% 2015 & SP$month %in% c("04","05","06")])
REV_BL <- (TI_L*85.13 + TI_M*127.69 + TI_H*574.63)
# revenue per ticket
RPT <- REV_BL/(TI_L + TI_M + TI_H)

# revenue due to new tickets
NR <- NT*RPT
NR
