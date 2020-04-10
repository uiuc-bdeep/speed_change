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
OP.p <-"appendix/figures/C1_tickets_by_distance/tickets_by_distance.pdf"



# main ----------------------------------------------------------------------------------------
# read data
PD <- readRDS(PD.p)
PD <- as.data.frame(PD)

# exclude point NA
PD <- PD[!is.na(PD$point_id),]
# subet to 2014 - 2017
PD <- PD[year(PD$date_m) >= 2014,]
# exclude November 2014 (missing information)
PD <- PD[!(PD$date_m %in% as.Date("2014-11-01")),]

# clear tickets NA
PD$tickets.rodizio_m[is.na(PD$tickets.rodizio_m)] <- 0
PD$tickets.speed_m[is.na(PD$tickets.speed_m)] <- 0

# prepare data - quarters
PD$year <- year(PD$date_m)
PD$quarter <- quarter(PD$date_m)
PD$q <- paste(PD$year, PD$quarter, sep = "_")
my.q <- unique(PD$q)
# quarters
for(i in my.q){PD[, paste0("YQ_",i)] <- ifelse(PD[,"q"] %in% i, 1, 0)}


# subset to segments with a positive number of tickets between Jan 2014 and June 2015
PD$pre <- ifelse(PD$date_m < as.Date("2015-07-01"), 1, 0)
PD$sp.tic_pre <- PD$pre*PD$tickets.speed_m
PD$sp.tic_pre_tot <- ave(PD$sp.tic_pre, PD$point_id, FUN=sum)
PD$sp.tic_pre <- ifelse(PD$pre == 0, Inf, PD$sp.tic_pre)
PD$sp.tic_pre_min <- ave(PD$sp.tic_pre, PD$point_id, FUN=min)

# subset to segments with cameras in the pre period
PD <- PD[PD$tickets.speed_m > 0, ]
PD <- PD[PD$sp.tic_pre_tot > 0, ]


PD$post <- ifelse(PD$date_m >= as.Date("2015-07-01"), 1, 0)


# identify segments based on tr co groups ad distance to treatment
PD$tr_group <- ifelse(is.na(PD$ChangeDate), "co", "tr")

mo <- matrix(nrow = 13, ncol = 3)
c <- 1

for( i in seq(400,2800,200)){
 #i <- 400
 SD <- PD
 SD[,"my.buff"] <- SD[,paste0("buff",i)]
 SD$group <- ifelse(SD$tr_group %in% "tr", "tr",
             ifelse(is.na(SD$my.buff), "out", "in"))
 SS <- SD[SD$tr_group %in% "co",]
  # log
 SS$log_tic <- log(SS$tickets.speed_m)
  # Regressions
 m2 <- felm(log_tic ~ post | point_id,
            data = SS[SS$group %in% "out",])
 mo[c,] <- c(summary(m2)$coefficients[1,1], summary(m2)$coefficients[1,2], i)
 
 
c <- c+1
}
mo <- as.data.frame(mo)
colnames(mo) <- c("b", "se", "buff")
mo$up <- mo$b + 1.96*mo$se
mo$do <- mo$b - 1.96*mo$se

ggplot() + 
 geom_hline(yintercept = 0, colour ="gray") +
 geom_point(aes(x=buff, y=b), data = mo) +
 geom_errorbar(aes(x = buff, ymin = do, ymax = up), data = mo, width = 0) +
 theme_bw() +
 theme(panel.grid = element_blank(), legend.position = "top", plot.title = element_text(hjust = 0.5)) +
 labs(x = "minimum distance to treated segments", y = "Change in tickets per camera\nafter July 2015") +
 scale_y_continuous(labels = percent, breaks = seq(-1,1,0.1)) +
 scale_x_continuous(labels = comma, breaks = seq(0,3000,200)) +
 coord_cartesian(ylim = c(-0.35,0.35))
ggsave(OP.p, w = 7, h = 4)


