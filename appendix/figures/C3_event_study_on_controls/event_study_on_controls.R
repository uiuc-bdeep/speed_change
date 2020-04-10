
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
library(cowplot)

# inputs ----------------------------------------------------------------------
PD.p <- "data/panel/monthly_panel_all_b1k.rds"


# output
OP.p <-"appendix/figures/C3_event_study_on_controls/"



# main ----------------------------------------------------------------------------------------
# read data
PD <- readRDS(PD.p)
PD <- as.data.frame(PD)

# define tr and co groups
PD$tr_group <- ifelse(is.na(PD$ChangeDate), "co", "tr")

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

# define q_dist for the control
PD$change_month_num <- (2015*12) + 7
PD$m_dist <- PD$month_num - PD$change_month_num
PD$q_dist <- floor(PD$m_dist/3)


# Create Relative Quarter Covariates
PD$Qpre <- ifelse(PD$q_dist <= -12, 1, 0)
PD$Q5 <- ifelse(PD$q_dist >= 5, 1, 0)
PD$Qpre[is.na(PD$Qpre)] <- 0
PD$Q5[is.na(PD$Q5)] <- 0



for(i in -12:-1){PD[, paste0("Q_",abs(i))] <- ifelse(PD[,"q_dist"] %in% i, 1, 0)}
for(i in 0:4){PD[, paste0("Q",abs(i))] <- ifelse(PD[,"q_dist"] %in% i, 1, 0)}
PD$mon <- as.character(substr(PD$date,1,7))

# exclude the month before Speed Limit Change Date
# PD <- PD[!(PD$tr_dist %in% c(-1)),]


# Create Data for Each Model ------------------------------------------------
# Model 1 - Control Only

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



 TS <- PD
 TS$tr_group[is.na(TS$buff200) & TS$tr_group %in% "co"]  <- "tco"
 TS$tr_group[is.na(TS$buff1600) & TS$tr_group %in% "tco"]  <- "tco2"
 
 TS <- TS[TS$tr_group %in% c("tco"),]
 
 #PP <- TS[!duplicated(TS$point_id),]
 #ggplot() + geom_point(aes(x=lon,y=lat, colour = tr_group), data = PP)


 # Regressions ----------------------------------------
 m5 <- femlm(accidents_m ~ Q_12 + Q_11 + Q_10 + Q_9 + 
                           Q_8 + Q_7 + Q_6 + Q_5 + Q_4 + Q_3  + Q_1 +
                           Q0 + Q1 + Q2 + Q3 + Q4 + Q5 +
                           camera + log_fuel + log_cameras + t | point_id,
             data = TS)


 a5 <- gentab(m5, 5, TS)
 a5 <- a5[1:17,]

a <- a5
a$d <- gsub("Q", "", a$v)
a$d <- gsub("_", "-", a$d)
#a$d <- gsub("pos", "7", a$d)
#a$d <- gsub("pre", "-9", a$d)
a$d <- as.numeric(a$d)

a$d <- a$d
rownames(a) <- paste(a$v, a$m)

a$star <- ifelse(a$p < 0.001, "***",
          ifelse(a$p < 0.01, "**",
          ifelse(a$p < 0.05, "*"," ")))

a$up <- a$b + 1.96*a$se
a$do <- a$b - 1.96*a$se

# signs and banners
sb <- as.data.frame(matrix(nrow = 1, ncol = 0))
sb$x0 <- -1
sb$xf <- 0
sb$y0 <- -Inf
sb$yf <- Inf

a0 <- a[1,]
a0[,1:4] <- 0
a0[,10:11] <- 0
a0$d <- c(-2)
a0$m <- c("m5")
a <- rbind(a,a0)

p1 <- ggplot() + 
 geom_hline(yintercept = 0, alpha = 0.15)+
 geom_point(aes(x=d+0.5, y=b, colour = m), data = a) +
 geom_errorbar(aes(x = d+0.5, ymin = do, ymax = up, colour = m), data = a, width = 0)+
 geom_vline(xintercept = 0, linetype = 2, colour ="red") +
 geom_rect(aes(xmin = x0, xmax = xf, ymin = y0, ymax = yf, fill = "sb"), alpha = 0.15, data = sb) +
 coord_cartesian(ylim =c(-0.5,0.5)) +
 theme_bw() +
 theme(panel.grid = element_blank(), legend.position = "top",
       plot.title = element_text(hjust = 0.5),
       plot.subtitle = element_text(hjust = 0.5)) +
 labs(x = "quarters after speed limit reduction", y = "changes in accidents",
      title = paste0("A: control segments 200m-1600m from treatement")) +
 scale_y_continuous(labels = percent) +
 scale_colour_manual(values = c("m5" = "#330000"),
                     labels = c("m5" = paste0("Non-treated segments\n(200m-1600m from treatment)")),
                     name = "Control Group:  ") +
 scale_fill_manual(values = c("sb" = "purple"),
                   labels = c("sb" = "signs and banners\non treated segm."),
                   name = "") +
 scale_x_continuous(breaks = seq(-19,8,1))







TS <- PD
TS$tr_group[is.na(TS$buff200) & TS$tr_group %in% "co"]  <- "tco"
TS$tr_group[is.na(TS$buff1600) & TS$tr_group %in% "tco"]  <- "tco2"

TS <- TS[TS$tr_group %in% c("tco2"),]

#PP <- TS[!duplicated(TS$point_id),]
#ggplot() + geom_point(aes(x=lon,y=lat, colour = tr_group), data = PP)


# Regressions ----------------------------------------
m5 <- femlm(accidents_m ~ Q_12 + Q_11 + Q_10 + Q_9 + 
                           Q_8 + Q_7 + Q_6 + Q_5 + Q_4 + Q_3  + Q_1 +
                           Q0 + Q1 + Q2 + Q3 + Q4 + Q5 +
             camera + log_fuel + log_cameras + t | point_id,
            data = TS)


a5 <- gentab(m5, 5, TS)
a5 <- a5[1:17,]

a <- a5
a$d <- gsub("Q", "", a$v)
a$d <- gsub("_", "-", a$d)
#a$d <- gsub("pos", "7", a$d)
#a$d <- gsub("pre", "-9", a$d)
a$d <- as.numeric(a$d)

a$d <- a$d
rownames(a) <- paste(a$v, a$m)

a$star <- ifelse(a$p < 0.001, "***",
                 ifelse(a$p < 0.01, "**",
                        ifelse(a$p < 0.05, "*"," ")))

a$up <- a$b + 1.96*a$se
a$do <- a$b - 1.96*a$se

# signs and banners
sb <- as.data.frame(matrix(nrow = 1, ncol = 0))
sb$x0 <- -1
sb$xf <- 0
sb$y0 <- -Inf
sb$yf <- Inf

a0 <- a[1,]
a0[,1:4] <- 0
a0[,10:11] <- 0
a0$d <- c(-2)
a0$m <- c("m5")
a <- rbind(a,a0)

p2 <- ggplot() + 
 geom_hline(yintercept = 0, alpha = 0.15)+
 geom_point(aes(x=d+0.5, y=b, colour = m), data = a) +
 geom_errorbar(aes(x = d+0.5, ymin = do, ymax = up, colour = m), data = a, width = 0)+
 geom_vline(xintercept = 0, linetype = 2, colour ="red") +
 geom_rect(aes(xmin = x0, xmax = xf, ymin = y0, ymax = yf, fill = "sb"), alpha = 0.15, data = sb) +
 coord_cartesian(ylim =c(-0.5,0.5)) +
 theme_bw() +
 theme(panel.grid = element_blank(), legend.position = "top",
       plot.title = element_text(hjust = 0.5),
       plot.subtitle = element_text(hjust = 0.5)) +
 labs(x = "quarters after speed limit reduction", y = "changes in accidents",
      title = paste0("B: control segments >1600m from treatement")) +
 scale_y_continuous(labels = percent) +
 scale_colour_manual(values = c("m5" = "#330000"),
                     labels = c("m5" = paste0("Non-treated segments\n(>1600m from treatment)")),
                     name = "Control Group:  ") +
 scale_fill_manual(values = c("sb" = "purple"),
                   labels = c("sb" = "signs and banners\non treated segm."),
                   name = "") +
 scale_x_continuous(breaks = seq(-19,8,1))

plot_grid(p1,NULL,p2,nrow=3,ncol=1, rel_heights = c(1,0.1,1))
ggsave(paste0(OP.p,"event_study_on_controls.pdf"), w = 12, h =12)
