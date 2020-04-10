# clear memory
rm(list=ls())
gc()

# required packages
library(ggplot2)
library(Cairo)
library(scales)
library(cowplot)
library(data.table)

# inputs ----------------------------------------------------------------------
AD.p <- "data/accidents/accidents.rds"

# output
o1 <- "appendix/figures/C7_citywide/"


# main ----------------------------------------------------------------------------------------
# read data
AD <- readRDS(AD.p)


# define month
AD$yr_mo <- paste0(year(AD$date), "_", month(AD$date))

AD$mo <- as.character(month(AD$date))

# count accidents per month
AD$acc <- 1
AD$acc_mo <- ave(AD$acc, AD$yr_mo, FUN=sum)


# dataset per month
MA <- AD[!duplicated(AD$yr_mo),]


# plot raw data

d0 <- as.Date("2012-01-01")
d1 <- as.Date("2015-07-01")
d2 <- as.Date("2016-01-01")
d3 <- as.Date("2017-01-01")

td1 <- (d1 - d0)/31
td2 <- (d2 - d0)/31
td3 <- (d3 - d2)/31

ggplot() +
 geom_vline(aes(xintercept = as.Date("2015-07-01"), colour = "slr"), linetype=2) +
 geom_vline(aes(xintercept = as.Date("2017-01-25"), colour = "sli"), linetype=2) +
 geom_rect(aes(xmin=d1, xmax=d2, ymin=-Inf, ymax=Inf, fill="reds"), alpha=0.2) +
 geom_point(aes(x=date, y = acc_mo), data=MA, size=1) +
 coord_cartesian(ylim = c(0,3000)) +
 scale_fill_manual(values=c("reds"="red"),
                   labels=c("reds"="speed limit reductions"),
                   name="") + 
 scale_colour_manual(values=c("slr"="red", "sli" = "blue"),
                     labels=c("slr"="first speed\nlimit reduction",
                              "sli" = "speed limit reversal\n(Marg. high. only)"),
                     name="Legend:    ",
                     limits = c("slr", "sli")) +
 theme(legend.position = "top") +
 guides(colour = guide_legend(order = 1), 
        fill = guide_legend(order = 2)) +
 scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
 labs(y="accidents per month",
      x="date")
ggsave(paste0(o1,"raw.png"), h=6,w=8, type="cairo-png")









MA$yr_mo <- as.factor(MA$yr_mo)
MA$yr_mo <- relevel(MA$yr_mo, ref="2015_6")

MA$d <- as.Date(paste0(year(MA$date),"-",month(MA$date), "-15"))
MA$t <- (MA$d - as.Date("2012-01-15"))/31

MAT <- MA[MA$date < as.Date("2015-07-01"),]
MAT$t <- (MAT$d - as.Date("2012-01-15"))/31



pm <- lm(acc_mo ~ mo + t, data=MAT)
summary(pm)
MA$pred <- predict.lm(pm, MA, se.fit=T)[[1]]
MA$pred_se <- predict.lm(pm, MA, se.fit=T)[[2]]
MA$pred_obs <-  MA$acc_mo - MA$pred


MA$pred_up <- MA$pred_obs + 1.96*MA$pred_se
MA$pred_do <- MA$pred_obs - 1.96*MA$pred_se


ggplot() +
 geom_hline(aes(yintercept = 0), alpha = 0.5) +
 geom_vline(aes(xintercept = as.Date("2015-07-01"), colour = "slr"), linetype=2) +
 geom_vline(aes(xintercept = as.Date("2017-01-25"), colour = "sli"), linetype=2) +
 geom_rect(aes(xmin=d1, xmax=d2, ymin=-Inf, ymax=Inf, fill="reds"), alpha=0.2) +
 geom_point(aes(x=d, y = pred_obs),size=1, data=MA[MA$date < as.Date("2017-01-01"),]) +
 geom_errorbar(aes(x=d, ymin=pred_do, ymax=pred_up), data=MA[MA$date < as.Date("2017-01-01"),], width=0) +
 coord_cartesian(ylim = c(-700,700)) +
 scale_fill_manual(values=c("reds"="red"),
                   labels=c("reds"="speed limit reductions"),
                   name="") + 
 scale_colour_manual(values=c("slr"="red", "sli" = "blue"),
                     labels=c("slr"="first speed\nlimit reduction",
                              "sli" = "speed limit reversal\n(Marg. High. only)"),
                     name="Legend:    ",
                     limits = c("slr", "sli")) +
 theme(legend.position = "top") +
 guides(colour = guide_legend(order = 1), 
        fill = guide_legend(order = 2)) +
 scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
 scale_y_continuous(breaks = seq(-800,800,200)) +
 labs(y="number of accidents\n(observed - predicted)",
      x="date")
ggsave(paste0(o1,"obs_pred.png"), h=6,w=8, type="cairo-png")











ggplot() +
 geom_vline(aes(xintercept = as.Date("2015-07-01"), colour = "slr"), linetype=2) +
 geom_vline(aes(xintercept = as.Date("2017-01-25"), colour = "sli"), linetype=2) +
 geom_rect(aes(xmin=d1, xmax=d2, ymin=-Inf, ymax=Inf, fill="reds"), alpha=0.2) +
 geom_point(aes(x=date, y = acc_mo, shape="obs", colour="obs"), data=MA[MA$date<as.Date("2017-01-01"),], size=2) +
 geom_point(aes(x=date, y = pred, shape="pred", colour="pred"), data=MA[MA$date<as.Date("2017-01-01"),], size=2) +
 coord_cartesian(ylim = c(0,3000)) +
 scale_fill_manual(values=c("reds"="red"),
                   labels=c("reds"="speed limit reductions"),
                   name="") + 
 scale_colour_manual(values=c("slr"="red", "sli" = "blue", "pred"="black", "obs"="red"),
                     labels=c("slr"="first speed\nlimit reduction",
                              "sli" = "speed limit reversal\n(Marg. high. only)"),
                     name="",
                     limits = c("slr", "sli")) +
 scale_shape_manual(values=c("obs"=20, "pred"=18),
                     labels=c("obs" = "observed accidents",
                              "pred" = "predicted accidents"),
                     name="",
                     limits = c("obs", "pred")) +
 theme(legend.position = "top", legend.box = "vertical",
       legend.box.just = "left") +
 guides(colour = guide_legend(order = 1, nrow=1, override.aes = list(shape ="") ), 
        fill = guide_legend(order = 2, nrow=1),
        shape = guide_legend(order=3, nrow=1,override.aes = list(colour =c("red","black") ) ) ) +
 scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
 labs(y="accidents per month",
      x="date")
ggsave(paste0(o1,"predobs_raw.pdf"), h=6,w=8)


MBA <- MA[MA$date > as.Date("2015-07-01") & MA$date < as.Date("2017-01-01"), c("date", "acc_mo", "pred")]
sum(MBA$acc_mo)
sum(MBA$pred)
(sum(MBA$pred) - sum(MBA$acc_mo))/sum(MBA$pred)


sum(MA$acc_mo[MA$date >= as.Date("2014-07-01") & MA$date < as.Date("2015-07-01")])
5471/22327
1858/5471





MAP <- MA[MA$date < as.Date("2014-07-01"),]
pl <- lm(acc_mo ~ mo + t, data=MAP)
summary(pl)
MA$pred_pl <- predict.lm(pl, MA, se.fit=T)[[1]]
MA$pred_se_pl <- predict.lm(pl, MA, se.fit=T)[[2]]
MA$pred_obs_pl <-  MA$acc_mo - MA$pred_pl

MA$pred_pl_up <- MA$pred_obs_pl + 1.96*MA$pred_se_pl
MA$pred_pl_do <- MA$pred_obs_pl - 1.96*MA$pred_se_pl



ggplot() +
 geom_hline(aes(yintercept = 0), alpha = 0.5) +
 geom_vline(aes(xintercept = as.Date("2014-07-01"), colour = "slr"), linetype=2) +
 geom_errorbar(aes(x=d, ymin=pred_pl_do, ymax=pred_pl_up), data=MA[MA$date < as.Date("2015-07-01"),], width=0) +
 geom_point(aes(x=d, y = pred_obs_pl), data=MA[MA$date < as.Date("2015-07-01"),]) +
 coord_cartesian(ylim = c(-700,700)) +
 scale_colour_manual(values=c("slr"="goldenrod"),
                     labels=c("slr"="placebo speed\nlimit reduction"),
                     name="Legend:    ") +
 theme(legend.position = "top") +
 scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
 labs(y="number of accidents\n(observed - predicted)",
      x="date") +
 scale_y_continuous(breaks = seq(-800,800,200))
ggsave(paste0(o1,"obs_pred_placebo.png"), h=6,w=8, type="cairo-png")



MBA <- MA[MA$date > as.Date("2015-07-01") & MA$date < as.Date("2017-01-01"), c("date", "pred_obs", "pred")]
