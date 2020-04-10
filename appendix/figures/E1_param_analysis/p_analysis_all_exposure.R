
# clear memory
rm(list=ls())
gc()

# required packages
library(ggplot2)
library(Cairo)
library(data.table)
library(scales)
library(lfe)
library(cowplot)

# inputs ----------------------------------------------------------------------

# BENEFITS
BEp <- "tables/5_cba/A_cba_benefits.csv"
# COSTS
COp <- "tables/5_cba/cost_scenario_3.csv"
# ACCIDENTS
ADp <- "data/accidents/accidents.rds"
# INFLATION
IDp <- "data/inflation/inflation.csv"


# VSL 0
VSL0 <- 3862030.46

# output ----------------------------------------------------------------------
OPp <- "appendix/figures/E1_param_analysis/"



# read data -----------------------------------------------------------------------------------
# accidents
AD <- readRDS(ADp)

# inflation
ID <- read.csv(IDp)
inflation <- ID$index[ID$date == as.character("2016-12-01")]/ID$index[ID$date == as.character("2014-12-01")]

AD <- AD[!is.na(AD$damages),]
AD$non_fatal_damages <- AD$damages - (AD$mortos*VSL0)
AD <- AD[AD$non_fatal_damages > 0,]


# my alternative VSLs
my_vsl <- seq(0, 2*VSL0, VSL0/99.5)

# calculate mean cost of accidents for each alternative value of the VSL
OD <- as.data.frame(matrix(nrow=200,ncol=0))
OD$vsl <- 0
OD$mca <- 0

for(i in 1:length(my_vsl)){#i<-190
 AD$damages_temp <- AD$non_fatal_damages + (AD$mortos*my_vsl[i])
 AD$cost_temp <- AD$damages_temp*inflation
 OD$vsl[i] <- my_vsl[i]
 OD$mca[i] <- mean(AD$cost_temp, na.rm=T)
}


# BENEFITS
BE <- read.csv(BEp)
# averted accidents
AVAC <- BE$av_m0[BE$scen=="SLR_marg"]

# COSTS
CO <- read.csv(COp)
CO <- CO[2:6,]
CO$vot <- c("iw100", "iw50", "mw100", "mw50", "vtpi")


# policy benefits by VSl
OD$ben <- OD$mca*AVAC/10^6
OD$vsl <- OD$vsl/10^6

OD$vtpi_ben <- OD$ben + CO$lost[CO$vot=="vtpi"]
OD$abs_vtpi_ben <- abs(OD$vtpi_ben)

VSL_break0 <- OD$vsl[OD$abs_vtpi_ben==min(OD$abs_vtpi_ben)]


my_p <- list()


my_p[[1]] <- ggplot() +
 geom_segment(aes(x=VSL0/10^6, xend=VSL0/10^6,
                  y=0, yend=150, linetype="vsl"), colour="firebrick") +
 geom_segment(aes(x=VSL_break0, xend=VSL_break0,
                  y=0, yend=155, linetype="vsl")) +
 geom_segment(aes(x=0, xend=7.15,
                  y=-lost, yend=-lost, linetype="c"), data=CO) +
 geom_segment(aes(x=0, xend=7.15,
                  y=-CO$lost[CO$vot=="vtpi"], yend=-CO$lost[CO$vot=="vtpi"],
                  linetype="c"), data=CO, colour="firebrick", size=1) +
 geom_line(aes(x=vsl,y=ben,linetype="b"),data=OD[OD$vsl <=7,]) +
 coord_cartesian(ylim = c(0,155),
                 xlim=c(0,7.15), clip = "off") +
 scale_x_continuous(breaks = 0:10, expand = c(0,0)) +
 scale_y_continuous(breaks = seq(0,155,25), expand = c(0,0)) +
 labs(x="VSL\n(R$ million)", y="R$ million",
      title = "PANEL A: Benefits Based on Event Study Without Controls\n") +
 theme_cowplot() +
 theme(plot.margin = margin(5, 2, 0, 0, "cm"),
       legend.position=c(0.5,1.3), legend.direction = "horizontal",
       legend.justification = 0.5, legend.text = element_text(size=18),
       plot.title = element_text(size=20)) +
 annotate("text", label = "baseline VSL",
          x = VSL0/10^6+0.05, y = 150, size = 5, hjust=0, colour="firebrick") +
 annotate("text", label = "breakeven VSL\nVOT=VTPI",
          x = VSL_break0, y = 150, size = 5, hjust=0) +
 annotate("text", label = "benefits from\naverted accidents",
          x = 7.15, y = 137.5, size = 5, hjust=1) +
 annotate("text", label = "VOT = individual net wages",
          x = 7.15, y = -CO$lost[CO$vot=="iw100"]+1,
          size = 5, hjust=1, vjust=0) +
 annotate("text", label = "VOT = median net wage",
          x = 7.15, y = -CO$lost[CO$vot=="mw100"]+1,
          size = 5, hjust=1, vjust=0) +
 annotate("text", label = "VOT = 50% of individual net wages",
          x = 7.15, y = -CO$lost[CO$vot=="iw50"]+1,
          size = 5, hjust=1, vjust=0) +
 annotate("text", label = "baseline VOT = VTPI",
          x = 7.15, y = -CO$lost[CO$vot=="iw50"]-1,
          size = 5, hjust=1, vjust=1, colour="firebrick") +
 annotate("text", label = "VOT = 50% of median net wage",
          x = 7.15, y = -CO$lost[CO$vot=="mw50"]-1,
          size = 5, hjust=1, vjust=1) +
 scale_linetype_manual(values = c("vsl" = 2, "c" = 3, "b" = 1),
                       labels = c("vsl" = "VSL            ",
                                  "b" = "total policy benefits\nfrom averted accidents            ",
                                  "c" = "total policy cost\ndue to additional time in traffic            "),
                       name = "") +
 guides(linetype = guide_legend(override.aes=list(shape = 15,
                                                  size = 5,
                                                  linetype = 0))) +
 geom_segment(aes(x=0.7,xend=0.85,y=195,yend=208)) +
 geom_segment(aes(x=2.825,xend=3.025,y=200.5,yend=200.5), linetype=3) +
 geom_segment(aes(x=5.55,xend=5.55,y=195,yend=208), linetype=2)







# read data -----------------------------------------------------------------------------------
# accidents
AD <- readRDS(ADp)

# inflation
ID <- read.csv(IDp)
inflation <- ID$index[ID$date == as.character("2016-12-01")]/ID$index[ID$date == as.character("2014-12-01")]

AD <- AD[!is.na(AD$damages),]
AD$non_fatal_damages <- AD$damages - (AD$mortos*VSL0)
AD <- AD[AD$non_fatal_damages > 0,]


# my alternative VSLs
my_vsl <- seq(0, 2*VSL0, VSL0/99.5)

# calculate mean cost of accidents for each alternative value of the VSL
OD <- as.data.frame(matrix(nrow=200,ncol=0))
OD$vsl <- 0
OD$mca <- 0

for(i in 1:length(my_vsl)){#i<-190
 AD$damages_temp <- AD$non_fatal_damages + (AD$mortos*my_vsl[i])
 AD$cost_temp <- AD$damages_temp*inflation
 OD$vsl[i] <- my_vsl[i]
 OD$mca[i] <- mean(AD$cost_temp, na.rm=T)
}


# BENEFITS
BE <- read.csv(BEp)
# averted accidents
AVAC <- BE$av_m3[BE$scen=="SLR_marg"]

# COSTS
CO <- read.csv(COp)
CO <- CO[2:6,]
CO$vot <- c("iw100", "iw50", "mw100", "mw50", "vtpi")


# policy benefits by VSl
OD$ben <- OD$mca*AVAC/10^6
OD$vsl <- OD$vsl/10^6

OD$vtpi_ben <- OD$ben + CO$lost[CO$vot=="vtpi"]
OD$abs_vtpi_ben <- abs(OD$vtpi_ben)

OD$med100vot_ben <- OD$ben + CO$lost[CO$vot=="mw100"]
OD$abs_med100_ben <- abs(OD$med100vot_ben)


OD$ind100vot_ben <- OD$ben + CO$lost[CO$vot=="iw100"]
OD$abs_ind100_ben <- abs(OD$ind100vot_ben)


VSL_break <- OD$vsl[OD$abs_vtpi_ben==min(OD$abs_vtpi_ben)]
VSL_break2 <- OD$vsl[OD$abs_med100_ben==min(OD$abs_med100_ben)]
VSL_break3 <- OD$vsl[OD$abs_ind100_ben==min(OD$abs_ind100_ben)]

VSL_break/(VSL0/(10^6))

my_p[[2]] <- ggplot() +
 geom_segment(aes(x=VSL0/10^6, xend=VSL0/10^6,
                  y=0, yend=150, linetype="vsl"), colour="firebrick") +
 geom_segment(aes(x=VSL_break, xend=VSL_break,
                  y=0, yend=150, linetype="vsl")) +
 geom_segment(aes(x=0, xend=7.15,
                  y=-lost, yend=-lost, linetype="c"), data=CO) +
 geom_segment(aes(x=0, xend=7.15,
                  y=-CO$lost[CO$vot=="vtpi"], yend=-CO$lost[CO$vot=="vtpi"],
                  linetype="c"), data=CO, colour="firebrick", size=1) +
 geom_line(aes(x=vsl,y=ben,linetype="b"),data=OD[OD$vsl <=7,]) +
 coord_cartesian(ylim = c(0,155),
                 xlim=c(0,7.15), clip = "off") +
 scale_x_continuous(breaks = 0:10, expand = c(0,0)) +
 scale_y_continuous(breaks = seq(0,155,25), expand = c(0,0)) +
 labs(x="VSL\n(R$ million)", y="R$ million",
      title = "PANEL B: Benefits Based on Event Study with Controls\n(Sample 3 - Matched Controls)\n") +
   theme_cowplot() +
   theme(plot.margin = margin(0, 2, 0, 0, "cm"),
       legend.position="none",
       plot.title = element_text(size=20)) +
 annotate("text", label = "baseline VSL",
          x = VSL0/10^6+0.05, y = 150, size = 5, hjust=0, colour="firebrick") +
 annotate("text", label = "breakeven VSL\nVOT=VTPI",
          x = VSL_break+0.05, y = 150, size = 5, hjust=0) +
 annotate("text", label = "benefits from\naverted accidents",
          x = 7.15, y = 100.5, size = 5, hjust=1) +
 annotate("text", label = "VOT = individual net wages",
          x = 7.15, y = -CO$lost[CO$vot=="iw100"]+1,
          size = 5, hjust=1, vjust=0) +
 annotate("text", label = "VOT = median net wage",
          x = 7.15, y = -CO$lost[CO$vot=="mw100"]+1,
          size = 5, hjust=1, vjust=0) +
 annotate("text", label = "VOT = 50% of individual net wages",
          x = 7.15, y = -CO$lost[CO$vot=="iw50"]+1,
          size = 5, hjust=1, vjust=0) +
 annotate("text", label = "baseline VOT = VTPI",
          x = 7.15, y = -CO$lost[CO$vot=="iw50"]-1,
          size = 5, hjust=1, vjust=1, colour="firebrick") +
 annotate("text", label = "VOT = 50% of median net wage",
          x = 7.15, y = -CO$lost[CO$vot=="mw50"]-1,
          size = 5, hjust=1, vjust=1) +
 scale_linetype_manual(values = c("vsl" = 2, "c" = 3, "b" = 1),
                       labels = c("vsl" = "VSL            ",
                                  "b" = "total policy benefits\nfrom averted accidents            ",
                                  "c" = "total policy cost\ndue to additional time in traffic            "),
                       name = "",
                       show)
 



plot_grid(my_p[[1]],NULL,my_p[[2]],nrow=3,ncol=1, rel_heights = c(1,0.1,1))
ggsave(paste0(OPp,"p_analysis_all_exposure.pdf"), w = 14, h =20)



# VSL ratio (breakeven/baseline)
VSL_break/(VSL0/10^6)
VSL_break2/(VSL0/10^6)