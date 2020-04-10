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
library(lubridate)

# inputs ----------------------------------------------------------------------
CD.p <- "data/crawler/crawled_data_car.rds"

# output
OP.p <- "appendix/figures/A8_travel_time_raw_plot/travel_time_raw_plot.pdf"


# main ----------------------------------------------------------------------------------------
# read data
CD <- readRDS(CD.p)

# fix NAs
CD$length.marg[is.na(CD$length.marg)] <- 0
CD$length.marg[CD$length.marg < 100] <- 0
CD$marg.ratio[is.na(CD$marg.ratio)] <- 0

CD$osrm.length[is.na(CD$osrm.length)] <- 0

CD$length.art[is.na(CD$length.art)] <- 0
CD$length.art[CD$length.art < 200] <- 0
CD$arterial.ratio[is.na(CD$arterial.ratio)] <- 0

#CD$length.ns[is.na(CD$length.ns)] <- 0
#CD$length.ns[CD$length.ns < 200] <- 0
#CD$northsouth.ratio[is.na(CD$northsouth.ratio)] <- 0


# count kms on Marginais and other roads
CD$tot.km <- CD$osrm.length/1000
CD$marg.km <- CD$length.marg/1000
CD$art.km <- CD$length.art/1000
CD$oth.km <- with(CD, tot.km - marg.km - art.km)


# treatment status (Speed Limit Reduction)
CD$SLI <- ifelse(CD$date >= as.Date("2017-01-25"), 1, 0)
CD$SLI.d <- as.numeric(CD$date - as.Date("2017-01-25"))


CD$SLI.w <- floor(CD$SLI.d/7)
CD$SLI.m <- floor(CD$SLI.d/28)
CD$SLI.q <- floor(CD$SLI.d/84)
CD$SLI.s <- floor(CD$SLI.d/365)

CD$SLI.w_f <- as.factor(as.character(CD$SLI.w))
CD$SLI.m_f <- as.factor(as.character(CD$SLI.m))
CD$SLI.q_f <- as.factor(as.character(CD$SLI.q))
CD$SLI.s_f <- as.factor(as.character(CD$SLI.s))

# count observations per week
CD$obs_w <- ave(CD$obs, CD$SLI.w_f, FUN=sum)
CD$obs_m <- ave(CD$obs, CD$SLI.m_f, FUN=sum)
CD$obs_q <- ave(CD$obs, CD$SLI.q_f, FUN=sum)
CD$obs_s <- ave(CD$obs, CD$SLI.s_f, FUN=sum)

# travel time in hours
CD$tr.time_hr <- CD$tr.time/60
CD$log_tr.time <- log(CD$tr.time_hr)


# travel time in hours
CD$oth.ratio <- 1 - CD$marg.ratio - CD$arterial.ratio
CD$log_tr.time <- log(CD$tr.time_hr)

# exclude driving restriction period
# CD <- CD[CD$Susp_Driv_Restr == 0,]
#CD <- CD[CD$SLI.w_f %in% c(-1,0),]
CD$month_chr <- as.character(CD$month)
CD$year_chr <- as.character(CD$year)
CD$month_year <- paste(CD$month, CD$year)

CD$length <- CD$osrm.length

CD$week <- CD$SLI.w_f
CD$month <- CD$SLI.m_f
CD$quarter <- CD$SLI.q_f
CD$semester <- CD$SLI.s_f

CD$ratio.marg_spill_1000[is.na(CD$ratio.marg_spill_1000)] <- 0
CD$ratio.marg_spill_3000[is.na(CD$ratio.marg_spill_3000)] <- 0
CD$ratio.marg_spill_5000[is.na(CD$ratio.marg_spill_5000)] <- 0
CD$ratio.marg_spill_7000[is.na(CD$ratio.marg_spill_7000)] <- 0

CD$ratio.marg_spill_1000[is.na(CD$ratio.marg_spill_1000)] <- 0
CD$ratio.marg_spill_3000[is.na(CD$ratio.marg_spill_3000)] <- 0
CD$ratio.marg_spill_5000[is.na(CD$ratio.marg_spill_5000)] <- 0
CD$ratio.marg_spill_7000[is.na(CD$ratio.marg_spill_7000)] <- 0


CD$mean <- ave(CD$tr.time_hr, CD$trip_id, FUN=mean)
CD$rel_time <- CD$tr.time_hr/CD$mean

CD$gr <- ifelse(CD$marg.km >0.2, "tr", "co")


CD$w <- as.numeric(as.character(CD$week))
CD$week_gr <- paste(CD$gr, CD$w)


CD$rel_time_w <- ave(CD$rel_time, CD$week_gr, FUN=mean)

DD <- CD[!duplicated(CD$week_gr),c("week_gr", "rel_time_w", "gr", "w", "date")]
DD$w <- ifelse(DD$gr %in% "tr", DD$w + 0.55, DD$w+0.45)


ggplot() +
 geom_hline(yintercept = 1, alpha = 0.25, colour = "gray") +
 geom_vline(aes(colour = "sc", xintercept = as.Date("2017-01-25")), linetype = 2) +
 geom_point(aes(x=date, y=rel_time_w, colour = gr, pch=gr),data=DD, size = 1) +
 coord_cartesian(ylim =c(0.8,1.2), xlim = c(as.Date("2016-06-20"),as.Date("2017-09-01"))) +
 theme_bw() +
 theme(panel.grid = element_blank(), legend.position = "top", plot.title = element_text(hjust = 0.5)) +
 labs(x = "date\n",
      y = "average travel time\n(relative to pre-treatment mean)") +
 scale_colour_manual(values = c("tr" = "firebrick", "co" = "gray20", "sc" = "forestgreen"),
                     labels = c("tr" = "treatment\n(trips on Marginais Highways)",
                                "co" = "control\n(trips on all other roads)",
                                "sc" = "speed limit increase\n(Marginais Highways)"),
                     limits = c("tr", "co", "sc"),
                     name = "",
                     guide = guide_legend(override.aes = list(linetype = c("blank", "blank", "dashed"),
                                                              shape = c(16, 2, NA)))) +
 scale_shape_manual(values = c("tr" = 16, "co" = 2, "sc" = 16),
                    labels = c("tr" = "treatment\n(trips on Marginais Highways)",
                               "co" = "control\n(trips on all other roads)",
                               "sc" = "speed limit increase\n(Marginais Highways)"),
                    limits = c("tr", "co", "sc"),
                    name = "",
                    guide = guide_legend(override.aes = list(linetype = c("blank", "blank", "dashed"),
                                                             shape = c(16, 2, NA)))) +
 scale_x_date(date_breaks = "2 month", date_labels = "%b\n%Y") +
 geom_text(aes(x = as.Date("2017-04-15"), y = 1.145),
           label = "period with\nmissing data", size = 2, hjust = 0.5, alpha = 0.5) + 
 geom_segment(aes(x = as.Date("2017-03-20"), y = 1.12, xend = as.Date("2017-05-15"), yend = 1.12), size = 0.25, alpha = 0.5) +
 geom_segment(aes(x = as.Date("2017-03-20"), y = 1.115, xend = as.Date("2017-03-20"), yend = 1.125), size = 0.25, alpha = 0.5) +
 geom_segment(aes(x = as.Date("2017-05-15"), y = 1.115, xend = as.Date("2017-05-15"), yend = 1.125), size = 0.25, alpha = 0.5)
ggsave(filename = OP.p, w = 8, h = 4)
