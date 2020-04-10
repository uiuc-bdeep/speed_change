#     ---------------------------------------------------------------------------------------- 
#   |  plot the evolution of the total number of speed cameras over time in São Paulo         |
#   |  Figure 2                                                                                       |
#   |  By:  Renato Vieira                                                                     |
#   |       Big Data for Environmental Economics and Policy                                   |
#   |       University of Illinois at Urbana Chamapaign                                       |
#     ---------------------------------------------------------------------------------------- 

# Prelims -------------------------------------------------------------------------------------
# clear memory
rm(list=ls())
gc()

#required packages
library(ggplot2)
library(scales)


# inputs 
TD.p <- "data/tickets/tickets_over_segments.rds"

# output
OD.p <- "figures/2_cameras_over_time/cameras_over_time.pdf"

# Main ----------------------------------------------------------------------------------------
# read tickets data
TD <- readRDS(TD.p)


# count unique locations per year_month (LYM)
TD$place_month_id <- paste(TD$place.id, TD$year_month)

# aggregate by location-year_month
LYM <- TD[!duplicated(TD$place_month_id),]

# count observations per month
LYM$obs <- 1
LYM$cameras_m <- ave(LYM$obs, LYM$year_month, FUN=sum)

# aggregate by month (Cameras per month: CM)
CM <- LYM[!duplicated(LYM$year_month),]
# fix date to mid-month
CM$date <- as.Date(paste0(CM$year_month, "-15"))


# create speed limit treatment period data frame
CE <- as.data.frame(matrix(nrow=1,ncol=0))
CE$sx <- as.Date("2015-01-01")
CE$ex <- as.Date("2015-12-15")
CE$sy <- -Inf
CE$ey <- 1000


# create speed limit treatment period data frame
TP <- as.data.frame(matrix(nrow=1,ncol=0))
TP$sx <- as.Date("2015-07-01")
TP$ex <- as.Date("2015-12-30")
TP$sy <- -Inf
TP$ey <- 950

# create speed limit reversal treatment period
RP <- as.data.frame(matrix(nrow=1,ncol=0))
RP$sx <- as.Date("2017-01-17")
RP$ex <- as.Date("2017-01-24")
RP$sy <- -Inf
RP$ey <- 1000



# plot cameras per month
ggplot() +
 geom_rect(aes(xmin = sx, xmax = ex,
               ymin = sy, ymax = ey, fill = "ce"), alpha = 0.15, data = CE, size = 0.1) +
 geom_rect(aes(xmin = sx, xmax = ex,
               ymin = sy, ymax = ey, fill = "tr"), alpha = 0.15, data = TP, size = 0.1) +
 geom_point(aes(x = date, y = cameras_m), shape = 21, data = CM) +
 geom_vline(aes(xintercept = as.Date("2017-01-25"), colour = "rt"), linetype = 2, alpha = 0.5) +
 theme_bw() +
 scale_x_date(labels = date_format("%Y"),
              limits = c(as.Date("2014-01-01"), as.Date("2018-01-01")),
              date_breaks = "1 year") +
 scale_y_continuous(expand=c(0,1), limits = c(0, 1000), label = comma, breaks = seq(0,1000,200)) +
 theme(panel.grid = element_blank(), legend.position = "top") +
 labs(x = "Date\n", y = "Total Number of Speed Control Cameras") +
 scale_fill_manual(values = c("tr" = "red",
                              "ce" = "gold"),
                   labels = c("tr" = "Speed Limit    \nReductions",
                              "ce" = "Camera\nExpansion    "),
                   limits = c("ce", "tr"), 
                   name = "Legend:    ") +
 scale_colour_manual(values = c("rt" = "black"),
                     labels = c("rt" = "Speed Limit    \nReversal"),
                     name = "") +
 guides(fill = guide_legend(order = 1), colour = guide_legend(order = 2))
ggsave(plot = last_plot(), filename = OD.p, w = 6, h =4)

