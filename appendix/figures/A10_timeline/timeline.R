# clear memory
rm(list=ls())
gc()

# inputs ----------------------------------------------------------------------
library(ggplot2)
library(scales)
library(Cairo)

# output (data with damages)
OP.p <- "appendix/figures/A10_timeline/timeline.pdf"

# main ----------------------------------------------------------------------------------------
# read data
MD <- as.data.frame(matrix(ncol=0, nrow = 3))
MD$x0 <- as.Date(c("2012-01-01", "2014-01-01",  "2016-07-04"))
MD$xf <- as.Date(c("2017-12-31", "2017-12-31",  "2017-09-01"))
MD$y0 <- c(0.3, 1.3,  2.3)
MD$yf <- c(0.7, 1.7,  2.7)
MD$g <- c("acc", "cam", "cra")

PD <- as.data.frame(matrix(ncol=0, nrow = 2))
PD$x0 <- as.Date(c("2015-07-20", "2015-01-01"))
PD$xf <- as.Date(c("2015-12-30", "2015-12-15"))
PD$y0 <- c(-Inf, -Inf)
PD$yf <- c(3, Inf)
PD$g <- c("reduction", "cameras")

ggplot() +
 geom_vline(aes(xintercept = as.Date("2017-01-25"), colour = "rt"), linetype = 2, alpha = 0.5) +
 geom_rect(aes(xmin = x0, xmax = xf, ymin = y0, ymax = yf, fill = g), data = PD, alpha = 0.1) +
 geom_rect(aes(xmin = x0, xmax = xf, ymin = y0, ymax = yf), fill = "gray80", colour = "gray70", data = MD, alpha = 0.5) +
 theme_bw() +
 theme(panel.grid.minor = element_blank(),
       panel.grid.major.y = element_blank(),
       legend.position = "top") +
 scale_y_continuous(breaks=c(0.5,1.5,2.5),
                    labels=c("Accidents", "Cameras", "Crawled Trips")) +
 scale_x_date(date_breaks = "1 year", labels = date_format("%Y")) +
 scale_fill_manual(values = c("reduction" = "red", "cameras" = "gold"),
                   limits = c("reduction", "cameras"),
                   labels = c("reduction" = "Speed Limit    \nReductions",
                              "cameras" = "Camera Expansion "),
                   name = "Policy Changes:    ") + 
 labs(x = "Date\n", y = "Data") +
 scale_colour_manual(values = c("rt" = "forestgreen"),
                     labels = c("rt" = "Speed Limit    \nReversal"),
                     name = "") +
 guides(fill = guide_legend(order = 1), colour = guide_legend(order = 2))
ggsave(filename = OP.p, w = 8, h = 4)
 
 

