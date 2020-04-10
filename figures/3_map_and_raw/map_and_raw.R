#     ---------------------------------------------------------------------------------------- 
#   |  plot raw accidents series and the location of segments per control group               |
#   |                                                                                         |
#   |  By:  Renato Vieira                                                                     |
#   |       Big Data for Environmental Economics and Policy                                   |
#   |       University of Illinois at Urbana Chamapaign                                       |
#     ---------------------------------------------------------------------------------------- 

# Prelims -------------------------------------------------------------------------------------

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

library(ggplot2)
library(Cairo)
library(ggmap)
library(rgdal)
library(rgeos)
library(ggsn)

library(png)
library(grid)

# inputs ----------------------------------------------------------------------
PD.p <- "data/panel/monthly_panel_all_b1k.rds"

# city area
city.dsn <- "data/city_area"
city.path <- "DEINFO_MUNICIPIO"



# output
OP.p <-"figures/3_map_and_raw/map_and_raw.pdf"




# main ----------------------------------------------------------------------------------------
# read data
PD <- readRDS(PD.p)
PD <- as.data.frame(PD)

# exclude point_id NA
PD <- PD[!is.na(PD$point_id),]

# subset to segments with more than 2 accidents
PD <- PD[PD$accidents_total >=1,]

# define groups
PD$tr_group <- ifelse(is.na(PD$ChangeDate), "co", "tr")

# count accidents before policy per segment
PD$pre <- ifelse(PD$date < as.Date("2015-07-01"), 1, 0)
PD$acc_pre <- PD$pre*PD$accidents_m
PD$acc_pre_seg <- ave(PD$acc_pre, PD$point_id, FUN=sum)


mp <- list()

# City Area
# read kml
CA.sp <- readOGR(city.dsn, city.path)
CA.sp <- spTransform(CA.sp, "+proj=longlat +ellps=WGS84")
CA <- fortify(CA.sp)


# map
mybox <- c(left = -46.83, bottom = -23.8, right = -46.362, top = -23.37)
mymap <- get_stamenmap(mybox, zoom = 8, maptype = "toner-lines")

mapbox <- as.data.frame(matrix(nrow = 4, ncol = 2))
mapbox$x <- c(-46.83, -46.83, -46.362, -46.362)
mapbox$y <- c(-23.8, -23.37, -23.37, -23.8)

#scale
img <- readPNG("figures/3_map_and_raw/scale.png")
g_pic <- rasterGrob(img, interpolate=TRUE)

# fig 1 - all data
M1 <- PD[!duplicated(PD$point_id),]

mp[[1]] <- 
 ggplot() +
 geom_path(aes(x=long, y = lat, group = group, colour = "sp"), data = CA, alpha = 0.5) +
 geom_point(aes(x=lon, y = lat, colour = tr_group),
            data = M1,alpha = 0.7, size = 0.2) +
 scale_colour_manual(values = c("tr" = "firebrick", "co" = "gray", "sp" = "black"),
                     labels = c("road" = "Roads With\nSpeed Limit  \nReduction",
                                "tr" = "Treated\nSegments",
                                "co" = "Control\nSegments",
                                "sp" = "S?o Paulo\nCity Limits"),
                     name = "Legend:  ",
                     limits = c("tr", "co", "sp"),
                     guide = guide_legend(override.aes = list(linetype = c("blank", "blank", "solid"),
                                                              shape = c(16, 16, NA),
                                                              size = c(1,1,1)))) +
 theme_void() +
 theme(legend.title=element_text(size=8, face = "bold"),
       legend.text=element_text(size=8),
       legend.position="top") +
 coord_cartesian(xlim = c(-46.81,-46.364), ylim = c(-23.78, -23.39)) +
 annotation_custom(g_pic, xmin=-46.5, xmax=-46.364, ymin=-23.78, ymax=-23.68)



# fig 2 - >20
M2 <- M1[M1$tr_group %in% "tr" | is.na(M1$buff1600),]

mp[[2]] <- 
 ggplot() +
 geom_path(aes(x=long, y = lat, group = group, colour = "sp"), data = CA, alpha = 0.5) +
 geom_point(aes(x=lon, y = lat, colour = tr_group),
            data = M2,alpha = 0.7, size = 0.2) +
 scale_colour_manual(values = c("tr" = "firebrick", "co" = "gray", "sp" = "black"),
                     labels = c("road" = "Roads With\nSpeed Limit  \nReduction",
                                "tr" = "Treated\nSegments",
                                "co" = "Control\nSegments",
                                "sp" = "S?o Paulo\nCity Limits"),
                     name = "Legend:  ",
                     limits = c("tr", "co", "sp"),
                     guide = guide_legend(override.aes = list(linetype = c("blank", "blank", "solid"),
                                                              shape = c(16, 16, NA),
                                                              size = c(1,1,1)))) +
 theme_void() +
 theme(legend.title=element_text(size=8, face = "bold"),
       legend.text=element_text(size=8),
       legend.position="top")+
 coord_cartesian(xlim = c(-46.81,-46.364), ylim = c(-23.78, -23.39)) +
 annotation_custom(g_pic, xmin=-46.5, xmax=-46.364, ymin=-23.78, ymax=-23.68)




# fig 3 -> Matched

# match
SD <- M2
SD <- SD[!duplicated(SD$point_id),]
SD$tr <- ifelse(SD$tr_group == "tr",1,0)
m.out <- matchit(tr ~ acc_pre_seg,
                 data = SD[,c("tr", "acc_pre_seg", "point_id")],
                 method = "nearest",
                 replace=T)

MS <- match.data(m.out)
match_segs <- unique(MS$point_id)
M4 <- M2[M2$point_id %in% match_segs,]

mp[[3]] <- 
 ggplot() +
 geom_path(aes(x=long, y = lat, group = group, colour = "sp"), data = CA, alpha = 0.5) +
 geom_point(aes(x=lon, y = lat, colour = tr_group),
            data = M4,alpha = 0.7, size = 0.2) +
 scale_colour_manual(values = c("tr" = "firebrick", "co" = "gray", "sp" = "black"),
                     labels = c("road" = "Roads With\nSpeed Limit  \nReduction",
                                "tr" = "Treated\nSegments",
                                "co" = "Control\nSegments",
                                "sp" = "S?o Paulo\nCity Limits"),
                     name = "Legend:  ",
                     limits = c("tr", "co", "sp"),
                     guide = guide_legend(override.aes = list(linetype = c("blank", "blank", "solid"),
                                                              shape = c(16, 16, NA),
                                                              size = c(1,1,1)))) +
 theme_void() +
 theme(legend.title=element_text(size=8, face = "bold"),
       legend.text=element_text(size=8),
       legend.position="top") +
 coord_cartesian(xlim = c(-46.81,-46.364), ylim = c(-23.78, -23.39)) +
 annotation_custom(g_pic, xmin=-46.5, xmax=-46.364, ymin=-23.78, ymax=-23.68)


#r1 <- plot_grid(mp[[1]], mp[[2]],
#                ncol = 2, nrow=1, align = "v", rel_widths = c(1, 1))
#r3 <- plot_grid(mp[[3]], mp[[4]],
#                ncol = 2, nrow=1, align = "v", rel_widths = c(1, 1))






# RAW DATA ########################################################################





# inputs ----------------------------------------------------------------------



# main ----------------------------------------------------------------------------------------
# read data
PD <- readRDS(PD.p)
PD <- as.data.frame(PD)

# exclude point_id NA
PD <- PD[!is.na(PD$point_id),]

# subset to segments with more than 2 accidents
PD <- PD[PD$accidents_total >=1,]

# define groups
PD$tr_group <- ifelse(is.na(PD$ChangeDate), "co", "tr")

# count accidents before policy per segment
PD$pre <- ifelse(PD$date < as.Date("2015-07-01"), 1, 0)
PD$acc_pre <- PD$pre*PD$accidents_m
PD$acc_pre_seg <- ave(PD$acc_pre, PD$point_id, FUN=sum)



# treatment period
sb <- as.data.frame(matrix(nrow = 1, ncol = 0))
sb$x0 <- as.Date("2015-07-20")
sb$xf <- as.Date("2015-12-30")
sb$y0 <- -Inf
sb$yf <- Inf


# fig 4 - all data
PD$gr_m <- paste(PD$tr_group, PD$date_m)
PD$acc_gr_m <- ave(PD$accidents_m, PD$gr_m, FUN=mean)
M1 <- PD[!duplicated(PD$gr_m),]
M1$acc_gr_rel <- ifelse(M1$tr_group == "tr",
                        M1$acc_gr_m/M1$acc_gr_m[M1$tr_group == "tr" & M1$date_m == "2015-06-01"],
                        M1$acc_gr_m/M1$acc_gr_m[M1$tr_group == "co" & M1$date_m == "2015-06-01"])

mp[[4]] <- 
 ggplot() + 
 geom_point(aes(x=date_m, y=acc_gr_rel, colour = tr_group, pch = tr_group), data = M1) +
 geom_rect(aes(xmin = x0, xmax = xf, ymin = y0, ymax = yf, fill = "slr"), alpha = 0.15, data = sb) +
 coord_cartesian(ylim =c(0,1.5)) +
 theme_bw() +
 theme(panel.grid = element_blank(), legend.position = "top", plot.title = element_text(hjust = 0.5)) +
 labs(x = "date\n", y = "accidents per month per km\n(relative to June 2015)") +
 scale_colour_manual(values = c("tr" = "firebrick", "co" = "gray50"),
                     labels = c("tr" = "treatment\n(arterial and highways)",
                                "co" = "control\n(non-treated avenues)"),
                     name = "") +
 scale_shape_manual(values = c("tr" = 16, "co" = 2),
                    labels = c("tr" = "treatment\n(arterial and highways)",
                               "co" = "control\n(non-treated avenues)"),
                    name = "") +
 scale_fill_manual(values = c("slr" = "red"),
                   labels = c("slr" = "speed limit\nreductions"),
                   name = "") +
 scale_x_date(date_breaks = "1 year", date_labels = "%Y") 


# fig 5 - control segments more than 1600m from treatment
SD <- PD[PD$tr_group =="tr" | is.na(PD$buff1600),]
SD$acc_gr_m <- ave(SD$accidents_m, SD$gr_m, FUN=mean)*2.5

M2 <- SD[!duplicated(SD$gr_m),]
M2$acc_gr_rel <- ifelse(M2$tr_group == "tr",
                        M2$acc_gr_m/M2$acc_gr_m[M2$tr_group == "tr" & M2$date_m == "2015-06-01"],
                        M2$acc_gr_m/M2$acc_gr_m[M2$tr_group == "co" & M2$date_m == "2015-06-01"])
mp[[5]] <- 
 ggplot() + 
 geom_point(aes(x=date_m, y=acc_gr_rel, colour = tr_group, pch = tr_group), data = M2) +
 geom_rect(aes(xmin = x0, xmax = xf, ymin = y0, ymax = yf, fill = "slr"), alpha = 0.15, data = sb) +
 coord_cartesian(ylim =c(0,1.5)) +
 theme_bw() +
 theme(panel.grid = element_blank(), legend.position = "top", plot.title = element_text(hjust = 0.5)) +
 labs(x = "date\n", y = "accidents per month per km\n(relative to June 2015)") +
 scale_colour_manual(values = c("tr" = "firebrick", "co" = "gray50"),
                     labels = c("tr" = "treatment\n(arterial and highways)",
                                "co" = "control\n(non-treated avenues)"),
                     name = "") +
 scale_shape_manual(values = c("tr" = 16, "co" = 2),
                    labels = c("tr" = "treatment\n(arterial and highways)",
                               "co" = "control\n(non-treated avenues)"),
                    name = "") +
 scale_fill_manual(values = c("slr" = "red"),
                   labels = c("slr" = "speed limit\nreductions"),
                   name = "") +
 scale_x_date(date_breaks = "1 year", date_labels = "%Y") 




# fig 6 - matched segments

# match
SS <- SD[!duplicated(SD$point_id),]
SS$tr <- ifelse(SS$tr_group == "tr",1,0)
m.out <- matchit(tr ~ acc_pre_seg,
                 data = SS[,c("tr", "acc_pre_seg", "point_id")],
                 method = "nearest",
                 replace=T)
MS <- match.data(m.out)
match_segs <- unique(MS$point_id)
MD <- SD[SD$point_id %in% match_segs,]
MD$acc_gr_m <- ave(MD$accidents_m, MD$gr_m, FUN=mean)*2.5
M4 <- MD[!duplicated(MD$gr_m),]
M4$acc_gr_rel <- ifelse(M4$tr_group == "tr",
                        M4$acc_gr_m/M4$acc_gr_m[M4$tr_group == "tr" & M4$date_m == "2015-06-01"],
                        M4$acc_gr_m/M4$acc_gr_m[M4$tr_group == "co" & M4$date_m == "2015-06-01"])
mp[[6]] <- 
 ggplot() + 
 geom_point(aes(x=date_m, y=acc_gr_rel, colour = tr_group, pch = tr_group), data = M4) +
 geom_rect(aes(xmin = x0, xmax = xf, ymin = y0, ymax = yf, fill = "slr"), alpha = 0.15, data = sb) +
 coord_cartesian(ylim =c(0,1.5)) +
 theme_bw() +
 theme(panel.grid = element_blank(), legend.position = "top", plot.title = element_text(hjust = 0.5)) +
 labs(x = "date\n", y = "accidents per month per km\n(relative to June 2015)") +
 scale_colour_manual(values = c("tr" = "firebrick", "co" = "gray50"),
                     labels = c("tr" = "treatment\n(arterial and highways)",
                                "co" = "control\n(non-treated avenues)"),
                     name = "") +
 scale_shape_manual(values = c("tr" = 16, "co" = 2),
                    labels = c("tr" = "treatment\n(arterial and highways)",
                               "co" = "control\n(non-treated avenues)"),
                    name = "") +
 scale_fill_manual(values = c("slr" = "red"),
                   labels = c("slr" = "speed limit\nreductions"),
                   name = "") +
 scale_x_date(date_breaks = "1 year", date_labels = "%Y") 


t1 <- ggdraw() + draw_label("Panel A: All treated and all control segments")
t2 <- ggdraw() + draw_label("Panel B: Control segm. more than 1.6km from treatment")
t3 <- ggdraw() + draw_label("Panel C: Matched treated and control segm. - control segm. more than 1.6km away from treatment")



ll <- get_legend(mp[[1]])
rl <- get_legend(mp[[4]])


r0 <- plot_grid(ll,rl,
                ncol = 2, nrow=1, align = "v", rel_widths = c(1, 1))

r1 <- plot_grid(mp[[1]] + theme(legend.position = "none"), mp[[4]]+ theme(legend.position = "none"),
                ncol = 2, nrow=1, align = "v", rel_widths = c(1, 1))
r2 <- plot_grid(mp[[2]]+ theme(legend.position = "none"), mp[[5]]+ theme(legend.position = "none"),
                ncol = 2, nrow=1, align = "v", rel_widths = c(1, 1))
r3 <- plot_grid(mp[[3]]+ theme(legend.position = "none"), mp[[6]]+ theme(legend.position = "none"),
                ncol = 2, nrow=1, align = "v", rel_widths = c(1, 1))



plots <- plot_grid(r0, NULL, t1,r1,NULL,t2,r2,NULL,t3,r3,
                   ncol = 1, nrow=10, align = "v", rel_heights = c(0.2,0.05, 0.1,1, 0.05, 0.1,1, 0.05, 0.1,1))

ggsave(plots, filename = OP.p, w = 12, h = 16)
