rm(list=ls())
library(osrm)
library(ggplot2)
library(rgeos)
library(rgdal)
library(scales)
library(cowplot)
st <- Sys.time()


# read trips
TD <-readRDS("data/osrm_simulations/2.2_simulation_data_MRs2.rds")

# output
O1 <- "appendix/figures/C6_rerouting_savings/rerouting_savings_S1_S2.pdf"
O2 <- "appendix/figures/C6_rerouting_savings/rerouting_savings_S2_S3.pdf"
O3 <- "appendix/figures/C6_rerouting_savings/rerouting_savings_both.pdf"

TD$oth_ratio_1 <- 1 - TD$marg_ratio_1 - TD$art_ratio_1
TD$oth_ratio_2 <- 1 - TD$marg_ratio_2 - TD$art_ratio_2
TD$oth_ratio_3 <- 1 - TD$marg_ratio_3 - TD$art_ratio_3



# calculate lengths
TD$art_length_1 <- TD$dis1*TD$art_ratio_1
TD$art_length_2 <- TD$dis2*TD$art_ratio_2
TD$art_length_3 <- TD$dis3*TD$art_ratio_3

TD$marg_length_1 <- TD$dis1*TD$marg_ratio_1
TD$marg_length_2 <- TD$dis2*TD$marg_ratio_2
TD$marg_length_3 <- TD$dis3*TD$marg_ratio_3

TD$oth_length_1 <- TD$dis1*TD$oth_ratio_1
TD$oth_length_2 <- TD$dis2*TD$oth_ratio_2
TD$oth_length_3 <- TD$dis3*TD$oth_ratio_3

# convert duration to hours
TD$dur1 <- TD$dur1/60
TD$dur2 <- TD$dur2/60
TD$dur3 <- TD$dur3/60

# convert duration to hours
m1 <- lm(dur1~-1+art_length_1+marg_length_1+oth_length_1,data=TD)
sp1 <- as.data.frame(1/summary(m1)$coefficients[,1])
colnames(sp1) <- "sp1"
sp1$road <-c("art","marg","oth")

m2 <- lm(dur2~-1+art_length_2+marg_length_2+oth_length_2,data=TD)
sp2 <- as.data.frame(1/summary(m2)$coefficients[,1])
colnames(sp2) <- "sp2"
sp2$road <-c("art","marg","oth")

m3 <- lm(dur3~-1+art_length_3+marg_length_3+oth_length_3,data=TD)
sp3 <- as.data.frame(1/summary(m3)$coefficients[,1])
colnames(sp3) <- "sp3"
sp3$road <-c("art","marg","oth")

sp <- merge(sp1, sp2, by="road")
sp <- merge(sp, sp3, by="road")

sp$sp1_inv <- 1/sp$sp1
sp$sp2_inv <- 1/sp$sp2
sp$sp3_inv <- 1/sp$sp3


TD$durS_sp1_path1 <- TD$art_length_1*sp$sp1_inv[1] + TD$marg_length_1*sp$sp1_inv[2] + TD$oth_length_1*sp$sp1_inv[3] 
TD$durS_sp2_path1 <- TD$art_length_1*sp$sp2_inv[1] + TD$marg_length_1*sp$sp2_inv[2] + TD$oth_length_1*sp$sp1_inv[3] 

TD$durS_sp2_path2 <- TD$art_length_2*sp$sp2_inv[1] + TD$marg_length_2*sp$sp2_inv[2] + TD$oth_length_2*sp$sp1_inv[3] 
TD$durS_sp3_path2 <- TD$art_length_2*sp$sp2_inv[1] + TD$marg_length_2*sp$sp1_inv[2] + TD$oth_length_2*sp$sp1_inv[3] 

TD$durS_sp3_path3 <- TD$art_length_3*sp$sp2_inv[1] + TD$marg_length_3*sp$sp1_inv[2] + TD$oth_length_3*sp$sp1_inv[3] 


TD$rerouting_savings_12 <- TD$durS_sp2_path1 - TD$durS_sp2_path2
TD$rerouting_savings_12 <- ifelse(TD$rerouting_savings_12<0,0,TD$rerouting_savings_12)
TD$rerouting_savings_12 <- TD$rerouting_savings_12*60*60

p1 <-ggplot()+
 geom_histogram(aes(rerouting_savings_12, y=..count../sum(..count..)),
                data=TD[TD$rerouting_savings_12>0,],
                binwidth = 2.5) +
 geom_vline(xintercept = median(TD$rerouting_savings_12[TD$rerouting_savings_12>0]),
            linetype=2, size=0.5) +
 theme_bw() +
 theme(panel.grid = element_blank()) +
 coord_cartesian(xlim = c(0,240), ylim=c(0,0.3)) +
 scale_x_continuous(breaks = seq(0,240,30)) +
 scale_y_continuous(breaks = seq(0,1,0.1),
                    labels = percent_format(accuracy = 1L)) +
 labs(x="seconds saved due to re-routing", y="\n\nshare of trips \n (with time savings from re-routing)\n",
      title = "A: Speed Limit Reductions of 2015",
      subtitle = "The dashed line indicates median savings")
ggsave(O1,p1, w=6,h=4)

TD$rerouting_savings_23 <- TD$durS_sp3_path2 - TD$durS_sp3_path3
TD$rerouting_savings_23 <- ifelse(TD$rerouting_savings_23<0,0,TD$rerouting_savings_23)
TD$rerouting_savings_23 <- TD$rerouting_savings_23*60*60

p2 <- ggplot()+
 geom_histogram(aes(rerouting_savings_23, y=..count../sum(..count..)),
                data=TD[TD$rerouting_savings_23>0,],
                binwidth = 2.5) +
 geom_vline(xintercept = median(TD$rerouting_savings_23[TD$rerouting_savings_23>0]),
            linetype=2, size=0.5) +
 theme_bw() +
 theme(panel.grid = element_blank()) +
 coord_cartesian(xlim = c(0,240), ylim=c(0,0.3)) +
 scale_x_continuous(breaks = seq(0,240,30)) +
 scale_y_continuous(breaks = seq(0,1,0.1),
                    labels = percent_format(accuracy = 1L)) +
 labs(x="seconds saved due to re-routing", y="\n\nshare of trips \n (with time savings from re-routing)\n",
      title = "B: Speed Limit Reversal of 2017",
      subtitle = "The dashed line indicates median savings")
ggsave(O2,p2, w=6,h=4)

cp <- plot_grid(p1,p2, ncol = 2)
ggsave(O3,cp, w=12,h=4)