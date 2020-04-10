rm(list=ls())
gc()

# required packages
library(data.table)
library(ggplot2)
library(scales)
library(lfe)
library(stargazer)
library(cowplot)


# main ------------------------------------------------------------------------
# read cleaned data
SD <- readRDS("data/pollution/1.1_cleaned_data.rds")

# output
OP <- "appendix/figures/D2_pollutants/all_pollutants.pdf"

RR <- list()
c <- 1
my_polls <- c("CO", "MP10", "MP2.5", "NO", "NO2", "NOx")


for(i in my_polls){
  # i <- "CO"
  SD <- as.data.frame(SD)
  min_poll <- min(SD[,i][SD[,i]>0],na.rm=T) 
  SD[,"my.var"] <- log(SD[,i] + min_poll)
  RD <- SD[!is.na(SD$my.var),]
  #RD$my.var_q95 <- ave(RD$my.var, RD$st, FUN= function(x) quantile(x, 0.95))
  #RD <- RD[RD$my.var < RD$my.var_q95,]
  
  m2 <- felm(my.var ~ ym2014_01 + ym2014_02 + ym2014_03 + ym2014_04 +
                      ym2014_05 + ym2014_06 + ym2014_07 + ym2014_08 +
                      ym2014_09 + ym2014_10 + ym2014_11 + ym2014_12 +
               
                      ym2015_01 + ym2015_02 + ym2015_03 + ym2015_04 +
                      ym2015_05 + ym2015_06 + ym2015_08 +
                      ym2015_09 + ym2015_10 + ym2015_11 + ym2015_12 +
               
               ym2016_01 + ym2016_02 + ym2016_03 + ym2016_04 +
               ym2016_05 + ym2016_06 + ym2016_07 + ym2016_08 +
               ym2016_09 + ym2016_10 + ym2016_11 + ym2016_12 +
               
               ym2017_01 + ym2017_02 + ym2017_03 + ym2017_04 +
               ym2017_05 + ym2017_06 + ym2017_07 + ym2017_08 +
               ym2017_09 + ym2017_10 + ym2017_11 + ym2017_12 +
               
                      PRESS_f + TEMP_f + VV_f + UR_f|
               dh + st | 0 | st_date, data = RD)
  
  a <- as.data.frame(summary(m2)$coefficients)
  a$b <- a$Estimate
  a$up <- a$Estimate + 1.96*a$`Cluster s.e.`
  a$do <- a$Estimate - 1.96*a$`Cluster s.e.`
  
  a <- a[1:47,]
  a$name <- rownames(a)
  a$m <- as.Date(paste0(gsub("_", "-", substr(a$name, 3,9)),"-01" ))
  
  a0 <- a[1,]
  a0[1,5:7] <- 0
  a0[1,9] <- as.Date("2015-07-01")
  row.names(a0) <- "ym2015_07"
  
  a <- rbind(a,a0)
  
  RR[[c]] <- ggplot() +
    geom_hline(yintercept = 0, alpha = 0.5, colour = "gray") +
    geom_vline(aes(colour = "slr", xintercept = as.Date("2015-07-20")), alpha = 0.5, linetype = 2) +
    geom_vline(aes(colour = "sli", xintercept = as.Date("2017-01-24")), alpha = 0.5, linetype = 2) +
    geom_point(aes(x = m , y = b), data = a) + 
    geom_errorbar(aes(x = m , ymin = do, ymax = up), data = a, width = 0) + 
    theme_bw() +
    theme(panel.grid = element_blank(), legend.position = "top", plot.title = element_text(hjust = 0.5)) +
    scale_y_continuous(labels = percent) +
    labs(y = "relative change", x = "date", title = paste0("Pollutant: ", i)) +
    coord_cartesian(ylim = c(-0.8,0.8)) + 
    scale_colour_manual(values = c("slr" = "red", "sli" = "blue"),
                        labels = c("slr" = "Speed Limit\nReduction", "sli" = "Speed Limit\nIncrease"),
                        name = "Legend:    ")
  c<-c+1
}

p <- plot_grid(RR[[1]], RR[[2]], NULL, NULL,
               RR[[3]], RR[[4]], NULL, NULL,
               RR[[5]], RR[[6]], nrow = 5, ncol=2, rel_heights = c(1,0.1,1,0.1,1))
ggsave(p, filename = OP, w = 12, h = 12)
