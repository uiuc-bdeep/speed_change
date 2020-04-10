# clear memory
rm(list=ls())
gc()

# required packages
library(ggplot2)
library(scales)
library(Cairo)
# inputs
ODp <- "data/trips/household_survey_2012.rds"
IF.p <- "data/inflation/inflation.csv"
# outputs
O.p <- "appendix/figures/A4_income_education/income_education.pdf"


# Main ----------------------------------------------------------------------------------------
IF <- read.csv(IF.p)
inflation <- IF$index[IF$date == as.character("2016-12-01")]/IF$index[IF$date == as.character("2012-12-01")]

# read main data
OD <- readRDS(ODp)

#subset to individuals >=18
ID <- OD[!duplicated(OD$ID_PESS),]
ID <- ID[ID$IDADE >= 18,]
ID$educ <- ID$GRAU_INS
ID$educ[ID$educ %in% 1] <- 2


# wage
ID$wage_m <- ifelse(ID$CO_REN_I %in% 1, ID$VL_REN_I,
                    ifelse(ID$CO_REN_I %in% 2, 0,
                           ID$RENDA_FA/ID$NO_MORAF))*inflation

# mean wage by educ
ID$wage_educ <- ave(ID$wage_m, ID$educ, FUN=mean)

ED <- ID[!duplicated(ID$educ), c("educ", "wage_educ")]

# plot

ggplot() +
 geom_col(aes(x = educ, y = wage_educ, fill = "inc"), data = ED, width = 0.4, alpha = 0.75) +
 theme_bw() +
 theme(panel.grid = element_blank(), legend.position = "top") +
 scale_fill_manual(values = c("inc" = "goldenrod"),
                   labels = c("inc" = "Mean Individual Income by Educational Atainment"),
                   name = "Legend:    ") +
 scale_y_continuous(breaks = seq(0,5000,1000), label = comma, expand = c(0,0)) +
 scale_x_discrete(limits = c(2:5), 
                  labels = c("no primary", "primary", "secondary", "college")) +
 labs(x = "Educational Attainment", y = "BRL") +
 coord_cartesian(ylim = c(0,5000))
ggsave(O.p, w = 5, h =3)
