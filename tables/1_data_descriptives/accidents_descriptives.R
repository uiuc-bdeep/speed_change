# clear memory
rm(list=ls())
gc()

# required packages
library(ggplot2)
library(Cairo)
library(scales)
library(cowplot)


# inputs ----------------------------------------------------------------------
AD.p <- "data/accidents/accidents.rds"
VI.p <- "data/accidents/victims.rds"

# output
o1 <- "tables/1_descriptives/accidents_descriptives.csv"


# main ----------------------------------------------------------------------------------------
# read accidents data
AD <- readRDS(AD.p)

# fix year
AD$year <- substr(as.character(AD$date), 1, 4)

# accident indicator
AD$acc <- 1

# count injured victims ad fatalities
AD$inj <- AD$feridos
AD$fat <- AD$mortos

### count pedestrian victims and fatalities per accident
# read victims data
VI <- readRDS(VI.p)
# victims indicator
VI$vic <- 1

# fatality indicator of victim
VI$fat <- ifelse(VI$classificacao %in% c("M", "MORTO"), 1, 0)

# pedestrian and motorcyclist indicator of victims
VI$vic_ped <- ifelse(VI$tipo_vitima %in% "PD", 1, 0)
VI$vic_mot <- ifelse(VI$tipo_veiculo %in% "MO", 1, 0)

# pedestrian and motorcyclist indicator of fatal victims
VI$fat_ped <- VI$fat*VI$vic_ped
VI$fat_mot <- VI$fat*VI$vic_mot

## aggregate at accident level
# victims per accident
VI$vic_a <- ave(VI$vic, VI$id_acidente, FUN=sum)
# pedestrian victims per accident
VI$vic_ped_a <- ave(VI$vic_ped, VI$id_acidente, FUN=sum)
# motorcyclist victims per accident
VI$vic_mot_a <- ave(VI$vic_mot, VI$id_acidente, FUN=sum)
# pedestrian fatalities per accident
VI$fat_ped_a <- ave(VI$fat_ped, VI$id_acidente, FUN=sum)
# motorcyclist fatalities per accident
VI$fat_mot_a <- ave(VI$fat_mot, VI$id_acidente, FUN=sum)

# fix NAs
VI$vic_a[is.na(VI$vic_a)] <- 0
VI$vic_ped_a[is.na(VI$vic_ped_a)] <- 0
VI$vic_mot_a[is.na(VI$vic_mot_a)] <- 0
VI$fat_ped_a[is.na(VI$fat_ped_a)] <- 0
VI$fat_mot_a[is.na(VI$fat_mot_a)] <- 0

# aggreagate
VIA <- VI[!duplicated(VI$id_acidente), c("id_acidente", "vic_a", "vic_ped_a", "vic_mot_a", "fat_ped_a", "fat_mot_a")]
# merge aggregate info to accident data
AD <- merge(AD, VIA, by = "id_acidente", all.x=T)

# fix NAs
AD$vic_a[is.na(AD$vic_a)] <- 0
AD$vic_ped_a[is.na(AD$vic_ped_a)] <- 0
AD$vic_mot_a[is.na(AD$vic_mot_a)] <- 0
AD$fat_ped_a[is.na(AD$fat_ped_a)] <- 0
AD$fat_mot_a[is.na(AD$fat_mot_a)] <- 0


### OUTPUT
# create output table
mo <- as.data.frame(matrix(nrow = 28, ncol = 7))
# define names
colnames(mo) <- c("All_Years", "Y2012", "Y2013", "Y2014", "Y2015", "Y2016", "Y2017")
rownames(mo) <- c("acc_all", "inj_all", "inj_all_ped","inj_all_mot", "fat_all","fat_all_ped", "fat_all_mot",
                  "acc_marg", "inj_marg", "inj_marg_ped","inj_marg_mot", "fat_marg","fat_marg_ped", "fat_marg_mot",
                  "acc_art", "inj_art", "inj_art_ped","inj_art_mot", "fat_art","fat_art_ped", "fat_art_mot",
                  "acc_oth", "inj_oth", "inj_oth_ped","inj_oth_mot", "fat_oth","fat_oth_ped", "fat_oth_mot")
# list of accident dataframe subsets
my.d <- list(AD, AD[AD$marg %in% 1,],
             AD[AD$tr_group %in% "tr" & AD$marg %in% 0,], AD[AD$tr_group %in% "co",])


for(i in 1:4){#i <- 1
 MD <- my.d[[i]]
 
 # count total accidents per subset
 mo[ ((i-1)*7) + 1,] <- c(sum(MD$acc), 
            sum(MD$acc[MD$year == 2012]),
            sum(MD$acc[MD$year == 2013]),
            sum(MD$acc[MD$year == 2014]),
            sum(MD$acc[MD$year == 2015]),
            sum(MD$acc[MD$year == 2016]),
            sum(MD$acc[MD$year == 2017]))

 # count total victims per subset
 mo[((i-1)*7) + 2,] <- c(sum(MD$vic_a), 
            sum(MD$vic_a[MD$year == 2012]),
            sum(MD$vic_a[MD$year == 2013]),
            sum(MD$vic_a[MD$year == 2014]),
            sum(MD$vic_a[MD$year == 2015]),
            sum(MD$vic_a[MD$year == 2016]),
            sum(MD$vic_a[MD$year == 2017]))

 # count pedestrian victims per subset
 mo[((i-1)*7) + 3,] <- c(sum(MD$vic_ped_a), 
            sum(MD$vic_ped_a[MD$year == 2012]),
            sum(MD$vic_ped_a[MD$year == 2013]),
            sum(MD$vic_ped_a[MD$year == 2014]),
            sum(MD$vic_ped_a[MD$year == 2015]),
            sum(MD$vic_ped_a[MD$year == 2016]),
            sum(MD$vic_ped_a[MD$year == 2017]))

 # count motorcyclist victims per subset
 mo[((i-1)*7) + 4,] <- c(sum(MD$vic_mot_a), 
            sum(MD$vic_mot_a[MD$year == 2012]),
            sum(MD$vic_mot_a[MD$year == 2013]),
            sum(MD$vic_mot_a[MD$year == 2014]),
            sum(MD$vic_mot_a[MD$year == 2015]),
            sum(MD$vic_mot_a[MD$year == 2016]),
            sum(MD$vic_mot_a[MD$year == 2017]))

 # count fatalities per subset
 mo[((i-1)*7) + 5,] <- c(sum(MD$fat), 
            sum(MD$fat[MD$year == 2012]),
            sum(MD$fat[MD$year == 2013]),
            sum(MD$fat[MD$year == 2014]),
            sum(MD$fat[MD$year == 2015]),
            sum(MD$fat[MD$year == 2016]),
            sum(MD$fat[MD$year == 2017]))

 # count pedestrian fatalities per subset
 mo[((i-1)*7) + 6,] <- c(sum(MD$fat_ped_a), 
            sum(MD$fat_ped_a[MD$year == 2012]),
            sum(MD$fat_ped_a[MD$year == 2013]),
            sum(MD$fat_ped_a[MD$year == 2014]),
            sum(MD$fat_ped_a[MD$year == 2015]),
            sum(MD$fat_ped_a[MD$year == 2016]),
            sum(MD$fat_ped_a[MD$year == 2017]))

 # count motorcyclist fatalities per subset
 mo[((i-1)*7) + 7,] <- c(sum(MD$fat_mot_a), 
            sum(MD$fat_mot_a[MD$year == 2012]),
            sum(MD$fat_mot_a[MD$year == 2013]),
            sum(MD$fat_mot_a[MD$year == 2014]),
            sum(MD$fat_mot_a[MD$year == 2015]),
            sum(MD$fat_mot_a[MD$year == 2016]),
            sum(MD$fat_mot_a[MD$year == 2017]))
}

# save output
 write.csv(mo, o1)
 
 
