# clear memory
rm(list=ls())
gc()

# required packages

# inputs
ODp <- "data/trips/mobilidade_2012_google_info.csv"

# outputs
O.p <- "appendix/tables/A3_survey_descriptives/survey_descriptives.csv"

# Main ----------------------------------------------------------------------------------------

# read main data
OD <- read.csv(ODp)

# subset to motorized trips
MT <- OD[OD$TIPOVG %in% c(1,2),]

# motive



# create output data frame
SD <- as.data.frame(matrix(ncol = 0, nrow = 4))
SD$labels <- c("trips", "share", "distance", "time")
# total trips
SD$tot <- c(sum(MT$FE_VIA),
            sum(MT$FE_VIA)/sum(MT$FE_VIA),
            weighted.mean(MT$DISTANCIA, MT$FE_VIA),
            weighted.mean(MT$DURACAO, MT$FE_VIA))

# types of trip
# mode
MT$bus <- ifelse(MT$MODOPRIN %in% c(1,2,3,4,5,9,10,11), 1, 0)
MT$rail <- ifelse(MT$MODOPRIN %in% c(12,13), 1, 0)
MT$car <- ifelse(MT$MODOPRIN %in% c(6,7,8), 1, 0)
MT$moto <- ifelse(MT$MODOPRIN %in% c(14), 1, 0)
# motivation

MT$work <- ifelse(MT$MOTIVO_D %in% c(1,2,3) | MT$MOTIVO_O %in% c(1,2,3),  1, 0)
MT$educ <- ifelse(MT$MOTIVO_D %in% c(4) | MT$MOTIVO_O %in% c(4), 1, 0)
MT$oth <- ifelse(MT$MOTIVO_D %in% c(5,6,7,9) | MT$MOTIVO_O %in% c(5,6,7,9), 1, 0)

my.types <- c("bus","rail","car","moto",
              "work","educ","oth")

for(i in 1:7){
 SD[,my.types[i]] <- c(sum(MT$FE_VIA[MT[,my.types[i]] == 1]),
                       sum(MT$FE_VIA[MT[,my.types[i]] == 1])/sum(MT$FE_VIA),
                       weighted.mean(MT$DISTANCIA[MT[,my.types[i]] == 1],
                                     MT$FE_VIA[MT[,my.types[i]] == 1]),
                       weighted.mean(MT$DURACAO[MT[,my.types[i]] == 1],
                                     MT$FE_VIA[MT[,my.types[i]] == 1]))
}



# output
write.csv(t(SD), O.p)






