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


# inputs ----------------------------------------------------------------------
TI.p <- "data/tickets/tickets_over_segments_all.rds"
PD.p <- "data/panel/monthly_panel_all_b1k.rds"

# output
OP.p <-"tables/3_tickets_estimation/tickets_estimation.csv"



# main ----------------------------------------------------------------------------------------

# read tickets data
TI <- readRDS(TI.p)
PD <- readRDS(PD.p)

## get change date of each segment
# panel data at segment level
PP <- PD[!duplicated(PD$point_id), c("point_id", "ChangeDate")]
TI <- merge(TI, PP, by="point_id", all.x=T)
# subset to cameras before the reversal
TT <- TI[TI$date <= as.Date("2017-01-25"),]

# sum speeding tickets per camera per date
# camera date ID
TT$cam_date <- paste(as.numeric(TT$Local), TT$date)
# tickets per camera per day
TT$speeding_camday <- ave(TT$ticket.speed*TT$tickets, TT$cam_date, FUN=sum)
TT$restriction_camday <- ave(TT$ticket.rodizio*TT$tickets,  TT$cam_date, FUN=sum)
TT$other_camday <- ave(TT$ticket.other*TT$tickets,  TT$cam_date, FUN=sum)

# recreate data at camera date level
CD <- TT[!duplicated(TT$cam_date),]

# define distance to treatment for each observation
CD$tr_dist <- as.numeric(CD$date - CD$ChangeDate)
# distance in quarters
CD$tr_dist_q <- floor(CD$tr_dist/90)

# exclude extreme quarters
CD <-CD[CD$tr_dist_q < 6 & CD$tr_dist_q >-8 ,]

## relative quarter dummies
# negative
for(q in -7:-1){CD[,paste0("q_",abs(q))] <- ifelse(CD[,"tr_dist_q"] %in% q,1,0)}
# positive
for(q in 0:5){ CD[,paste0("q",q)] <- ifelse(CD[,"tr_dist_q"] %in% q,1,0)}

# define long term quarter
CD$q3 <- ifelse(CD$tr_dist_q %in% c(3:6),1,0)

## regressions
# define list of dependent variables
my_vars <- c("speeding_camday", "restriction_camday", "other_camday")
              
my_model <- c("speeding", "rodizio", "other")
# list of summaries
ss <- list()
# regress each var
for(i in 1:3){#i<-1
 # temporary camera data
 CD[,"my_var"] <- CD[,my_vars[i]]
 TCD <- CD[CD$my_var >0,]
 # take log of dependent variable
 TCD$ln_v <- log(TCD$my_var)
 # run model
 mm <- felm(ln_v~q_7+q_6+q_5+q_4+q_3+q_2+
                   q0+q1+q2+q3|Local+date|0|0,
           data = TCD)
 # temporary summary
 ts <- as.data.frame(summary(mm)$coefficients)

 # define quarter
 ts$q <- gsub("q", "",row.names(ts))
 ts$q <- as.numeric(gsub("_", "-",ts$q))
 
 # cleaner coefficient labels
 ts$b <- ts$Estimate
 ts$se <- ts$`Std. Error`
 ts$up <- ts$b+1.96*ts$se
 ts$do <- ts$b-1.96*ts$se
 
 # get baseline month coefficient 
 ts0 <- ts[1,]
 ts0[] <- 0
 ts0$q <- -1
 ts <- rbind(ts,ts0)
 
 # add dependent variable
 ts$model <- my_model[i]
 # stars
 ts$star <- ifelse(ts$`Pr(>|t|)` < 0.001, "***",
                   ifelse(ts$`Pr(>|t|)` < 0.01, "**",
                          ifelse(ts$`Pr(>|t|)` < 0.05, "*"," ")))
 
 # fix row names
 ts$var <- row.names(ts)
 row.names(ts) <- paste(ts$var,ts$model)
 
 # get number of observations
 ts$n <- mm$N
 # save summary to lists
 ss[[i]] <- ts
}

# bind all results
ar <- rbind(ss[[1]],ss[[2]],ss[[3]])


# see results vizualy
ggplot() +
 geom_point(aes(x=q, y=b, color=model), data=ar) +
 geom_errorbar(aes(x=q, ymin=do, ymax=up, color=model), data=ar, width=0)
 



# save as csv
write.csv(ar,OP.p)












