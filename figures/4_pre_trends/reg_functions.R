
gencov <- function(my.model, model.number, my.data){
 #my.model <- m0
 #my.data <- ES
 mm <- my.model
 MD <- my.data
 ss <- summary(mm, se = "cluster", cluster = MD$StreetName)
 cov <- ss$cov.scaled
 bb <- exp(mm$coefficients) - 1
 se <- as.numeric(rep(0,length(mm$coefficients)))
 mf <- list()
 for(f in 1:length(bb)){mf[[f]] <- as.formula(paste0("~ exp(x",f,") - 1"))}
 gcov <- deltamethod(mf, mm$coefficients, cov, ses=F)
 rownames(gcov) <- names(bb)
 colnames(gcov) <- names(bb)
 return(gcov)
}


# calculate quarterly coefficients by model
agg_Q <- function(my.model,  model.number, my.data, my.weights){
 #my.model <- m0
 #model.number <- 0
 #my.data <- ES
 #my.weights <- WW
 
 
 bb <- gentab(my.model, 0, my.data)
 cc <- gencov(my.model, 0, my.data)
 ww <- my.weights
 
 bb<-bb[1:102,]
 bb$c <- as.numeric(substr(row.names(bb),2,2))
 bb$rm <- as.numeric(gsub("_","-", substr(row.names(bb),4,6)) )
 
 bb <- merge(bb, ww, by="c",all.x=T)
 bb$rq <- bb$rm
 
 bb$bw <- bb$b*bb$w
 bb$b_q <- ave(bb$bw, bb$rq, FUN=sum)
 bbq <- bb[!duplicated(bb$rq),c("rq","b_q")]
 
 
 cc <- as.data.frame(cc[1:102,1:102])
 cc$c_o <- as.numeric(substr(row.names(cc),2,2))
 cc$rm_o <- as.numeric(gsub("_","-",substr(row.names(cc),4,6)))
 ccl <-list()
 
 
 for(i in 1:102){#i<-1
  cct <- cc[,c(i,103,104)] 
  c_d<-as.numeric(substr(colnames(cct)[1],2,2))
  rm_d<-as.numeric(gsub("_","-",substr(colnames(cct)[1],4,6)))
  colnames(cct) <- c("vcov","c_o", "rm_o")
  cct$c_d <- c_d
  cct$rm_d <- rm_d
  ccl[[i]]<-cct
 }
 
 ccb <- do.call(rbind, ccl)
 
 ww_o <- ww
 colnames(ww_o) <- c("c_o", "w_o")
 ccb <- merge(ccb, ww_o, by="c_o", all.x=T)
 
 ww_d <- ww
 colnames(ww_d) <- c("c_d", "w_d")
 ccb <- merge(ccb, ww_d, by="c_d", all.x=T)
 
 ccb$rq_o <- ccb$rm_o
 ccb$rq_d <- ccb$rm_d
 
 ccb <- ccb[ccb$rq_o==ccb$rq_d,]
 ccb$rq <- ccb$rq_o
 
 
 ccb$vcov_ww <- ccb$vcov*ccb$w_o*ccb$w_d
 ccb$var_q <- ave(ccb$vcov_ww, ccb$rq, FUN=sum)
 
 ccq <- ccb[!duplicated(ccb$rq),c("rq","var_q")]
 
 bbq <- merge(bbq,ccq,by="rq",all.x=T)
 bbq$se_q <- sqrt(bbq$var_q)
 bbq$model <- model.number
 bbq$t <- bbq$b_q/bbq$se_q
 bbq$p <- 2*pt(-abs(bbq$t),df=nrow(my.data)-1)
 
 bbq$v <- paste0("Q",bbq$rq)
 bbq$b <- bbq$b_q
 bbq$se <- bbq$se_q
 bbq$m <- "m0c"
 bbq$n <- my.model$n
 bbq <- bbq[,c("b","se","t","p","m","v","n")]
 return(bbq)
}



gentab <- function(my.model, model.number, my.data){
 MD <- my.data
 mm <- my.model
 ss <- summary(mm, se = "cluster", cluster = MD$StreetName)
 cov <- ss$cov.scaled
 bb <- exp(mm$coefficients) - 1
 se <- as.numeric(rep(0,length(mm$coefficients)))
 for(i in 1:length(mm$coefficients)){se[i] <- deltamethod(as.formula(paste0("~ exp(x",i,") - 1")), mm$coefficients, cov)}
 pp <- 2 * pnorm(abs(bb/se), lower.tail=FALSE)
 tt <- as.data.frame(cbind("b" = bb, "se" = se, "t"= abs(bb/se), "p" = pp))
 tt$m <- paste0("m", model.number)
 tt$v <- row.names(tt)
 tt$n <- mm$n
 return(tt)
}



gentabL <- function(my.model, model.number, my.data){
 MD <- my.data
 mm <- my.model
 ss <- summary(mm, se = "cluster", cluster = MD$StreetName)
 cov <- ss$cov.scaled
 bb <- exp(mm$coefficients) - 1
 se <- as.numeric(rep(0,length(mm$coefficients)))
 for(i in 1:length(mm$coefficients)){se[i] <- deltamethod(as.formula(paste0("~ x")), mm$coefficients, cov)}
 pp <- 2 * pnorm(abs(bb/se), lower.tail=FALSE)
 tt <- as.data.frame(cbind("b" = bb, "se" = se, "t"= abs(bb/se), "p" = pp))
 tt$m <- paste0("m", model.number)
 tt$v <- row.names(tt)
 tt$n <- mm$n
 return(tt)
}