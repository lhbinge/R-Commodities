
toy <- rbind(c(NA, 90,NA,100),
             c(100,90,110,100),
             c(NA,92,116,103.8),
             c(NA,93,NA,104.1),
             c(NA,NA,117,105.5),
             c(110,99,121,110),
             c(110,NA,NA,110)) 
toy <- as.data.frame(toy)
toy$t <- 1:7

#toy1 <- melt(toy[,-4], id="col.names")
#toy1$lnvalue <- log(toy1$value) 

maak <- function(naam) {
    toy2 <- toy
    toy2 <- toy2[!is.na(toy[,naam]),c(naam,"t")]
    for(i in 1:(nrow(toy2)-1)) {
        toy2$P[1] <- 0
        toy2$P[i+1] <- log(toy2[i+1,naam]/toy2[i,naam])
    }
    xmat <- array(0, dim = c(nrow(toy2)-1, nrow(toy))) 
    for(i in 1:(nrow(toy2)-1)) {
        xmat[i,toy2$t[i]] <- -1
        xmat[i,toy2$t[i+1]] <- 1
    }
    einde <- cbind(toy2$P[-1],xmat)  
    return(einde)
}

einde <- as.data.frame(rbind(maak("V1"),maak("V2"),maak("V3")))
dy <- einde$V1
xmat <- as.matrix(einde[,2:8])

rsales <- lm(dy ~ xmat + 0)
rs_index <- as.data.frame(exp(rsales$coefficients))
n <- rs_index[1,1]
rs_index <- rs_index/n*100
rs_index <- na.locf(rs_index)
#rs_index2 <- exp(as.data.frame(ps.RS$coefficients))*100

#---------------------------------------------------
#test with real example

toy <- dcast(comdata, time_id + datum ~ town, mean, value.var="wheat")
toy <- toy[21:30,c("time_id","datum","Beaufort West","Cape Town","Worcester")]
toy$t <- 1:nrow(toy)
#toy1$lnvalue <- log(toy1$value) 

maak <- function(naam) {
    toy2 <- toy
    toy2 <- toy2[!is.na(toy[,naam]),c(naam,"t")]
    for(i in 1:(nrow(toy2)-1)) {
        toy2$P[1] <- 0
        toy2$P[i+1] <- log(toy2[i+1,naam]/toy2[i,naam])
    }
    xmat <- array(0, dim = c(nrow(toy2)-1, nrow(toy))) 
    for(i in 1:(nrow(toy2)-1)) {
        xmat[i,toy2$t[i]] <- -1
        xmat[i,toy2$t[i+1]] <- 1
    }
    einde <- cbind(toy2$P[-1],xmat)  
    return(einde)
}

einde <- as.data.frame(rbind(maak("Beaufort West"),maak("Cape Town"),maak("Worcester")))
dy <- einde$V1
xmat <- as.matrix(einde[,2:ncol(einde)])

rsales <- lm(dy ~ xmat + 0)
rs_index <- as.data.frame(exp(rsales$coefficients))
n <- rs_index[1,1]
rs_index <- rs_index/n*100
rs_index <- na.locf(rs_index)
#rs_index2 <- exp(as.data.frame(ps.RS$coefficients))*100

toy <- cbind(toy,rs_index)
toy <- toy[,c(2,3,4,5,7)]
colnames(toy)[c(1,5)] <- c("Date","Index")

#---------------------------------------------------    
rep <- repsaledata(toy1$lnvalue,toy1$Var1,toy1$Var2)  #transform the data to sales pairs
rsales <- repsale(rep$price0,rep$time0,rep$price1,rep$time1,mergefirst = 1, graph=FALSE)   #generate the repeat sales index
rs_index <- exp(as.data.frame(rsales$pindex))*100
check <- rsales$xmat
check2 <- as.data.frame(rsales$dy)

dy <- repdata1$price1 - repdata1$price0
ah0 <- model.matrix(~repdata1$ah_code0)
ah1 <- model.matrix(~repdata1$ah_code1)
dah <- ah1 - ah0

timevar <- levels(factor(c(repdata1$time0, repdata1$time1)))
nt = length(timevar)
n = length(dy)
xmat <- array(0, dim = c(n, nt - 1))
for (j in seq(1 + 1, nt)) {
    xmat[,j-1] <- ifelse(repdata1$time1 == timevar[j], 1, xmat[,j-1])
    xmat[,j-1] <- ifelse(repdata1$time0 == timevar[j],-1, xmat[,j-1])
}
colnames(xmat) <- paste("Time", seq(1 + 1, nt))

ps.RS <- lm(dy ~ dah + xmat + 0)



