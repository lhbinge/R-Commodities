
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



##==========================
## Temporal disaggregation
##==========================


library(tempdisagg)

blue <- read.csv("Blue_Books.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)

wheat.a <- blue[blue$town=="Cape", c("date","wheat")]
#wheat.a <- as.data.frame(wheat.a[wheat.a$date>1901,])
ts.wheat.a <- as.ts(wheat.a[,-1], start=1889, end= 1907, frequency = 1)
#xts.wheat.a <- as.xts(wheat.a$wheat, order.by=as.Date(as.character(wheat.a$date),"%Y"))

ts.wheat.a1 <- na.approx(ts.wheat.a, na.rm=FALSE)
ts.wheat.a2 <- na.locf(ts.wheat.a1, na.rm=FALSE)
ts.wheat.a3 <- na.locf(ts.wheat.a2, na.rm=FALSE, fromLast=TRUE)

#ta <- seq_along(ts.wheat.a)
#tm <- seq_along(ts.wheat.m)

m1 <- td(ts.wheat.a3 ~ 1, to = "monthly", conversion = "average", method = "denton-cholette")
plot(predict(m1))

s <- as.data.frame(predict(m1))

comdata <- read.csv("Commodities.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
#comdata$date <- as.Date(comdata$date, "%Y/%m/%d")
comdata$datum <- paste(comdata$datum, comdata$Year)
comdata$date <- as.Date(as.yearmon(as.character(comdata$datum),"%B %Y"))
comdata$datum <- factor(as.yearmon(as.character(comdata$datum),"%B %Y"))


wheat.m <- comdata[comdata$town=="Cape Town", c("date","wheat")]
#wheat.m <- as.data.frame(wheat.m[wheat.m$date>"1901-12-01" & wheat.m$date<"1908-01-01",])
ts.wheat.m <- as.ts(wheat.m[,-1], start=c(1902,1),end=c(1914,8), frequency = 12)

ts.wheat.m1 <- na.approx(ts.wheat.m)

plot(ts.wheat.a3)
plot(ts.wheat.m)
plot(ts.wheat.m1)
plot(ts.wheat.m2)
plot(ts.wheat.m3)

m2 <- td(ts.wheat.a3 ~ 0 + ts.wheat.m3, to= "monthly", conversion = "average", method = "chow-lin-maxlog")
summary(m2)
plot(predict(m2))

wheat.m <- aggregate(wheat.m$wheat, by=list(wheat.m$date), FUN=mean)
ts.wheat.m <- as.ts(wheat.m[,-1], start=c(1889,10),end=c(1914,8), frequency = 12)
ts.wheat.m1 <- na.approx(ts.wheat.m, na.rm=FALSE)
ts.wheat.m2 <- na.locf(ts.wheat.m1, na.rm=FALSE)
ts.wheat.m3 <- na.locf(ts.wheat.m2, na.rm=FALSE, fromLast=TRUE)


#kan ons net gewone interpolasie doen op elke monthly series
approx   (x, y = NULL, xout, method = "linear", n = 50,
          yleft, yright, rule = 1, f = 0, ties = mean)
smooth.spline()
na.approx(object, along = index(object), na.rm = TRUE, ...) 
na.spline(object, along = index(object), na.rm = TRUE, ...) 

na.locf(wheat.q)
na.locf(wheat.q[1:7,-1], fromLast=TRUE)
na.approx(wheat.q)

wheat.qint <- na.approx(wheat.q$wheat[-1:-6])
ts.wheat.q1 <- as.ts(wheat.qint[], start=c(1890,4),end=c(1914,8), frequency = 12)

m2 <- td(wheat.a ~ wheat.q1, to = 12, conversion = "average")


#demo(tempdisagg)
data(swisspharma)
m1 <- td(sales.a ~ 1, to = "quarterly", method = "denton-cholette")
predict(m1)

# Because we cannot use more than one indicator with the 'denton-cholette' or
# 'denton' method, the intercept must be specified as missing in the formula
m2 <- td(sales.a ~ 0 + exports.q, method = "denton-cholette")

# A Chow-Lin regression of the same problem as above
m3 <- td(sales.a ~ exports.q)

# The coefficients are the result of a GLS regression between the annual series.
summary(m3)

# The estimation of the AR1 parameter, Rho, was estimated to be negative; in
# order to avoid the undesirable side-effects of a negative Rho, it has been
# truncated to 0. This feature can be turned off:
td(sales.a ~ exports.q, truncated.rho = -1)

m4 <- td(formula = sales.a ~ exports.q + imports.q)
summary(m4)


td(formula, conversion = "sum", to = "quarterly",
   method = "chow-lin-maxlog", truncated.rho = 0, fixed.rho = 0.5,
   criterion = "proportional", h = 1, start = NULL, end = NULL, ...)










