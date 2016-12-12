##===============================================================================================##
## -------------------------------- COMMODITY INDEX ---------------------------------------------##
##===============================================================================================##

##=====================##
## READING IN THE DATA ##
##=====================##
library(zoo)            
library(ggplot2)
library(plyr)
library(dplyr)
library(reshape2)
library(stargazer)
library(micEcon)
library(quantreg)
library(McSpatial)
library(quantmod)
library(xtable)
library(scales)
library(tseries)
library(urca)
library(lmtest)
library(grid)
library(tempdisagg)

setwd("C:\\Users\\Laurie\\OneDrive\\Documents\\BING\\Commodity Cycles\\R Commodities")

comdata <- read.csv("Commodities.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)

#comdata$date <- as.Date(comdata$date, "%Y/%m/%d")
comdata$datum <- paste(comdata$datum, comdata$Year)
comdata$date <- as.Date(as.yearmon(as.character(comdata$datum),"%B %Y"))
comdata$datum <- factor(as.yearmon(as.character(comdata$datum),"%B %Y"))



coms <- aggregate(comdata$wheat, by=list(comdata$date), FUN = function(x) sum(!is.na(x)))
for(i in colnames(comdata)[7:29]) {
    coms1 <- aggregate(comdata[,i], by=list(comdata$date), FUN = function(x) sum(!is.na(x)))
    coms <- merge(coms, coms1, by="Group.1",all.x=TRUE)
}
colnames(coms) <- c("Date",colnames(comdata)[6:29])

complot <- melt(coms, id="Date") 
g <- ggplot(complot, aes(x=Date,value,colour=variable,fill=variable))
g <- g + geom_bar(stat="identity")
g <- g + theme(legend.title=element_blank())
g <- g + ylab("Total obs")
g <- g + theme(legend.key.size = unit(0.5,"cm"))
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g


coms <- aggregate(comdata$wheat, by=list(comdata$date), FUN = function(x) sum(!is.na(x)))
for(i in colnames(comdata)[7:29]) {
    coms1 <- aggregate(comdata[,i], by=list(comdata$date), FUN = function(x) sum(!is.na(x)))
    coms <- merge(coms, coms1, by="Group.1",all.x=TRUE)
}
colnames(coms) <- c("Date",colnames(comdata)[6:29])
complot <- aggregate(comdata$town, by=list(comdata$date, comdata$wheat), FUN = function(x) sum(!is.na(x)))

com.plot <- function(commodity="wheat") {
    complot <- aggregate(comdata[,commodity], by=list(comdata$date, comdata$town), FUN = function(x) sum(!is.na(x)))
    g <- ggplot(complot, aes(x=Group.1, y=x,fill=Group.2))
    g <- g + geom_bar(stat="identity")
    g <- g + theme(legend.title=element_blank())
    g <- g + theme(legend.key.size = unit(0.4,"cm"))
    g <- g + ylab(commodity)
    g <- g + xlab("Date")
    g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
    g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
    g
}

com.plot("wheat")
com.plot("mealies")
com.plot("eggs")
com.plot("s.horses")


#"wheat","wheat.flour","boer.meal","mealies","mealie.meal","barley","oats","oathay",
#"lucerne.hay","potatoes","tobacco","beef","mutton","butter","eggs","cattle","sheep",
#"pigs","bread","oranges","s.horses","tr.oxen","m.cows","w.sheep"

dorp.plot <- function(dorp="Cape Town") {
    town1 <- comdata[comdata$town == dorp,c(2,6:29)]
    town2 <- town1[,-1]
    town2[!is.na(town2)] <- 1
    town2 <- cbind(town1[,1],town2)
    colnames(town2)[1] <- "Date"
    
    complot <- melt(town2,  id.vars = "Date", variable.name = 'variable')
    g <- ggplot(complot, aes(x=Date,value,colour=variable,fill=variable))
    g <- g + geom_bar(stat="identity")
    g <- g + theme(legend.title=element_blank())
    g <- g + theme(legend.key.size = unit(0.5,"cm"))
    g <- g + ylab(dorp)
    g <- g + xlab("Date")
    g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
    g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
    g
}

dorp.plot("Cape Town")
dorp.plot("Aliwal North")
dorp.plot("Bloemfontein")
dorp.plot("Kimberley")
dorp.plot("Durban, Natal")
dorp.plot("East London")

#"Aliwal North","Beaufort West","Bloemfontein""Bulawayo","Burghersdorp","Cape Town",              
#"Clanwilliam","Colesberg","Cradock","Dordrecht","Durban, Natal","East London",            
#"Graaff-Reinet","Graham's Town","Johannesburg","Kimberley","King William's Town",
#"Malmesbury","Mossel Bay","Pietermaritzburg, Natal","Port Alfred",#"Port Elizabeth",
#"Pretoria","Queen's Town","Salisbury","Tarkastad","Vryburg","Worcester"              

complot <- aggregate(comdata$wheat, by=list(comdata$datum, comdata$town), FUN = mean)
toets <- melt(complot, id="Group.1")
colnames(complot) <- c("Date","Town","Wheat")
toets <- acast(complot, Date ~ Town, mean, value.var = "Wheat")
toets1 <- toets[21:33,wc.towns]

#coms.dorpe <- aggregate(comdata$wheat, by=list(comdata$date,comdata$town), FUN = function(x) sum(!is.na(x)))
#coms.dorpe$Group.2 <- paste(coms.dorpe$Group.2, "wheat")
#for(i in colnames(comdata)[7:29]) {
#    coms.dorpe1 <- aggregate(comdata[,i], by=list(comdata$date,comdata$town), FUN = function(x) sum(!is.na(x)))
#    coms.dorpe1$Group.2 <- paste(coms.dorpe1$Group.2, i)
#    coms.dorpe <- rbind(coms.dorpe, coms.dorpe1)
#}
#colnames(coms.dorpe) <- c("Date","variable","value")

#g <- ggplot(coms.dorpe, aes(x=Group.1,y=x,colour=Group.2,fill=Group.2))
#g <- g + geom_bar(stat="identity")
#g <- g + theme(legend.position="none")
#g <- g + ylab("Total obs")
#g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
#g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
#g

#---------------------------------------------------
#REPEAT SALES TOWN EXAMPLE
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



##====================##
## REPEAT SALES INDEX ##
##====================##


rscomdata <- comdata[,c("time_id","date","town","wheat")]
rscomdata$commodity <- "wheat"
colnames(rscomdata) <- c("counter","date","town","price","commodity")
for(i in colnames(comdata)[7:29]) {
    rscomdata1 <- comdata[,c("time_id","date","town",i)]
    rscomdata1$commodity <- i
    colnames(rscomdata1) <- c("counter","date","town","price","commodity")
    rscomdata <- rbind(rscomdata, rscomdata1)
}
rscomdata$lnprice <- log(rscomdata$price)
rscomdata <- transform(rscomdata, id = as.numeric(interaction(factor(town),factor(commodity),drop=TRUE)))


#==================================================================
#REPEAT SALES by Commodity (e.g. Group by Wheat)
#==================================================================
wc.towns <- c("Beaufort West","Cape Town","Clanwilliam","Malmesbury","Mossel Bay","Worcester")

ec.towns <- c("Aliwal North","Burghersdorp","Cradock","Dordrecht","East London","Graaff-Reinet","Graham's Town",
              "King William's Town","Port Alfred","Port Elizabeth","Queen's Town","Tarkastad")

kzn.towns <- c("Pietermaritzburg, Natal","Durban, Natal")

#in.towns <- c("Bloemfontein","Bulawayo","Colesberg","Kimberley","Pretoria","Salisbury","Vryburg")
in.towns <- c("Bloemfontein","Bulawayo","Colesberg","Johannesburg","Kimberley","Pretoria","Salisbury","Vryburg")

cape <- c(wc.towns,ec.towns)
col.towns <- c(cape,kzn.towns)
all.towns <- c(wc.towns,ec.towns,kzn.towns,in.towns)

#wtest <- c("Beaufort West","Cape Town","Worcester")

rscomdata <- rscomdata[rscomdata$town %in% cape,]
#-------------------------------------------------------------------

rscomdata1 <- rscomdata[rscomdata$commodity=="wheat",]
#"wheat","mealies","eggs","tobacco","butter","beef"
#rscomdata1 <- rscomdata1[rscomdata1$counter >20 & rscomdata1$counter <31 ,]
#rscomdata1$price.int <- na.approx(rscomdata1$price,rule=2)
#unique(rscomdata1$town)

g <- ggplot(data=rscomdata1,aes(x=date, y=price, colour=town)) 
g <- g + geom_point(size = 0.5) 
g <- g + geom_line()
g <- g + ylab("Wheat prices")
g <- g + xlab("")
g <- g + theme(legend.key.size = unit(0.5,"cm"))
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank())
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g


rscomdata1 <- rscomdata1[complete.cases(rscomdata1),]

repdata <- repsaledata(rscomdata1$lnprice,rscomdata1$counter,rscomdata1$id)  #transform the data to sales pairs
repdata <- repdata[complete.cases(repdata),]
repeatsales <- repsale(repdata$price0,repdata$time0,repdata$price1,repdata$time1,mergefirst=1,
                       graph=FALSE)   #generate the repeat sales index

RS_index <- exp(as.data.frame(repeatsales$pindex))*100
RS_index$Date <- seq(1,1,length.out = ncol(RS_index))
RS_index$Date <- unique(rscomdata$date)[c(1,sort(unique(c(repdata$time1,repdata$time0))))][-1]
colnames(RS_index) <- c("Index","Date")
RS_index <- RS_index[complete.cases(RS_index),]

RS_index.ex <- aggregate(comdata$town, by=list(comdata$date), FUN = function(x) sum(!is.na(x)))
colnames(RS_index.ex) <- c("Date","x")
RS_index.ex <- merge(RS_index.ex, RS_index, by="Date", all=TRUE)[,-2]

index_plot <- cbind(RS_index.ex,"Index")
index_plot <- index_plot[,c(1,3,2)]
colnames(index_plot) <- c("date","town","price")
index_plot <- rbind(index_plot, rscomdata[rscomdata$commodity=="wheat",c(2,3,4)])
g <- ggplot(data=index_plot,aes(x=date, y=price, colour=town)) 
g <- g + geom_point(size = 0.5) 
g <- g + geom_line()
g <- g + ylab("Wheat prices")
g <- g + xlab("")
g <- g + theme(legend.key.size = unit(0.5,"cm"))
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank())
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g

index_plot <- cbind(RS_index.ex,"Index")
index_plot <- index_plot[,c(1,3,2)]
colnames(index_plot) <- c("date","town","price")
g <- ggplot(data=index_plot,aes(x=date, y=price, colour=town)) 
g <- g + geom_point(size = 0.5) 
g <- g + geom_line()
g <- g + ylab("Wheat Index")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) + theme(legend.position="bottom")
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g

index_plot <- melt(RS_index, id="Date")  # convert to long format
g <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_point(size = 1) 
g <- g + geom_line()
g <- g + ylab("Wheat Index")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) + theme(legend.position="bottom")
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g



##=================##
## BLUE BOOKS DATA ##
##=================##

blue <- read.csv("Blue_Books.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)

coms <- aggregate(blue$oatmeal, by=list(blue$date), FUN = function(x) sum(!is.na(x)))
for(i in colnames(blue)[4:62]) {
    coms1 <- aggregate(blue[,i], by=list(blue$date), FUN = function(x) sum(!is.na(x)))
    coms <- merge(coms, coms1, by="Group.1",all.x=TRUE)
}
colnames(coms) <- c("Date",colnames(blue)[4:62])

complot <- melt(coms, id="Date") 
g <- ggplot(complot, aes(x=Date,value))
g <- g + geom_bar(stat="identity")
g <- g + theme(legend.title=element_blank())
g <- g + ylab("Total obs")
g <- g + theme(legend.key.size = unit(0.5,"cm"))
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g


rsblue <- blue[,c("date","town","oatmeal")]
rsblue$commodity <- "oatmeal"
colnames(rsblue) <- c("date","town","price","commodity")
for(i in colnames(blue)[4:62]) {
    rsblue1 <- blue[,c("date","town",i)]
    rsblue1$commodity <- i
    colnames(rsblue1) <- c("date","town","price","commodity")
    rsblue <- rbind(rsblue, rsblue1)
}
rsblue$lnprice <- log(rsblue$price)
rsblue <- transform(rsblue, id = as.numeric(interaction(factor(town),factor(commodity),drop=TRUE)))

#REPEAT SALES-----------------------------------
rsblue1 <- rsblue[rsblue$commodity=="wheat",]
#rsblue1$price.int <- na.approx(rsblue1$price,rule=2)

g <- ggplot(data=rsblue1,aes(x=date, y=price, colour=town)) 
g <- g + geom_point(size = 1) 
g <- g + geom_line()
g <- g + ylab("Wheat prices")
g <- g + xlab("")
g <- g + theme(legend.key.size = unit(0.3,"cm"))
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank())
g

rsblue1 <- rsblue1[complete.cases(rsblue1),]
repdata <- repsaledata(rsblue1$lnprice,rsblue1$date,rsblue1$id)  #transform the data to sales pairs
repdata <- repdata[complete.cases(repdata),]
repeatsales <- repsale(repdata$price0,repdata$time0,repdata$price1,repdata$time1,mergefirst=1,
                       graph=FALSE)   #generate the repeat sales index

rs_index.a <- exp(as.data.frame(repeatsales$pindex))*100
rs_index.a$Date <- seq(1,1,length.out = ncol(rs_index.a))
rs_index.a$Date <- unique(rsblue$date)
colnames(rs_index.a) <- c("Index","Date")

index_plot <- melt(rs_index.a, id="Date")  # convert to long format
g <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_point(size = 2) 
g <- g + geom_line()
g <- g + ylab("Wheat Index")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) + theme(legend.position="bottom")
g

#---------------------
#Temporal distribution
#---------------------

wheat.a <- rs_index.a
ts.wheat.a <- as.ts(wheat.a[,-2], start=1889, end= 1907, frequency = 1)

m1 <- td(ts.wheat.a ~ 1, to = "monthly", conversion = "last", method = "denton-cholette")
plot(predict(m1))
rs_index1 <- as.data.frame(predict(m1)[-1:-9])


wheat.m <- RS_index.ex
ts.wheat.m <- as.ts(wheat.m[,-1], start=c(1889,10),end=c(1914,8), frequency = 12)
ts.wheat.m1 <- na.approx(ts.wheat.m, na.rm=FALSE)
ts.wheat.m1 <- na.locf(ts.wheat.m1, na.rm=FALSE)
ts.wheat.m1 <- na.locf(ts.wheat.m1, na.rm=FALSE, fromLast=TRUE)

plot(ts.wheat.m)
plot(ts.wheat.m1)

m2 <- td(ts.wheat.a ~ 0 + ts.wheat.m1, to= "monthly", conversion = "last", method = "chow-lin-maxlog")
summary(m2)
plot(predict(m2))

rs_index2 <- as.data.frame(predict(m2))

#-------
#Combine

rs_index1$Date <- RS_index.ex$Date[-220:-299]
rsdata <- merge(RS_index.ex,rs_index1,by="Date", all = TRUE)
colnames(rsdata) <- c("Date","Com","Blue")
rsdata <- melt(rsdata,id="Date")
rsdata$lnprice <- log(rsdata$value)
rsdata <- rsdata[complete.cases(rsdata),]

repdata <- repsaledata(rsdata$lnprice,rsdata$Date,rsdata$variable)  #transform the data to sales pairs

repeatsales <- repsale(repdata$price0,repdata$time0,repdata$price1,repdata$time1,mergefirst=1,
                       graph=FALSE)   #generate the repeat sales index

Index <- exp(as.data.frame(repeatsales$pindex))*100
Index$Date <- seq(1,1,length.out = ncol(Index))
Index$Date <- sort(unique(c(repdata$time1,repdata$time0)))

Index1 <- merge(RS_index.ex,Index,by="Date", all = TRUE)
Index1 <- cbind(Index1,merge(RS_index.ex,rs_index1,by="Date", all = TRUE))[,-4:-5]
colnames(Index1) <- c("Date","Journal_Index","Index","Blue_Index")

index_plot <- melt(Index1, id="Date")  # convert to long format
g <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_point(size = 1) 
g <- g + geom_line()
g <- g + ylab("Wheat Index")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) + theme(legend.position="bottom")
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g


##===================================================================
#Do this for all the commodities

rscomdata1 <- rscomdata[rscomdata$commodity==c("cattle","d.oxen","m.cows","beef"),]

g <- ggplot(data=rscomdata1,aes(x=date, y=price, colour=town)) 
g <- g + geom_point(size = 0.5) 
g <- g + geom_line()
g <- g + ylab("Prices")
g <- g + xlab("")
g <- g + theme(legend.key.size = unit(0.5,"cm"))
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank())
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g


rscomdata1 <- rscomdata1[complete.cases(rscomdata1),]
repdata <- repsaledata(rscomdata1$lnprice,rscomdata1$counter,rscomdata1$id)  #transform the data to sales pairs
#repdata <- repdata[complete.cases(repdata),]
repeatsales <- repsale(repdata$price0,repdata$time0,repdata$price1,repdata$time1,mergefirst=1,
                       graph=FALSE)   #generate the repeat sales index

RS_index <- exp(as.data.frame(repeatsales$pindex))*100
RS_index$Date <- seq(1,1,length.out = ncol(RS_index))
RS_index$Date <- unique(rscomdata$date)[c(1,sort(unique(c(repdata$time1,repdata$time0))))][-1]
colnames(RS_index) <- c("Index","Date")

RS_index.ex <- aggregate(comdata$town, by=list(comdata$date), FUN = function(x) sum(!is.na(x)))
colnames(RS_index.ex) <- c("Date","x")
RS_index.ex <- merge(RS_index.ex, RS_index, by="Date", all=TRUE)[,-2]

index_plot <- cbind(RS_index.ex,"Index")
index_plot <- index_plot[,c(1,3,2)]
colnames(index_plot) <- c("date","town","price")
index_plot <- rbind(index_plot, rscomdata[rscomdata$commodity=="beef",c(2,3,4)])
g <- ggplot(data=index_plot,aes(x=date, y=price, colour=town)) 
g <- g + geom_point(size = 0.5) 
g <- g + geom_line()
g <- g + ylab("Prices")
g <- g + xlab("")
g <- g + theme(legend.key.size = unit(0.5,"cm"))
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank())
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g

index_plot <- cbind(RS_index.ex,"Index")
index_plot <- index_plot[,c(1,3,2)]
colnames(index_plot) <- c("date","town","price")
g <- ggplot(data=index_plot,aes(x=date, y=price, colour=town)) 
g <- g + geom_point(size = 0.5) 
g <- g + geom_line()
g <- g + ylab("Index")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) + theme(legend.position="bottom")
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g


rsblue1 <- rsblue[rsblue$commodity==c("cattle","d.oxen","m.cows","beef"),]

g <- ggplot(data=rsblue1,aes(x=date, y=price, colour=town)) 
g <- g + geom_point(size = 1) 
g <- g + geom_line()
g <- g + ylab("Prices")
g <- g + xlab("")
g <- g + theme(legend.key.size = unit(0.3,"cm"))
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank())
g

rsblue1 <- rsblue1[complete.cases(rsblue1),]
repdata <- repsaledata(rsblue1$lnprice,rsblue1$date,rsblue1$id)  #transform the data to sales pairs
#repdata <- repdata[complete.cases(repdata),]
repeatsales <- repsale(repdata$price0,repdata$time0,repdata$price1,repdata$time1,mergefirst=1,
                       graph=FALSE)   #generate the repeat sales index

rs_index.a <- exp(as.data.frame(repeatsales$pindex))*100
rs_index.a$Date <- seq(1,1,length.out = ncol(rs_index.a))
rs_index.a$Date <- unique(rsblue1$date)
colnames(rs_index.a) <- c("Index","Date")

index_plot <- melt(rs_index.a, id="Date")  # convert to long format
g <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_point(size = 2) 
g <- g + geom_line()
g <- g + ylab("Index")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) + theme(legend.position="bottom")
g


wheat.a <- rs_index.a
ts.wheat.a <- as.ts(wheat.a[,-2], start=1889, end= 1907, frequency = 1)
m1 <- td(ts.wheat.a ~ 1, to = "monthly", conversion = "last", method = "denton-cholette")
rs_index1 <- as.data.frame(predict(m1)[-1:-9])

rs_index1$Date <- RS_index.ex$Date[-220:-299]
rsdata <- merge(RS_index.ex,rs_index1,by="Date", all = TRUE)
colnames(rsdata) <- c("Date","Com","Blue")
rsdata <- melt(rsdata,id="Date")
rsdata$lnprice <- log(rsdata$value)
rsdata <- rsdata[complete.cases(rsdata),]

repdata <- repsaledata(rsdata$lnprice,rsdata$Date,rsdata$variable)  #transform the data to sales pairs

repeatsales <- repsale(repdata$price0,repdata$time0,repdata$price1,repdata$time1,mergefirst=1,
                       graph=FALSE)   #generate the repeat sales index

Index <- exp(as.data.frame(repeatsales$pindex))*100
Index$Date <- seq(1,1,length.out = ncol(Index))
Index$Date <- sort(unique(c(repdata$time1,repdata$time0)))

Index1 <- merge(RS_index.ex,Index,by="Date", all = TRUE)
Index1 <- cbind(Index1,merge(RS_index.ex,rs_index1,by="Date", all = TRUE))[,-4:-5]
colnames(Index1) <- c("Date","Journal_Index","Index","Blue_Index")

index_plot <- melt(Index1, id="Date")  # convert to long format
g <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_point(size = 1) 
g <- g + geom_line()
g <- g + ylab("Wheat Index")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) + theme(legend.position="bottom")
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g



##===================================================================
#Do this for all the commodities


makeindex <- function(produk) {
    rscomdata1 <- rscomdata[rscomdata$commodity==produk,]
    if(nrow(rscomdata1)>0) {
        rscomdata1 <- rscomdata1[complete.cases(rscomdata1),]
        repdata <- repsaledata(rscomdata1$lnprice,rscomdata1$counter,rscomdata1$id)  
        repdata <- repdata[complete.cases(repdata),]
        repeatsales <- repsale(repdata$price0,repdata$time0,repdata$price1,repdata$time1,mergefirst=1,graph=FALSE)   
        RS_index <- exp(as.data.frame(repeatsales$pindex))*100
        RS_index$Date <- seq(1,1,length.out = ncol(RS_index))
        RS_index$Date <- unique(rscomdata$date)[c(1,sort(unique(c(repdata$time1,repdata$time0))))][-1]
        colnames(RS_index) <- c("Journal_Index","Date")
        RS_index.ex <- aggregate(comdata$town, by=list(comdata$date), FUN = function(x) sum(!is.na(x)))
        colnames(RS_index.ex) <- c("Date","x")
        RS_index.ex <- merge(RS_index.ex, RS_index, by="Date", all=TRUE)[,-2]
    } else { 
        RS_index.ex <- aggregate(comdata$town, by=list(comdata$date), FUN = function(x) sum(!is.na(x)))
        colnames(RS_index.ex) <- c("Date","x")
    }
    
    rsblue1 <- rsblue[rsblue$commodity==produk,]
    if(nrow(rsblue1)>0) {
        rsblue1 <- rsblue1[complete.cases(rsblue1),]
        repdata <- repsaledata(rsblue1$lnprice,rsblue1$date,rsblue1$id)  
        repdata <- repdata[complete.cases(repdata),]
        repeatsales <- repsale(repdata$price0,repdata$time0,repdata$price1,repdata$time1,mergefirst=1,graph=FALSE)   
        rs_index.a <- exp(as.data.frame(repeatsales$pindex))*100
        rs_index.a$Date <- seq(1,1,length.out = ncol(rs_index.a))
        rs_index.a$Date <- unique(rsblue$date)
        colnames(rs_index.a) <- c("Index","Date")
    
        wheat.a <- rs_index.a
        ts.wheat.a <- as.ts(wheat.a[,-2], start=1889, end= 1907, frequency = 1)
        m1 <- td(ts.wheat.a ~ 1, to = "monthly", conversion = "last", method = "denton-cholette")
        rs_index1 <- as.data.frame(predict(m1)[-1:-9])
        rs_index1$Date <- RS_index.ex$Date[-220:-299]
        rs_index1 <- merge(RS_index.ex,rs_index1,by="Date", all = TRUE)[,-2]
        colnames(rs_index1) <- c("Date","Blue_Index")
    } else {
        rs_index1 <- aggregate(comdata$town, by=list(comdata$date), FUN = function(x) sum(!is.na(x))) 
    }
    
    if(nrow(rscomdata1)==0) { Index1 <- rs_index1 } 
    if(nrow(rsblue1)==0)    { Index1 <- RS_index.ex }
    
    if(nrow(rscomdata1)>0 & nrow(rsblue1)>0) {
        rsdata <- merge(RS_index.ex,rs_index1,by="Date", all = TRUE)
        colnames(rsdata) <- c("Date","Journal_Index","Blue_Index")
        rsdata <- melt(rsdata,id="Date")
        rsdata$lnprice <- log(rsdata$value)
        rsdata <- rsdata[complete.cases(rsdata),]
        repdata <- repsaledata(rsdata$lnprice,rsdata$Date,rsdata$variable)  
        repeatsales <- repsale(repdata$price0,repdata$time0,repdata$price1,repdata$time1,mergefirst=1,graph=FALSE)   
        
        Index <- exp(as.data.frame(repeatsales$pindex))*100
        Index$Date <- seq(1,1,length.out = ncol(Index))
        Index$Date <- sort(unique(c(repdata$time1,repdata$time0)))
        Index1 <- merge(RS_index.ex,Index,by="Date", all = TRUE)
        Index1 <- cbind(Index1,merge(RS_index.ex,rs_index1,by="Date", all = TRUE))[,-4:-5]
        colnames(Index1) <- c("Date","Journal_Index","Index","Blue_Index")
    }
    return(Index1)
}

product <- makeindex(c("mealies")) 

index_plot <- melt(product, id="Date")  # convert to long format
g <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_point(size = 1) 
g <- g + geom_line()
g <- g + ylab("Index")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) + theme(legend.position="bottom")
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g


#==================
#comdata produkte:
#"wheat","wheat.flour","boer.meal","mealies","mealie.meal","barley","oats","oathay","lucerne.hay","potatoes","tobacco"
#"beef","mutton","butter","eggs","cattle","sheep","pigs","bread","oranges","s.horses","tr.oxen","mi.cows","w.sheep"    

#blue produkte:
#"oatmeal","flour","bread","mutton","beef","pork","bacon","butter.fresh","butter.salt","cheese","tea","coffee","sugar","rice"
#"tobacco","dried.fruit","salt","wine","brandy","beer.eng","beer.col","milk","cond.milk","candles","lamp.oil","s.horse"        
#"d.horse","mules","asses","d.oxen","m.cows","w.sheep","c.sheep","swine","goats","fowls","ducks","w.wool","u.wool","butter"         
#"fat.tallow","soap","hides","sheep.skins","goat.skins",
#"wheat","barley","rye","oats","mealies","peas.beans","potatoes","wine.better","wine.ordinary"  
#"brandy.better","brandy.ordinary","pumpkins","d.fruit","aloes","argol"     


#AGRICULTURAL PRODUCE: 
#"wheat","barley","oats","oathay","rye","peas.beans","potatoes","tobacco",c("dried.fruit","d.fruit") 
#c("wine","wine.better","wine.ordinary"),c("brandy","brandy.better","brandy.ordinary")
produce <- cbind(wheat=makeindex("wheat")[,c(1,3)],mealies=makeindex("mealies")[,3],barley=makeindex("barley")[,3],oats=makeindex("oats")[,3],
                 oathay=makeindex("oathay")[,2],rye=makeindex("rye")[,2],peas.beans=makeindex("peas.beans")[,2],
                 potatoes=makeindex("potatoes")[,3],tobacco=makeindex("tobacco")[,3],d.fruit=makeindex(c("dried.fruit","d.fruit"))[,2],
                 wine=makeindex(c("wine","wine.better","wine.ordinary"))[,2],brandy=makeindex(c("brandy","brandy.better","brandy.ordinary"))[,2]) 
colnames(produce)[1:2] <- c("Date","wheat")

#LIVESTOCK:
#c("cattle","d.oxen","m.cows"),c("s.horse","d.horse","mules","asses"),c("sheep","w.sheep","c.sheep"),
#"swine","goats",c("fowls","ducks")
livestock <- cbind(cattle=makeindex(c("cattle","d.oxen","m.cows"))[,c(1,3)],horses=makeindex(c("s.horse","d.horse","mules","asses"))[,2],
                   sheep=makeindex(c("sheep","w.sheep","c.sheep"))[,3],swine=makeindex("swine")[,2],goats=makeindex("goats")[,2],
                   fowls=makeindex(c("fowls","ducks"))[,2]) 
colnames(livestock)[1:2] <- c("Date","cattle")

#PASTORAL PRODUCTS: 
#c("butter","butter.fresh","butter.salt"),c("w.wool","u.wool"),"hides",c("sheep.skins","goat.skins"),"cheese","fat.tallow","soap"
pastoral <- cbind(wool=makeindex(c("w.wool","u.wool"))[,1:2],hides=makeindex(c("hides"))[,2],skins=makeindex(c("sheep.skins","goat.skins"))[,2],
                  butter=makeindex(c("butter","butter.fresh","butter.salt"))[,3],cheese=makeindex("cheese")[,2],
                  fat.tallow=makeindex("fat.tallow")[,2],soap=makeindex("soap")[,2]) 
colnames(pastoral)[1:2] <- c("Date","wool")

#PROVISIONS: 
#"beef","mutton",c("pork","bacon"),"eggs","bread",c("beer.eng","beer.col"),c("wheat.flour","flour"),"mealie.meal","boer.meal","oatmeal"
#"tea","coffee","sugar","rice","salt","milk","candles"
provisions <- cbind(beef=makeindex("beef")[,c(1,3)],mutton=makeindex("mutton")[,3],pork=makeindex(c("pork","bacon"))[,2],
                    eggs=makeindex("eggs")[,2],bread=makeindex("bread")[,3],beer=makeindex(c("beer.eng","beer.col"))[,2],
                    mealie.meal=makeindex("mealie.meal")[,2],boer.meal=makeindex("boer.meal")[,2],oatmeal=makeindex("oatmeal")[,2],
                    tea=makeindex("tea")[,2],coffee=makeindex("coffee")[,2],sugar=makeindex("sugar")[,2],rice=makeindex("rice")[,2],
                    salt=makeindex("salt")[,2],milk=makeindex("milk")[,2],candles=makeindex("candles")[,2])  
colnames(provisions)[1:2] <- c("Date","beef")


#Uitgelos:
#Com produkte:"lucerne.hay","oranges","tr.oxen","mi.cows","s.horses",
#Blue produkte:"pumpkins","aloes","argol","pigs","cond.milk" 

index_plot <- melt(provisions, id="Date")  # convert to long format
g <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_point(size = 1) 
g <- g + geom_line()
g <- g + ylab("Index")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) + theme(legend.position="bottom")
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g


#Calculate Average Prices for 1904
gewig <- read.csv("Weights.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)

#colnames(livestock)[-1]
#colnames(gewig)[20:25]
#check <- priceIndex(colnames(livestock)[-1],colnames(gewig)[20:25],1,toets,na.rm=FALSE, weights = TRUE)


##MOVE BUTTER TO PRODUCE OR PROVISIONS !!!!!
toets <- produce
for(j in 2:ncol(produce)) {   #maak eers die growth rates
    toets[1,j] <- 1
    for(i in 2:299) {
        tel <- 0
        repeat {
            tel <- tel + 1 
            if(!is.na(produce[i-tel,j])) {
                toets[i,j] <- produce[i,j]/produce[i-tel,j]
                break
            }
        }
        
    }
}

toets <- cbind(toets,gewig[,1:12])  #Kry dan weighted average
check <- data.frame()
for(i in 1:299) {
    check[i,1] <- weighted.mean(toets[i,2:13],toets[i,14:25],na.rm=TRUE)
}

toets$produce[1] <- check[1,1]*100 
for(i in 2:299) {        #maak dan die indeks
    if(!is.na(toets$produce[i-1])) {
        toets$produce[i] <- check[i,1]*toets$produce[i-1]
    } else {
        tel <- 1
        repeat {
            tel <- tel + 1 
            if(!is.na(toets$produce[i-tel])) {
                toets$produce[i] <- check[i,1]*toets$produce[i-tel]
                break
            }
        }
    }
}

produce$index <- toets$produce


toets <- pastoral
for(j in 2:ncol(pastoral)) {   #maak eers die growth rates
    toets[1,j] <- 1
    for(i in 2:299) {
        tel <- 0
        repeat {
            tel <- tel + 1 
            if(!is.na(pastoral[i-tel,j])) {
                toets[i,j] <- pastoral[i,j]/pastoral[i-tel,j]
                break
            }
        }
        
    }
}

toets <- cbind(toets,gewig[,13:19])  #Kry dan weighted average
check <- data.frame()
for(i in 1:299) {
    check[i,1] <- weighted.mean(toets[i,2:8],toets[i,9:15],na.rm=TRUE)
}

toets$pastoral[1] <- check[1,1]*100 
for(i in 2:299) {        #maak dan die indeks
    if(!is.na(toets$pastoral[i-1])) {
        toets$pastoral[i] <- check[i,1]*toets$pastoral[i-1]
    } else {
        tel <- 1
        repeat {
            tel <- tel + 1 
            if(!is.na(toets$pastoral[i-tel])) {
                toets$pastoral[i] <- check[i,1]*toets$pastoral[i-tel]
                break
            }
        }
    }
}

pastoral$index <- toets$pastoral


toets <- livestock
for(j in 2:ncol(livestock)) {   #maak eers die growth rates
    toets[1,j] <- 1
    for(i in 2:299) {
        tel <- 0
        repeat {
            tel <- tel + 1 
            if(!is.na(livestock[i-tel,j])) {
                toets[i,j] <- livestock[i,j]/livestock[i-tel,j]
                break
            }
        }
        
    }
}

toets <- cbind(toets,gewig[,20:25])  #Kry dan weighted average
check <- data.frame()
for(i in 1:299) {
    check[i,1] <- weighted.mean(toets[i,2:7],toets[i,8:13],na.rm=TRUE)
}

toets$livestock[1] <- check[1,1]*100 
for(i in 2:299) {        #maak dan die indeks
    if(!is.na(toets$livestock[i-1])) {
        toets$livestock[i] <- check[i,1]*toets$livestock[i-1]
    } else {
        tel <- 1
        repeat {
            tel <- tel + 1 
            if(!is.na(toets$livestock[i-tel])) {
                toets$livestock[i] <- check[i,1]*toets$livestock[i-tel]
                break
            }
        }
    }
}

livestock$index <- toets$livestock

#----------------------------------
toets <- cbind(produce,gewig[,1:12])
check <- data.frame()
for(i in 1:299) {
    check[i,1] <- weighted.mean(toets[i,2:13],toets[i,14:25],na.rm=TRUE)
}

toets <- cbind(pastoral,gewig[,13:19])
check <- data.frame()
for(i in 1:299) {
    check[i,1] <- weighted.mean(toets[i,2:8],toets[i,9:15],na.rm=TRUE)
}

toets <- cbind(livestock,gewig[,20:25])
check <- data.frame()
for(i in 1:299) {
    check[i,1] <- weighted.mean(toets[i,2:7],toets[i,8:13],na.rm=TRUE)
}

toets <- cbind(provisions,gewig[,20:25])
check <- data.frame()
for(i in 1:299) {
    check[i,1] <- weighted.mean(toets[i,2:7],toets[i,8:13],na.rm=TRUE)
}



#====================================================================
#REPEAT SALES by TOWN and COMMODITY (e.g. Group by Cape Town & Wheat)
#====================================================================
comnames1 <- c("wheat","mealies","eggs","tobacco","butter","beef","mutton")
comnames2 <- c("wheat","wheat.flour","boer.meal","mealies","mealie.meal","barley","oats","oathay",
               "potatoes","tobacco","beef","mutton","butter","eggs")
comnames.all <- c("wheat","wheat.flour","boer.meal","mealies","mealie.meal","barley","oats","oathay","lucerne.hay",
                  "potatoes","tobacco","beef","mutton","butter","eggs","cattle","sheep","pigs","bread","oranges",
                  "s.horses","tr.oxen","m.cows","w.sheep")

#-------------------------------------------------------------------

rscomdata1 <- rscomdata[rscomdata$commodity=="wheat",]
rscomdata1 <- rscomdata1[rscomdata1$town =="Beaufort West",]
rscomdata1$price.int <- na.approx(rscomdata1$price,rule=2)

g <- ggplot(data=rscomdata1,aes(x=date, y=price)) 
g <- g + geom_point(size = 2) 
g <- g + geom_line()
g <- g + ylab("Prices")
g <- g + xlab("")
g <- g + theme(legend.key.size = unit(0.5,"cm"))
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank())
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g

rsblue1 <- rsblue[rsblue$commodity =="wheat",]
rsblue1 <- rsblue1[rsblue1$town=="Beaufort West",]

g <- ggplot(data=rsblue1,aes(x=date, y=price, colour=commodity)) 
g <- g + geom_point(size = 2) 
g <- g + geom_line()
g <- g + ylab("Prices")
g <- g + xlab("")
g <- g + theme(legend.key.size = unit(0.5,"cm"))
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank())
g

wheat.a <- rsblue1$price
#wheat.a <- dcast(rsblue1, date ~ commodity, mean, value.var="price")
ts.wheat.a <- as.ts(wheat.a, start=1889, end= 1907, frequency = 1)
ts.wheat.a1 <- na.approx(ts.wheat.a, na.rm=FALSE)
ts.wheat.a1 <- na.locf(ts.wheat.a1, na.rm=FALSE)
ts.wheat.a1 <- na.locf(ts.wheat.a1, na.rm=FALSE, fromLast=TRUE)

m1 <- td(ts.wheat.a1 ~ 1, to = "monthly", conversion = "average", method = "denton-cholette")
plot(predict(m1))

wheat.am <- cbind(as.data.frame(RS_index.ex$Date[1:219]),as.data.frame(predict(m1)[-1:-9]))
colnames(wheat.am) <- c("Date","Wheat")

wheat.am[format(wheat.am$Date,'%Y') %in% rsblue1$date[is.na(rsblue1$price)],2] <- NA 
wheat.am <- cbind(wheat.am, lnprice=log(wheat.am[,2]),counter=1:219, id=1)

wheat.am1 <- rbind(wheat.am[,c(4,3,5)],rscomdata1[,c(1,6,7)])
wheat.am1 <- wheat.am1[complete.cases(wheat.am1),]

repdata <- repsaledata(wheat.am1$lnprice,wheat.am1$counter,wheat.am1$id)  #transform the data to sales pairs
repeatsales <- repsale(repdata$price0,repdata$time0,repdata$price1,repdata$time1,mergefirst=1,
                       graph=FALSE)   #generate the repeat sales index

RS_index <- exp(as.data.frame(repeatsales$pindex))*100
RS_index$Date <- seq(1,1,length.out = ncol(RS_index))
RS_index$Date <- unique(rscomdata$date)[c(1,sort(unique(c(repdata$time1,repdata$time0))))][-1]
colnames(RS_index) <- c("Index","Date")

RS_index.ex <- aggregate(comdata$town, by=list(comdata$date), FUN = function(x) sum(!is.na(x)))
colnames(RS_index.ex) <- c("Date","x")
RS_index.ex <- merge(RS_index.ex, RS_index, by="Date", all=TRUE)[,-2]
capewheat <- aggregate(rscomdata1$price, by=list(rscomdata1$date), FUN = mean)


index_plot <- cbind(RS_index.ex, Cape = capewheat$x)
index_plot <- merge(index_plot,wheat.am[,c(1,2)], by="Date", all=TRUE)
colnames(index_plot) <- c("Date","Index","Beaufort West (lbs)","Blue Books (bushel)")

index_plot <- melt(index_plot, id="Date")  # convert to long format
g <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_point(size = 0.5) 
g <- g + geom_line()
g <- g + ylab("Index")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) + theme(legend.position="bottom")
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g

#----------------------------------------------------------------------------------------------------------
#Temporally disaggregate almal in blue into town commodity pairs
wheat.am <- as.data.frame(RS_index.ex$Date[1:219])
colnames(wheat.am) <- "Date"
rsblue1 <- rsblue[rsblue$commodity =="wheat",]
tel <- 1

for(i in levels(rsblue$town)) {
    
    rsblue2 <- rsblue1[rsblue1$town==i,]
    
    if(sum(!is.na(rsblue2$price))>1) { 
        tel <- tel + 1
        wheat.a <- rsblue2$price
        ts.wheat.a <- as.ts(wheat.a, start=1889, end= 1907, frequency = 1)
        ts.wheat.a1 <- na.approx(ts.wheat.a, na.rm=FALSE)
        ts.wheat.a1 <- na.locf(ts.wheat.a1, na.rm=FALSE)
        ts.wheat.a1 <- na.locf(ts.wheat.a1, na.rm=FALSE, fromLast=TRUE)
        m1 <- td(ts.wheat.a1 ~ 1, to = "monthly", conversion = "average", method = "denton-cholette")
        wheat.am <- cbind(wheat.am,i=as.data.frame(predict(m1)[-1:-9]))
        wheat.am[format(wheat.am$Date,'%Y') %in% rsblue2$date[is.na(rsblue2$price)],tel] <- NA 
        colnames(wheat.am)[tel] <- i
    }
}



wheat.am <- cbind(counter=1:219,lnprice=log(wheat.am),id=1)
colnames(wheat.am) <- c("counter","lnprice","id")
wheat.am <- rbind(wheat.am,rscomdata1[,c(1,6,7)])
wheat.am <- wheat.am[complete.cases(wheat.am),]

repdata <- repsaledata(wheat.am$lnprice,wheat.am$counter,wheat.am$id)  #transform the data to sales pairs
repeatsales <- repsale(repdata$price0,repdata$time0,repdata$price1,repdata$time1,mergefirst=1,
                       graph=FALSE)   #generate the repeat sales index

RS_index <- exp(as.data.frame(repeatsales$pindex))*100
RS_index$Date <- seq(1,1,length.out = ncol(RS_index))
RS_index$Date <- unique(rscomdata$date)[c(1,sort(unique(c(repdata$time1,repdata$time0))))][-1]
colnames(RS_index) <- c("Index","Date")

RS_index.ex <- aggregate(comdata$town, by=list(comdata$date), FUN = function(x) sum(!is.na(x)))
colnames(RS_index.ex) <- c("Date","x")
RS_index.ex <- merge(RS_index.ex, RS_index, by="Date", all=TRUE)[,-2]
capewheat <- aggregate(rscomdata1$price, by=list(rscomdata1$date), FUN = mean)


index_plot <- cbind(RS_index.ex, Cape = capewheat$x)
wheat.am <- as.data.frame(predict(m1)[-1:-9])
wheat.am$Date <- index_plot$Date[1:219]
index_plot <- merge(index_plot,wheat.am, by="Date", all=TRUE)
colnames(index_plot) <- c("Date","Index","Cape Town (lbs)","Blue Books (bushel)")

index_plot <- melt(index_plot, id="Date")  # convert to long format
g <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_point(size = 0.5) 
g <- g + geom_line()
g <- g + ylab("Index")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) + theme(legend.position="bottom")
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g






