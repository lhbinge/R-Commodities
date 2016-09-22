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

#setwd("C:/Users/Laurie/OneDrive/Documents/BING/METRICS/PhD Proposal Readings/Art Price Index")
setwd("C:\\Users\\Laurie\\OneDrive\\Documents\\BING\\PhD Proposal Readings\\Commodity Cycles\\R Commodities")
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

in.towns <- c("Bloemfontein","Bulawayo","Colesberg","Kimberley","Pretoria","Salisbury","Vryburg")
#in.towns <- c("Bloemfontein","Bulawayo","Colesberg","Johannesburg","Kimberley","Pretoria","Salisbury","Vryburg")

cape <- c(wc.towns,ec.towns)
col.towns <- c(cape,kzn.towns)
all.towns <- c(wc.towns,ec.towns,kzn.towns,in.towns)

#-------------------------------------------------------------------
rscomdata1 <- rscomdata[rscomdata$commodity=="mealies",]
#"wheat","mealies","eggs","tobacco","butter","beef"
rscomdata1 <- rscomdata1[rscomdata1$town %in% col.towns,]

rscomdata1$price.int <- na.approx(rscomdata1$price,rule=2)

g <- ggplot(data=rscomdata1,aes(x=date, y=price, colour=town)) 
g <- g + geom_point(size = 2) 
g <- g + geom_line()
g <- g + ylab("Wheat prices")
g <- g + xlab("")
g <- g + theme(legend.key.size = unit(0.5,"cm"))
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank())
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g

g <- ggplot(data=rscomdata1,aes(x=date, y=price.int, colour=town)) 
g <- g + geom_point(size = 1) 
g <- g + geom_line()
g <- g + ylab("Wheat prices")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank())
g <- g + theme(legend.key.size = unit(0.5,"cm"))
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g


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
index_plot <- rbind(index_plot, rscomdata1[,c(2,3,4)])

g <- ggplot(data=index_plot,aes(x=date, y=price, colour=town)) 
g <- g + geom_point(size = 2) 
g <- g + geom_line()
g <- g + ylab("Wheat prices")
g <- g + xlab("")
g <- g + theme(legend.key.size = unit(0.5,"cm"))
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank())
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g

RS_index.ex$int <- na.approx(RS_index.ex$Index,rule=2)
index_plot <- cbind(RS_index.ex[,c(1,3)],"Index")
index_plot <- index_plot[,c(1,3,2)]
colnames(index_plot) <- c("date","town","price")
index_plot <- rbind(index_plot, rscomdata1[,c(2,3,4)])

g <- ggplot(data=index_plot,aes(x=date, y=price, colour=town)) 
g <- g + geom_point(size = 1) 
g <- g + geom_line()
g <- g + ylab("Wheat prices")
g <- g + xlab("")
g <- g + theme(legend.key.size = unit(0.5,"cm"))
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank())
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g


index_plot <- melt(RS_index.ex, id="Date")  # convert to long format
g <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_point(size = 2) 
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


#==================================================================
#REPEAT SALES by TOWN (e.g. Group by Cape Town)
#==================================================================
comnames1 <- c("wheat","mealies","eggs","tobacco","butter","beef","mutton")
comnames2 <- c("wheat","wheat.flour","boer.meal","mealies","mealie.meal","barley","oats","oathay",
               "potatoes","tobacco","beef","mutton","butter","eggs")
comnames.all <- c("wheat","wheat.flour","boer.meal","mealies","mealie.meal","barley","oats","oathay","lucerne.hay",
                  "potatoes","tobacco","beef","mutton","butter","eggs","cattle","sheep","pigs","bread","oranges",
                  "s.horses","tr.oxen","m.cows","w.sheep")

#-------------------------------------------------------------------

rscomdata1 <- rscomdata[rscomdata$town =="Cape Town",]
rscomdata1 <- rscomdata1[rscomdata1$commodity %in% comnames2,]

rscomdata1$price.int <- na.approx(rscomdata1$price,rule=2)

g <- ggplot(data=rscomdata1,aes(x=date, y=price, colour=commodity)) 
g <- g + geom_point(size = 2) 
g <- g + geom_line()
g <- g + ylab("Prices")
g <- g + xlab("")
g <- g + theme(legend.key.size = unit(0.5,"cm"))
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank())
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g

g <- ggplot(data=rscomdata1,aes(x=date, y=price.int, colour=commodity)) 
g <- g + geom_point(size = 1) 
g <- g + geom_line()
g <- g + ylab("Prices")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank())
g <- g + theme(legend.key.size = unit(0.5,"cm"))
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g


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
colnames(index_plot) <- c("date","commodity","price")
index_plot <- rbind(index_plot, rscomdata1[,c(2,5,4)])

g <- ggplot(data=index_plot,aes(x=date, y=price, colour=commodity)) 
g <- g + geom_point(size = 3) 
g <- g + geom_line()
g <- g + ylab("Index")
g <- g + xlab("")
g <- g + theme(legend.key.size = unit(0.5,"cm"))
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank())
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g

RS_index.ex$int <- na.approx(RS_index.ex$Index,rule=2)

index_plot <- cbind(RS_index.ex[,c(1,3)],"Index")
index_plot <- index_plot[,c(1,3,2)]
colnames(index_plot) <- c("date","commodity","price")
index_plot <- rbind(index_plot, rscomdata1[,c(2,5,4)])

g <- ggplot(data=index_plot,aes(x=date, y=price, colour=commodity)) 
g <- g + geom_point(size = 1) 
g <- g + geom_line()
g <- g + ylab("Index")
g <- g + xlab("")
g <- g + theme(legend.key.size = unit(0.5,"cm"))
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank())
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g


index_plot <- melt(RS_index.ex, id="Date")  # convert to long format
g <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_point(size = 3) 
g <- g + geom_line()
g <- g + ylab("Index")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) + theme(legend.position="bottom")
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g

index_plot <- melt(RS_index, id="Date")  # convert to long format
g <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_point(size = 1) 
g <- g + geom_line()
g <- g + ylab("Index")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) + theme(legend.position="bottom")
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g

#----------------------------------------------------------------------------------------------------------
#======= Total: Almal aggregated
#----------------------------------------------------------------------------------------------------------
repdata <- repsaledata(rscomdata$lnprice,rscomdata$counter,rscomdata$id)  #transform the data to sales pairs
repdata <- repdata[complete.cases(repdata),]
repeatsales <- repsale(repdata$price0,repdata$time0,repdata$price1,repdata$time1,mergefirst=1,graph=FALSE)   #generate the repeat sales index

RS_index <- exp(as.data.frame(repeatsales$pindex))*100
RS_index$Date <- seq(1,1,length.out = ncol(RS_index))
RS_index <- RS_index[complete.cases(RS_index),]
RS_index$Date <- unique(rscomdata$date)[c(1,sort(unique(repdata$time1)))]
colnames(RS_index) <- c("Index","Date")

RS_index.ex <- aggregate(comdata$town, by=list(comdata$date), FUN = function(x) sum(!is.na(x)))
colnames(RS_index.ex) <- c("Date","x")
RS_index.ex <- merge(RS_index.ex, RS_index, by="Date", all=TRUE)[,-2]

index_plot <- melt(RS_index.ex, id="Date")  # convert to long format
g <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_point(size = 1) 
g <- g + geom_line()
g <- g + ylab("Index")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) + theme(legend.position="bottom")
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g

index_plot <- melt(RS_index, id="Date")  # convert to long format
g <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_point(size = 1) 
g <- g + geom_line()
g <- g + ylab("Index")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) + theme(legend.position="bottom")
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g

#------------------------------------------------------------------------
# old versions
rscomdata1 <- rscomdata[rscomdata$commodity=="wheat",]
#rscomdata1 <- rscomdata[rscomdata$commodity=="mealies",]
#rscomdata1 <- rscomdata[rscomdata$commodity=="eggs",]

#rscomdata1 <- rscomdata[rscomdata$town=="Cape Town",]
#rscomdata1 <- rscomdata[rscomdata$town=="Kimberley",]
#rscomdata1 <- rscomdata[rscomdata$town=="Bloemfontein",]
#rscomdata1 <- rscomdata[rscomdata$town=="East London",]

repdata <- repsaledata(rscomdata1$lnprice,rscomdata1$counter,rscomdata1$id)  #transform the data to sales pairs
repdata <- repdata[complete.cases(repdata),]
repeatsales <- repsale(repdata$price0,repdata$time0,repdata$price1,repdata$time1,mergefirst=1,
                       graph=FALSE)   #generate the repeat sales index

RS_index <- exp(as.data.frame(repeatsales$pindex))*100
RS_index$Date <- seq(1,1,length.out = ncol(RS_index))
#RS_index <- RS_index[complete.cases(RS_index),]
#RS_index$Date <- unique(rscomdata$date)[c(1,sort(unique(repdata$time1)))]
#colnames(RS_index) <- c("Index","Date")

RS_index$Date <- unique(rscomdata$date)[c(1,sort(unique(c(repdata$time1,repdata$time0))))][-1]
colnames(RS_index) <- c("Index","Date")
RS_index <- RS_index[complete.cases(RS_index),]

RS_index.ex <- aggregate(comdata$town, by=list(comdata$date), FUN = function(x) sum(!is.na(x)))
colnames(RS_index.ex) <- c("Date","x")
RS_index.ex <- merge(RS_index.ex, RS_index, by="Date", all=TRUE)[,-2]


index_plot <- melt(RS_index.ex, id="Date")  # convert to long format
g <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_point(size = 1) 
g <- g + geom_line()
g <- g + ylab("Index")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) + theme(legend.position="bottom")
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g

index_plot <- melt(RS_index, id="Date")  # convert to long format
g <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_point(size = 1) 
g <- g + geom_line()
g <- g + ylab("Index")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) + theme(legend.position="bottom")
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g


#-------------------------------------------------------------------

RSindex <- function(rscomdata1, titel) {
    repdata <- repsaledata(rscomdata1$lnprice,rscomdata1$counter,rscomdata1$id)  #transform the data to sales pairs
    repdata <- repdata[complete.cases(repdata),]
    repeatsales <- repsale(repdata$price0,repdata$time0,repdata$price1,repdata$time1,mergefirst=1,graph=FALSE)   #generate the repeat sales index
    
    RS_index <- exp(as.data.frame(repeatsales$pindex))*100
    RS_index$Date <- seq(1,1,length.out = ncol(RS_index))
    RS_index <- RS_index[complete.cases(RS_index),]
    RS_index$Date <- unique(rscomdata$date)[c(1,sort(unique(repdata$time1)))]
    colnames(RS_index) <- c(titel,"Date")
    
    RS_index.ex <- aggregate(comdata$town, by=list(comdata$date), FUN = function(x) sum(!is.na(x)))
    colnames(RS_index.ex) <- c("Date","x")
    RS_index.ex <- merge(RS_index.ex, RS_index, by="Date", all=TRUE)[,-2]
    return(RS_index.ex)   
}

RS_index.c <- cbind(RSindex(rscomdata[rscomdata$commodity=="wheat",],"wheat"),
                    RSindex(rscomdata[rscomdata$commodity=="mealies",],"mealies"),
                    RSindex(rscomdata[rscomdata$commodity=="tobacco",],"tobacco"),
                    RSindex(rscomdata[rscomdata$commodity=="eggs",],"eggs"))

RS_index.c <- RS_index.c[,c(1,2,4,6,8)]

index_plot <- melt(RS_index.c, id="Date")  # convert to long format
g <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_point(size = 1) 
g <- g + geom_line()
g <- g + ylab("Index")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) + theme(legend.position="bottom")
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g

RS_index.d <- cbind(RSindex(rscomdata[rscomdata$town=="Cape Town",],"Cape Town"),
                    RSindex(rscomdata[rscomdata$town=="Kimberley",],"Kimberley"),
                    RSindex(rscomdata[rscomdata$town=="Bloemfontein",],"Bloemfontein"),
                    RSindex(rscomdata[rscomdata$town=="East London",],"East London"))

RS_index.d <- RS_index.d[,c(1,2,4,6,8)]

index_plot <- melt(RS_index.d, id="Date")  # convert to long format
g <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_point(size = 1) 
g <- g + geom_line()
g <- g + ylab("Index")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) + theme(legend.position="bottom")
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g


##============
#Interpolation
##============

library(tempdisagg)
td(formula, conversion = "sum", to = "quarterly", method = "chow-lin-maxlog",
   truncated.rho = 0, fixed.rho = 0.5, criterion = "proportional", h = 1,
   start = NULL, end = NULL, ...)

m1 <- td(y.a ~ 1, conversion="average",to="monthly",method = "denton-cholette")
m1 <- td(y.a ~ x1.q + x2.q, conversion="average",to="monthly",method = "litterman-minrss")
y.q <- predict(m1)



