##===============================================================================================##
## -------------------------------- COMMODITY INDEX ---------------------------------------------##
##===============================================================================================##

##=====================##
## READING IN THE DATA ##
##=====================##
suppressMessages(library(zoo))           
suppressMessages(library(ggplot2))
suppressMessages(library(plyr))
suppressMessages(library(dplyr))
suppressMessages(library(reshape2))
suppressMessages(library(stargazer))
suppressMessages(library(micEcon))
suppressMessages(library(quantreg))
suppressMessages(library(McSpatial))
suppressMessages(library(quantmod))
suppressMessages(library(xtable))
suppressMessages(library(scales))
suppressMessages(library(tseries))
suppressMessages(library(urca))
suppressMessages(library(lmtest))
suppressMessages(library(grid))
suppressMessages(library(tempdisagg))
suppressMessages(library(gridExtra))


setwd("C:\\Users\\Laurie\\OneDrive\\Documents\\BING\\Commodity Cycles\\R Commodities")

GDP <- read.csv("GDP.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
GDP$Date <- as.Date(GDP$Date)

suppressMessages(library(BCDating))

ts.GDP.a <- ts(GDP[,c("lnRGDP")],start =c(1856),end=c(1909),frequency=1)
m1 <- td(ts.GDP.a ~ 1, to = "quarterly", conversion = "last", method = "denton-cholette")
ts.GDP.m <- predict(m1)

#datums <- as.Date(time(ts.GDP.m), frac = 1)
#write.csv(indicator_plot,"dates.csv")
datums <- read.csv("dates.csv")
datums$Date <- as.Date(datums$Date)

dat <- BBQ(ts.GDP.m, mincycle = 5, minphase = 2, name="lnRGDP")
tp <- as.data.frame(show(dat))[,-3]
tp$Peaks <- as.character(tp$Peaks)
tp$Peaks <- as.Date(as.yearqtr(tp$Peaks, format = "%YQ%q"), frac = 1)
tp$Troughs <- as.character(tp$Troughs)
tp$Troughs <- as.Date(as.yearqtr(tp$Troughs, format = "%YQ%q"), frac = 1)
tp$Troughs[nrow(tp)] <- "1909-12-31"

indicator_plot <- cbind(datums$Date,as.data.frame(ts.GDP.q))
colnames(indicator_plot) <- c("Date","lnRGDP")
indicator_plot$lnRGDP <- as.numeric(indicator_plot$lnRGDP)
g <- ggplot(indicator_plot[-1:-100,]) 
g <- g + labs(color="Legend text")
g <- g + geom_line(aes(x=Date, y=lnRGDP, colour="lnRGDP"), size = 1)
g <- g + geom_rect(data=tp[-1:-6,], aes(xmin=Peaks, xmax=Troughs, ymin=-Inf, ymax=+Inf), fill='grey', alpha=0.5)
g <- g + ylab("log Real GDP") + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
#g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g <- g + scale_x_date(limits = c(as.Date("1879-12-31"), as.Date("1910-12-31")), 
                      breaks = seq(as.Date("1879-12-31"), as.Date("1910-12-31"), "year") - c(0,1,1,0),
                      date_labels = "%Y", expand=c(0,30))
g <- g + theme(legend.position="none")
g

detach("package:BCDating", unload=TRUE)

#Alternative
alt_recessions.df = read.table(textConnection(
    "Peak, Trough
    1881-12-31, 1885-12-31
    1889-12-31, 1894-12-31
    1899-12-31, 1900-12-31
    1906-12-31, 1909-12-31"), sep=',',
    colClasses=c('Date','Date'), header=TRUE)

indicator_plot <- GDP[-1:-25,c("Date","lnRGDP")]
g <- ggplot(indicator_plot) 
#g <- g + theme_bw()
g <- g + labs(color="Legend text")
#g <- g + geom_line(aes(x=Date, y=J_lnRGDP, colour="J_lnRGDP"), size = 1)
g <- g + geom_line(aes(x=Date, y=lnRGDP, colour="lnRGDP"), size = 1)
g <- g + geom_rect(data=tp[-1:-6,], aes(xmin=Peaks, xmax=Troughs, ymin=-Inf, ymax=+Inf), fill='grey', alpha=0.5)
g <- g + ylab("log Real GDP") + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g <- g + theme(legend.position="none")
g


#---------------------------------------------------
trade <- read.csv("Trade.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
trade$Date <- as.Date(trade$Date)

alt_recessions.df = read.table(textConnection(
    "Peaks, Troughs
    1889, 1890
    1899, 1900
    1906, 1909"), sep=',', header=TRUE)

indicator_plot <- trade[,c("Date","Imports","Exports","Trade_Balance")]
g <- ggplot(indicator_plot) 
#g <- g + theme_bw()
g <- g + labs(color="Legend text")
g <- g + geom_line(aes(x=Date, y=Imports, colour="Imports"), size = 1)
g <- g + geom_line(aes(x=Date, y=Exports, colour="Exports"), size = 1)
#g <- g + geom_bar(aes(x=Date, y=Trade_Balance, fill="Trade_Balance"),size = 0.5,stat="identity")
g <- g + geom_rect(data=tp[-1:-7,], aes(xmin=Peaks, xmax=Troughs, ymin=-Inf, ymax=+Inf), fill='grey', alpha=0.5)
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + scale_x_date(labels = date_format("%Y-%m"),breaks = date_breaks("year"))
#g <- g + scale_x_date(limits = c(as.Date("1885-12-31"), as.Date("1910-12-31")), 
#                      breaks = seq(as.Date("1885-12-31"), as.Date("1910-12-31"), "year") - c(0,1,1,0),
#                      date_labels = "%Y", expand=c(0,30))
g <- g + theme(legend.title=element_blank()) + theme(legend.position="bottom")
g <- g + scale_y_continuous(name="Value (pounds)", labels = comma)
g



indicator_plot <- GDP[-1:-25,c("Date","Currency","Total_Savings")]
g <- ggplot(indicator_plot) 
g <- g + labs(color="Legend text")
g <- g + geom_line(aes(x=Date, y=Currency, colour="Currency"), size = 1)
g <- g + geom_line(aes(x=Date, y=Total_Savings, colour="Total_Savings"), size = 1)
g <- g + geom_rect(data=tp[-1:-6,], aes(xmin=Peaks, xmax=Troughs, ymin=-Inf, ymax=+Inf), fill='grey', alpha=0.5)
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + scale_x_date(labels = date_format("%Y-%m"),breaks = date_breaks("year"))
g <- g + scale_y_continuous(name="Money Supply (pounds)", labels = comma)
g <- g + theme(legend.title=element_blank()) + theme(legend.position="bottom")
g


CPI <- read.csv("CPI.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
CPI$Date <- as.Date(CPI$Date)

indicator_plot <- CPI
g <- ggplot(indicator_plot) 
g <- g + labs(color="Legend text")
g <- g + geom_line(aes(x=Date, y=Bare.Bones.Index, colour="Bare.Bones.Index"), size = 1)
g <- g + geom_line(aes(x=Date, y=Respectable.Index, colour="Respectable.Index"), size = 1)
g <- g + geom_line(aes(x=Date, y=Verhoef.CPI, colour="Verhoef.CPI"), size = 1)
g <- g + geom_rect(data=tp[-1:-6,], aes(xmin=Peaks, xmax=Troughs, ymin=-Inf, ymax=+Inf), fill='grey', alpha=0.5)
g <- g + ylab("Consumer Price Indices") + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + scale_x_date(labels = date_format("%Y-%m"),breaks = date_breaks("year"))
g <- g + theme(legend.title=element_blank()) + theme(legend.position="bottom")
g

##=====================
##Agricultural Journals
##=====================

comdata <- read.csv("Commodities.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
#comdata$date <- as.Date(comdata$date, "%Y/%m/%d")
comdata$datum <- paste(comdata$datum, comdata$Year)
comdata$date <- as.Date(as.yearmon(as.character(comdata$datum),"%B %Y"),frac=1)
comdata$datum <- factor(as.yearmon(as.character(comdata$datum),"%B %Y"))

#-------------------------------------------------------------------
wc.towns <- c("Beaufort West","Bredasdorp","Caledon","Cape Town","Ceres","Clanwilliam",
              "George","Knysna","Ladismith","Malmesbury","Mossel Bay","Oudtshoorn","Paarl","Piquetberg",
              "Prince Albert","Riversdale","Robertson","Stellenbosch","Swellendam","Tulbagh","Uniondale",
              "Worcester","Van Rhyn's Dorp","Wynberg")
ec.towns <- c("Albany","Albert","Aliwal North","Burghersdorp","Cradock","Dordrecht","East London","Graaff-Reinet",
              "Graham's Town","Humansdorp","King Williams Town","King William's Town","Middelburg",
              "Port Alfred","Port Elizabeth","Queen's Town","Somerset East","Tarkastad","Uitenhage","Willowmore",
              "Mount Currie","Kokstad","Umtata")
nc.towns <- c("Colesberg","Kimberley","Philipstown","Richmond")
kzn.towns <- c("Pietermaritzburg, Natal","Durban, Natal")
in.towns <- c("Bloemfontein","Bulawayo","Johannesburg","Pretoria","Salisbury","Vryburg")
cape <- c(wc.towns,ec.towns,nc.towns)
col.towns <- c(cape,kzn.towns)
all.towns <- c(wc.towns,ec.towns,nc.towns,kzn.towns,in.towns)
#-------------------------------------------------------------------
comdata <- comdata[comdata$town %in% cape,]

coms <- aggregate(comdata$wheat, by=list(comdata$date), FUN = function(x) sum(!is.na(x)))
for(i in colnames(comdata)[7:29]) {
    coms1 <- aggregate(comdata[,i], by=list(comdata$date), FUN = function(x) sum(!is.na(x)))
    coms <- merge(coms, coms1, by="Group.1",all.x=TRUE)
}
colnames(coms) <- c("Date",colnames(comdata)[6:29])
rm(coms1)

complot <- melt(coms, id="Date") 
complot$value[complot$value=="0"] <- NA
g <- ggplot(complot, aes(x=Date,value,colour=variable,fill=variable))
g <- g + geom_bar(stat="identity")
g <- g + theme(legend.title=element_blank())
g <- g + ylab("Total observations")
g <- g + theme(legend.key.size = unit(0.4,"cm"))
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
#g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g <- g + scale_x_date(limits = c(as.Date("1887-12-01"), as.Date("1915-12-01")), 
                      breaks = seq(as.Date("1887-12-31"), as.Date("1914-12-31"), "year") - c(0,1,1,0),
                      date_labels = "%Y", expand=c(0,30))
g


#-----------------------
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


rscomdata1 <- rscomdata[rscomdata$commodity=="wheat",]
g <- ggplot(data=rscomdata1,aes(x=date, y=price, colour=town)) 
g <- g + geom_point(size = 0.5) 
g <- g + geom_line()
g <- g + ylab("Wheat prices") + xlab("")
g <- g + theme(legend.key.size = unit(0.5,"cm"))
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank())
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g


##=================##
## BLUE BOOKS DATA ##
##=================##
blue <- read.csv("Blue_Books.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
blue <- blue[blue$town!="Walfish Bay",]

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

#-----------------------------------
rsblue1 <- rsblue[rsblue$commodity=="wheat",]
g <- ggplot(data=rsblue1,aes(x=date, y=price, colour=town)) 
g <- g + geom_point(size = 1) 
g <- g + geom_line()
g <- g + ylab("Wheat prices") + xlab("")
g <- g + theme(legend.key.size = unit(0.3,"cm"))
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) 
g


rscomdata1 <- rscomdata1[rscomdata1$town=="Cape Town",]
rsblue1 <- rsblue1[rsblue1$town=="Cape Town",]
rsblue1$date <- paste(rsblue1$date,"-11-30",sep="")
rsblue1$date <- as.Date(rsblue1$date)

wheat <- merge(rscomdata1[,c(2,4)],rsblue1[,c(1,3)],by.x="date",by.y="date",all.x=TRUE)
wheat <- wheat[1:325,]
#wheat$price.y <- wheat$price.y*0.5
colnames(wheat) <- c("date","Wheat per 100lbs (Agri Journals)","Wheat per bushel (Blue Books)")

complot <- melt(wheat, id="date") 
g <- ggplot(data=complot,aes(x=date, y=value, colour=variable)) 
g <- g + geom_line()
g <- g + geom_point(aes(size = variable)) 
g <- g + ylab("Wheat prices")+ xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) + theme(legend.position="bottom")
g


##==================================
#Toy Example
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

xt <- xtable(toy, caption="Repeat sales example with wheat prices")
print(xt, "latex", include.rownames=FALSE,comment=FALSE, 
      caption.placement = getOption("xtable.caption.placement", "top"), scalebox = 0.9)


toy.df = read.table(textConnection(
    "Date,Period,Beaufort West,Cape Town,Worcester,Index
    Jun 1891,1,150,NA,210,100.00
    Jul 1891,2,135,138,NA,88.83
    Aug 1891,3,135,150,NA,92.01
    Sep 1891,4,NA,NA,288,138.94
    Oct 1891,5,NA,144,NA,86.37
    Nov 1891,6,NA,144,NA,84.45
    Dec 1891,7,120,144,NA,82.57
    Jan 1892,8,NA,NA,144,70.38
    Feb 1892,9,NA,126,144,71.31
    Mar 1892,10,NA,126,NA,71.31"), sep=',', header=TRUE)

xt <- xtable(toy.df, caption="Repeat sales example with wheat prices",auto = TRUE)
align(xt) <- rep("r", 7)
print(xt, "latex", include.rownames=FALSE,comment=FALSE, 
      caption.placement = getOption("xtable.caption.placement", "top"), scalebox = 0.9)


colnames(einde) <- c("ln(Pt/Ps)","D1","D2","D3","D4","D5","D6","D7","D8","D9","D10")
xt <- xtable(toy.df, caption="Rgression input of repeat sales example with wheat prices",digits=c(3,3,0,0,0,0,0,0,0,0,0,0))
print(xt, "latex", include.rownames=FALSE,comment=FALSE, 
      caption.placement = getOption("xtable.caption.placement", "top"), scalebox = 0.9)

#--------------------------
#Wheat example
#--------------------------
#Journal Index
rscomdata1 <- rscomdata[rscomdata$commodity==c("wheat"),]
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

#Blue Index
rsblue1 <- rsblue[rsblue$commodity==c("wheat"),]
rsblue1 <- rsblue1[complete.cases(rsblue1),]
repdata <- repsaledata(rsblue1$lnprice,rsblue1$date,rsblue1$id)  #transform the data to sales pairs
repdata <- repdata[complete.cases(repdata),]
repeatsales <- repsale(repdata$price0,repdata$time0,repdata$price1,repdata$time1,mergefirst=1,
                       graph=FALSE)   #generate the repeat sales index
rs_index.a <- exp(as.data.frame(repeatsales$pindex))*100
rs_index.a$Date <- seq(1,1,length.out = ncol(rs_index.a))
rs_index.a$Date <- unique(rsblue$date)
colnames(rs_index.a) <- c("Index","Date")

rs_index.a$Date <- paste(rs_index.a$Date,"-11-30",sep="")
rs_index.a$Date <- as.Date(rs_index.a$Date)
rs_index3 <- merge(RS_index.ex,rs_index.a,by="Date",all=TRUE)

#Combine
rsdata <- rs_index3
colnames(rsdata) <- c("Date","Journal","Blue")
rsdata <- melt(rsdata,id="Date")
rsdata$lnprice <- log(rsdata$value)
rsdata <- rsdata[complete.cases(rsdata),]
repdata <- repsaledata(rsdata$lnprice,rsdata$Date,rsdata$variable)  #transform the data to sales pairs
repeatsales <- repsale(repdata$price0,repdata$time0,repdata$price1,repdata$time1,mergefirst=1,
                       graph=FALSE)   #generate the repeat sales index

Index <- exp(as.data.frame(repeatsales$pindex))*100
Index$Date <- seq(1,1,length.out = ncol(Index))
Index$Date <- sort(unique(c(repdata$time1,repdata$time0)))

Index1 <- merge(rs_index3,Index,by="Date", all = TRUE)
colnames(Index1) <- c("Date","Journal_Index","Blue_Index","Total_Index")
#Index1[,4] <- na.approx(Index1[,4])
    
index_plot <- Index1  # convert to long format
g <- ggplot(data=index_plot) 
g <- g + geom_line(aes(x=Date, y=Journal_Index, colour="Journal_Index"), size = 0.5)
#g <- g + geom_line(aes(x=Date, y=Blue_Index, colour="Blue_Index"), size = 1)
g <- g + geom_line(aes(x=Date, y=Total_Index, colour="Total_Index"), size = 1)
g <- g + geom_point(aes(x=Date, y=Journal_Index, colour="Journal_Index"), size = 0.5)
g <- g + geom_point(aes(x=Date, y=Blue_Index, colour="Blue_Index"), size = 2)
g <- g + geom_point(aes(x=Date, y=Total_Index, colour="Total_Index"), size = 1)
g <- g + ylab("Wheat Index") + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) + theme(legend.position="bottom")
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g


Index1[,4] <- na.approx(Index1[,4],na.rm = FALSE)
index_plot <- Index1  # convert to long format
g <- ggplot(data=index_plot) 
g <- g + geom_line(aes(x=Date, y=Journal_Index, colour="Journal_Index"), size = 0.5)
#g <- g + geom_line(aes(x=Date, y=Blue_Index, colour="Blue_Index"), size = 1)
g <- g + geom_line(aes(x=Date, y=Total_Index, colour="Total_Index"), size = 1)
g <- g + geom_point(aes(x=Date, y=Journal_Index, colour="Journal_Index"), size = 0.5)
g <- g + geom_point(aes(x=Date, y=Blue_Index, colour="Blue_Index"), size = 2)
g <- g + geom_point(aes(x=Date, y=Total_Index, colour="Total_Index"), size = 1)
g <- g + ylab("Wheat Index") + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) + theme(legend.position="bottom")
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g


#---------------------
#Temporal distribution
#---------------------
#Opsie1: Interpoleer annual based on last
wheat.a <- rs_index.a
ts.wheat.a <- as.ts(wheat.a[,-2], start=1889, end= 1907, frequency = 1)
ts.wheat.a1 <- na.approx(ts.wheat.a, na.rm=FALSE)
ts.wheat.a1 <- na.locf(ts.wheat.a1, na.rm=FALSE)
ts.wheat.a1 <- na.locf(ts.wheat.a1, na.rm=FALSE, fromLast=TRUE)
m1 <- td(ts.wheat.a1 ~ 1, to = "monthly", conversion = "average", method = "denton-cholette")
rs_index1 <- as.data.frame(predict(m1)[-1:-9])

#Opsie2: Interpoleer based on indicator variable
wheat.m <- RS_index.ex
ts.wheat.m <- as.ts(wheat.m[,-1], start=c(1889,10),end=c(1914,8), frequency = 12)
ts.wheat.m1 <- na.approx(ts.wheat.m, na.rm=FALSE)
ts.wheat.m1 <- na.locf(ts.wheat.m1, na.rm=FALSE)
ts.wheat.m1 <- na.locf(ts.wheat.m1, na.rm=FALSE, fromLast=TRUE)
RS_index.ex2 <- as.data.frame(ts.wheat.m1)
RS_index.ex2$Date <- RS_index.ex$Date
m2 <- td(ts.wheat.a1 ~ 0 + ts.wheat.m1, to= "monthly", conversion = "last", method = "chow-lin-maxlog")
rs_index2 <- as.data.frame(predict(m2))

#Opsie3: Interpoleer net eenvoudig
rs_index.a$Date <- paste(rs_index.a$Date,"-11-30",sep="")
rs_index.a$Date <- as.Date(rs_index.a$Date)
rs_index3 <- merge(RS_index.ex,rs_index.a,by="Date",all=TRUE)
wheat.m <- rs_index3
ts.wheat.m <- as.ts(wheat.m[,2:3], start=c(1889,10),end=c(1914,8), frequency = 12)
ts.wheat.m1 <- na.approx(ts.wheat.m, na.rm=FALSE)
rs_index3[,2:3] <- ts.wheat.m1

#Plot die 3 saammet monthly [1:219]
interpol <- cbind(rs_index3[1:219,-2],rs_index1,rs_index2[1:219,],RS_index.ex2[1:219,1])
colnames(interpol) <- c("Date","Interpolate_last","Interpolate_average",
                        "Interpolate_indicator","Monthly Indicator")

index_plot <- melt(interpol, id="Date")  # convert to long format
g <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_point(size = 1) 
g <- g + geom_line()
g <- g + ylab("Monthly Interpolated Index")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) + theme(legend.position="bottom")
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g


##==================================
comgroups.df = read.table(textConnection(
    "Crops,Agri Produce,Pastoral Products,Livestock,Pastoral Provisions,Agricultural Provisions,Other Provisions
    Wheat, Tobacco,Wool,Cattle,Beef,Bread,Tea
    Mealies,Dried Fruit,Hides,Horses Mules & Asses,Mutton,Flour,Coffee
    Barley,Wine,Skins,Sheep,Pork,Mealie Meal,Sugar
    Oats,Brandy,Cheese,Pigs,Eggs,Boer Meal,Beer
    Oathay,	,Fat & Tallow,Goats,Butter,Oatmeal,Rice
    Rye, ,Soap,Fowls & Ducks,Milk, ,Salt
    Peas & Beans, ,	, , , ,Candles
    Potatoes, , , , , ,"), sep=',', header=TRUE)

xt <- xtable(comgroups.df, caption="Commodity classification")
print(xt, "latex", include.rownames=FALSE,comment=FALSE, 
      caption.placement = getOption("xtable.caption.placement", "top"), scalebox = 0.9)


##==================================
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
        #RS_index.ex[,-1] <- na.approx(RS_index.ex[,-1], na.rm=FALSE)
        
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
        rs_index.a$Date <- sort(unique(c(repdata$time1,repdata$time0)))
        colnames(rs_index.a) <- c("Blue_Index","Date")
        
        rs_index.ex <- aggregate(blue$town, by=list(blue$date), FUN = function(x) sum(!is.na(x)))
        colnames(rs_index.ex) <- c("Date","x")
        rs_index.a <- merge(rs_index.ex, rs_index.a, by="Date", all=TRUE)[,-2]
        
        rs_index.a$Date <- paste(rs_index.a$Date,"-11-30",sep="")
        rs_index.a$Date <- as.Date(rs_index.a$Date)
        rs_index1 <- merge(RS_index.ex,rs_index.a,by="Date",all=TRUE)[,-2]
        #rs_index1[,-1] <- na.approx(rs_index1[,-1], na.rm=FALSE)

    } else {
        rs_index1 <- aggregate(comdata$town, by=list(comdata$date), FUN = function(x) sum(!is.na(x))) 
    }
    
    if(nrow(rscomdata1)==0) { Index1 <- rs_index1 } 
    if(nrow(rsblue1)==0)    { Index1 <- RS_index.ex }
    
    if(nrow(rscomdata1)>0 & nrow(rsblue1)>0) {
        rsdata <- merge(RS_index.ex,rs_index1,by="Date", all = TRUE)
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
        Index1 <- Index1[,c(1,3,2,4)]
        colnames(Index1) <- c("Date","Total_Index","Journal_Index","Blue_Index")
    }
    
    return(Index1)
}


#--------------------------
#Alternative example
#--------------------------

makeindex <- function(produk) {
    rscomdata1 <- rscomdata[rscomdata$commodity==produk,]
    rscomdata1 <- rscomdata1[complete.cases(rscomdata1),]
    if(nrow(rscomdata1)>0) {rscomdata1$bron <- "Journal"}
    rsblue1 <- rsblue[rsblue$commodity==produk,]
    rsblue1 <- rsblue1[complete.cases(rsblue1),]
    
    if(nrow(rsblue1)>0) {
        rsblue1$date <- paste(rsblue1$date,"-11-30",sep="")
        rsblue1$date <- as.Date(rsblue1$date)
        rsblue1$bron <- "Blue"
        rscomdata1 <- rbind(rscomdata1[,-1],rsblue1)
    }
    
    rscomdata1 <- transform(rscomdata1, id = as.numeric(interaction(factor(town),factor(commodity),factor(bron),drop=TRUE)))
    
    repdata <- repsaledata(rscomdata1$lnprice,rscomdata1$date,rscomdata1$id)  #transform the data to sales pairs
    repdata <- repdata[complete.cases(repdata),]
    repeatsales <- repsale(repdata$price0,repdata$time0,repdata$price1,repdata$time1,mergefirst=1,
                           graph=FALSE)   #generate the repeat sales index
    RS_index <- exp(as.data.frame(repeatsales$pindex))*100
    RS_index$Date <- seq(1,1,length.out = ncol(RS_index))
    RS_index$Date <- sort(unique(c(repdata$time1,repdata$time0)))
    
    colnames(RS_index) <- c("Index","Date")
    RS_index <- RS_index[complete.cases(RS_index),]
    RS_index.ex <- aggregate(comdata$town, by=list(comdata$date), FUN = function(x) sum(!is.na(x)))
    colnames(RS_index.ex) <- c("Date","x")
    RS_index.ex <- merge(RS_index.ex, RS_index, by="Date", all=TRUE)[,-2]
    RS_index.ex[,3] <- RS_index.ex[,2]
    RS_index.ex[,2] <- na.approx(RS_index.ex[,3], na.rm=FALSE)
    #colnames(RS_index.ex) <- c("Date", produk[1])
    return(RS_index.ex)
}


#==================
#AGRICULTURAL PRODUCE (8 + 5): 
#"wheat","barley","oats","oathay","rye","peas.beans","potatoes","tobacco",c("dried.fruit","d.fruit") 
#c("wine","wine.better","wine.ordinary"),c("brandy","brandy.better","brandy.ordinary")
crops <- cbind(wheat=makeindex("wheat")[,1:2],mealies=makeindex("mealies")[,1:2],barley=makeindex("barley")[,1:2],oats=makeindex("oats")[,1:2],
               oathay=makeindex("oathay")[,1:2],rye=makeindex("rye")[,1:2],peas.beans=makeindex("peas.beans")[,1:2],
               potatoes=makeindex("potatoes")[,1:2])
crops <- crops[,c(1,2,4,6,8,10,12,14,16)]
colnames(crops)[1] <- "Date"

produce <- cbind(tobacco=makeindex("tobacco")[,1:2],d.fruit=makeindex(c("dried.fruit","d.fruit"))[,1:2],
                 wine=makeindex(c("wine","wine.better","wine.ordinary"))[,1:2],brandy=makeindex(c("brandy","brandy.better","brandy.ordinary"))[,1:2]) 
produce <- produce[,c(1,2,4,6,8)]
colnames(produce)[1] <- "Date"

#PASTORAL PRODUCTS (6): 
#c("w.wool","u.wool"),"hides",c("sheep.skins","goat.skins"),"cheese","fat.tallow","soap"
pastoral <- cbind(wool=makeindex(c("w.wool","u.wool"))[,1:2],hides=makeindex(c("hides"))[,1:2],skins=makeindex(c("sheep.skins","goat.skins"))[,1:2],
                  cheese=makeindex("cheese")[,1:2],fat.tallow=makeindex("fat.tallow")[,1:2],soap=makeindex("soap")[,1:2]) 
pastoral <- pastoral[,c(1,2,4,6,8,10,12)]
colnames(pastoral)[1] <- "Date"

#LIVESTOCK (6):
#c("cattle","tr.oxen","mi.cows","d.oxen","m.cows"),c("s.horse","d.horse","mules","asses"),c("sheep","wo.sheep","w.sheep","c.sheep"),
#"swine","goats",c("fowls","ducks")
livestock <- cbind(cattle=makeindex(c("cattle","tr.oxen","mi.cows","d.oxen","m.cows"))[,1:2],horses=makeindex(c("s.horse","d.horse","mules","asses"))[,1:2],
                   sheep=makeindex(c("sheep","wo.sheep","w.sheep","c.sheep"))[,1:2],swine=makeindex("swine")[,1:2],goats=makeindex("goats")[,1:2],
                   fowls=makeindex(c("fowls","ducks"))[,1:2])
livestock <- livestock[,c(1,2,4,6,8,10,12)]
colnames(livestock)[1] <- "Date"

#PROVISIONS (6 + 5 + 7): 
#"beef","mutton",c("pork","bacon"),"eggs",c("butter","butter.fresh","butter.salt"),"bread",c("beer.eng","beer.col"),c("wheat.flour","flour"),"mealie.meal","boer.meal","oatmeal"
#"tea","coffee","sugar","rice","salt","milk","candles"
p.provisions <- cbind(beef=makeindex("beef")[,1:2],mutton=makeindex("mutton")[,1:2],pork=makeindex(c("pork","bacon"))[,1:2],
                      eggs=makeindex("eggs")[,1:2],butter=makeindex(c("butter","butter.fresh","butter.salt"))[,1:2],milk=makeindex("milk")[,1:2])
p.provisions <- p.provisions[,c(1,2,4,6,8,10,12)]
colnames(p.provisions)[1] <- "Date"

a.provisions <- cbind(bread=makeindex("bread")[,1:2],flour=makeindex(c("wheat.flour"))[,1:2],
                      mealie.meal=makeindex("mealie.meal")[,1:2],boer.meal=makeindex("boer.meal")[,1:2],oatmeal=makeindex("oatmeal")[,1:2])
a.provisions <- a.provisions[,c(1,2,4,6,8,10)]
colnames(a.provisions)[1] <- "Date"

o.provisions <- cbind(tea=makeindex("tea")[,1:2],coffee=makeindex("coffee")[,1:2],sugar=makeindex("sugar")[,1:2],beer=makeindex(c("beer.eng","beer.col"))[,1:2],
                      rice=makeindex("rice")[,1:2],salt=makeindex("salt")[,1:2],candles=makeindex("candles")[,1:2])
o.provisions <- o.provisions[,c(1,2,4,6,8,10,12,14)]
colnames(o.provisions)[1] <- "Date"


#==================
#ALTERNATIVE
#AGRICULTURAL PRODUCE (8 + 5): 
crops <- cbind(wheat=makeindex("wheat")[,1:2],mealies=makeindex("mealies")[,1:2],barley=makeindex("barley")[,1:2],oats=makeindex("oats")[,1:2],
               oathay=makeindex("oathay")[,1:2],rye=makeindex("rye")[,1:2],peas.beans=makeindex("peas.beans")[,1:2],
               potatoes=makeindex("potatoes")[,1:2],bread=makeindex("bread")[,1:2],flour=makeindex(c("wheat.flour"))[,1:2],
               mealie.meal=makeindex("mealie.meal")[,1:2],boer.meal=makeindex("boer.meal")[,1:2],oatmeal=makeindex("oatmeal")[,1:2])
crops <- crops[,c(1,seq(2,ncol(crops),by=2))]
colnames(crops)[1] <- "Date"

produce <- cbind(tobacco=makeindex("tobacco")[,1:2],d.fruit=makeindex(c("dried.fruit","d.fruit"))[,1:2],
                 wine=makeindex(c("wine","wine.better","wine.ordinary"))[,1:2],brandy=makeindex(c("brandy","brandy.better","brandy.ordinary"))[,1:2],
                 wool=makeindex(c("w.wool","u.wool"))[,1:2],hides=makeindex(c("hides"))[,1:2],skins=makeindex(c("sheep.skins","goat.skins"))[,1:2],
                 cheese=makeindex("cheese")[,1:2],fat.tallow=makeindex("fat.tallow")[,1:2],soap=makeindex("soap")[,1:2],
                 tea=makeindex("tea")[,1:2],coffee=makeindex("coffee")[,1:2],sugar=makeindex("sugar")[,1:2],beer=makeindex(c("beer.eng","beer.col"))[,1:2],
                 rice=makeindex("rice")[,1:2],salt=makeindex("salt")[,1:2],candles=makeindex("candles")[,1:2]) 
produce <- produce[,c(1,seq(2,ncol(produce),by=2))]
colnames(produce)[1] <- "Date"


#LIVESTOCK (6):
livestock <- cbind(cattle=makeindex(c("cattle","tr.oxen","mi.cows","d.oxen","m.cows"))[,1:2],horses=makeindex(c("s.horse","d.horse","mules","asses"))[,1:2],
                   sheep=makeindex(c("sheep","wo.sheep","w.sheep","c.sheep"))[,1:2],swine=makeindex("swine")[,1:2],goats=makeindex("goats")[,1:2],
                   fowls=makeindex(c("fowls","ducks"))[,1:2],beef=makeindex("beef")[,1:2],mutton=makeindex("mutton")[,1:2],pork=makeindex(c("pork","bacon"))[,1:2],
                   eggs=makeindex("eggs")[,1:2],butter=makeindex(c("butter","butter.fresh","butter.salt"))[,1:2],milk=makeindex("milk")[,1:2])
livestock <- livestock[,c(1,seq(2,ncol(livestock),by=2))]
colnames(livestock)[1] <- "Date"


#-----------------------------------------------
#Plot examples for individual commodities
product <- cbind(crops[,c(1,2)],produce[,2],livestock[,2],p.provisions[,2])
colnames(product) <- c("Date","Wheat","Tobacco","Cattle","Beef")
product[,-1] <- na.approx(product[,-1], na.rm=FALSE)

#Seasonal adjustment
#product[,-1] <- na.locf(product[,-1], na.rm=FALSE, fromLast=TRUE)
#m <- ts(product[,-1],start=c(1889,10), end= c(1914,8), frequency = 12)
#dec <- decompose(m, "additive")
#product[,-1] <- as.data.frame(m - dec$seasonal)

index_plot <- product[,c(1,2)]
g1 <- ggplot(index_plot, aes(x=Date,y=Wheat,group=1))
g1 <- g1 + geom_line(colour="#F8766D")
#g1 <- g1 + geom_point(colour="#F8766D",size=1)
g1 <- g1 + theme(legend.title=element_blank())
g1 <- g1 + ggtitle("Wheat Index") 
g1 <- g1 + ylab("Index Value") + xlab("")
g1 <- g1 + theme(legend.position="none")
g1 <- g1 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g1 <- g1 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

index_plot <- product[,c(1,3)]
g2 <- ggplot(index_plot, aes(x=Date,y=Tobacco,group=1))
g2 <- g2 + geom_line(colour="#7CAE00")
#g2 <- g2 + geom_point(colour="#7CAE00",size=1)
g2 <- g2 + theme(legend.title=element_blank()) 
g2 <- g2 + ggtitle("Tobacco Index") 
g2 <- g2 + ylab("Index Value") + xlab("")
g2 <- g2 + theme(legend.position="none")
g2 <- g2 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g2 <- g2 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

index_plot <- product[,c(1,4)]
g3 <- ggplot(index_plot, aes(x=Date,y=Cattle,group=1))
g3 <- g3 + geom_line(colour="#00BFC4")
#g3 <- g3 + geom_point(colour="#00BFC4",size=1)
g3 <- g3 + theme(legend.title=element_blank()) 
g3 <- g3 + ggtitle("Cattle Index") 
g3 <- g3 + ylab("Index Value") + xlab("")
g3 <- g3 + theme(legend.position="none")
g3 <- g3 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g3 <- g3 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

index_plot <- product[,c(1,5)]
g4 <- ggplot(index_plot, aes(x=Date,y=Beef,group=1))
g4 <- g4 + geom_line(colour="#C77CFF")
#g4 <- g4 + geom_point(colour="#C77CFF",size=1)
g4 <- g4 + theme(legend.title=element_blank()) 
g4 <- g4 + ggtitle("Beef Index") 
g4 <- g4 + ylab("Index Value") + xlab("")
g4 <- g4 + theme(legend.position="none")
g4 <- g4 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g4 <- g4 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

grid.arrange(g1, g2, g3, g4, ncol=2, nrow =2)


#-------------------------
#Check indekse
cattle=makeindex(c("cattle","tr.oxen","mi.cows","d.oxen","m.cows"))[,1:2]
sheep=makeindex(c("sheep","wo.sheep","w.sheep","c.sheep"))[,1:2]
                                                           
bread=makeindex("bread")
flour=makeindex(c("wheat.flour","flour"))
mealie.meal=makeindex("mealie.meal")
boer.meal=makeindex("boer.meal")
oatmeal=makeindex("oatmeal")

product <- makeindex(c("tobacco"))
product[,2] <- na.approx(product[,2], na.rm=FALSE)

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

index_plot <- melt(crops[,c(1,3)], id="Date")  # convert to long format
g <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_line()
g <- g + ylab("Wheat Index")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) + theme(legend.position="bottom")
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g

#---------------------------------
#Calculate commodity group indices
gewig <- read.csv("Weights.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)

weeg <- function(data,w) {
    data <- na.approx(data[,-1], na.rm=FALSE)
    data <- na.locf(data, na.rm=FALSE, fromLast=TRUE)
    m <- ts(data,start=c(1889,10), end= c(1914,8), frequency = 12)
    dec <- decompose(m, "additive")
    toets <- as.data.frame(diff(log(m - dec$seasonal),lag=1))
    n <- ncol(toets)

    for(i in 1:298) {
        toets[i,n+1] <- weighted.mean(toets[i,1:n], w, na.rm=TRUE)
        data[i+1,1] <- data[i,1]*exp(toets[i,n+1])
    }
    return(data[,1])
}

crops$Crops <- weeg(crops,gewig[,1:8])
produce$Produce <- weeg(produce,gewig[,9:12])
pastoral$Pastoral <- weeg(pastoral,gewig[,13:18])
livestock$Livestock <- weeg(livestock,gewig[,19:24])
p.provisions$P.Provisions <- weeg(p.provisions,gewig[,25:30])
a.provisions$A.Provisions <- weeg(a.provisions,gewig[,31:35])
o.provisions$O.Provisions <- weeg(o.provisions,gewig[,36:42])

indices <- cbind(crops[,c("Date","Crops")],produce[,"Produce"],pastoral[,"Pastoral"],livestock[,"Livestock"],
                 p.provisions[,"P.Provisions"],a.provisions[,"A.Provisions"],o.provisions[,"O.Provisions"])
colnames(indices) <- c("Date","Crops","Produce","Pastoral","Livestock","P.Provisions","A.Provisions","O.Provisions")
indices$Total <- weeg(indices,gewig[,43:49])


#Alternative weighting
weeg <- function(data,w) {
    data[,-1] <- na.locf(data[,-1], na.rm=FALSE, fromLast=TRUE)
    toets <- data
    n <- ncol(toets)
    for(i in 2:299) {
        toets[i,2:n] <- data[i,2:n]/data[(i-1),2:n]
        toets[i,n+1] <- weighted.mean(toets[i,2:n],w,na.rm=TRUE)
        toets[1,n+2] <- 100
        toets[i,n+2] <- toets[i-1,n+2]*toets[i,n+1]
    }
    return(toets[,c(n+2)])
}

weeg <- function(data,w) {
    data <- data[,-1]
    data <- na.locf(data, na.rm=FALSE, fromLast=TRUE)
    m <- ts(data,start=c(1889,10), end= c(1914,8), frequency = 12)
    dec <- decompose(m, "additive")
    toets <- as.data.frame(diff(log(m - dec$seasonal),lag=1))
    n <- ncol(toets)
    
    for(i in 1:298) {
        toets[i,n+1] <- weighted.mean(toets[i,1:n], w, na.rm=TRUE)
        data[i+1,1] <- data[i,1]*exp(toets[i,n+1])
    }
    return(data[,1])
}

library(fpp)

#crops$Crops <- weeg(crops,cbind(gewig[,1:8],gewig[,31:35]))
#livestock$Livestock <- weeg(livestock,cbind(gewig[,19:24],gewig[,25:30]))
#produce$Produce <- weeg(produce,cbind(gewig[,9:12],gewig[,13:18],gewig[,36:42]))
#---------------------------------
#Alternative weighting
gewig <- read.csv("Weights.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)

crops[,-1] <- na.locf(crops[,-1], na.rm=FALSE)
crops[,-1] <- na.locf(crops[,-1], na.rm=FALSE, fromLast=TRUE)
toets <- cbind(crops,gewig[,1:8])
for(i in 1:299) {
    crops[i,10] <- weighted.mean(toets[i,2:9],toets[i,10:17],na.rm=TRUE)
}
colnames(crops)[10] <- "Crops"

produce[,-1] <- na.locf(produce[,-1], na.rm=FALSE)
produce[,-1] <- na.locf(produce[,-1], na.rm=FALSE, fromLast=TRUE)
toets <- cbind(produce,gewig[,9:12])
for(i in 1:299) {
    produce[i,6] <- weighted.mean(toets[i,2:5],toets[i,6:9],na.rm=TRUE)
}
colnames(produce)[6] <- "Produce"

pastoral[,-1] <- na.locf(pastoral[,-1], na.rm=FALSE)
pastoral[,-1] <- na.locf(pastoral[,-1], na.rm=FALSE, fromLast=TRUE)
toets <- cbind(pastoral,gewig[,13:18])
for(i in 1:299) {
    pastoral[i,8] <- weighted.mean(toets[i,2:7],toets[i,8:13],na.rm=TRUE)
}
colnames(pastoral)[8] <- "Pastoral"

livestock[,-1] <- na.locf(livestock[,-1], na.rm=FALSE)
livestock[,-1] <- na.locf(livestock[,-1], na.rm=FALSE, fromLast=TRUE)
toets <- cbind(livestock,gewig[,19:24])
for(i in 1:299) {
    livestock[i,8] <- weighted.mean(toets[i,2:7],toets[i,8:13],na.rm=TRUE)
}
colnames(livestock)[8] <- "Livestock"

p.provisions[,-1] <- na.locf(p.provisions[,-1], na.rm=FALSE)
p.provisions[,-1] <- na.locf(p.provisions[,-1], na.rm=FALSE, fromLast=TRUE)
toets <- cbind(p.provisions,gewig[,25:30])
for(i in 1:299) {
    p.provisions[i,8] <- weighted.mean(toets[i,2:7],toets[i,8:13],na.rm=TRUE)
}
colnames(p.provisions)[8] <- "P.Provisions"

a.provisions[,-1] <- na.locf(a.provisions[,-1], na.rm=FALSE)
a.provisions[,-1] <- na.locf(a.provisions[,-1], na.rm=FALSE, fromLast=TRUE)
toets <- cbind(a.provisions,gewig[,31:35])
for(i in 1:299) {
    a.provisions[i,7] <- weighted.mean(toets[i,2:6],toets[i,7:11],na.rm=TRUE)
}
colnames(a.provisions)[7] <- "A.Provisions"

o.provisions[,-1] <- na.locf(o.provisions[,-1], na.rm=FALSE)
o.provisions[,-1] <- na.locf(o.provisions[,-1], na.rm=FALSE, fromLast=TRUE)
toets <- cbind(o.provisions,gewig[,36:42])
for(i in 1:299) {
    o.provisions[i,9] <- weighted.mean(toets[i,2:8],toets[i,9:15],na.rm=TRUE)
}
colnames(o.provisions)[9] <- "O.Provisions"


indices <- cbind(crops[,c("Date","Crops")],produce[,"Produce"],pastoral[,"Pastoral"],livestock[,"Livestock"],
                 p.provisions[,"P.Provisions"],a.provisions[,"A.Provisions"],o.provisions[,"O.Provisions"])
colnames(indices) <- c("Date","Crops","Produce","Pastoral","Livestock","P.Provisions","A.Provisions","O.Provisions")

toets <- cbind(indices,gewig[,43:49])
for(i in 1:299) {
    indices[i,9] <- weighted.mean(toets[i,2:8],toets[i,9:15],na.rm=TRUE)
}
colnames(indices)[9] <- "Total"

#-----------------------------------------------
#Plot commodity Groups
index_plot <- indices[,c(1,2)]
g1 <- ggplot(index_plot, aes(x=Date,y=Crops,group=1))
g1 <- g1 + geom_line(colour="#F8766D")
g1 <- g1 + theme(legend.title=element_blank())
g1 <- g1 + ggtitle("Crops Index") 
g1 <- g1 + ylab("Index Value") + xlab("")
g1 <- g1 + theme(legend.position="none")
g1 <- g1 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g1 <- g1 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

index_plot <- indices[,c(1,3)]
g2 <- ggplot(index_plot, aes(x=Date,y=Produce,group=1))
g2 <- g2 + geom_line(colour="#7CAE00")
g2 <- g2 + theme(legend.title=element_blank()) 
g2 <- g2 + ggtitle("Produce Index") 
g2 <- g2 + ylab("Index Value") + xlab("")
g2 <- g2 + theme(legend.position="none")
g2 <- g2 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g2 <- g2 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

index_plot <- indices[,c(1,4)]
index_plot[220:299,2] <- NA
g3 <- ggplot(index_plot, aes(x=Date,y=Pastoral,group=1))
g3 <- g3 + geom_line(colour="#00BFC4")
g3 <- g3 + theme(legend.title=element_blank()) 
g3 <- g3 + ggtitle("Pastoral Index") 
g3 <- g3 + ylab("Index Value") + xlab("")
g3 <- g3 + theme(legend.position="none")
g3 <- g3 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g3 <- g3 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

index_plot <- indices[,c(1,5)]
g4 <- ggplot(index_plot, aes(x=Date,y=Livestock,group=1))
g4 <- g4 + geom_line(colour="#C77CFF")
g4 <- g4 + theme(legend.title=element_blank()) 
g4 <- g4 + ggtitle("Livestock Index") 
g4 <- g4 + ylab("Index Value") + xlab("")
g4 <- g4 + theme(legend.position="none")
g4 <- g4 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g4 <- g4 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

grid.arrange(g1, g3, g2, g4, ncol=2, nrow =2)


index_plot <- indices[,c(1,6)]
g1 <- ggplot(index_plot, aes(x=Date,y=P.Provisions,group=1))
g1 <- g1 + geom_line(colour="#F8766D")
g1 <- g1 + theme(legend.title=element_blank())
g1 <- g1 + ggtitle("Pastoral Provisions Index") 
g1 <- g1 + ylab("Index Value") + xlab("")
g1 <- g1 + theme(legend.position="none")
g1 <- g1 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g1 <- g1 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

index_plot <- indices[,c(1,7)]
g2 <- ggplot(index_plot, aes(x=Date,y=A.Provisions,group=1))
g2 <- g2 + geom_line(colour="#7CAE00")
g2 <- g2 + theme(legend.title=element_blank()) 
g2 <- g2 + ggtitle("Agri Provisions Index") 
g2 <- g2 + ylab("Index Value") + xlab("")
g2 <- g2 + theme(legend.position="none")
g2 <- g2 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g2 <- g2 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

index_plot <- indices[,c(1,8)]
index_plot[220:299,2] <- NA
g3 <- ggplot(index_plot, aes(x=Date,y=O.Provisions,group=1))
g3 <- g3 + geom_line(colour="#00BFC4")
g3 <- g3 + theme(legend.title=element_blank()) 
g3 <- g3 + ggtitle("Other Provisions Index") 
g3 <- g3 + ylab("Index Value") + xlab("")
g3 <- g3 + theme(legend.position="none")
g3 <- g3 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g3 <- g3 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

index_plot <- indices[,c(1,9)]
g4 <- ggplot(index_plot, aes(x=Date,y=Total,group=1))
g4 <- g4 + geom_line(colour="#C77CFF")
g4 <- g4 + theme(legend.title=element_blank()) 
g4 <- g4 + ggtitle("Total Index") 
g4 <- g4 + ylab("Index Value") + xlab("")
g4 <- g4 + theme(legend.position="none")
g4 <- g4 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g4 <- g4 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

grid.arrange(g1, g2, g3, g4, ncol=2, nrow =2)


#---------------------------------------------
##For Grpahing Business cycles
alt_recessions.df = read.table(textConnection(
    "Peaks, Troughs
    1889-12-31, 1894-12-31
    1899-12-31, 1900-12-31
    1906-12-31, 1909-12-31"), sep=',',
    colClasses=c('Date','Date'), header=TRUE)

#png(file = "Commodity_plot.png", width=720,height=480)
indicator_plot <- indices[,c("Date","Total")]
g <- ggplot(indicator_plot) 
g <- g + labs(color="Legend text")
g <- g + geom_line(aes(x=Date, y=Total, colour="Total"), size = 1)
g <- g + geom_rect(data=tp[-1:-7,], aes(xmin=Peaks, xmax=Troughs, ymin=-Inf, ymax=+Inf), fill='grey', alpha=0.5)
g <- g + ylab("Index") + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
#g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"),expand=c(0,0),
#                      limits = as.Date(c("1888-12-31", NA)))
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g <- g + theme(legend.position="none")
g
#dev.off()


#--------------------------------
#Calculate annual average
indices$Year <- format(indices$Date,format="%Y")
annual_index <- aggregate(Total ~ Year, indices, FUN=mean)
annual_index$Total_Index <- annual_index$Total/annual_index$Total[1]*100

maak_indeks <- function(indeks) {
    for(i in 2:ncol(indeks)) {
        indeks[,i] <- indeks[,i]/indeks[1,i]*100
    }
    return(indeks)
}

a_indices <- cbind(CPI[-1:-9,],annual_index$Total)
a_indices$Date <- format(a_indices$Date,format="%Y")
colnames(a_indices)[6] <- "WPI" 

index_plot <- melt(maak_indeks(a_indices[-1,-5]), id="Date")  # convert to long format
g <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_point(size = 1) 
g <- g + geom_line()
g <- g + ylab("Index") + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) + theme(legend.position="bottom")
g

m <- ts(a_indices[,-1],start=c(1889), end= c(1914), frequency = 1)
g_indices <- as.data.frame(diff(log(m),lag=1))
g_indices$Date <- a_indices$Date[-1]
    
index_plot <- melt(g_indices[,-4], id="Date")  # convert to long format
g <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_point(size = 1) 
g <- g + geom_line()
g <- g + ylab("Index") + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) + theme(legend.position="bottom")
g

#Check correlations (in levels)
source("corstarsl.R")
xt <- xtable(corstarsl(a_indices[,-1]), caption="Correlations in Levels")
print(xt, "latex",comment=FALSE, caption.placement = getOption("xtable.caption.placement", "top"))

xt <- xtable(corstarsl(g_indices[,-6]), caption="Correlations in Levels")
print(xt, "latex",comment=FALSE, caption.placement = getOption("xtable.caption.placement", "top"))


#Currency
#png(file = "Commodity_plot.png", width=720,height=480)
indicator_plot <- cbind(GDP[-1:-33,c("Date", "Currency")],annual_index$Total[-22:-26])
colnames(indicator_plot) <- c("Date","Currency","WPI")
indicator_plot[,-1] <- scale(indicator_plot[,-1])
g <- ggplot(indicator_plot) 
g <- g + labs(color="Legend text")
g <- g + geom_line(aes(x=Date, y=WPI, colour="WPI"), size = 1)
g <- g + geom_line(aes(x=Date, y=Currency, colour="Currency"), size = 1)
g <- g + geom_rect(data=tp[-1:-7,], aes(xmin=Peaks, xmax=Troughs, ymin=-Inf, ymax=+Inf), fill='grey', alpha=0.5)
g <- g + ylab("Index") + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g <- g + theme(legend.position="bottom")
g
#dev.off()

temp_indices <- indicator_plot[,-1]
xt <- xtable(corstarsl(temp_indices), caption="Correlations in Levels")
print(xt, "latex",comment=FALSE, caption.placement = getOption("xtable.caption.placement", "top"))

#png(file = "Commodity_plot.png", width=720,height=480)
indicator_plot <- cbind(GDP[-1:-33,c("Date", "Money_Supply")],annual_index$Total[-22:-26])
colnames(indicator_plot) <- c("Date","Money_Supply","WPI")
indicator_plot[,-1] <- scale(indicator_plot[,-1])
g <- ggplot(indicator_plot) 
#g <- g + labs(color="Legend text")
g <- g + geom_line(aes(x=Date, y=WPI, colour="WPI"), size = 1)
g <- g + geom_line(aes(x=Date, y=Money_Supply, colour="Money_Supply"), size = 1)
g <- g + geom_rect(data=tp[-1:-7,], aes(xmin=Peaks, xmax=Troughs, ymin=-Inf, ymax=+Inf), fill='grey', alpha=0.5)
g <- g + ylab("Index") + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g <- g + theme(legend.title=element_blank()) + theme(legend.position="bottom")
g
#dev.off()

temp_indices <- indicator_plot[,-1]
xt <- xtable(corstarsl(temp_indices), caption="Correlations in Levels")
print(xt, "latex",comment=FALSE, caption.placement = getOption("xtable.caption.placement", "top"))



UK <- read.csv("UK.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
UK$Date <- indices$Date
UK <- cbind(UK,Cape.WPI=indices$Total,
            Cape.Wheat=na.approx(crops$wheat.Index,na.rm=FALSE))
UK <- UK[-1:-3,c(1,2,7,3:6)]
UK <- maak_indeks(UK)

index_plot <- melt(UK[,c(1:3)], id="Date")  # convert to long format
g1 <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
#g1 <- g1 + geom_point(size = 1) 
g1 <- g1 + geom_line()
g1 <- g1 + ylab("Index")
g1 <- g1 + xlab("")
g1 <- g1 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g1 <- g1 + theme(legend.title=element_blank()) + theme(legend.position="bottom")

index_plot <- melt(UK[,c(1,4:7)], id="Date")  # convert to long format
g2 <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
#g2 <- g2 + geom_point(size = 1) 
g2 <- g2 + geom_line()
g2 <- g2 + ylab("")
g2 <- g2 + xlab("")
g2 <- g2 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g2 <- g2 + theme(legend.title=element_blank()) + theme(legend.position="bottom")

grid.arrange(g1, g2, ncol=2, nrow =1)

#Check correlations (in levels)
source("corstarsl.R")
xt <- xtable(corstarsl(UK[,-1]), caption="Correlations in Levels")
print(xt, "latex",comment=FALSE, caption.placement = getOption("xtable.caption.placement", "top"))


#========================
#---DECOMPOSITION--------
#========================
gewig2 <- read.csv("Weights2.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)

#Do this on Journal data
allDup <- function(value) {  #remove duplicated values
    duplicated(value) | duplicated(value, fromLast = TRUE)
}
comdata1 <- comdata[!allDup(comdata[,c("date","town")]),]

dp <- data.frame()
for(i in unique(comdata1$town)) {
    temp <- comdata1[comdata1$town==i,]
    for(j in 6:ncol(temp)) {
        temp[-1,j] <- diff(log(temp[,j]), lag =1)*100
    }
    dp <- rbind(dp,temp[-1,])
}

adp <- aggregate(dp[,6:ncol(dp)], by=list(dp$date), FUN = function(x) mean(x, na.rm=TRUE))

for(i in 1:275) {
    adp$Pi[i] <- weighted.mean(adp[i,2:25], gewig2, na.rm=TRUE)
}

fr <- dp
fr[,6:ncol(fr)] <- ifelse(fr[,6:ncol(fr)]!=0,1,0)
Ind <- aggregate(fr[,6:ncol(fr)], by=list(fr$date), FUN = function(x) mean(x, na.rm=TRUE))
Ind$Ind <- rowMeans(Ind[,-1],na.rm=TRUE)
#zInd <- scale(Ind[,-1])

I_s <- ifelse(Ind[,2:(ncol(Ind)-1)]<=0.6,1,0)
I_f <- ifelse(Ind[,2:(ncol(Ind)-1)]>0.6,1,0)

#MAAL MET ACTUAL INFLATION
adp$Pi_s <- rowMeans(I_s*adp$Pi, na.rm=TRUE)
adp$Pi_f <- rowMeans(I_f*adp$Pi, na.rm=TRUE)


#------------------------------------------------------

check <- indices[,c("Date","Total")]
check$Total[-1] <- diff(log(check$Total))*100
check <- check[-1,]
check <- merge(check,adp[,c("Group.1","Pi","Pi_s","Pi_f")],by.x="Date",by.y="Group.1",all=TRUE)

#Interpoleer opsies:
check[is.na(check)] <- 0
#for(i in 1:298) {
#    if(is.na(check[i,3:5])) { check[i,3:5] <- check[i,2] }
#}

check2 <- indices[,1:5]
colnames(check2) <- c("Date","WPI","Pi","Pi_s","Pi_f")
for(i in 2:299) {
    check2[i,2:5] <- check2[(i-1),2:5]*(1+check[i,2:5]/100)
    #check2[i,2:5] <- check[i,2:5]*100
}


index_plot <- melt(check2, id="Date")  # convert to long format
g <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_line()
#g <- g + geom_point()
g <- g + ylab("Wheat Index")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) + theme(legend.position="bottom")
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g


#Annual inflation
check3 <- check2[-1:-12,]
for(i in 2:5) {
    check3[,i] <- diff(log(check2[,i]), lag=12)
}

index_plot <- melt(check3, id="Date")  # convert to long format
g <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_line()
#g <- g + geom_point()
g <- g + ylab("Wheat Index")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) + theme(legend.position="bottom")
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g

corstarsl(check2[,-1])
corstarsl(check3[,-1])

#===========================================
#Interopleer dadelik

dp <- data.frame()
for(i in unique(comdata1$town)) {
    temp <- comdata1[comdata1$town==i,]
    #temp[,-1:-5] <- na.approx(temp[,-1:-5], na.rm=FALSE)
    temp[,-1:-5] <- na.locf(temp[,-1:-5], na.rm=FALSE)
    temp[,-1:-5] <- na.locf(temp[,-1:-5], na.rm=FALSE, fromLast=TRUE)
    for(j in 6:ncol(temp)) {
        temp[-1,j] <- diff(log(temp[,j]), lag =1)*100
    }
    dp <- rbind(dp,temp[-1,])
}

adp <- aggregate(dp[,6:ncol(dp)], by=list(dp$date), FUN = function(x) mean(x, na.rm=TRUE))

for(i in 1:275) {
    adp$Pi[i] <- weighted.mean(adp[i,2:25], gewig2, na.rm=TRUE)
}

fr <- dp
fr[,6:ncol(fr)] <- ifelse(fr[,6:ncol(fr)]!=0,1,0)
Ind <- aggregate(fr[,6:ncol(fr)], by=list(fr$date), FUN = function(x) mean(x, na.rm=TRUE))
Ind$Ind <- rowMeans(Ind[,-1],na.rm=TRUE)
#zInd <- scale(Ind[,-1])

I_s <- ifelse(Ind[,2:(ncol(Ind)-1)]<=0.5,1,0)
I_f <- ifelse(Ind[,2:(ncol(Ind)-1)]>0.5,1,0)

#MAAL MET ACTUAL INFLATION
adp$Pi_s <- rowMeans(I_s*adp$Pi, na.rm=TRUE)
adp$Pi_f <- rowMeans(I_f*adp$Pi, na.rm=TRUE)


#------------------------------------------------------

check <- indices[,c("Date","Total")]
check$Total[-1] <- diff(log(check$Total))*100
check <- check[-1,]
check <- merge(check,adp[,c("Group.1","Pi","Pi_s","Pi_f")],by.x="Date",by.y="Group.1",all=TRUE)

#Interpoleer opsies:
check[is.na(check)] <- 0
#for(i in 1:298) {
#    if(is.na(check[i,3:5])) { check[i,3:5] <- check[i,2] }
#}

check2 <- indices[,1:5]
colnames(check2) <- c("Date","WPI","Pi","Pi_s","Pi_f")
for(i in 2:299) {
    check2[i,2:5] <- check2[(i-1),2:5]*(1+check[i,2:5]/100)
    #check2[i,2:5] <- check[i,2:5]*100
}


index_plot <- melt(check2, id="Date")  # convert to long format
g <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_line()
#g <- g + geom_point()
g <- g + ylab("Wheat Index")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) + theme(legend.position="bottom")
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g


#Annual inflation
check3 <- check2[-1:-12,]
for(i in 2:5) {
    check3[,i] <- diff(log(check2[,i]), lag=12)
}

index_plot <- melt(check3, id="Date")  # convert to long format
g <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_line()
#g <- g + geom_point()
g <- g + ylab("Wheat Index")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) + theme(legend.position="bottom")
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g

corstarsl(check2[,-1])
corstarsl(check3[,-1])

#-----------------------------------------------------------
#TRY OP WPIs: frequency is altyd 1
products <- cbind(crops[,1:(ncol(crops)-1)],produce[,2:(ncol(produce)-1)],pastoral[,2:(ncol(pastoral)-1)],
                  livestock[,2:(ncol(livestock)-1)],p.provisions[,2:(ncol(p.provisions)-1)],
                  a.provisions[,2:(ncol(a.provisions)-1)],o.provisions[,2:(ncol(o.provisions)-1)])


#====================================================================
#REPEAT SALES by TOWN and COMMODITY (e.g. Group by Cape Town & Wheat)
#====================================================================

towncom <- function(region, produk) {
    rscomdata1 <- rscomdata[rscomdata$town %in% region,]
    rscomdata1 <- rscomdata1[rscomdata1$commodity==produk,]
    rscomdata1 <- rscomdata1[complete.cases(rscomdata1),]
    if(nrow(rscomdata1)>0) {rscomdata1$bron <- "Journal"}
    rsblue1 <- rsblue[rsblue$town %in% region,]
    rsblue1 <- rsblue1[rsblue1$commodity==produk,]
    rsblue1 <- rsblue1[complete.cases(rsblue1),]
    
    if(nrow(rsblue1)>0) {
        rsblue1$date <- paste(rsblue1$date,"-11-30",sep="")
        rsblue1$date <- as.Date(rsblue1$date)
        rsblue1$bron <- "Blue"
        rscomdata1 <- rbind(rscomdata1[,-1],rsblue1)
    }
    rscomdata1 <- transform(rscomdata1, id = as.numeric(interaction(factor(town),factor(commodity),factor(bron),drop=TRUE)))
    
    repdata <- repsaledata(rscomdata1$lnprice,rscomdata1$date,rscomdata1$id)  #transform the data to sales pairs
    repdata <- repdata[complete.cases(repdata),]
    repeatsales <- repsale(repdata$price0,repdata$time0,repdata$price1,repdata$time1,mergefirst=1,
                           graph=FALSE)   #generate the repeat sales index
    
    RS_index <- exp(as.data.frame(repeatsales$pindex))*rscomdata1$price[min(which(!is.na(rscomdata1$price)))]
    RS_index$Date <- seq(1,1,length.out = ncol(RS_index))
    RS_index$Date <- sort(unique(c(repdata$time1,repdata$time0)))
    
    colnames(RS_index) <- c("Index","Date")
    RS_index <- RS_index[complete.cases(RS_index),]
    RS_index.ex <- aggregate(comdata$town, by=list(comdata$date), FUN = function(x) sum(!is.na(x)))
    colnames(RS_index.ex) <- c("Date","x")
    RS_index.ex <- merge(RS_index.ex, RS_index, by="Date", all=TRUE)[,-2]
    RS_index.ex[,3] <- RS_index.ex[,2]
    RS_index.ex[,2] <- na.approx(RS_index.ex[,3], na.rm=FALSE)
    #colnames(RS_index.ex) <- c("Date", produk[1])
    return(RS_index.ex)
}


#region<-"Cape Town"
#produk <- "mealies"
CTwheat <- towncom("Cape Town","mealies")
BWwheat <- towncom("Beaufort West","mealies")

index_plot <- melt(CTwheat, id="Date")  # convert to long format
g1 <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g1 <- g1 + geom_point(size = 1) 
g1 <- g1 + geom_line()
g1 <- g1 + ylab("Index") + ggtitle("Cape Town Wheat")
g1 <- g1 + xlab("")
g1 <- g1 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g1 <- g1 + theme(legend.title=element_blank()) + theme(legend.position="bottom")

index_plot <- melt(BWwheat, id="Date")  # convert to long format
g2 <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g2 <- g2 + geom_point(size = 1) 
g2 <- g2 + geom_line()
g2 <- g2 + ylab("") + ggtitle("Beaufort West Wheat")
g2 <- g2 + xlab("")
g2 <- g2 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g2 <- g2 + theme(legend.title=element_blank()) + theme(legend.position="bottom")

grid.arrange(g1, g2, ncol=2, nrow =1)

prodorp <- cbind(CTwheat[,c(1,3)],BWwheat[,3])
corstarsl(prodorp[,-1])
#cor(CTwheat$Total_Index,BWwheat$Total_Index,na.remove)



regional1 <- function(region) {
    crops <- cbind(wheat=towncom(region,"wheat")[,1:2],mealies=towncom(region,"mealies")[,1:2],barley=towncom(region,"barley")[,1:2],oats=towncom(region,"oats")[,1:2],
                   oathay=towncom(region,"oathay")[,1:2],rye=towncom(region,"rye")[,1:2],peas.beans=towncom(region,"peas.beans")[,1:2],
                   potatoes=towncom(region,"potatoes")[,1:2])
    crops <- crops[,c(1,2,4,6,8,10,12,14,16)]
    colnames(crops)[1] <- "Date"
    
    produce <- cbind(tobacco=makeindex2("tobacco")[,1:2],d.fruit=makeindex2(c("dried.fruit","d.fruit"))[,1:2],
                     wine=makeindex2(c("wine","wine.better","wine.ordinary"))[,1:2],brandy=makeindex2(c("brandy","brandy.better","brandy.ordinary"))[,1:2]) 
    produce <- produce[,c(1,2,4,6,8)]
    colnames(produce)[1] <- "Date"
    
    pastoral <- cbind(wool=makeindex2(c("w.wool","u.wool"))[,1:2],hides=makeindex2(c("hides"))[,1:2],skins=makeindex2(c("sheep.skins","goat.skins"))[,1:2],
                      cheese=makeindex2("cheese")[,1:2],fat.tallow=makeindex2("fat.tallow")[,1:2],soap=makeindex2("soap")[,1:2]) 
    pastoral <- pastoral[,c(1,2,4,6,8,10,12)]
    colnames(pastoral)[1] <- "Date"
    
    #LIVESTOCK (6):
    #c("cattle","tr.oxen","mi.cows","d.oxen","m.cows"),c("s.horse","d.horse","mules","asses"),c("sheep","wo.sheep","w.sheep","c.sheep"),
    #"swine","goats",c("fowls","ducks")
    livestock <- cbind(cattle=makeindex2(c("cattle","tr.oxen","mi.cows","m.cows"))[,1:2],horses=makeindex2(c("s.horse","d.horse","mules","asses"))[,1:2],
                       sheep=makeindex2(c("sheep","wo.sheep","w.sheep","c.sheep"))[,1:2],swine=makeindex2("swine")[,1:2],goats=makeindex2("goats")[,1:2],
                       fowls=makeindex2(c("fowls","ducks"))[,1:2])
    livestock <- livestock[,c(1,2,4,6,8,10,12)]
    colnames(livestock)[1] <- "Date"
    
    #PROVISIONS (6 + 5 + 7): 
    #"beef","mutton",c("pork","bacon"),"eggs",c("butter","butter.fresh","butter.salt"),"bread",c("beer.eng","beer.col"),c("wheat.flour","flour"),"mealie.meal","boer.meal","oatmeal"
    #"tea","coffee","sugar","rice","salt","milk","candles"
    p.provisions <- cbind(beef=makeindex2("beef")[,1:2],mutton=makeindex2("mutton")[,1:2],pork=makeindex2(c("pork","bacon"))[,1:2],
                          eggs=makeindex2("eggs")[,1:2],butter=makeindex2(c("butter","butter.fresh","butter.salt"))[,1:2],milk=makeindex2("milk")[,1:2])
    p.provisions <- p.provisions[,c(1,2,4,6,8,10,12)]
    colnames(p.provisions)[1] <- "Date"
    
    a.provisions <- cbind(bread=makeindex2("bread")[,1:2],flour=makeindex2(c("wheat.flour","flour"))[,1:2],
                          mealie.meal=makeindex2("mealie.meal")[,1:2],boer.meal=makeindex2("boer.meal")[,1:2],oatmeal=makeindex2("oatmeal")[,1:2])
    a.provisions <- a.provisions[,c(1,2,4,6,8,10)]
    colnames(a.provisions)[1] <- "Date"
    
    o.provisions <- cbind(tea=makeindex2("tea")[,1:2],coffee=makeindex2("coffee")[,1:2],sugar=makeindex2("sugar")[,1:2],beer=makeindex2(c("beer.eng","beer.col"))[,1:2],
                          rice=makeindex2("rice")[,1:2],salt=makeindex2("salt")[,1:2],candles=makeindex2("candles")[,1:2])
    o.provisions <- o.provisions[,c(1,2,4,6,8,10,12,14)]
    colnames(o.provisions)[1] <- "Date"
    
    #---------------------------------
    crops$Crops <- weeg(crops,gewig[,1:8])
    produce$Produce <- weeg(produce,gewig[,9:12])
    pastoral$Pastoral <- weeg(pastoral,gewig[,13:18])
    livestock$Livestock <- weeg(livestock,gewig[,19:24])
    p.provisions$P.Provisions <- weeg(p.provisions,gewig[,25:30])
    a.provisions$A.Provisions <- weeg(a.provisions,gewig[,31:35])
    o.provisions$O.Provisions <- weeg(o.provisions,gewig[,36:42])
    
    indices <- cbind(crops[,c("Date","Crops")],produce[,"Produce"],pastoral[,"Pastoral"],livestock[,"Livestock"],
                     p.provisions[,"P.Provisions"],a.provisions[,"A.Provisions"],o.provisions[,"O.Provisions"])
    colnames(indices) <- c("Date","Crops","Produce","Pastoral","Livestock","P.Provisions","A.Provisions","O.Provisions")
    indices$Total <- weeg(indices,gewig[,43:49])
    return(indices)
} 


wc <- regional1(wc.towns)
ec <- regional1(ec.towns)
nc <- regional1(nc.towns)

p <- 9
regions <- cbind(wc[,c(1,p)],ec[,p],nc[,p])
colnames(regions) <- c("Date","Western Cape","Eastern Cape","Northern Cape")
regions <- maak_indeks(regions)

index_plot <- melt(regions, id="Date")  # convert to long format
g <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_point(size = 1) 
g <- g + geom_line()
g <- g + ylab("Monthly Index")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) + theme(legend.position="bottom")
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g

corstarsl(regions[1:152,-1])
corstarsl(regions[153:299,-1])

source("corstarsl.R")
temp_indices <- 
for(i in 2:ncol(temp_indices)) {temp_indices[,i] <- as.numeric(temp_indices[,i]) }
ts.all_indices <- as.ts(temp_indices, start=c(1889,10),end=c(1914,8), frequency = 12) 

ct <- regional1("Cape Town")
km <- regional1("Kimberley")
pe <- regional1("Port Elizabeth")

p <- 18
regions <- cbind(ct[,c(1,p)],pe[,p],km[,p])
colnames(regions) <- c("Date","CT","PE","KM")
regions <- maak_indeks(regions)

index_plot <- melt(regions, id="Date")  # convert to long format
g <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_point(size = 1) 
g <- g + geom_line()
g <- g + ylab("Monthly Index")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) + theme(legend.position="bottom")
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g

corstarsl(regions[,-1])
corstarsl(regions[1:152,-1])
corstarsl(regions[153:299,-1])


toets <- dcast(data = rscomdata,formula = id~date+town+commodity+price+lnprice)

CT <- regional1("Cape Town")
PE <- regional1("Port Elizabeth")
EL <- regional1("East London")
KB <- regional1("Kimberley")

WC <- regional1("Worcester")
MM <- regional2("Malmesbury")
BW <- regional1("Beaufort West") 
MB <- regional1("Mossel Bay")
CW <- regional2("Clanwilliam")

GT <- regional2("Graham's Town")
PA <- regional2("Port Alfred")
GR <- regional1("Graaff-Reinet")
CR <- regional2("Cradock")
CB <- regional2("Colesberg")

KW <- regional2("King William's Town")
QT <- regional2("Queen's Town")
BD <- regional2("Burghersdorp")
AN <- regional1("Aliwal North")
DD <- regional2("Dordrecht")
TS <- regional2("Tarkastad")


p <- 2
regions <- cbind(CT[,c(1,p)],PE[,p],EL[,p],KB[,p],
                 WC[,p],MM[,p],BW[,p],MB[,p],CW[,p],
                 GT[,p],PA[,p],GR[,p],CR[,p],CB[,p],
                 KW[,p],QT[,p],BD[,p],AN[,p],DD[,p],TS[,p])

colnames(regions) <- c("Date","Cape Town","Port Elizabeth","East London","Kimberley",
           "Worcester","Malmesbury","Beaufort West","Mossel Bay","Clanwilliam",
           "Graham's Town","Port Alfred","Graaff-Reinet","Cradock","Colesberg",
           "King William's Town","Queen's Town","Burghersdorp","Aliwal North","Dordrecht","Tarkastad")


index_plot <- melt(regions[,c("Date","Cape Town","Port Elizabeth","Kimberley","Graaff-Reinet",
                              "Colesberg","Burghersdorp","Aliwal North")], id="Date")  # convert to long format
g <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_point(size = 1) 
g <- g + geom_line()
g <- g + ylab("Monthly Interpolated Index")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank())
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g


kort <- c("Worcester","Malmesbury","Mossel Bay",
          "Port Alfred","Graaff-Reinet","Cradock","Colesberg",
          "Dordrecht","Tarkastad")
regions2 <- regions
regions2[256:299,kort] <- NA 

ts.all_indices <- as.ts(regions2[1:152,-1], start=c(1889,10),end=c(1902,5), frequency = 12) 
source("corstars2.R")
xt <- xtable(corstars2(ts.all_indices)[,1:4], caption="Pre-War correlations")
print(xt, "latex",comment=FALSE, caption.placement = getOption("xtable.caption.placement", "top"),
      scalebox = 0.8,sanitize.text.function = function(x) x)

ts.all_indices <- as.ts(regions2[153:299,-1], start=c(1902,6),end=c(1914,10), frequency = 12) 
for(i in 1:20) {
    if(colnames(ts.all_indices)[i] %in% kort) {
        colnames(ts.all_indices)[i] <- paste(colnames(ts.all_indices)[i], "+", sep="")
    } 
}
source("corstars2.R")
xt <- xtable(corstars2(ts.all_indices)[,1:4], caption="Post-War correlations")
print(xt, "latex",comment=FALSE, caption.placement = getOption("xtable.caption.placement", "top"),
      scalebox = 0.8,sanitize.text.function = function(x) x)

#ts.after2 <- as.ts(regions[153:255,-1], start=c(1889,10),end=c(1907,12), frequency = 12) 
#corstarsl(ts.after2)[1:4]


#---------------------------------------------------------------------    
ratios <- sapply(regions[-1],FUN = function(x) x/regions[,2])
ratios <- log(ratios)
ratios <- ratios[,-1]

ps <- data.frame()
ts <- data.frame()
for(i in 1:19) {
    ps[1,i] <- adf.test(ratios[1:152,i])$p.value
    ps[2,i] <- adf.test(ratios[153:299,i])$p.value
    ps[3,i] <- adf.test(ratios[153:255,i])$p.value
    ps[4,i] <- adf.test(ratios[,i])$p.value
    ps[5,i] <- adf.test(ratios[1:255,i])$p.value
    
    ts[1,i] <- adf.test(ratios[1:152,i])$statistic
    ts[2,i] <- adf.test(ratios[153:299,i])$statistic
    ts[3,i] <- adf.test(ratios[153:255,i])$statistic
    ts[4,i] <- adf.test(ratios[,i])$statistic
    ts[5,i] <- adf.test(ratios[1:255,i])$statistic
    
}
colnames(ps) <- colnames(ratios)
row.names(ps) <- c("1889-1902","1902-1914","1902-1910","1889-1914","1889-2010")

colnames(ts) <- colnames(ratios)
row.names(ts) <- c("1889-1902","1902-1914","1902-1910","1889-1914","1889-2010")

mystars <- ifelse(ps < .01, "***", ifelse(ps < .05, "** ", ifelse(ps < .1, "* ", " ")))
ts <- format(round(cbind(rep(-1.11, 5), ts), 2))[,-1] 

Rnew <- data.frame()
for(i in 1:19) {
    Rnew[1:5,i] <- paste(ts[1:5,i], mystars[1:5,i], sep="") 
}

colnames(Rnew) <- colnames(ratios)
row.names(Rnew) <- c("1889-1902","1902-1914","1902-1910","1889-1914","1889-2010")

kort <- c("Worcester","Malmesbury","Mossel Bay",
          "Port Alfred","Graaff-Reinet","Cradock","Colesberg",
          "Dordrecht","Tarkastad")

Rnew[2,kort] <- Rnew[3,kort]
Rnew[4,kort] <- Rnew[5,kort]
Rnew <- Rnew[c(1,2,4),]
row.names(Rnew) <- c("Pre-War","Post-War","Full Period")
for(i in 1:19) {
    if(colnames(Rnew)[i] %in% kort) {
        colnames(Rnew)[i] <- paste(colnames(Rnew)[i], "+", sep="")
    } 
}
Rnew <- t(Rnew)

note <- paste0("\\hline \n \\multicolumn{4}{l}",
               "{\\scriptsize{+ indicates restricted sample (Dec 1910)}} \n")

xt <- xtable(Rnew, caption="Unit root test statistics")
print.xtable(xt,"latex",comment=FALSE, caption.placement = getOption("xtable.caption.placement", "top"),
             hline.after=c(-1, 0),add.to.row = list(pos = list(19),command = note))






