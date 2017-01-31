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

##For Grpahing Business cycles
recessions.df = read.table(textConnection(
    "Peak, Trough
    1862-01-01, 1864-01-01
    1869-01-01, 1870-01-01
    1873-01-01, 1874-01-01
    1877-01-01, 1879-01-01
    1881-01-01, 1886-01-01
    1893-01-01, 1896-01-01
    1869-01-01, 1870-01-01
    1899-01-01, 1902-01-01
    1905-01-01, 1909-01-01"), sep=',',
    colClasses=c('Date','Date'), header=TRUE)

indicator_plot <- GDP[,c("Date","lnRGDP")]
g <- ggplot(indicator_plot) 
g <- g + theme_bw()
g <- g + labs(color="Legend text")
g <- g + geom_line(aes(x=Date, y=lnRGDP, colour="lnRGDP"), size = 1)
g <- g + geom_rect(data=recessions.df, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='grey', alpha=0.5)
g <- g + ylab("log Real GDP")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g <- g + theme(legend.position="none")
g


trade <- read.csv("Trade.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
trade$Date <- as.Date(trade$Date)

##For Grpahing Business cycles
recessions.df = read.table(textConnection(
    "Peak, Trough
    1893-01-01, 1896-01-01
    1899-01-01, 1902-01-01
    1905-01-01, 1909-01-01"), sep=',',
    colClasses=c('Date','Date'), header=TRUE)

indicator_plot <- trade[,c("Date","Imports","Exports","Trade_Balance")]
g <- ggplot(indicator_plot) 
g <- g + theme_bw()
g <- g + labs(color="Legend text")
g <- g + geom_line(aes(x=Date, y=Imports, colour="Imports"), size = 1)
g <- g + geom_line(aes(x=Date, y=Exports, colour="Exports"), size = 1)
#g <- g + geom_bar(aes(x=Date, y=Trade_Balance, fill="Trade_Balance"),size = 0.5,stat="identity")
g <- g + geom_rect(data=recessions.df, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='grey', alpha=0.5)
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g <- g + theme(legend.title=element_blank()) + theme(legend.position="bottom")
g <- g + scale_y_continuous(name="Value (pounds)", labels = comma)
g

##=====================
##Agricultural Journals
##=====================

comdata <- read.csv("Commodities.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
#comdata$date <- as.Date(comdata$date, "%Y/%m/%d")
comdata$datum <- paste(comdata$datum, comdata$Year)
comdata$date <- as.Date(as.yearmon(as.character(comdata$datum),"%B %Y"))
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

comdata <- comdata[comdata$town %in% cape,]
#-------------------------------------------------------------------
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
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
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
g <- g + ylab("Wheat prices")
g <- g + xlab("")
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
g <- g + ylab("Wheat prices")
g <- g + xlab("")
g <- g + theme(legend.key.size = unit(0.3,"cm"))
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) 
g


rscomdata1 <- rscomdata1[rscomdata1$town=="Cape Town",]
rsblue1 <- rsblue1[rsblue1$town=="Cape Town",]
rsblue1$date <- paste(rsblue1$date,"-12-01",sep="")
rsblue1$date <- as.Date(rsblue1$date)

wheat <- merge(rscomdata1[,c(2,4)],rsblue1[,c(1,3)],by.x="date",by.y="date",all.x=TRUE)
wheat <- wheat[1:325,]
#wheat$price.y <- wheat$price.y*1.5
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

xt <- xtable(toy.df, caption="Repeat sales example with wheat prices")
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

#Interpolate both
#Opsie3: Interpoleer net eenvoudig
rs_index.a$Date <- paste(rs_index.a$Date,"-12-01",sep="")
rs_index.a$Date <- as.Date(rs_index.a$Date)
rs_index3 <- merge(RS_index.ex,rs_index.a,by="Date",all=TRUE)

wheat.m <- rs_index3
ts.wheat.m <- as.ts(wheat.m[,2:3], start=c(1889,10),end=c(1914,8), frequency = 12)
ts.wheat.m1 <- na.approx(ts.wheat.m, na.rm=FALSE)
#ts.wheat.m1 <- na.locf(ts.wheat.m1, na.rm=FALSE)
#ts.wheat.m1 <- na.locf(ts.wheat.m1, na.rm=FALSE, fromLast=TRUE)
rs_index3[,2:3] <- ts.wheat.m1


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
colnames(Index1) <- c("Date","Journal_Index","Blue_Index","Total Index")

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
rs_index.a$Date <- paste(rs_index.a$Date,"-12-01",sep="")
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
        
        wheat.m <- RS_index.ex$Journal_Index
        ts.wheat.m <- as.ts(wheat.m, start=c(1889,10),end=c(1914,8), frequency = 12)
        ts.wheat.m1 <- na.approx(ts.wheat.m, na.rm=FALSE)
        RS_index.ex$Journal_Index <- ts.wheat.m1
        
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
        
        rs_index.a$Date <- paste(rs_index.a$Date,"-12-01",sep="")
        rs_index.a$Date <- as.Date(rs_index.a$Date)
        rs_index1 <- merge(RS_index.ex,rs_index.a,by="Date",all=TRUE)[,-2]
        
        wheat.m <- rs_index1$Blue_Index
        ts.wheat.m <- as.ts(wheat.m, start=c(1889,10),end=c(1914,8), frequency = 12)
        ts.wheat.m1 <- na.approx(ts.wheat.m, na.rm=FALSE)
        rs_index1$Blue_Index <- ts.wheat.m1

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

a.provisions <- cbind(bread=makeindex("bread")[,1:2],flour=makeindex(c("wheat.flour","flour"))[,1:2],
                      mealie.meal=makeindex("mealie.meal")[,1:2],boer.meal=makeindex("boer.meal")[,1:2],oatmeal=makeindex("oatmeal")[,1:2])
a.provisions <- a.provisions[,c(1,2,4,6,8,10)]
colnames(a.provisions)[1] <- "Date"

o.provisions <- cbind(tea=makeindex("tea")[,1:2],coffee=makeindex("coffee")[,1:2],sugar=makeindex("sugar")[,1:2],beer=makeindex(c("beer.eng","beer.col"))[,1:2],
                      rice=makeindex("rice")[,1:2],salt=makeindex("salt")[,1:2],candles=makeindex("candles")[,1:2])
o.provisions <- o.provisions[,c(1,2,4,6,8,10,12,14)]
colnames(o.provisions)[1] <- "Date"

#-----------------------------------------------
#Plot examples for individual commodities
product <- cbind(crops[,1:2],produce$tobacco.Total_Index,livestock$cattle.Total_Index,p.provisions$beef.Total_Index)
colnames(product) <- c("Date","Wheat","Tobacco","Cattle","Beef")

index_plot <- product[,c(1,2)]
g1 <- ggplot(index_plot, aes(x=Date,y=Wheat,group=1))
g1 <- g1 + geom_line(colour="#F8766D")
g1 <- g1 + theme(legend.title=element_blank())
g1 <- g1 + ggtitle("Wheat Index") 
g1 <- g1 + ylab("Index Value") + xlab("")
g1 <- g1 + theme(legend.position="none")
g1 <- g1 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g1 <- g1 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

index_plot <- product[,c(1,3)]
g2 <- ggplot(index_plot, aes(x=Date,y=Tobacco,group=1))
g2 <- g2 + geom_line(colour="#7CAE00")
g2 <- g2 + theme(legend.title=element_blank()) 
g2 <- g2 + ggtitle("Tobacco Index") 
g2 <- g2 + ylab("Index Value") + xlab("")
g2 <- g2 + theme(legend.position="none")
g2 <- g2 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g2 <- g2 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

index_plot <- product[,c(1,4)]
g3 <- ggplot(index_plot, aes(x=Date,y=Cattle,group=1))
g3 <- g3 + geom_line(colour="#00BFC4")
g3 <- g3 + theme(legend.title=element_blank()) 
g3 <- g3 + ggtitle("Cattle Index") 
g3 <- g3 + ylab("Index Value") + xlab("")
g3 <- g3 + theme(legend.position="none")
g3 <- g3 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g3 <- g3 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

index_plot <- product[,c(1,5)]
g4 <- ggplot(index_plot, aes(x=Date,y=Beef,group=1))
g4 <- g4 + geom_line(colour="#C77CFF")
g4 <- g4 + theme(legend.title=element_blank()) 
g4 <- g4 + ggtitle("Beef Index") 
g4 <- g4 + ylab("Index Value") + xlab("")
g4 <- g4 + theme(legend.position="none")
g4 <- g4 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g4 <- g4 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

grid.arrange(g1, g2, g3, g4, ncol=2, nrow =2)


product <- makeindex(c("cattle","tr.oxen","mi.cows","d.oxen","m.cows")) 

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

#---------------------------------
#Calculate commodity group indices
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


indicator_plot <- indices[,c("Date","Total")]
g <- ggplot(indicator_plot) 
g <- g + theme_bw()
g <- g + labs(color="Legend text")
g <- g + geom_line(aes(x=Date, y=Total, colour="Total"), size = 1)
g <- g + geom_rect(data=recessions.df, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='grey', alpha=0.5)
g <- g + ylab("log Real GDP")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g <- g + theme(legend.position="none")
g

#--------------------------------
#Calculate annual average
indices$Year <- format(indices$Date,format="%Y")

annual_index <- aggregate(Total ~ Year, indices, FUN=mean)
annual_index$Total_Index <- annual_index$Total/102.96677*100 

CPI <- read.csv("CPI.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
CPI <- cbind(CPI,Total_Index=annual_index$Total_Index)

index_plot <- melt(CPI, id="Date")  # convert to long format
g <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_point(size = 1) 
g <- g + geom_line()
g <- g + ylab("Index")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) + theme(legend.position="bottom")
g

#Check correlations (in levels)
source("corstarsl.R")
temp_indices <- CPI[,-1]
for(i in 2:ncol(temp_indices)) {temp_indices[,i] <- as.numeric(temp_indices[,i]) }
ts.all_indices <- as.ts(temp_indices,start=1889, end= 1914, frequency = 1) 
xt <- xtable(corstarsl(ts.all_indices), caption="Correlations in Levels")
print(xt, "latex",comment=FALSE, caption.placement = getOption("xtable.caption.placement", "top"))


UK <- read.csv("UK.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
UK$Date <- indices$Date
UK <- cbind(UK,Cape.Wholesale.Index=indices$Total,Cape.Wheat.Index=crops$wheat.Total_Index)

index_plot <- melt(UK[,c(1,2,6)], id="Date")  # convert to long format
g1 <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g1 <- g1 + geom_point(size = 1) 
g1 <- g1 + geom_line()
g1 <- g1 + ylab("Index")
g1 <- g1 + xlab("")
g1 <- g1 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g1 <- g1 + theme(legend.title=element_blank()) + theme(legend.position="bottom")

index_plot <- melt(UK[,c(1,4,5)], id="Date")  # convert to long format
g2 <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g2 <- g2 + geom_point(size = 1) 
g2 <- g2 + geom_line()
g2 <- g2 + ylab("")
g2 <- g2 + xlab("")
g2 <- g2 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g2 <- g2 + theme(legend.title=element_blank()) + theme(legend.position="bottom")

grid.arrange(g1, g2, ncol=2, nrow =1)

#Check correlations (in levels)
source("corstarsl.R")
temp_indices <- UK[,c(-1,-3)]
for(i in 2:ncol(temp_indices)) {temp_indices[,i] <- as.numeric(temp_indices[,i]) }
ts.all_indices <- as.ts(temp_indices, start=c(1889,10),end=c(1914,8), frequency = 12) 
xt <- xtable(corstarsl(ts.all_indices), caption="Correlations in Levels")
print(xt, "latex",comment=FALSE, caption.placement = getOption("xtable.caption.placement", "top"))


#====================================================================
#REPEAT SALES by TOWN and COMMODITY (e.g. Group by Cape Town & Wheat)
#====================================================================
regional1 <- function(region) {
    comdata1 <- comdata[comdata$town %in% region,]
    rscomdata <- comdata1[,c("time_id","date","town","wheat")]
    rscomdata$commodity <- "wheat"
    colnames(rscomdata) <- c("counter","date","town","price","commodity")
    for(i in colnames(comdata1)[7:29]) {
        rscomdata1 <- comdata1[,c("time_id","date","town",i)]
        rscomdata1$commodity <- i
        colnames(rscomdata1) <- c("counter","date","town","price","commodity")
        rscomdata <- rbind(rscomdata, rscomdata1)
    }
    rscomdata$lnprice <- log(rscomdata$price)
    rscomdata <- transform(rscomdata, id = as.numeric(interaction(factor(town),factor(commodity),drop=TRUE)))
    
    blue1 <- blue[blue$town %in% region,]
    rsblue <- blue1[,c("date","town","oatmeal")]
    rsblue$commodity <- "oatmeal"
    colnames(rsblue) <- c("date","town","price","commodity")
    for(i in colnames(blue1)[4:62]) {
        rsblue1 <- blue1[,c("date","town",i)]
        rsblue1$commodity <- i
        colnames(rsblue1) <- c("date","town","price","commodity")
        rsblue <- rbind(rsblue, rsblue1)
    }
    rsblue$lnprice <- log(rsblue$price)
    rsblue <- transform(rsblue, id = as.numeric(interaction(factor(town),factor(commodity),drop=TRUE)))
    
    makeindex <- function(produk) {
        rscomdata1 <- rscomdata[rscomdata$commodity==produk,]
        rscomdata1 <- rscomdata1[complete.cases(rscomdata1),]
        if(sum(duplicated(rscomdata1$id))>0) {
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
            
            wheat.m <- RS_index.ex$Journal_Index
            ts.wheat.m <- as.ts(wheat.m, start=c(1889,10),end=c(1914,8), frequency = 12)
            ts.wheat.m1 <- na.approx(ts.wheat.m, na.rm=FALSE)
            RS_index.ex$Journal_Index <- ts.wheat.m1
            
        } else { 
            RS_index.ex <- aggregate(comdata$town, by=list(comdata$date), FUN = function(x) sum(!is.na(x)))
            colnames(RS_index.ex) <- c("Date","x")
        }
        
        rsblue1 <- rsblue[rsblue$commodity==produk,]
        rsblue1 <- rsblue1[complete.cases(rsblue1),]
        if(sum(duplicated(rsblue1$id))>0) {
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
            
            rs_index.a$Date <- paste(rs_index.a$Date,"-12-01",sep="")
            rs_index.a$Date <- as.Date(rs_index.a$Date)
            rs_index1 <- merge(RS_index.ex,rs_index.a,by="Date",all=TRUE)[,-2]
            
            wheat.m <- rs_index1$Blue_Index
            ts.wheat.m <- as.ts(wheat.m, start=c(1889,10),end=c(1914,8), frequency = 12)
            ts.wheat.m1 <- na.approx(ts.wheat.m, na.rm=FALSE)
            rs_index1$Blue_Index <- ts.wheat.m1
            
        } else {
            rs_index1 <- aggregate(comdata$town, by=list(comdata$date), FUN = function(x) sum(!is.na(x))) 
        }
        
        if(sum(duplicated(rscomdata1$id))==0) { Index1 <- rs_index1 } 
        if(sum(duplicated(rsblue1$id))==0)    { Index1 <- RS_index.ex }
        
        if(sum(duplicated(rscomdata1$id))>0 & sum(duplicated(rsblue1$id))>0) {
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
    
    #==================
    crops <- cbind(wheat=makeindex("wheat")[,1:2],mealies=makeindex("mealies")[,1:2],barley=makeindex("barley")[,1:2],oats=makeindex("oats")[,1:2],
                   oathay=makeindex("oathay")[,1:2],rye=makeindex("rye")[,1:2],peas.beans=makeindex("peas.beans")[,1:2],
                   potatoes=makeindex("potatoes")[,1:2])
    crops <- crops[,c(1,2,4,6,8,10,12,14,16)]
    colnames(crops)[1] <- "Date"
    
    produce <- cbind(tobacco=makeindex("tobacco")[,1:2],d.fruit=makeindex(c("dried.fruit","d.fruit"))[,1:2],
                     wine=makeindex(c("wine","wine.better","wine.ordinary"))[,1:2],brandy=makeindex(c("brandy","brandy.better","brandy.ordinary"))[,1:2]) 
    produce <- produce[,c(1,2,4,6,8)]
    colnames(produce)[1] <- "Date"
    
    pastoral <- cbind(wool=makeindex(c("w.wool","u.wool"))[,1:2],hides=makeindex(c("hides"))[,1:2],skins=makeindex(c("sheep.skins","goat.skins"))[,1:2],
                      cheese=makeindex("cheese")[,1:2],fat.tallow=makeindex("fat.tallow")[,1:2],soap=makeindex("soap")[,1:2]) 
    pastoral <- pastoral[,c(1,2,4,6,8,10,12)]
    colnames(pastoral)[1] <- "Date"
    
    #LIVESTOCK (6):
    #c("cattle","tr.oxen","mi.cows","d.oxen","m.cows"),c("s.horse","d.horse","mules","asses"),c("sheep","wo.sheep","w.sheep","c.sheep"),
    #"swine","goats",c("fowls","ducks")
    livestock <- cbind(cattle=makeindex(c("cattle","tr.oxen","mi.cows","m.cows"))[,1:2],horses=makeindex(c("s.horse","d.horse","mules","asses"))[,1:2],
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
    
    a.provisions <- cbind(bread=makeindex("bread")[,1:2],flour=makeindex(c("wheat.flour","flour"))[,1:2],
                          mealie.meal=makeindex("mealie.meal")[,1:2],boer.meal=makeindex("boer.meal")[,1:2],oatmeal=makeindex("oatmeal")[,1:2])
    a.provisions <- a.provisions[,c(1,2,4,6,8,10)]
    colnames(a.provisions)[1] <- "Date"
    
    o.provisions <- cbind(tea=makeindex("tea")[,1:2],coffee=makeindex("coffee")[,1:2],sugar=makeindex("sugar")[,1:2],beer=makeindex(c("beer.eng","beer.col"))[,1:2],
                          rice=makeindex("rice")[,1:2],salt=makeindex("salt")[,1:2],candles=makeindex("candles")[,1:2])
    o.provisions <- o.provisions[,c(1,2,4,6,8,10,12,14)]
    colnames(o.provisions)[1] <- "Date"
    
    #---------------------------------
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
    
    indices <- cbind(indices,crops,produce,pastoral,livestock,p.provisions,a.provisions,o.provisions)
    return(indices)
} 


wc <- regional1(wc.towns)
ec <- regional1(ec.towns)
nc <- regional1(nc.towns)

regions <- cbind(wc[,c(1,9)],ec[,9],nc[,9])
colnames(regions) <- c("Date","Western Cape","Eastern Cape","Northern Cape")

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

source("corstarsl.R")
temp_indices <- regions[,-1]
for(i in 2:ncol(temp_indices)) {temp_indices[,i] <- as.numeric(temp_indices[,i]) }
ts.all_indices <- as.ts(temp_indices, start=c(1889,10),end=c(1914,8), frequency = 12) 
corstarsl(ts.all_indices)




towncom <- function(region, produk) {
    comdata1 <- comdata[comdata$town %in% region,]
    rscomdata <- comdata1[,c("time_id","date","town","wheat")]
    rscomdata$commodity <- "wheat"
    colnames(rscomdata) <- c("counter","date","town","price","commodity")
    rscomdata$lnprice <- log(rscomdata$price)
    rscomdata <- transform(rscomdata, id = as.numeric(interaction(factor(town),factor(commodity),drop=TRUE)))
    
    blue1 <- blue[blue$town %in% region,]
    rsblue <- blue1[,c("date","town","wheat")]
    rsblue$commodity <- "wheat"
    colnames(rsblue) <- c("date","town","price","commodity")
    rsblue$lnprice <- log(rsblue$price)
    rsblue <- transform(rsblue, id = as.numeric(interaction(factor(town),factor(commodity),drop=TRUE)))
    
    rscomdata1 <- rscomdata[rscomdata$commodity==produk,]
    rscomdata1 <- rscomdata1[complete.cases(rscomdata1),]
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

    wheat.m <- RS_index.ex$Journal_Index
    ts.wheat.m <- as.ts(wheat.m, start=c(1889,10),end=c(1914,8), frequency = 12)
    ts.wheat.m1 <- na.approx(ts.wheat.m, na.rm=FALSE)
    RS_index.ex$Journal_Index <- ts.wheat.m1
            
    rsblue1 <- rsblue[rsblue$commodity==produk,]
    rsblue1 <- rsblue1[complete.cases(rsblue1),]
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
            
    rs_index.a$Date <- paste(rs_index.a$Date,"-12-01",sep="")
    rs_index.a$Date <- as.Date(rs_index.a$Date)
    rs_index1 <- merge(RS_index.ex,rs_index.a,by="Date",all=TRUE)[,-2]
            
    wheat.m <- rs_index1$Blue_Index
    ts.wheat.m <- as.ts(wheat.m, start=c(1889,10),end=c(1914,8), frequency = 12)
    ts.wheat.m1 <- na.approx(ts.wheat.m, na.rm=FALSE)
    rs_index1$Blue_Index <- ts.wheat.m1
            
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
    Index1 <- merge(Index1,RS_index,by="Date", all = TRUE)[-2]
    Index1 <- cbind(Index1,merge(RS_index.ex,rs_index1,by="Date", all = TRUE))[,-4:-5]
    colnames(Index1) <- c("Date","Total_Index","Journal_Index","Blue_Index")
    return(Index1) 
}

#region<-"Beaufort West"
#produk <- "wheat"
CTwheat <- towncom("Cape Town","wheat")
BWwheat <- towncom("Beaufort West","wheat")

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





regional2 <- function(region) {
    comdata1 <- comdata[comdata$town %in% region,]
    rscomdata <- comdata1[,c("time_id","date","town","wheat")]
    rscomdata$commodity <- "wheat"
    colnames(rscomdata) <- c("counter","date","town","price","commodity")
    for(i in colnames(comdata1)[7:29]) {
        rscomdata1 <- comdata1[,c("time_id","date","town",i)]
        rscomdata1$commodity <- i
        colnames(rscomdata1) <- c("counter","date","town","price","commodity")
        rscomdata <- rbind(rscomdata, rscomdata1)
    }
    rscomdata$lnprice <- log(rscomdata$price)
    rscomdata <- transform(rscomdata, id = as.numeric(interaction(factor(town),factor(commodity),drop=TRUE)))
    
    makeindex <- function(produk) {
        rscomdata1 <- rscomdata[rscomdata$commodity==produk,]
        rscomdata1 <- rscomdata1[complete.cases(rscomdata1),]
        if(sum(duplicated(rscomdata1$id))>0) {
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
            
            wheat.m <- RS_index.ex$Journal_Index
            ts.wheat.m <- as.ts(wheat.m, start=c(1889,10),end=c(1914,8), frequency = 12)
            ts.wheat.m1 <- na.approx(ts.wheat.m, na.rm=FALSE)
            RS_index.ex$Journal_Index <- ts.wheat.m1
            
        } else { 
            RS_index.ex <- aggregate(comdata$town, by=list(comdata$date), FUN = function(x) sum(!is.na(x)))
            colnames(RS_index.ex) <- c("Date","x")
            RS_index.ex$x <- NA
            
        }
        
        Index1 <- RS_index.ex
        
        return(Index1)
    }
    
    #==================
    crops <- cbind(wheat=makeindex("wheat")[,1:2],mealies=makeindex("mealies")[,1:2],barley=makeindex("barley")[,1:2],oats=makeindex("oats")[,1:2],
                   oathay=makeindex("oathay")[,1:2],rye=makeindex("rye")[,1:2],peas.beans=makeindex("peas.beans")[,1:2],
                   potatoes=makeindex("potatoes")[,1:2])
    crops <- crops[,c(1,2,4,6,8,10,12,14,16)]
    colnames(crops)[1] <- "Date"
    
    produce <- cbind(tobacco=makeindex("tobacco")[,1:2],d.fruit=makeindex(c("dried.fruit","d.fruit"))[,1:2],
                     wine=makeindex(c("wine","wine.better","wine.ordinary"))[,1:2],brandy=makeindex(c("brandy","brandy.better","brandy.ordinary"))[,1:2]) 
    produce <- produce[,c(1,2,4,6,8)]
    colnames(produce)[1] <- "Date"
    
    pastoral <- cbind(wool=makeindex(c("w.wool","u.wool"))[,1:2],hides=makeindex(c("hides"))[,1:2],skins=makeindex(c("sheep.skins","goat.skins"))[,1:2],
                      cheese=makeindex("cheese")[,1:2],fat.tallow=makeindex("fat.tallow")[,1:2],soap=makeindex("soap")[,1:2]) 
    pastoral <- pastoral[,c(1,2,4,6,8,10,12)]
    colnames(pastoral)[1] <- "Date"
    
    #LIVESTOCK (6):
    #c("cattle","tr.oxen","mi.cows","d.oxen","m.cows"),c("s.horse","d.horse","mules","asses"),c("sheep","wo.sheep","w.sheep","c.sheep"),
    #"swine","goats",c("fowls","ducks")
    livestock <- cbind(cattle=makeindex(c("cattle","tr.oxen","mi.cows","m.cows"))[,1:2],horses=makeindex(c("s.horse","d.horse","mules","asses"))[,1:2],
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
    
    a.provisions <- cbind(bread=makeindex("bread")[,1:2],flour=makeindex(c("wheat.flour","flour"))[,1:2],
                          mealie.meal=makeindex("mealie.meal")[,1:2],boer.meal=makeindex("boer.meal")[,1:2],oatmeal=makeindex("oatmeal")[,1:2])
    a.provisions <- a.provisions[,c(1,2,4,6,8,10)]
    colnames(a.provisions)[1] <- "Date"
    
    o.provisions <- cbind(tea=makeindex("tea")[,1:2],coffee=makeindex("coffee")[,1:2],sugar=makeindex("sugar")[,1:2],beer=makeindex(c("beer.eng","beer.col"))[,1:2],
                          rice=makeindex("rice")[,1:2],salt=makeindex("salt")[,1:2],candles=makeindex("candles")[,1:2])
    o.provisions <- o.provisions[,c(1,2,4,6,8,10,12,14)]
    colnames(o.provisions)[1] <- "Date"
    
    #---------------------------------
    #Calculate commodity group indices
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
    
    indices <- cbind(indices,crops,produce,pastoral,livestock,p.provisions,a.provisions,o.provisions)
    return(indices)
} 


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



