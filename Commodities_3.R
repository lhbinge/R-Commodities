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
wc.towns <- c("Beaufort West","Cape Town","Clanwilliam","Malmesbury","Mossel Bay","Worcester")
ec.towns <- c("Aliwal North","Burghersdorp","Cradock","Dordrecht","East London","Graaff-Reinet","Graham's Town",
              "King William's Town","Port Alfred","Port Elizabeth","Queen's Town","Tarkastad")
kzn.towns <- c("Pietermaritzburg, Natal","Durban, Natal")
in.towns <- c("Bloemfontein","Bulawayo","Colesberg","Johannesburg","Kimberley","Pretoria","Salisbury","Vryburg")
cape <- c(wc.towns,ec.towns)
col.towns <- c(cape,kzn.towns)
all.towns <- c(wc.towns,ec.towns,kzn.towns,in.towns)

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


#AGRICULTURAL PRODUCE (8 + 5): 
#"wheat","barley","oats","oathay","rye","peas.beans","potatoes","tobacco",c("dried.fruit","d.fruit") 
#c("wine","wine.better","wine.ordinary"),c("brandy","brandy.better","brandy.ordinary")
crops <- cbind(wheat=makeindex("wheat")[,c(1,3)],mealies=makeindex("mealies")[,3],barley=makeindex("barley")[,3],oats=makeindex("oats")[,3],
               oathay=makeindex("oathay")[,2],rye=makeindex("rye")[,2],peas.beans=makeindex("peas.beans")[,2],
               potatoes=makeindex("potatoes")[,3])
colnames(crops)[1:2] <- c("Date","wheat")

produce <- cbind(tobacco=makeindex("tobacco")[,c(1,3)],d.fruit=makeindex(c("dried.fruit","d.fruit"))[,2],
                 wine=makeindex(c("wine","wine.better","wine.ordinary"))[,2],brandy=makeindex(c("brandy","brandy.better","brandy.ordinary"))[,2]) 
colnames(produce)[1:2] <- c("Date","tobacco")

#PASTORAL PRODUCTS (6): 
#c("w.wool","u.wool"),"hides",c("sheep.skins","goat.skins"),"cheese","fat.tallow","soap"
pastoral <- cbind(wool=makeindex(c("w.wool","u.wool"))[,1:2],hides=makeindex(c("hides"))[,2],skins=makeindex(c("sheep.skins","goat.skins"))[,2],
                  cheese=makeindex("cheese")[,2],fat.tallow=makeindex("fat.tallow")[,2],soap=makeindex("soap")[,2]) 
colnames(pastoral)[1:2] <- c("Date","wool")

#LIVESTOCK (6):
#c("cattle","tr.oxen","mi.cows","d.oxen","m.cows"),c("s.horse","d.horse","mules","asses"),c("sheep","wo.sheep","w.sheep","c.sheep"),
#"swine","goats",c("fowls","ducks")
livestock <- cbind(cattle=makeindex(c("cattle","tr.oxen","mi.cows","d.oxen","m.cows"))[,c(1,3)],horses=makeindex(c("s.horse","d.horse","mules","asses"))[,2],
                   sheep=makeindex(c("sheep","wo.sheep","w.sheep","c.sheep"))[,3],swine=makeindex("swine")[,2],goats=makeindex("goats")[,2],
                   fowls=makeindex(c("fowls","ducks"))[,2]) 
colnames(livestock)[1:2] <- c("Date","cattle")

#PROVISIONS (6 + 5 + 7): 
#"beef","mutton",c("pork","bacon"),"eggs",c("butter","butter.fresh","butter.salt"),"bread",c("beer.eng","beer.col"),c("wheat.flour","flour"),"mealie.meal","boer.meal","oatmeal"
#"tea","coffee","sugar","rice","salt","milk","candles"
p.provisions <- cbind(beef=makeindex("beef")[,c(1,3)],mutton=makeindex("mutton")[,3],pork=makeindex(c("pork","bacon"))[,2],
                      eggs=makeindex("eggs")[,2],butter=makeindex(c("butter","butter.fresh","butter.salt"))[,3],milk=makeindex("milk")[,2])
colnames(p.provisions)[1:2] <- c("Date","beef")

a.provisions <- cbind(bread=makeindex("bread")[,c(1,3)],flour=makeindex(c("wheat.flour","flour"))[,3],
                      mealie.meal=makeindex("mealie.meal")[,2],boer.meal=makeindex("boer.meal")[,2],oatmeal=makeindex("oatmeal")[,2])
colnames(a.provisions)[1:2] <- c("Date","bread")

o.provisions <- cbind(tea=makeindex("tea")[,c(1,2)],coffee=makeindex("coffee")[,2],sugar=makeindex("sugar")[,2],beer=makeindex(c("beer.eng","beer.col"))[,2],
                      rice=makeindex("rice")[,2],salt=makeindex("salt")[,2],candles=makeindex("candles")[,2])
colnames(o.provisions)[1:2] <- c("Date","tea")


