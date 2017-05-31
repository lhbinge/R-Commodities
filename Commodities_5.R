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
trade <- read.csv("Trade.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
trade$Date <- as.Date(trade$Date)
CPI <- read.csv("CPI.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
CPI$Date <- as.Date(CPI$Date)

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
detach("package:BCDating", unload=TRUE)


indicator_plot <- cbind(datums$Date,as.data.frame(ts.GDP.m))
colnames(indicator_plot) <- c("Date","lnRGDP")
indicator_plot$lnRGDP <- as.numeric(indicator_plot$lnRGDP)
g <- ggplot(indicator_plot[-1:-120,]) 
g <- g + labs(color="Legend text")
g <- g + geom_line(aes(x=Date, y=lnRGDP, colour="lnRGDP"), size = 1)
g <- g + geom_rect(data=tp[-1:-7,], aes(xmin=Peaks, xmax=Troughs, ymin=-Inf, ymax=+Inf), fill='grey', alpha=0.5)
g <- g + ylab("log Real GDP") + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g <- g + theme(legend.position="none")
g

indicator_plot <- trade[,c("Date","Imports","Exports","Trade_Balance")]
g <- ggplot(indicator_plot) 
g <- g + labs(color="Legend text")
g <- g + geom_line(aes(x=Date, y=Imports, colour="Imports"), size = 1)
g <- g + geom_line(aes(x=Date, y=Exports, colour="Exports"), size = 1)
g <- g + geom_rect(data=tp[-1:-7,], aes(xmin=Peaks, xmax=Troughs, ymin=-Inf, ymax=+Inf), fill='grey', alpha=0.5)
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + scale_x_date(limits = c(as.Date("1886-12-31"), as.Date("1909-12-31")),
                      labels = date_format("%Y"),breaks = date_breaks("year"))
g <- g + theme(legend.title=element_blank()) + theme(legend.position="bottom")
g <- g + scale_y_continuous(name="Value (pounds)", labels = comma)
g

indicator_plot <- GDP[-1:-30,c("Date","Currency","Total_Savings")]
indicator_plot$Money_Supply <- indicator_plot$Currency+indicator_plot$Total_Savings
g <- ggplot(indicator_plot) 
g <- g + labs(color="Legend text")
g <- g + geom_line(aes(x=Date, y=Money_Supply, colour="Money_Supply"), size = 1)
#g <- g + geom_line(aes(x=Date, y=Total_Savings, colour="Total_Savings"), size = 1)
g <- g + geom_rect(data=tp[-1:-7,], aes(xmin=Peaks, xmax=Troughs, ymin=-Inf, ymax=+Inf), fill='grey', alpha=0.5)
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g <- g + scale_y_continuous(name="Money Supply (pounds)", labels = comma)
g <- g + theme(legend.title=element_blank()) + theme(legend.position="none")
g


indicator_plot <- CPI
g <- ggplot(indicator_plot[-1:-6,]) 
g <- g + labs(color="Legend text")
g <- g + geom_line(aes(x=Date, y=Bare.Bones.Index, colour="Bare.Bones.Index"), size = 1)
g <- g + geom_line(aes(x=Date, y=Respectable.Index, colour="Respectable.Index"), size = 1)
g <- g + geom_line(aes(x=Date, y=Verhoef.CPI, colour="Verhoef.CPI"), size = 1)
g <- g + geom_rect(data=tp[-1:-7,], aes(xmin=Peaks, xmax=Troughs, ymin=-Inf, ymax=+Inf), fill='grey', alpha=0.5)
g <- g + ylab("Consumer Price Indices") + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + scale_x_date(limits = c(as.Date("1886-12-31"), as.Date("1913-12-31")),
                      labels = date_format("%Y"),breaks = date_breaks("year"))
g <- g + theme(legend.title=element_blank()) + theme(legend.position="bottom")
g


##=====================
##Agricultural Journals
##=====================
comdata <- read.csv("Commodities.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
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
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g


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



rscomdata1 <- rscomdata[rscomdata$commodity=="wheat.flour",]
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

toy <- cbind(toy,rs_index)
toy <- toy[,c(2,3,4,5,7)]
colnames(toy)[c(1,5)] <- c("Date","Index")

xt <- xtable(toy, caption="Repeat sales example with wheat prices")
print(xt, "latex", include.rownames=FALSE,comment=FALSE, 
      caption.placement = getOption("xtable.caption.placement", "top"), scalebox = 0.9)

colnames(einde) <- c("ln(Pt/Ps)","D1","D2","D3","D4","D5","D6","D7","D8","D9","D10")
xt <- xtable(einde, caption="Rgression input of repeat sales example with wheat prices",digits=c(3,3,0,0,0,0,0,0,0,0,0,0))
print(xt, "latex", include.rownames=FALSE,comment=FALSE, 
      caption.placement = getOption("xtable.caption.placement", "top"), scalebox = 0.9)

rm(toy,einde)
#rm(toy.df)

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

Index <- merge(rs_index3,Index,by="Date", all = TRUE)
colnames(Index) <- c("Date","Journal_Index","Blue_Index","Combined_Index")

#-------------------------
#Alternative wheat example
rscomdata1 <- rscomdata[rscomdata$commodity==c("wheat"),]
rscomdata1 <- rscomdata1[complete.cases(rscomdata1),]
rscomdata1$bron <- "Journal"
rsblue1 <- rsblue[rsblue$commodity==c("wheat"),]
rsblue1 <- rsblue1[complete.cases(rsblue1),]

rsblue1$date <- paste(rsblue1$date,"-11-30",sep="")
rsblue1$date <- as.Date(rsblue1$date)
rsblue1$bron <- "Blue"
rscomdata1 <- rbind(rscomdata1[,-1],rsblue1)

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
colnames(RS_index.ex) <- c("Date","Total_Index_i","Total_Index")

Index <- cbind(Index,RS_index.ex[,-1])

index_plot <- Index  # convert to long format
g <- ggplot(data=index_plot) 
g <- g + geom_point(aes(x=Date, y=Blue_Index, colour="Blue_Index"), size = 2)
g <- g + geom_line(aes(x=Date, y=Journal_Index, colour="Journal_Index"), size = 0.5)
g <- g + geom_line(aes(x=Date, y=Combined_Index, colour="Combined_Index"), size = 1)
g <- g + geom_line(aes(x=Date, y=Total_Index_i, colour="Total_Index"), size = 1)
g <- g + geom_point(aes(x=Date, y=Journal_Index, colour="Journal_Index"), size = 0.5)
g <- g + geom_point(aes(x=Date, y=Total_Index, colour="Total_Index"), size = 1)
g <- g + geom_point(aes(x=Date, y=Combined_Index, colour="Combined_Index"), size = 1)
g <- g + ylab("Wheat Index") + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) + theme(legend.position="bottom")
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g

rm(Index1,repdata,RS_index,RS_index.ex,rs_index,rs_index.a,rs_index3)


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
#Make Comodity Indices
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


#Seasonal adjustment
seisoen <- function(product) {
    product[,-1] <- na.locf(product[,-1], na.rm=FALSE, fromLast=TRUE)
    for(i in 2:ncol(product)) {
        m <- ts(product[,i],start=c(1889,09), end= c(1914,7), frequency = 12)
        dec <- decompose(m, "additive")
        product[,i] <- as.numeric(m - dec$seasonal)
    }
    return(product)
}

crops <- seisoen(crops)
produce <- seisoen(produce)
pastoral <- seisoen(pastoral)
livestock <- seisoen(livestock)
p.provisions <- seisoen(p.provisions)
a.provisions <- seisoen(a.provisions)
o.provisions <- seisoen(o.provisions)


#Plot examples for individual commodities
product <- cbind(crops[,c(1,2)],produce[,c(1,2)],livestock[,c(1,2)],p.provisions[,c(1,2)])[,c(-3,-5,-7)]
colnames(product) <- c("Date","Wheat","Tobacco","Cattle","Beef")
product[,-1] <- na.approx(product[,-1], na.rm=FALSE)


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


#---------------------------------
#Calculate commodity group indices
gewig <- read.csv("Weights.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)

weeg <- function(data,w) {
    data <- data[,-1]
    m <- ts(data,start=c(1889,9), end= c(1914,7), frequency = 12)
    toets <- as.data.frame(diff(log(m)))
    n <- ncol(toets)
    for(i in 1:298) {
        toets[i,n+1] <- weighted.mean(toets[i,1:n], w, na.rm=TRUE)
        data[i+1,1] <- data[i,1]*exp(toets[i,n+1])
    }
    return(data[,1])
}

data <- a.provisions[,-7]
w <- gewig[,31:35]

crops$Crops <- weeg(crops[,1:9],gewig[,1:8])
produce$Produce <- weeg(produce[,1:5],gewig[,9:12])
pastoral$Pastoral <- weeg(pastoral[,1:7],gewig[,13:18])
livestock$Livestock <- weeg(livestock[,1:7],gewig[,19:24])
p.provisions$P.Provisions <- weeg(p.provisions[,1:7],gewig[,25:30])
a.provisions$A.Provisions <- weeg(a.provisions[,1:6],gewig[,31:35])
o.provisions$O.Provisions <- weeg(o.provisions[,1:8],gewig[,36:42])

indices <- cbind(crops[,c("Date","Crops")],produce[,c("Date","Produce")],
                 pastoral[,c("Date","Pastoral")],livestock[,c("Date","Livestock")],
                 p.provisions[,c("Date","P.Provisions")],a.provisions[,c("Date","A.Provisions")],
                 o.provisions[,c("Date","O.Provisions")])[,c(-3,-5,-7,-9,-11,-13)]
colnames(indices) <- c("Date","Crops","Produce","Pastoral","Livestock","P.Provisions","A.Provisions","O.Provisions")
indices$Total <- weeg(indices,gewig[,43:49])


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
#png(file = "Commodity_plot.png", width=720,height=480)
indicator_plot <- indices[,c("Date","Total")]
g <- ggplot(indicator_plot) 
g <- g + labs(color="Legend text")
g <- g + geom_line(aes(x=Date, y=Total, colour="Total"), size = 1)
g <- g + geom_rect(data=alt_recessions.df, aes(xmin=Peaks, xmax=Troughs, ymin=-Inf, ymax=+Inf), fill='grey', alpha=0.5)
g <- g + ylab("Index") + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
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


















