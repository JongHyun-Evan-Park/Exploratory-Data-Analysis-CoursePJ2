set.seed(2021)
par(mar=rep(0.2,4))
dataMatrix <- matrix(rnorm(400),nrow=40)
image %>% (1:10, 1:40,t(dataMatrix)[,40:1])

par(mar=rep(0.2,4))
heatmap(dataMatrix)

set.seed(678910)
for(i in 1:40){
        coinFlip <- rbinom(1, size = 1, prob = 0.5)
        if(coinFlip){
                dataMatrix[i,] <- dataMatrix[i,] + rep(c(0,3), each = 5)
        }
}

par(mar=rep(2,4))
image %>% (1:10,1:40, t(dataMatrix)[,nrow(dataMatrix):1])
heatmap(dataMatrix)

hh <- hclust(dist(dataMatrix))
dataMatrixOrdered <- dataMatrix[hh$order,]
par(mfrow = c(1,3))
image %>% (t(dataMatrixOrdered)[,nrow(dataMatrixOrdered):1])
plot(rowMeans(dataMatrixOrdered),1:40,xlab="Row Mean", ylab = "Row", pch = 19)
plot(colMeans(dataMatrixOrdered), xlab="Column", ylab = "Column Mean", pch =19)
debug(plot)

abline(c(0,1))

str(scale(dataMatrixOrdered))
svd(scale(dataMatrixOrdered))

constantMatrix <- dataMatrixOrdered*0
for(i in 1:dim(constantMatrix)[1]){
        constantMatrix[i,] <- rep(c(0,1),each = 5)
}
constantMatrix
svd1 <- svd(constantMatrix)
par(mfrow=c(1,3))
image %>% (t(constantMatrix)[,nrow(constantMatrix):1])
image %>% 10(constantMatrix[nrow(constantMatrix):1,])
plot(svd1$d, xlab="Column", ylab = "Singular Value", pch=19)
plot(svd1$d^2/sum(svd1$d^2),xlab="Column",ylab="Prop. of variance explained", pch=19)

set.seed(678910)
for(i in 1:40){
        coinFlip1 <- rbinom(1, size=1, prob=0.5)
        coinFlip2 <- rbinom(1, size=1, prob=0.5)
        if(coinFlip1){
                dataMatrix[i,] <- dataMatrix[i,]+rep(c(0,5),each=5)
        }
        if(coinFlip2){
                dataMatrix[i,] <- dataMatrix[i,]+rep(c(0,5),5)
        }
}
hh <- hclust(dist(dataMatrix))
dataMatrixOrdered <- dataMatrix[hh$order,]

svd2 <- svd(scale(dataMatrixOrdered))
par(mfrow=c(1,3))
image %>% (t(dataMatrixOrdered)[,nrow(dataMatrixOrdered):1])
plot(svd2$v[,1], pch=19)
plot(svd2$v[,2],pch=19)

if (!requireNamespace("BiocManager", quietly = TRUE))
        install.packages("BiocManager")

BiocManager::install("impute")
library(impute)

dataMatrix2 <- dataMatrixOrdered
dataMatrix2[sample(1:100,size=40,replace=FALSE)] <- NA
View(dataMatrix2)
dataMatrix2 <- impute.knn(dataMatrix2)$data
svd1 <- svd(scale(dataMatrixOrdered));svd2 <- svd(scale(dataMatrix2))
par(mfrow=c(1,2)); plot(svd1$v[,1],pch=19); plot(svd2$v[,1],pch=19)

approx1 <- svd1$u[,1] %*% t(svd$v[,1]) %*% svd1$d[1]
svd1$d[1]
View(svd1$u)
svd1$u[,1:5]
View(svd1$u[,1])
View(t(svd1$v[,1]))
View(svd1$u[,1] %*% t(svd1$v[,1]))

?grDevices()
library(help="grDevices")

# library(help="ggplot2")

pal <- colorRamp(c("cyan","yellow"))
pal(0)

# RColorBrewer
install.packages("RColorBrewer")
library(RColorBrewer)
# name of the pallete
brewer.pal.info
library(help="RColorBrewer")
?RColorBrewer

cols <- brewer.pal(3,"Accent") # 첫번째: 색깔 수, 두번째: pallete 종류
pal <- colorRampPalette(cols)
image %>% (volcano, col = pal(20))

# smoothScatter function
x <- rnorm()
y <- rnorm()
smoothScatter(x,y)

# rgb function
# colorspace function
install.packages <- function() {
  install.packages("colorspace")
}
library(colorspace)
library(help="colorspace")

library(swirl)
swirl()

source("addPatt.R",local=TRUE)

View(ssd$subject)

# R 파일 창 띄우기
myedit <- function(fname){
  #fxfer(fname)
  #file.edit(fname)
  mypath <- pathtofile(fname)
  file.edit(mypath)
}

# showMe() - 색깔 보여준다

myplclust <- function( hclust, lab=hclust$labels, lab.col=rep(1,length(hclust$labels)), hang=0.1,...){
  ## modifiction of plclust for plotting hclust objects *in colour*!
  ## Copyright Eva KF Chan 2009
  ## Arguments:
  ##    hclust:    hclust object
  ##    lab:        a character vector of labels of the leaves of the tree
  ##    lab.col:    colour for the labels; NA=default device foreground colour
  ##    hang:     as in hclust & plclust
  ## Side effect:
  ##    A display of hierarchical cluster with coloured leaf labels.
  y <- rep(hclust$height,2)
  x <- as.numeric(hclust$merge)
  y <- y[which(x<0)]
  x <- x[which(x<0)]
  x <- abs(x)
  y <- y[order(x)]
  x <- x[order(x)]
  plot( hclust, labels=FALSE, hang=hang, ... )
  text( x=x, y=y[hclust$order]-(max(hclust$height)*hang), labels=lab[hclust$order], col=lab.col[hclust$order], srt=90, adj=c(1,0.5), xpd=NA, ... )}




Url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
if(!file.exists("./data")){dir.create("./data")}
download.file(Url, destfile="./data/exdata_data_NEI_data")
FilePath <-  "./data/exdata_data_NEI_data"
unzip(FilePath, exdir = "./data")
NEI <- readRDS("./data/summarySCC_PM25.rds")
SCC <- readRDS("./data/Source_Classification_Code.rds")
library(dplyr)
library(ggplot2)

#1 Use base plotting system to make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.
Url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
if(!file.exists("./data")){dir.create("./data")}
download.file(Url, destfile="./data/exdata_data_NEI_data")
FilePath <-  "./data/exdata_data_NEI_data"
unzip(FilePath, exdir = "./data")
NEI <- readRDS("./data/summarySCC_PM25.rds")
SCC <- readRDS("./data/Source_Classification_Code.rds")
library(dplyr)
library(ggplot2)

TOTALEM <- tapply(NEI$Emissions,NEI$year,sum)
png("plot1.png",width=480,height=480)
barplot(height=TOTALEM, names.arg=dimnames(TOTALEM)[[1]],xlab = "Year", ylab="Total Emission", main="Total PM2.5 Emissions by Year", col= c(1:4))
dev.off()

#2 Total emissions from PM2.5 in the Baltimore City, Maryland (fips == "24510") from 1999 to 2008
Url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
if(!file.exists("./data")){dir.create("./data")}
download.file(Url, destfile="./data/exdata_data_NEI_data")
FilePath <-  "./data/exdata_data_NEI_data"
unzip(FilePath, exdir = "./data")
NEI <- readRDS("./data/summarySCC_PM25.rds")
SCC <- readRDS("./data/Source_Classification_Code.rds")
library(dplyr)
library(ggplot2)

EmBalt <- NEI[which(NEI$fips=="24510"),]
EmBaltTotal <- tapply(EmBalt$Emissions,EmBalt$year,sum)
png("plot2.png",width=480,height=480)
barplot(height=EmBaltTotal, names.arg=dimnames(EmBaltTotal)[[1]],xlab = "Year", ylab="Total Emission", main="Total PM2.5 Emissions by Year in Baltimore", col= c(1:4))
dev.off()

#3 Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? Which have seen increases in emissions from 1999–2008?
Url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
if(!file.exists("./data")){dir.create("./data")}
download.file(Url, destfile="./data/exdata_data_NEI_data")
FilePath <-  "./data/exdata_data_NEI_data"
unzip(FilePath, exdir = "./data")
NEI <- readRDS("./data/summarySCC_PM25.rds")
SCC <- readRDS("./data/Source_Classification_Code.rds")
library(dplyr)
library(ggplot2)

table(NEI$type)
EmBalt <- NEI[which(NEI$fips=="24510"),]
EmBaltType <- aggregate(Emissions~year+type,EmBalt, sum)  
plotEmBaltType <- ggplot(EmBaltType, aes(year, Emissions, color=type))
plotEmBaltType+geom_line()+ ggsave("plot3.png", width = 5, height = 5)

#4 
Url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
if(!file.exists("./data")){dir.create("./data")}
download.file(Url, destfile="./data/exdata_data_NEI_data")
FilePath <-  "./data/exdata_data_NEI_data"
unzip(FilePath, exdir = "./data")
NEI <- readRDS("./data/summarySCC_PM25.rds")
SCC <- readRDS("./data/Source_Classification_Code.rds")
library(dplyr)
library(ggplot2)

summary(SCC)
SCCCoal <- SCC[grepl("Coal", SCC$Short.Name, ignore.case = TRUE),1]
NEICoal <- filter(NEI,NEI$SCC %in% SCCCoal)
plot <- ggplot(NEICoal, aes(year, Emissions))
plot + geom_bar(stat="identity") + ggsave("plot4.png", width = 5, height = 5)

#5 How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?
Url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
if(!file.exists("./data")){dir.create("./data")}
download.file(Url, destfile="./data/exdata_data_NEI_data")
FilePath <-  "./data/exdata_data_NEI_data"
unzip(FilePath, exdir = "./data")
NEI <- readRDS("./data/summarySCC_PM25.rds")
SCC <- readRDS("./data/Source_Classification_Code.rds")
library(dplyr)
library(ggplot2)

SCCMotor <- SCC[grepl("mobile",SCC$EI.Sector, ignore.case=TRUE) & grepl("vehicle",SCC$SCC.Level.Two, ignore.case = TRUE),1]
EmBalt <- NEI[which(NEI$fips=="24510"),]
EmBaltMotor <- filter(EmBalt, EmBalt$SCC %in% SCCMotor)
ggplot(EmBaltMotor, aes(year, Emissions)) + geom_bar(stat="identity")+ ggsave("plot5.png", width = 5, height = 5)

#6 Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County, California (fips=="06037")
Url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
if(!file.exists("./data")){dir.create("./data")}
download.file(Url, destfile="./data/exdata_data_NEI_data")
FilePath <-  "./data/exdata_data_NEI_data"
unzip(FilePath, exdir = "./data")
NEI <- readRDS("./data/summarySCC_PM25.rds")
SCC <- readRDS("./data/Source_Classification_Code.rds")
library(dplyr)
library(ggplot2)

SCCMotor <- SCCMotor <- SCC[grepl("mobile",SCC$EI.Sector, ignore.case=TRUE) & grepl("vehicle",SCC$SCC.Level.Two, ignore.case = TRUE),1]
BalLaMotor <- NEI %>% filter(fips==c("24510","06037") & SCC %in% SCCMotor)  
BalLaMotor <- aggregate(Emissions~year + fips,BalLaMotor,sum)
BalLaMotor
ggplot(data=BalLaMotor,aes(x= year,y= Emissions, fill=fips)) + geom_bar(stat="identity", position = "dodge") + scale_fill_discrete(name = "City", labels = c("LA", "Baltimore")) + labs(x="Year",y="Total PM2.5 Emissions", title = "Total PM2.5 Emissions: Los Angeles vs Baltimore") + ggsave("plot6.png", width = 5, height = 5)

