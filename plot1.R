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