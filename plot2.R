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