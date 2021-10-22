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