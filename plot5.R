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