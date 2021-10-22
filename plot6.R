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