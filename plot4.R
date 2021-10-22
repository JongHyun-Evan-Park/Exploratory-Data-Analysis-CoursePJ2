#4 Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?
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