
## Project: Fine particulate matter pollution in the 
## United states over a10-year period
## Author: Alessandro Pelliccioni
## date 4.7.2024

## The overall goal of this assignment is to explore the National Emissions 
## Inventory database and see what it say about fine particulate matter 
## pollution in the United states over the 10-year period 1999–2008.

## 1. Data

## Fine particulate matter (PM2.5) is an ambient air pollutant for which there 
## is strong evidence that it is harmful to human health. In the United States, 
## the Environmental Protection Agency (EPA) is tasked with setting national 
## ambient air quality standards for fine PM and for tracking the emissions of 
## this pollutant into the atmosphere. Approximatly every 3 years, the EPA 
## releases its database on emissions of PM2.5. This database is known as the 
## National Emissions Inventory (NEI). You can read more information about the 
## NEI at the EPA National Emissions Inventory web site.

## We will use two datasets
## PM2.5 Emissions Data (summarySCC_PM25.rdssummarySCC_PM25.rds): This file 
## contains a data frame with all of the PM2.5 emissions data for 1999, 2002, 
## 2005, and 2008. For each year, the table contains number of tons of PM2.5 
## emitted from a specific type of source for the entire year.
## Which containsa the following columns:

## fips: A five-digit number (represented as a string) indicating the U.S. county
## SCCSCC: The name of the source as indicated by a digit string (see source code classification table)
## PollutantPollutant: A string indicating the pollutant
## EmissionsEmissions: Amount of PM2.5 emitted, in tons
## typetype: The type of source (point, non-point, on-road, or non-road)
## yearyear: The year of emissions recorded

## And the second one is: Source Classification Code Table 
## (Source_Classification_Code.rdsSource_Classification_Code.rds): This table
## provides a mapping from the SCC digit strings in the Emissions table to the
## actual name of the PM2.5 source. The sources are categorized in a few 
## different ways from more general to more specific and you may choose to 
## explore whatever categories you think are most useful. For example, 
## source “10100101” is known as 
## “Ext Comb /Electric Gen /Anthracite Coal /Pulverized Coal”.

## load data sets and libraries

library(dplyr)
library(ggplot2)

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

## 2. Research questions:

## 2.1. Have total emissions from PM2.5 decreased in the United States from 
## 1999 to 2008? Using the base plotting system, make a plot showing the total
## PM2.5 emission from all sources for each of the years 1999, 2002, 2005, 
## and 2008.

## 2.2 Have total emissions from PM2.5 decreased in the Baltimore City, 
## Maryland (fips == "24510") from 1999 to 2008? Use the base
## plotting system to make a plot answering this question.

## 2.3 Of the four types of sources indicated by the typetype (point, nonpoint,
## onroad, nonroad) variable, which of these four sources have seen decreases 
## in emissions from 1999–2008 for Baltimore City? Which have seen increases 
## in emissions from 1999–2008? Use the ggplot2 plotting system to make a plot
## answer this question.

## 2.4 Across the United States, how have emissions from coal combustion-
## related sources changed from 1999–2008?

## 2.5 How have emissions from motor vehicle sources changed from 1999–2008 in
## Baltimore City?

## 2.6 Compare emissions from motor vehicle sources in Baltimore City with 
## emissions from motor vehicle sources in Los Angeles County, California 
## (fips == "06037"fips == "06037"). Which city has seen greater changes over 
## time in motor vehicle emissions?

## 3. Data analysis

## 3.1 Question 2.1
## we are interestend in the total sum over every pollutant for the whole country
## so we will build a new dataset with just two rows, the year of measurements
## and the sum of all the pollutants over the whole of the US, and plot it

## we check first that there are not missing values or negative values

summary(NEI$Emissions)

Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
0.0      0.0      0.0      3.4      0.1 646952.0 

## we see that there is one outlier with a huge PM2.5 value in tons 646952
## which we interpret as a mistake and remove from the dataset

max <- NEI$Emissions > 100000
NEI[max,]

4685317 02290 2810001000  PM25-PRI  646952.0 NONPOINT 2002
5077020 41033 2810001000  PM25-PRI  112619.8 NONPOINT 2002

## the data seems to have few outliers with extreme values, 
## as the dataset comes from Coursera, we have no way to check
## if those are real values or errors, we have to ignore it and
## go on with the research questions

## let's create a simpler dataset with only Emissions and year

dt <- NEI[, c(4,6)]

grouped <- dt %>% group_by(dt$year)

totals <- grouped %>% summarize(totals = sum(Emissions))

## let's check it looks ok

totals
# A tibble: 4 x 2
  `sum$year`   totals
       <int>    <dbl>
1       1999 7332967.
2       2002 5635780.
3       2005 5454703.
4       2008 3464206.

## we want to change the column names to better names

names(totals) <- c("Year", "Total")

plot(totals$Year, totals$Total, pch = 20)

## from the plot we see that the total emissions have decreased between 1999 and 2008 steadily
## plot is saved in file  plot1.png

png(file = "Plot1.png")
plot(totals$Year, totals$Total, pch = 20)
dev.off()

## 3.2 Question 2.2
## now we are interested in answering the same question as before, but instead
## of considering the whole US, we consider only total pollution on the city
## of Baltimore fips == "24510". Let's take a subset of NEI with only data
## related to Baltimore

baltimore <- subset(NEI, fips == "24510", c(Emissions, year))

grouped <- baltimore %>% group_by(baltimore$year)

totals <- grouped %>% summarize(totals = sum(Emissions))

## let's check it looks ok
totals

# A tibble: 4 x 2
  `baltimore$year` totals
             <int>  <dbl>
1             1999  3274.
2             2002  2454.
3             2005  3091.
4             2008  1862.

## we want to change the column names to better names

names(totals) <- c("Year", "Total")

plot(totals$Year, totals$Total, pch = 20)

## from the plot we see that the total emissions have decreased between 1999 
## and 2008, though in 2005 emissions were higher than in 2002
## plot is saved in file  plot2.png

png(file = "Plot2.png")
plot(totals$Year, totals$Total, pch = 20)
dev.off()

## 3.3 Question 2.3
## Now we want to examine how emissions have changed in Baltimora (fips == "24510")
## during the four years of the study, but looking at the four different
## source type (point, nonpoint, onroad, nonroad) using the ggplot library

## Let's take a subset of NEI with only data related to Baltimore and fewer columns

baltimore <- subset(NEI, fips == "24510", c(Emissions, type, year))

grouped <- baltimore %>% group_by(baltimore$type, baltimore$year, .groups = "keep")

totals <- grouped %>% summarize(totals = sum(Emissions))

## we want to change the column names to better names

names(totals) <- c("Type", "Year", "Total")

ggplot(totals, aes(x = Year, y = Total)) + 
  geom_line(aes(color = Type, linetype = Type), linewidth = 1.5) + 
  scale_color_manual(values = c("darkred", "steelblue", "orange", "springgreen4"))

png(file = "Plot3.png")

ggplot(totals, aes(x = Year, y = Total)) + 
  geom_line(aes(color = Type, linetype = Type), linewidth = 1.5) + 
  scale_color_manual(values = c("darkred", "steelblue", "orange", "springgreen4"))

dev.off()

## 3.4 Question 2.4
## Across the United States, how have emissions from coal combustion-
## related sources changed from 1999–2008? Now we want to filter out only
## the emissions related to a single Pollutant

## Let's study the structure of the SCC dataset, after several runs on code like
## s1 <- sort(unique(SCC$Short.Name))
## View(s1)
## we find out that the most promising column that contain the filter for
## coal combustion is the EI.Sector, we filter it out

coal <- SCC[grepl("Comb.*Coal", SCC$EI.Sector), ]
dim(coal) ## 99 rows

## extract unique SCC values to be usied in NEI dataset
indexes <- unique(coal$SCC)
length(indexes) ## 99 values

coal_emissions <- NEI[(NEI$SCC %in% indexes), ]

## reduce the dataset

dt <- coal_emissions[, c(4,6)]

grouped <- dt %>% group_by(dt$year)

totals <- grouped %>% summarize(totals = sum(Emissions))

## check that it looks ok
totals

# A tibble: 4 x 2
  `dt$year`  totals
      <int>   <dbl>
1      1999 572126.
2      2002 546789.
3      2005 552881.
4      2008 343432.

names(totals) <- c("Year", "Total")

ggplot(totals, aes(factor(Year), Total/1000, label = round(Total/1000))) + 
  geom_bar(stat = "identity", fill = "darkred") + 
  ggtitle("Total coal combustion related PM2.5 Emissions") + 
  xlab("Year") + ylab("PM2.5 Emissions in Kilotons") +
  ylim(c(0, 620)) + theme_classic()+ geom_text(size = 5, vjust = -1) + 
  theme(plot.title = element_text(hjust = 0.5))

png(file = "Plot4.png")

ggplot(totals, aes(factor(Year), Total/1000, label = round(Total/1000))) + 
  geom_bar(stat = "identity", fill = "darkred") + 
  ggtitle("Total coal combustion related PM2.5 Emissions") + 
  xlab("Year") + ylab("PM2.5 Emissions in Kilotons") +
  ylim(c(0, 620)) + theme_classic()+ geom_text(size = 5, vjust = -1) + 
  theme(plot.title = element_text(hjust = 0.5))

dev.off()
	 	
## 3.5 Question 2.5
## How have emissions from motor vehicle sources changed from 1999–2008 in
## Baltimore City?

## Let's study the structure of the SCC dataset, after several runs on code like
## s1 <- sort(unique(SCC$Short.Name))
## View(s1)
## we find out that the most promising column that contain the filter for
## motor vehicle sources is the SCC.Level.Two, we filter it out

source <- SCC[grepl("Vehicle", SCC$SCC.Level.Two), ]
dim(source)
[1] 1452   15

## extract unique SCC values to be usied in NEI dataset
indexes <- unique(source$SCC)
length(indexes) ## 99 values

motor_emissions <- NEI[(NEI$SCC %in% indexes), ]

## now we want to filter out also the data to only Baltimore, and
## at the same time reduce the dataset to a simpler one

baltimore <- subset(motor_emissions, fips == "24510", c(Emissions, year))

grouped <- baltimore %>% group_by(baltimore$year)

totals <- grouped %>% summarize(totals = sum(Emissions))

## check if it looks ok
 totals

# A tibble: 4 x 2
  `baltimore$year` totals
             <int>  <dbl>
1             1999   404.
2             2002   192.
3             2005   185.
4             2008   138.

names(totals) <- c("Year", "Total")

ggplot(totals, aes(factor(Year), Total, label = round(Total))) + 
  geom_bar(stat = "identity", fill = "darkred") + 
  ggtitle("Total Motor Vehicle Emissions in Baltimore City") + 
  xlab("Year") + ylab("PM2.5 Emissions in Tonnes") +
  ylim(c(0, 450)) + theme_classic()+ geom_text(size = 5, vjust = -1) + 
  theme(plot.title = element_text(hjust = 0.5))

png(file = "Plot5.png")

ggplot(totals, aes(factor(Year), Total, label = round(Total))) + 
  geom_bar(stat = "identity", fill = "darkred") + 
  ggtitle("Total Motor Vehicle Emissions in Baltimore City") + 
  xlab("Year") + ylab("PM2.5 Emissions in Tonnes") +
  ylim(c(0, 450)) + theme_classic()+ geom_text(size = 5, vjust = -1) + 
  theme(plot.title = element_text(hjust = 0.5))

dev.off()

## 3.6 Question 2.6
## Compare emissions from motor vehicle sources in Baltimore City with 
## emissions from motor vehicle sources in Los Angeles County, California 
## (fips == "06037"). Which city has seen greater changes over 
## time in motor vehicle emissions?

## same filters as in the 5th question, but now we keep two cities, we then
## group by year and then by city, and make a graph like in the answer to
## question 3.

source <- SCC[grepl("Vehicle", SCC$SCC.Level.Two), ]
dim(source)
[1] 1452   15

## extract unique SCC values to be usied in NEI dataset
indexes <- unique(source$SCC)
length(indexes) ## 99 values

motor_emissions <- NEI[(NEI$SCC %in% indexes), ]

## now we want to filter out also the data to only Baltimore and Los Angeles,
## and at the same time reduce the dataset to a simpler one

twocities <- subset(motor_emissions, fips == "24510" | fips == "06037", +
	c(Emissions, fips, year))

grouped <- twocities %>% group_by(twocities$fips, twocities$year, .groups = "keep")

totals <- grouped %>% summarize(totals = sum(Emissions))

## we want to change the column names to better names

names(totals) <- c("City", "Year", ".groups", "Total")

ggplot(totals, aes(x = Year, y = Total)) + 
  geom_line(aes(color = City, linetype = City), linewidth = 1.5) + 
  scale_color_manual(values = c("darkred", "steelblue"))

png(file = "Plot6.png")

ggplot(totals, aes(x = Year, y = Total)) + 
  geom_line(aes(color = City, linetype = City), linewidth = 1.5) + 
  scale_color_manual(values = c("darkred", "steelblue"))

dev.off()
