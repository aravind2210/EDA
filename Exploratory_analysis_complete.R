##reading the table
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
dim(NEI)

dates <- NEI$year
head(dates)
dates <- as.Date(as.character(dates), "%Y")
str(dates)

NEI$Emissions <- NEI$Emissions/1000 ##converting into kilo tons

## Plot 1
## Finding total number of emissions in each year
x = tapply(NEI$Emissions, NEI$year, sum)

plot(unique(dates), x, xlab = "Year", ylab = "Total emissions in KT", pch= 20,type = "o",lty = 1, main = "Emissions in each year")

## From the plot, the line chart shows a decreasing trend as we move across the years from 1999 to 2008.
## Thereby total no of emissions is decreasing from all sources over the years.

##plot2
## Subsetting Baltimore City, Maryland (fips == "24510") from NEI
Baltimore <- unique(subset(NEI, fips == 24510))

## Counting total no of emissions in each year using tapply
Baltimore_sum <- tapply(Baltimore$Emissions, Baltimore$year, sum)

plot(unique(dates), Baltimore_sum, xlab = "Year", ylab = "Total emissions in KT", pch= 20,type = "o",lty = 1, main = "Emissions of PM2.5", col = "red")

##From the line chart, the total no of emissions is first decreased from 1999 to 2002, then followed an incresing trend 
## till 2005, and then again decreasing till 2008.

## plot3
g <- ggplot(Baltimore, aes(factor(year), Emissions, fill = type))
g+ geom_bar(stat = "identity")+ facet_grid(.~type)+
  labs(x="Year", y = "Total Emissions in KT", title = "PM2.5 Emissions in Baltimore")
  
##plot4
## Merge SCC and NEI dataset and then subsetting the data frame containg coal and combustion.
y <- merge(NEI,SCC, by = "SCC")
coal_data <- y[grepl("coal", y$Short.Name,ignore.case=TRUE),]
g1 <- ggplot(coal_data, aes(factor(year), Emissions))
g1 + geom_bar(stat = "identity") + labs(x="Year", y = "Emissions PM2.5 in KT",title = "Coal Combustion Emissions")

## From the bar plot, the no of emissions across USA for coal combustion related data decreases from 1999 to 2002, followed 
## by an increase in 2005 and then decreasing trend in 2008.

##plot5
## Merge baltimore dataset and SCC together and extract rows containg motor vehicle sources
z = merge(Baltimore, SCC, by = "SCC")
vehicle_data <- z[grepl("Veh", z$Short.Name,ignore.case=TRUE),]
g2 <- ggplot(vehicle_data, aes(factor(year), Emissions))
g2 + geom_bar(stat = "identity", aes(fill = year)) + 
     labs(x="Year", y = "Emissions PM2.5 in KT",title = "Motor Vehicle Emissions")

## The bar graph shows that there is  a decreasing trend in total no of emissions
## as we move from 1999 to 2008 for motor vehicle related data

##plot6

California <- unique(subset(NEI, fips == "06037"))
z1 <-  merge(California, SCC, by = "SCC")
vehicle_data1 <- z1[grepl("Veh", z1$Short.Name,ignore.case=TRUE),]
vehicle_data$city <- "Baltimore"
vehicle_data1$city <- "LA"
Balt_Cal <- rbind(vehicle_data,vehicle_data1)

g3 <- ggplot(Balt_Cal, aes(x=factor(year), y=Emissions, fill=city))
 g3 +  geom_bar(aes(fill=year),stat="identity") +
  facet_grid(scales="free", space="free", .~city) +
  labs(x="Year", y = "Emissions PM2.5 in KT",title = "Motor Vehicle Emissions in Baltimore and LA county")

