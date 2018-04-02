# EDA

EPA National Emissions Inventory for PM 2.5 
================================================

Fine particulate matter (PM2.5) is an ambient air pollutant for which there is strong evidence that it is harmful to human health. In the United States, the Environmental Protection Agency (EPA) is tasked with setting national ambient air quality standards for fine PM and for tracking the emissions of this pollutant into the atmosphere. Approximatly every 3 years, the EPA releases its database on emissions of PM2.5. This database is known as the National Emissions Inventory (NEI). You can read more information about the NEI at the EPA National Emissions Inventory web site.

For each year and for each type of PM source, the NEI records how many tons of PM2.5 were emitted from that source over the course of the entire year. The data that I am going to use use for this project are for 1999, 2002, 2005, and 2008.

The data for this assignment are available from the course web site as a single zip file:

Data for Peer Assessment

The zip file contains two files:

1. PM2.5 Emissions Data (summarySCC_PM25.rds): This file contains a data frame with all of the PM2.5 emissions data for 1999, 2002, 2005, and 2008. For each year, the table contains number of tons of PM2.5 emitted from a specific type of source for the entire year. The variables are:
fips: A five-digit number (represented as a string) indicating the U.S. county

* SCC: The name of the source as indicated by a digit string (see source code classification table)

* Pollutant: A string indicating the pollutant

* Emissions: Amount of PM2.5 emitted, in tons

* type: The type of source (point, non-point, on-road, or non-road)

* year: The year of emissions recorded

2. Source Classification Code Table (Source_Classification_Code.rds): This table provides a mapping from the SCC digit strings in the Emissions table to the actual name of the PM2.5 source. The sources are categorized in a few different ways from more general to more specific and you may choose to explore whatever categories you think are most useful. For example, source "10100101" is known as "Ext Comb /Electric Gen /Anthracite Coal /Pulverized Coal".
The overall goal of this project is to explore the National Emissions Inventory database and see what it say about fine particulate matter pollution in the United states over the 10-year period 1999-2008.