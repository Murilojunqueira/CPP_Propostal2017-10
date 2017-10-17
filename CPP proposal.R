
# Script for graphs and data citation in Computational Public 
# Policy Lab (CPP) proposal 

#clean memory
rm(list=ls(all=TRUE))
gc()


 
################## Setup workspace ##################


# install.packages("tidyverse")
# install.packages("data.table")
# install.packages("wbstats") # World Bank Data
# install.packages("ecoseries") # Brazilians Economic Statistics data


# Read Packages
library(data.table)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(wbstats) # package for loading World Bank data
library(ecoseries) # Brazilians Economic Statistics data


################## Load Data From World Bank ##################

# Load indicators and countries names
WBCache <- wbcache()

# Check objects in WBCache
names(WBCache)

# Check indicators variables
names(WBCache$indicators)

# Create a data frame of indicators
Indicators <- as.data.frame(WBCache$indicators)

# Clean Menory
rm(WBCache)

# Filter GDP annual growth indicators
GDPGrow <- Indicators %>%
  filter(indicator == "GDP growth (annual %)")

# Check indicators
# View(GDPGrow) # "NY.GDP.MKTP.KD.ZG" seems to be the best indicator


# Filter Tax Revenue indicators
TaxRev <- Indicators %>%
  filter(indicator == "Tax revenue (% of GDP)")

# Check indicators
# View(TaxRev) # "GC.TAX.TOTL.GD.ZS" seems to be the best indicator


# Filter Tax Revenue indicators
GiniIndex <- Indicators %>%
  filter(indicator == "GINI index (World Bank estimate)")

# Check indicators
# View(GiniIndex) # "SI.POV.GINI" seems to be the best indicator

# Clean Menory
rm(GDPGrow, TaxRev, GiniIndex)

# Filter extreme poverty indicators
PovertyIndex <- Indicators %>%
  filter(indicator == "Poverty headcount ratio at $1.90 a day (2011 PPP) (% of population)")

# Check indicators
#View(PovertyIndex) # "SI.POV.DDAY" seems to be the best indicator

# Clean Menory
rm(PovertyIndex)


# Download indicators
WBData <- wb(indicator = c("NY.GDP.MKTP.KD.ZG", "GC.TAX.TOTL.GD.ZS", "SI.POV.GINI", "SI.POV.DDAY"))


################## Create BRICS Data ##################


# Brics names vector
BRICS <- c("Brazil",
           "China",
           "Russian Federation",
           "India",
           "South Africa")

BricsData <- WBData %>%
  # Filter BRICS countries
  filter(country %in% BRICS) %>%
  # Group BRICS
  group_by(date, indicatorID, indicator) %>%
  # Mean of values
  summarise(value = mean(value, na.rm = T)) %>%
  # Create variables
  mutate(country = "Brics") %>%
  mutate(iso2c = "Brics") %>%
  # Correct columns order
  select(value, date, indicatorID, indicator, iso2c, country)

# As data frame
BricsData <- as.data.frame(BricsData)

# Pool data
WBData <- rbind(WBData, BricsData)

# Clean menory
rm(BricsData, BRICS)


# Check columns classes
sapply(WBData, class)

# Country selection names
CountrySelection <- c("Brazil",
                      "Upper middle income",
                      "Brics")

# Select countries
WBDataFilter <- WBData %>%
  filter(country %in% CountrySelection)

# Clean Memory
rm(CountrySelection)

################## Load Data From IPEA ##################

# Gini index from IBGE/IPEA
ipeaGini <- series_ipeadata(37818, periodicity = c("Y"))
ipeaGiniData <- as.data.frame(ipeaGini[1])

# Poverty data from IBGE/IPEA
ipeaPoverty <- series_ipeadata(1422364781, periodicity = c("Y"))
ipeaPovertyData <- as.data.frame(ipeaPoverty[1])

# Clean Memory
rm(ipeaGini, ipeaPoverty)

# Create indicatorID varaible
ipeaGiniData$indicatorID <- "GiniIpea"
ipeaPovertyData$indicatorID <- "PovertyIpea"

# Correct variables names
names(ipeaGiniData)[1:2] <- c("date", "value")
names(ipeaPovertyData)[1:2] <- c("date", "value")

# Pool data
IpeaData <- rbind(ipeaGiniData, ipeaPovertyData)

# Clean Menory
rm(ipeaGiniData, ipeaPovertyData)

# make variables from IPEA compatible with WB dataset
IpeaData <- IpeaData %>%
  mutate(date = as.numeric(substr(date, 1, 4))) %>%
  mutate(indicator = indicatorID) %>%
  mutate(iso2c = "BR") %>%
  mutate(country = "Brazil") %>%
  select(value, date, indicatorID, indicator, iso2c, country)

# Pool data
WBDataFilter <- rbind(WBDataFilter, IpeaData)

# Clean Menory
rm(IpeaData)


  
####################### Graphs ########################

# GDP Grow Graph

## Select data
GDPGrowGraphData <- WBDataFilter %>%
  filter(indicatorID == "NY.GDP.MKTP.KD.ZG") %>%
  filter(date >= 1988) %>%
  select(country, date, value)


## Set graph data
GDPGrowGraph <- ggplot(GDPGrowGraphData, aes(x = date, y = value, group=country))

## graph formating
GDPGrowGraph <- GDPGrowGraph +
  geom_line(aes(color=country), size=1.2) +
  geom_point(aes(color=country)) +
  # ggtitle("Annual GDP Grow (%), 1988 - 2016") + 
  labs (x = "Year", y = "Annual GDP Grow (%)") +
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        plot.title = element_text(size = 18, face = "bold"))

## Show graph
GDPGrowGraph

# Clean menory
rm(GDPGrowGraphData, GDPGrowGraph)


# Gini and poverty graph

## Select data
GiniGraphData <- WBDataFilter %>%
  # filter(indicatorID %in% c("SI.POV.GINI", "SI.POV.DDAY")) %>%
  filter(indicatorID %in% c("GiniIpea", "PovertyIpea")) %>%
  filter(country == "Brazil") %>%
  filter(date >= 1995) %>%
  select(country, date, indicatorID ,  value) %>%
  spread(indicatorID, value)


## Set graph data
GiniGraph <- ggplot(GiniGraphData, aes(x = date, group=country))

## graph formating
GiniGraph <- GiniGraph +
  #geom_line(aes(y = SI.POV.GINI, color="Gini"), size=1.2) + 
  #geom_line(aes(y = SI.POV.DDAY*3, color="Extreme Poverty"), size=1.2) +
  geom_line(aes(y = GiniIpea*100, color="Gini"), size=1.2) + 
  geom_line(aes(y = PovertyIpea*3, color="Extreme Poverty"), size=1.2) +
  labs (x = "Year", y = "Gini (x100)") +
  scale_y_continuous(sec.axis = sec_axis(~.*.33, name = "Extreme Poverty (%)"), 
                     labels = scales::comma) + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        plot.title = element_text(size = 18, face = "bold"))


## Show graph
GiniGraph

# Clean mempry
rm(GiniGraphData , GiniGraph)


####################### Data citation ########################

# Inequality rank

GiniRank <- WBData %>%
  # Filter World Bank Inequality index
  filter(indicatorID == "SI.POV.GINI") %>%
  # Find last year with avaliable data
  group_by(country) %>%
  summarise(UltimoAno = max(date)) %>%
  right_join(WBData, by = "country") %>%
  # Filter World Bank Inequality index (again)
  filter(indicatorID == "SI.POV.GINI") %>%
  # Filter last year with avaliable data about inequality
  filter(date == UltimoAno) %>%
  # Data older than 2005 is not reliable, ignore then
  filter(date > 2005) %>%
  # Select only meanful columns
  select(country, date, value, indicatorID)%>%
  # Order countries from most inequal to less inequal
  arrange(desc(value))


# Brazil's position on unequality rank
which(GiniRank$country == "Brazil") # Brazil is the 8st more unequal country in the World


# Clean memory
rm(GiniGraph)

####################### Closing Project ########################

# Clean memory
rm(Indicators, WBData, WBDataFilter)

# End