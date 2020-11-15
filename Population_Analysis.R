# Project on Analysis of population in India 
getwd()
#we have some data file in directory

# Data files are stored in working directory for R :

db <- read.csv("india-pop.csv")
View(db)
summary(db)
head(db,10)
tail(db,10)

#-------------------------

# Adding a new column named "Regions" to generalize Data:
db$Regions <- "NA"
# defining values :
db$Regions[which(db$States %in% c("Delhi","J & K","Punjab","Himachal Pradesh","Uttarakhand","Haryana"))]<- "Northern India"
db$Regions[which(db$States %in% c("Rajasthan","Goa","Gujarat","Maharashtra","Madhya Pradesh"))]<- "Western India"
db$Regions[which(db$States %in% c("Karnataka","Telangana","Andra Pradesh","Tamil Nadu","Kerala"))]<- "Southern India"
db$Regions[which(db$States %in% c("Uttar Pradesh","Chhattisgarh","Odisha","Jharkhand","Bihar","West Bengal"))]<- "Central India"
db$Regions[which(db$States %in% c("Assam","Meghalaya","Tripura","Mizoram","Manipur","Nagaland","Arunachal Pradesh","Sikkim"))]<- "Eastern India"

View(db)

#--------------------------

# Aggregating the Data :
# CREATING mean of data in terms of Regions :

db <- aggregate(db[3:11],list(db$Regions),mean)
View(db)

#Since some Data haven't manipulated due to symbols etc. We will manipulate it by self :

# I used Excel to add values manually ...
#-------------------------

# Based on Population :
db$Population[c(1,2,3,4,5)] <- c(82615910,5685848,14676727,50245833,78497317)
#Decadal.growth :
db$Decadal.growth[c(1,2,3,4,5)] <- c( 19.73,17.67,18.39,9.42,19.23)
# Rural.Population :
db$Rural.Population[c(1,2,3,4,5)] <- c(64912570,4591230,9424969,9424970,50077110)
# Urban.population :
db$Urban.population[c(1,2,3,4,5)] <- c(17703339,1043205,7091808,20539480,2842020)
#Area.km2 :
db$Area.km2[c(1,2,3,4,5)] <- c(132409,32772,55124,150442,231584)
#Density..km2 :
db$Density..km2[c(1,2,3,4,5)] <- c(638,141,2138,469,300)

View(db)

# Our Data Manipulation for this Data is completed...
#-------------------------------

#Lets plot the Graphs:
# Plotting Corrplot Graph: 
#install.packages('corrplot) :
library(corrplot)
corrplot(cor(db[2:9]),method = 'square')
# Using different type and method functions:
corrplot(cor(db[2:9]),method = 'number',type='upper')

#--------------------------------

# Re-reading the file :
db <- read.csv("india-pop.csv")
View(db)

# Adding a new column named Regions :
db$Regions <- "NA"
# defining values :
db$Regions[which(db$States %in% c("Delhi","J & K","Punjab","Himachal Pradesh","Uttarakhand","Haryana"))]<- "Northern India"
db$Regions[which(db$States %in% c("Rajasthan","Goa","Gujarat","Maharashtra","Madhya Pradesh"))]<- "Western India"
db$Regions[which(db$States %in% c("Karnataka","Telangana","Andra Pradesh","Tamil Nadu","Kerala"))]<- "Southern India"
db$Regions[which(db$States %in% c("Uttar Pradesh","Chhattisgarh","Odisha","Jharkhand","Bihar","West Bengal"))]<- "Central India"
db$Regions[which(db$States %in% c("Assam","Meghalaya","Tripura","Mizoram","Manipur","Nagaland","Arunachal Pradesh","Sikkim"))]<- "Eastern India"

View(db)

# Now we will plot the Factors that can affect Population:

# Plot for sex ratio :
box <- ggplot(db, aes(x=Regions, y=Sex.Ratio,fill=Regions))

# boxplot() With legend:
box + geom_boxplot() + geom_jitter(aes(color=States),size=1.0) + ggtitle("Sex Ratio of states in Sub-Region") + coord_flip()  + ylab("Sex Ratio in terms (1:1000) women to mens") + xlab("Regions in India")

# boxplot() Without Legend :
box + geom_boxplot() + geom_jitter(aes(color=States),size=1.0) +ggtitle("Sex Ratio of states in Sub-Region") + coord_flip()  + ylab("Sex Ratio in terms (1:1000) women to mens") + xlab("Regions in India") +theme(legend.position = "none")

# Plot for Area and Density of the Region :
ggplot(db, aes(y=Area.km2,x=Density..km2)) + geom_point(aes(color=Regions),size=3,alpha=0.8) + geom_smooth(aes(color=States,fill=States) ,method="lm",fullrange=T) + facet_wrap(~Regions) + theme_bw() + ggtitle("Area(km2) vs Density(km2) rate in Regions of India") + ylab("Area (in km2)") + xlab("Density (in km2)")

#Rural population plots :
ggplot(db,aes(x=States,y=Rural.Population,color=States)) + geom_point(size=2) + ggtitle("Plots for Urban and Rural Population in each States") + geom_jitter(aes(x=Urban.population),color="black",legend=TRUE) + xlab("States") + ylab("Population (Rural and Urban")
# colourfull plots are for " Rural Population " and " Black for Urban " 

# plotting Birth vs Death Rate plots :
ggplot(db, aes(y=Birth.Rate,x=Death.Rate)) + geom_point(aes(color=Regions),size=3,alpha=0.8) + geom_smooth(aes(color=Regions,fill=Regions) ,method="lm",fullrange=T) + facet_wrap(~Regions) + theme_bw() + ggtitle("Birth vs Death rate in Regions of India") + ylab("Birth Rates") + xlab("Death Rates")

# smooth plot for birth and death rate :
ggplot(db,aes(x=Birth.Rate,y=Death.Rate)) + geom_smooth(aes(color=Regions),model="glm",alpha=0.8,size=1,position="identity",se=FALSE) +ggtitle("Birth vs Death rate in Regions of India") + ylab("Birth Rates") + xlab("Death Rates")

# Now the plot of factors is completed...
#------------------------------------------------

# Past Year Records of Population for India:
# Data set containing set of past years :
db <- read.csv("india.csv")
View(db)

# Lets plot the graphs :
library(ggplot2)

# Passing data to Plot:
plot <- ggplot(db,aes(x=Year,y=TotalPopulation,label=rownames(db)))
# Plotting Graph :
plot + geom_text(aes(label=Year),hjust=-0.1,angle=80) + geom_point() + geom_smooth(model=lm) + ggtitle("Past Year Records in India") + xlab("Years") + ylab("Population (in Crore)")
#-----------------------------------------------------

# Creating new Segments to simplify our plots:
db$Segment <-"NA"
# defining values :
db$Segment[which(db$Year%in% c(db$Year[1:9]))]<- c("Segment 2")
db$Segment[which(db$Year%in% c(db$Year[10:19]))] <- c("Segment 1")
View(db)

# Plotting for GrowthRate and Density for Past Years :
ggplot(db, aes(x=Density,y=GrowthRate)) + geom_point(aes(color=Year),size=2,alpha=0.8,color="black") + geom_smooth(aes(color=Year,fill=Year) ,method="lm",fullrange=T) + facet_wrap(~Segment) + theme_bw() + ggtitle("Past Year Records (1950-2020)") + ylab("Growth Rates") + xlab("Density") 
# segment 1 = 1950-2000
# segment 2 =2001-2020

#-----------------------------------------------

# Let's see continental Graphs

# Continental Bargraph:
db <- read.csv("continent.csv")
View(db)
library(rworldmap)
library(ggplot2)

# Bar graph :
ggplot(db,aes(x=Name,y=Pop ,fill= Name)) +geom_bar(stat="identity") + ggtitle("World Population (on basis of Continents)") + ylab("Population (in Billion)") + xlab("Continents")

#----------------------------------------------------


#

# Sub-contonental graphs :
population <- read.csv("sub-continental.csv")
population <- population[-1,]

View(population)


library(ggplot2)
ggplot(population,aes(x=Name,y=Pop ,fill= Name)) +geom_bar(stat="identity") + ggtitle("World Population (on basis of Sub-Continents)") + ylab("Population (in Billion)") + xlab(" Sub-Continents")

#Growth rate :
ggplot(population,aes(x=Name,y=Pop ,fill= GrowthRate)) +geom_bar(stat="identity") + ggtitle("World Population (on basis of Sub-Continents)") + ylab("Population (in Billion)") + xlab(" Sub-Continents")

#--------------------------------------------------------

#

# World Map on population :
db1 <- read.csv("country_wise_population.csv")
View(db1)

#this was 2020 projection :

d <- data.frame(country=db1$name, value=db1$pop2020)
#View(d)
n <- joinCountryData2Map(d, joinCode ="NAME", nameJoinColumn = "country" )
#View(n)
mapCountryData(n, nameColumnToPlot = "value",mapTitle= "World Population Map 2020", colourPalette="terrain")


# World map projection on Growth Rate:
d <- data.frame(country=db1$name, value=db1$GrowthRate)
#View(d)
n <- joinCountryData2Map(d, joinCode ="NAME", nameJoinColumn = "country" )
#View(n)
mapCountryData(n, nameColumnToPlot = "value",mapTitle= "Growth Rate of Population in World", colourPalette="terrain")


# World Projection for population 1970 :
# similarly we can plot data of 2000

db1 <- read.csv("country_wise_population.csv")
View(db1)

d <- data.frame(country=db1$name, value=db1$pop2000)
#View(d)
n <- joinCountryData2Map(d, joinCode ="NAME", nameJoinColumn = "country" )
#View(n)
mapCountryData(n, nameColumnToPlot = "value",mapTitle= "World Population Map 2000", colourPalette="terrain")


















