# install.packages("statnet")
library(statnet)
# install.packages("coda")
library(coda)
library(igraph)


data <- read.csv("Social Network Analysis.csv", header=TRUE, sep = ",", stringsAsFactors = F)
airport_code <- read.csv("Airport_Code.csv", header=TRUE, sep = ",", stringsAsFactors = F)

data$codeshare <- NULL
data$source.airport.id <- NULL
data$destination.airport.id <- NULL
data$airline.ID <- NULL
data$equipment <- NULL

#Remove the few observations where "stop" column is equal to 1 (we want only direct flights)
data <- subset(data, data$stops<1)
data$stops <- NULL

#add columns with city and country per airport code

set.seed(1)

my_network2 <- graph.data.frame(data[,2:3], directed = TRUE)
# don't run it takes too much time with entire data 
# plot(my_network2)

#subset
my_network3 <- graph.data.frame(data[1:50,2:3], directed = TRUE)
#my_network3 <- graph_from_data_frame(data, directed = TRUE, vertices = NULL) 2nd method
summary(my_network3)
plot(my_network3, main="Airport routes", cex.main=0.8) # Plot network



#flomodel_1 <- ergm(graph1~edges)
#model1 <- ergm(flomarriage~edges) 



###############

library(readxl)
library(statnet)
library(coda)
library(igraph)
library(dplyr)
library(intergraph)
library(network)

flights <- read.csv("Social Network Analysis.csv", header=TRUE, sep = ",", stringsAsFactors = F)

airport_codes <- read_excel("airport-codes.xls", sheet = "Airport_Codes")


flights$codeshare <- NULL
flights$source.airport.id <- NULL
flights$destination.airport.id <- NULL
flights$airline.ID <- NULL
flights$equipment <- NULL
flights$stops <- NULL



flights <- merge(flights, airport_codes, by.x = "source.airport", by.y = "Airport Code", all.x = T)
names(flights) <- c("source.airport","airline","destination.apirport","Source_Country")

flights <- merge(flights, airport_codes, by.x = "destination.apirport", by.y = "Airport Code", all.x = T)
names(flights) <- c("destination.airport","source.airport","airline","Source_Country", "Destination_Country")


flights$destination.airport <- NULL
flights$source.airport <- NULL

flights <- flights[((!is.na(flights$Source_Country)) & (!is.na(flights$Destination_Country))),]
any(is.na(flights))

# Remove Domestic flights
flights <- flights[flights$Source_Country != flights$Destination_Country,]

# flights <- flights %>%
#   group_by(Source_Country, Destination_Country) %>%
#   summarize(count = n()) %>%
#   arrange(Source_Country, Destination_Country)

set.seed(1)

flights <- unique(flights)

my_network2 <- graph.data.frame(flights[,2:3], directed = TRUE)
plot(my_network2)

my_network <- asNetwork(my_network2)


my_ergm_01 <- ergm(my_network~edges) 
summary(my_ergm_01)



my_ergm_01 <- ergm(my_network~ edges + triangle) 
summary(my_ergm_01)
