###########################################################
################## Loading the Libraries ##################
###########################################################

install.packages(c('readxl','statnet','coda','igraph','dplyr','intergraph','network','readr','ergm','data.table'))
library(readxl)
library(statnet)
library(coda)
library(igraph)
library(dplyr)
library(intergraph)
library(network)
library(readr)
library(ergm)
library(data.table)


######################################################
################## Loading the Data ##################
######################################################

# Flights Routing Data 
# Source - https://www.kaggle.com/open-flights/flight-route-database#routes.csv
flights <- read.csv("Social Network Analysis.csv", header=TRUE, sep = ",", stringsAsFactors = F)

# Airport wise country code
# Source - www.air.flyingway.com/books/xls/airport-codes.xls
airport_codes <- read_excel("airport-codes.xls", sheet = "Airport_Codes")

# Country Wise Continent
country <- read_delim("Country.csv", ";", escape_double = FALSE, trim_ws = TRUE)

# Country Wise GDP per Capita
# Source - http://worldpopulationreview.com/countries/countries-by-gdp/
GDP <- read_csv("GDP.csv")

############################################################
################## Preprocessing the Data ##################
############################################################

# Select only necessary columns
flights <- flights[,c("airline","source.airport","destination.apirport")]
country <- country[,c("name","region")]
GDP <- GDP[,c("country","gdpPerCapita")]


# We have 3425 unique Airport code, 
# creating a network for each airport can become really difficult to implement and interpret
# So we create a network for Country

# Merge the Source Airport code to fetch the source country
flights <- merge(flights, airport_codes, by.x = "source.airport", by.y = "Airport Code", all.x = T)
names(flights) <- c("source.airport","airline","destination.apirport","Source_Country")

# Merge the destination Airport code to fetch the destination country
flights <- merge(flights, airport_codes, by.x = "destination.apirport", by.y = "Airport Code", all.x = T)
names(flights) <- c("destination.airport","source.airport","airline","Source_Country", "Destination_Country")

# Now that we can countries we can delete the airport specific data
flights$destination.airport <- NULL
flights$source.airport <- NULL

# We remove the data where we are unable to find the airport code - country mapping
flights <- flights[((!is.na(flights$Source_Country)) & (!is.na(flights$Destination_Country))),]
any(is.na(flights))

# We keep only the international flights data and delete the domestic data
flights <- flights[flights$Source_Country != flights$Destination_Country,]

# Remove duplicates so that we have a combination of unique airlines flying from country A to B
flights <- unique(flights)

# We now create a base table for flights
# It contains 3 columns - Source Country, Destination Country and 
# the number of unique airline based on the route
flights_base <- flights %>%
  group_by(Source_Country, Destination_Country) %>%
  summarize(airlines = n()) %>%
  arrange(Source_Country, Destination_Country)

##############################################################
################## Creating attributes data ##################
##############################################################

# Remove duplicates
country <- unique(country)

# Create a variable for all unique countries
# We have 193 unique countries which means we will have 193 nodes in our network
alll_countries <- unique(c(flights$Source_Country, flights$Destination_Country))

# Check if all the unqiue countries we have are present in the country continent mapping
countries_not_found <- alll_countries[which(!alll_countries %in% country$name)]
table(countries_not_found)

# Check if all the unqiue countries we have are present in the country gdp mapping
countries_not_found <- alll_countries[which(!alll_countries %in% GDP$country)]
table(countries_not_found)

# Subset the data based on only the countries present
country_sub <- subset(country, country$name %in% alll_countries)
GDP_sub <- subset(GDP, GDP$country %in% alll_countries)

# Change the Continents character values to number which can be used to color the nodes differently for each continent
country_sub$region <- as.numeric(as.factor(country_sub$region)) + 1

# Merge the Country data with the GDP data so that Region and GDP per capita 
# can be used as attributes in the network
country_GDP <- merge(country_sub,GDP_sub, by.x = "name", by.y = "country", all.x = T)

# We identified that the number of out going airlines for country A to country B
# is equal to the number of airlines for  country B to country A
carriers_sub <- flights_base %>%
  group_by(Destination_Country) %>%
  summarize(num_of_airlines = sum(airlines)) %>%
  arrange(Destination_Country)

# Merge the Country data with the carriers data so that Region, GDP per capita 
# and number of airlines can be used as attributes in the network
country_GDP_car <- merge(country_GDP,carriers_sub, by.x = "name", by.y = "Destination_Country", all.x = T)

# Check if there are any NAs in the attributes data
any(is.na(country_GDP_car))


##########################################################
################## Creating the network ##################
##########################################################

library(igraph)

# Creating graph from data Source and Destination Country which will be undirected as
# the number incoming and outgoing airlines number is the same
set.seed(123)
my_graph <- graph.data.frame(flights_base[,1:2], directed = F)

# Create a adjacency matrix from the created graph
my_adj <- get.adjacency(my_graph)
flights_mat <- as.matrix(my_adj)
flights_mat[flights_mat == 2] <- 1
flights_mat[1:6,1:6]
detach("package:igraph")

# Create a flights network with attributes continent, GDP per capita and number of airlines
set.seed(123)
my_network <- network(flights_mat, vertex.attr=country_GDP_car, vertex.attrnames=
                        colnames(country_GDP_car), hyper=F, loops=F, multiple=F, 
                      bipartite=F, directed = F)

# Plot the network
plot(my_network, vertex.col=get.vertex.attribute(my_network, "region"),
     label=get.vertex.attribute(my_network, "name"), label.cex=.8)



###################################################################
################## Creating the network - Subset ##################
###################################################################

# For better interpretation/visualisation of the network we subset the routing data having
# number of unique airlines greater than 15
flights_base_sub <- subset(flights_base, flights_base$airlines > 15)

# Create a variable for all unique countries
# We have 18 unique countries which means we will have 18 nodes in our network
alll_countries <- unique(c(flights_base_sub$Source_Country, flights_base_sub$Destination_Country))

# Check if all the unqiue countries we have are present in the country continent mapping
countries_not_found <- alll_countries[which(!alll_countries %in% country$name)]
table(countries_not_found)

# Check if all the unqiue countries we have are present in the country gdp mapping
countries_not_found <- alll_countries[which(!alll_countries %in% GDP$country)]
table(countries_not_found)

# Subset the data based on only the countries present
country_sub <- subset(country, country$name %in% alll_countries)
GDP_sub <- subset(GDP, GDP$country %in% alll_countries)

# Change the Continents character values to number which can be used to color the nodes differently for each continent
country_sub$region <- as.numeric(as.factor(country_sub$region)) + 1

# Merge the Country data with the GDP data so that Region and GDP per capita 
# can be used as attributes in the network
country_GDP <- merge(country_sub,GDP_sub, by.x = "name", by.y = "country", all.x = T)

# We identified that the number of out going airlines for country A to country B
# is equal to the number of airlines for  country B to country A
carriers_sub <- flights_base %>%
  group_by(Destination_Country) %>%
  summarize(num_of_airlines = sum(airlines)) %>%
  arrange(Destination_Country)

# Merge the Country data with the carriers data so that Region, GDP per capita 
# and number of airlines can be used as attributes in the network
country_GDP_car_sub <- merge(country_GDP,carriers_sub, by.x = "name", by.y = "Destination_Country", all.x = T)

# Check if there are any NAs in the attributes data
any(is.na(country_GDP_car))

library(igraph)

# Creating graph from data Source and Destination Country which will be undirected as
# the number incoming and outgoing airlines number is the same
set.seed(123)
my_graph2 <- graph.data.frame(flights_base_sub[,1:2], directed = F)

# Create a adjacency matrix from the created graph
my_adj2 <- get.adjacency(my_graph2)
flights_mat2 <- as.matrix(my_adj2)
flights_mat2[flights_mat2 == 2] <- 1
flights_mat2[1:6,1:6]
detach("package:igraph")

# Create a flights network with attributes continent, GDP per capita and number of airlines
set.seed(123)
my_network2 <- network(flights_mat2, vertex.attr=country_GDP_car_sub, vertex.attrnames=
                        colnames(country_GDP_car_sub), hyper=F, loops=F, multiple=F, 
                      bipartite=F, directed = F)

# Plot the network
plot(my_network2, vertex.col=get.vertex.attribute(my_network2, "region"),
     label=get.vertex.attribute(my_network2, "name"), label.cex=.8)



###################################################################
##################      Centrality Measures      ##################
################## Integration to the ergm model ##################
###################################################################

## There are several centrality measures:
library(igraph)
airport_egde <- graph_from_adjacency_matrix(as.matrix(flights_mat), mode = "undirected")

# Degree of centrality
degreeAirport <- as.data.frame(degree(airport_egde))
names(degreeAirport) <- "degreeAirport"
index <- row.names(degreeAirport)
degreeAirport <- cbind(index,degreeAirport)

# Closeness of centrality
closenessAirport <- as.data.frame(closeness(airport_egde))
names(closenessAirport) <- "closenessAirport"
closenessAirport <- cbind(index,closenessAirport)

# Betweenness of centrality
betweennessAirport <- as.data.frame(betweenness(airport_egde))
names(betweennessAirport) <- "betweennessAirport"
betweennessAirport <- cbind(index,betweennessAirport)

# Eigen vector of centrality
eigenAirport <- as.data.table(centr_eigen(airport_egde)$vector)
names(eigenAirport) <- "eigenAirport"
eigenAirport <- cbind(index,eigenAirport)

# Pagerank of centrality
pagerankAirport <-  as.data.table(page_rank(airport_egde))
pagerankAirport$value <- NULL
names(pagerankAirport) <- "pagerankAirport"
pagerankAirport <- cbind(index,pagerankAirport)

# Merge all the centrality measures in the attributes table which can be integreted
# with the ERGM model
country_GDP_car <- merge(x=country_GDP_car, y=eigenAirport, by.x = 'name', by.y='index')
country_GDP_car <- merge(x=country_GDP_car, y=degreeAirport, by.x = 'name', by.y='index')
country_GDP_car <- merge(x=country_GDP_car, y=closenessAirport, by.x = 'name', by.y='index')
country_GDP_car <- merge(x=country_GDP_car, y=betweennessAirport, by.x = 'name', by.y='index')
country_GDP_car <- merge(x=country_GDP_car, y=pagerankAirport, by.x = 'name', by.y='index')

###################################################################
##################      Transitivity Measures    ##################
################## Integration to the ergm model ##################
###################################################################

# Get the transitivity measure for each country
transitivity <- data.frame(name = V(airport_egde)$name,
                           transitivity = transitivity(airport_egde, type = "local")) %>%
                            mutate(name = as.character(name))

# Merge the transitivity measure in the attributes table which can be integreted
# with the ERGM model
country_GDP_car <- merge(x=country_GDP_car, y=transitivity, by.x = 'name', by.y='name')


###############################################################
#################### ERGM - Only Edge Term ####################
###############################################################

detach("package:igraph")

# Create a flights network with attributes continent, GDP per capita, number of airlines,
# centrality and transitivity measures
set.seed(123)
my_network <- network(flights_mat, vertex.attr=country_GDP_car, vertex.attrnames=
                        colnames(country_GDP_car), hyper=F, loops=F, multiple=F, 
                      bipartite=F, directed = F)

#Benchmark with only the edge term
ergmedge <- ergm(my_network ~ edges,
                           control=control.ergm(MCMC.burnin=50000, MCMC.interval=5000))

# Check the summary
# AIC - 13140
summary(ergmedge)


###############################################################
################## ERGM - Without Attributes ##################
###############################################################



# ERGM without attributes
ergmstructuralonly <- ergm(my_network ~ edges + triangle 
                           + degree(1:10) + kstar(2:10)
                           , estimate='MPLE'
                           , control=control.ergm(MCMC.burnin=50000, MCMC.interval=5000))


# Check the summary
# AIC - 6512
summary(ergmstructuralonly)

# Plot the simulation
set.seed(123)
plot(simulate(ergmstructuralonly))

# Plot the original network
set.seed(123)
plot(my_network)

# Idea is to simulate a plot with just the structural paramaters. We compare the initial one with this one
# and we emphasize on the difference to explain that we will need additional source of data to find the real
# reasons of the network. 

# That's why, now we add some other data such as centrality, transitivity, continent, GPD, airline per link, etc

############################################################
################## ERGM - With Attributes ##################
############################################################

# ERGM with attributes
ergmfull <- ergm(my_network ~ edges + triangle  + nodematch("region") 
                 + degree(2:10) + kstar(2:10)
                 + absdiff('gdpPerCapita')
                 + nodecov('num_of_airlines') 
                 + absdiff('num_of_airlines')
                 + nodecov('eigenAirport') 
                 + nodecov('pagerankAirport') 
                 + nodecov('transitivity')
                 , estimate = "MPLE"
                 , control=control.ergm(MCMC.burnin=50000, MCMC.interval=5000))

# Check the summary
# AIC - 4208
summary(ergmfull)

# Plot the simulation
set.seed(123)
plot(simulate(ergmfull))

# Plot the original network
set.seed(123)
plot(my_network)

# Comparing the original network and the simulated network
# We observe that both of them are quite similar
# So we can conclude that including external data and centrality helped us 
# better replicate our network than just keeping routing data
