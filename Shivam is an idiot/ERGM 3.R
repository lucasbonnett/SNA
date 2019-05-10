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

country <- read_delim("C:/Users/rdoyen/Downloads/Country.csv", ";", 
                      escape_double = FALSE, trim_ws = TRUE)

flights <- fread('https://raw.githubusercontent.com/lucasbonnett/SNA/master/Social%20Network%20Analysis.csv')

airport_codes <- read_excel("C:/Users/rdoyen/Downloads/airport-codes.xls", sheet = "Airport_Codes")


flights$codeshare <- NULL
flights$`source airport id` <- NULL
flights$`destination airport id` <- NULL
flights$`airline ID` <- NULL
flights$equipment <- NULL
flights$stops <- NULL



flights <- merge(flights, airport_codes, by.x = "source airport", by.y = "Airport Code", all.x = T)
names(flights) <- c("source airport","airline","destination airport","Source_Country")

flights <- merge(flights, airport_codes, by.x = "destination airport", by.y = "Airport Code", all.x = T)
names(flights) <- c("destination airport","source airport","airline","Source_Country", "Destination_Country")


flights$`destination airport` <- NULL
flights$`source airport` <- NULL

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

flights_base <- flights %>%
  group_by(Source_Country, Destination_Country) %>%
  summarize(airlines = n()) %>%
  arrange(Source_Country, Destination_Country)


country <- country[,c("name","region")]
country <- unique(country)

alll_countries <- unique(c(flights$Source_Country, flights$Destination_Country))
countries_not_found <- alll_countries[which(!alll_countries %in% country$name)]
table(countries_not_found)

country$region <- as.numeric(as.factor(country$region)) + 1

library(igraph)

flights_base_sub <- subset(flights_base, flights_base$airlines >15)
imp_countries <- c(flights_base_sub$Source_Country,flights_base_sub$Destination_Country)
country_sub <- subset(country, country$name %in% imp_countries)

my_network2 <- graph.data.frame(flights_base_sub[,1:2], directed = F)
my_adj <- get.adjacency(my_network2)

flights_mat <- as.matrix(my_adj)
flights_mat[flights_mat == 2] <- 1
flights_mat[1:6,1:6]
detach("package:igraph")


my_network <- network(flights_mat, vertex.attr=country_sub, vertex.attrnames=
                        colnames(country_sub), hyper=F, loops=F, multiple=F, 
                      bipartite=F, directed = F)


plot(my_network, vertex.col=get.vertex.attribute(my_network, "region"),
     label=get.vertex.attribute(my_network, "name"), label.cex=.8)






library(igraph)
my_network <- graph.data.frame(flights_base[,1:2], directed = F)
my_adj <- get.adjacency(my_network)
View(my_adj)
flights_mat <- as.matrix(my_adj)
flights_mat[flights_mat == 2] <- 1
flights_mat[1:6,1:6]
detach("package:igraph")



## Centrality Measures that we can integrate to the ergm model ##
## There are several centrality measures: degree, closeness, betweeness, eigenvector, PageRank, Bonacich Alpha
library(igraph)
airport_egde <- graph_from_adjacency_matrix(as.matrix(flights_mat), mode = "undirected")


degreeAirport <- as.data.frame(degree(airport_egde))
closenessAirport <- as.data.frame(closeness(airport_egde))
betweennessAirport = as.data.frame(betweenness(airport_egde))
index <- row.names(degreeAirport)
eigenAirport <- as.data.table(centr_eigen(airport_egde)$vector)
eigenAirport <- cbind(index,eigenAirport)
pagerankAirport <-  as.data.table(page_rank(airport_egde))
pagerankAirport <- cbind(index,pagerankAirport)
alphaairport <- as.data.frame(alpha_centrality(airport_egde))


country <- merge(x=country, y=eigenAirport, by.x = 'name', by.y='index')
country <- merge(x=country, y=pagerankAirport, by.x = 'name', by.y='index')

##Transitivity measures


transitivity <- data.frame(name = V(airport_egde)$name,
                             transitivity = transitivity(airport_egde, type = "local")) %>%
    mutate(name = as.character(name))

country <- merge(x=country, y=transitivity, by.x = 'name', by.y='name')

### ERGM

library(igraph)
flights_base
my_network <- graph.data.frame(flights_base[,1:2], directed = F)
my_adj <- get.adjacency(my_network)

flights_mat <- as.matrix(my_adj)
flights_mat[flights_mat == 2] <- 1
flights_mat[1:6,1:6]
detach("package:igraph")

imp_countries <- c(flights_base$Source_Country,flights_base$Destination_Country)
country_sub <- subset(country, country$name %in% imp_countries)

my_network <- network(flights_mat, vertex.attr=country_sub, vertex.attrnames=
                        colnames(country_sub), hyper=F, loops=F, multiple=F, 
                      bipartite=F, directed = F)

my_ergm_01 <- ergm(my_network ~ edges, estimate = "MPLE") 
my_ergm_01
summary(my_ergm_01)


my_ergm_02 <- ergm(my_network ~ edges + triangle, estimate = "MPLE")
my_ergm_02
summary(my_ergm_02)


my_ergm_03 <- ergm(my_network ~ edges + nodematch("region"), estimate = "MPLE") 
my_ergm_03
summary(my_ergm_03)


my_ergm_04 <- ergm(my_network ~ edges + nodematch("region") + degree(1), estimate = "MPLE") 
my_ergm_04
summary(my_ergm_04)


my_ergm_05 <- ergm(my_network ~ edges + nodematch("region") + degree(1), estimate = "MPLE",
                   control=control.ergm(MCMC.burnin=50000, MCMC.interval=5000))
my_ergm_05
summary(my_ergm_05)


my_ergm_06 <- ergm(my_network ~ edges + triangle  + nodematch("region") + degree(1)+ nodecov('V1') + nodecov('vector'), estimate = "MPLE",
                   control=control.ergm(MCMC.burnin=50000, MCMC.interval=5000))
my_ergm_06
summary(my_ergm_06)



my_ergm_07 <- ergm(my_network ~ edges + nodecov('V1'), estimate = "MPLE",
                   control=control.ergm(MCMC.burnin=50000, MCMC.interval=5000))
my_ergm_07
summary(my_ergm_07)


ergmstructuralonly <- ergm(my_network ~ edges+triangle+ degree(1:15)+ kstar(2:15), estimate='MPLE',
                           control=control.ergm(MCMC.burnin=50000, MCMC.interval=5000))


ergmstructuralonly
summary(ergmstructuralonly)

plot(simulate(ergmstructuralonly))

#Idea is to simulate a plot with just the structural paramaters. We compare the initial one with this one
# and we emphasize on the difference to explain that we will need additional source of data to find the real
# reasons of the network. 

#That's why, now we add some other data such as centrality, transitivity, continent, GPD, airline per link, etc

ergmfull <- ergm(my_network ~ edges + triangle  + nodematch("region") + degree(1:15)+ kstar(2:15)
                 + nodecov('V1') + nodecov('vector') + absdiff('V1')+nodediff('vector'), estimate = "MPLE",
                 control=control.ergm(MCMC.burnin=50000, MCMC.interval=5000))
  
ergmfull
summary(ergmfull)

plot(simulate(ergmfull))
