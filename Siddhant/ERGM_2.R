library(readxl)
library(statnet)
library(coda)
library(igraph)
library(dplyr)
library(intergraph)
library(network)
library(readr)
library(ergm)

country <- read_delim("Country.csv", ";", 
                      escape_double = FALSE, trim_ws = TRUE)

flights <- read.csv("Social Network Analysis.csv", header=TRUE, sep = ",", stringsAsFactors = F)

airport_codes <- read_excel("airport-codes.xls", sheet = "Airport_Codes")
country$X5=NULL

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

flights_base_sub <- subset(flights_base, flights_base$airlines > 15)
imp_countries <- c(flights_base_sub$Source_Country,flights_base_sub$Destination_Country)
country_sub <- subset(country, country$name %in% imp_countries)

my_network2 <- graph.data.frame(flights_base_sub[,1:2], directed = F)
my_adj <- get.adjacency(my_network2)

flights_mat <- as.matrix(my_adj)
flights_mat[1:6,1:6]
detach("package:igraph")

my_network <- network(flights_mat, vertex.attr=country_sub, vertex.attrnames=
                        colnames(country_sub), hyper=F, loops=F, multiple=F, 
                      bipartite=F, directed = F)


plot(my_network, vertex.col=get.vertex.attribute(my_network, "region"),
     label=get.vertex.attribute(my_network, "name"), label.cex=.8)






library(igraph)
flights_base <- subset(flights_base, flights_base$airlines > 8)
my_network <- graph.data.frame(flights_base[,1:2], directed = F)
my_adj <- get.adjacency(my_network)

flights_mat <- as.matrix(my_adj)
flights_mat[1:6,1:6]
detach("package:igraph")

imp_countries <- c(flights_base$Source_Country,flights_base$Destination_Country)
country_sub <- subset(country, country$name %in% imp_countries)

my_network <- network(flights_mat, vertex.attr=country_sub, vertex.attrnames=
                        colnames(country_sub), hyper=F, loops=F, multiple=F, 
                      bipartite=F, directed = F)

my_ergm_01 <- ergm(my_network ~ edges) 
my_ergm_01
summary(my_ergm_01)


my_ergm_02 <- ergm(my_network ~ edges + triangle, estimate='MPLE')
my_ergm_02
summary(my_ergm_02)


my_ergm_03 <- ergm(my_network ~ edges + nodematch("region")) 
my_ergm_03
summary(my_ergm_03)


my_ergm_04 <- ergm(my_network ~ edges + nodematch("region") + degree(1)) 
my_ergm_04
summary(my_ergm_04)
mcmc.diagnostics(my_ergm_04)

my_network_1 <- network(flights_mat, vertex.attr=country_sub, vertex.attrnames=
                        colnames(country_sub), hyper=F, loops=F, multiple=F, 
                      bipartite=F, directed = T)

my_ergm_05 <- ergm(my_network ~ edges + concurrent)
