#import libraries
library(tidyverse)
library(DEoptim)
library(dplyr)
library(e1071)
set.seed(616)

#read the file
library(readr)
df <- read_csv("NonPeak.csv")
df <- data.frame(df[-1,])
row.names(df) <- df[,1]
df <- df[,-1]

#select the route
route_1 <- lower.tri(df)*df
route_1_acc <- data.frame(colSums(route_1))
route_1 <- route_1/3

#set the number of trials
n <- 1000

#create a list to contain trials
trials <- replicate(n,simplify=F,
                    expr = data.frame(demand = df))

#simulate in each trial a whole configuration of demand
for (i in 1:n){
  for (j in 1:21){
    for (k in 1:21){
      trials[[i]][[j]][[k]] = rnorm(n=1,mean=route_1[k,j],sd=0.2*route_1[k,j])}}}

#calculate the total number of people getting on from each certain station
for (i in 1:n){
  trials[[i]] <- mutate(trials[[i]],get_on=rowSums(trials[[i]]))}

#create another list to do the passenger calculation
station <- 21
pas_cal <- replicate(n, simplify=F,
                     expr = data.frame(station=1:station,
                                       get_on = NA, 
                                       get_off = NA, 
                                       acc_up = NA,
                                       acc_off = NA,
                                       on_bus = NA))

#calculate the number of people getting on from each station
for (i in 1:n){
  for (k in 1:station){
    pas_cal[[i]][[2]][[k]] = trials[[i]][[22]][[22-k]]}}
#calculate the number of people getting off at each station
for (i in 1:n){
  for (k in 1:station){
    pas_cal[[i]][[3]][[k]] = sum(trials[[i]][[22-k]])}}
#calculate the accumulated getting on number
for (i in 1:n){
  pas_cal[[i]][[4]] = cumsum(pas_cal[[i]][[2]])}
#calculate the accumulated getting off number
for (i in 1:n){
  pas_cal[[i]][[5]] = cumsum(pas_cal[[i]][[3]])}
#calculate the number of people on the bus after the bus leave each station
for (i in 1:n){
  pas_cal[[i]][[6]] = pas_cal[[i]][[4]] - pas_cal[[i]][[5]]}

#calculate the bus capacity
bus_seat <- 36
bus_standing <- 15
bus_capacity <- bus_seat + bus_standing

#create a list to contain headway values for each trial
headway_list <- rep(NA,n)
#run a for loop for optimization of headway
for (i in 1:n){
  #write a function, by inputting headway, obtain the load required by per bus
  load_per_bus <- function(headway){
    frequency <- 60/headway
    max_bus_load <- max(pas_cal[[i]][[6]])
    return(max_bus_load/frequency)}
  #write a function with constraint, if the load exceeds bus capacity, returns a very small value, if not, return the load
  constrained_load_per_bus <- function(headway){
    load <- load_per_bus(headway)
    threshold <- bus_capacity
    ifelse(load > 51, 
           -10000,
           load)}
  #optimize the function with constraint (maximizing) and obtain the headway which can not maximize the load per bus but also make sure it does not exceed bus capacity for each trial 
  optimise <- optimise(constrained_load_per_bus,interval=c(6,8),maximum=TRUE)
  headway_list[i] = optimise$maximum}

#show the headway values for each trial
headway_list
#find the value of headway that can achieve full customer satisfaction for 90% of time
quantile(headway_list,0.10)

#create lists for frequency, flow rate, and flow time
frequency_peak_1 <- rep(NA,n)
flow_rate_peak_1 <- rep(NA,n)
flow_time_peak_1 <- rep(NA,n)
number_of_buses_required <- rep(NA,n)
#calculate frequency for each trial
for (i in 1:n){
  frequency_peak_1[i] <- 60/headway_list[i]}
#calculate flow rate for each trial
for (i in 1:n){
  flow_rate_peak_1[i] <- frequency_peak_1[i]}/1
#calculate flow time for each trial
for (i in 1:n){
  flow_time_peak_1[i]=45/60}
#apply Little's Law to calculate the number of buses required for each trial
for (i in 1:n){
  number_of_buses_required[i] <- (flow_rate_peak_1[i]*flow_time_peak_1[i]) %>% round()}
cat(number_of_buses_required)

#calculate the optimal number of buses required to achieve full satisfaction for 90% of time
headway_opt <- quantile(headway_list,0.10)
frequency_opt <- 60/headway_opt
flow_rate_opt <- frequency_opt/1
flow_time_opt <- 35/60
number_of_buses_required_opt <- flow_time_opt*flow_rate_opt
number_of_buses_required_opt <- number_of_buses_required_opt %>% round()

headway_opt
number_of_buses_required_opt
