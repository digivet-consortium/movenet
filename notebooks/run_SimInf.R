library("SimInf")

n <- length(nodes$id) #number of nodes (holdings)
#this should be same as max(nodes$id), but could be a problem if nodes does not
#contain data for every single node (if a key was produced using the event data)

#Compartments & initial number of individuals in them when the simulation starts
#Here an SIR model with node sizes set from holding data
u0 <- data.frame(S = (nodes$size-1),
                 E = rep(0, n),
                 I = rep(0, n),
                 R = rep(0, n))

#Add single infectious individual to a single random node
infected_node<-sample(n,1)
u0[infected_node,1] <- u0[infected_node,1]-1
u0[infected_node,3] <- 1

#time period for simulation (vector of units of time, or of dates)
#Specify time points that you wish model to return results for.
time_unit <- "month" #or "5 days"
tspan <- seq(from = min(events$time),
             to = max(events$time),
             by = time_unit)
#This returns results by time unit, from the day min(events$time) represents
#(e.g. each 18th of the month, if min(events$time) is an 18th of the month)

#For a SISe_sp model with an environmental compartment:
#(https://rdrr.io/cran/SimInf/man/SISe_sp.html)
#Calculate the euclidian distances between nodes, if within the cutoff.
cutoff <- 2500 #distance cutoff
min_dist <- NULL #The minimum distance to separate two nodes.
#If the coordinates for two nodes are identical, the min_dist must be assigned
#or an error is raised. Default is NULL i.e. to raise an error.
distances <- distance_matrix(nodes$x, nodes$y,
                             cutoff = cutoff,
                             min_dist = min_dist)

#create and run model (on all available threads)
model <- SEIR(u0 = u0, tspan = tspan, #if using SIR model structure
              events = events, #moves between nodes
              beta = 0.45, #transmission rate
                           #Guinat et al 2016: 0.3 between-pen, 0.6 within-pen;
                           #used in Halasa 2016a as low & high tm levels
              epsilon = 0.1, #incubation rate (1/incubation period)
                             #Zhang et al 2021: 0.1 (0.05-0.25)
                             #based on Barongo et al 2016, Nielsen et al 2017
              gamma = 0.25) #mortality rate
                            #Zhang et al 2021: 0.25 (0.07-0.5)
                            #based on Guinat et al 2016
select_matrix(model) <- matrix(c(1, 0, 0, 0,
                                 1, 1, 1, 1,
                                 1, 1, 1, 0), nrow = 4)
#-> the 3rd column in the matrix selects from S, E and I (but not R) compartments.
result <- run(model = model)
plot(result)
