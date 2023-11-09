#--------------------#
#    Practical 2     #
#--------------------#

# ------- Group Introduction ------- #

# The following members form Group 27:
#   1. Anagha - S2596158 (30%)
#   2. Anjali - S2580177 (37%)
#   3. Navya  - S2602687 (33%)

# Contribution of each member:
# We collectively decided this distribution of questions. Anjali generated the qsim 
# function to obtain nb and nf vectors. Navya generated the qsim function to obtain
# the eq vector and estimated the probability of atleast one car missing the ferry
# departure. Anagha produced the 4-panel plot.

# ------- Overview of the project ------- #

# This practical involves simulating the flow of cars through French 
# and British passport control at a French ferry terminal in the context 
# of managing queues. The simulation is designed to model a 2-hour period 
# before a ferry departure.

# Setting seed for reproduceability
# Clearing all variables 
rm(list = ls())
set.seed(123)

# Define a function, qsim, with various parameters to simulate the queue system
qsim <- function(mf, mb, a.rate, trb, trf, tmb, tmf, maxb) {
  # Initialize variables for simulation of 2 hours
  # To track the average French and British queue length at each time step
  # And to track the average expected waiting time for a car at the 
  # start of the french queue for each time step
  simulation_time <- 7200
  check_in_closing_time <- 1800 #Check-in closes 30 min before departure
  nf <- numeric(simulation_time)  
  nb <- numeric(simulation_time)
  eq <- numeric(simulation_time)
  
  french_queues <- vector("list", mf)
  british_queues <- vector("list", mb)
  
  for (i in 1:mf) {
    french_queues[[i]] <- numeric(0)
  }
  for (i in 1:mb) {
    british_queues[[i]] <- numeric(0)
  }
  
  for (second in 1:simulation_time) {
    # for updating french queue lengths for each of the 5 french queues 
    french_queue_lengths <- sapply(french_queues, length)
    # if the car arrives at a French station, then it is processed and
    # the length of the french queue becomes 0 again
    for (i in 1:mf) {
      if (french_queue_lengths[i] > 0) {
        french_queue_lengths[i] <- french_queue_lengths[i] - 1
      }
    }
    # checking which values of the french_queue are 0 to get to a new queue
    new_queue_french <- which(french_queue_lengths == 0)
    
    # if the car is in a new queue we check if it can proceed to the British 
    # passport center, which can only happen if the total cars at the British
    # passport stations are less than maxb = 20.
    for (i in new_queue_french) {
      if (length(british_queues[[1]]) < maxb) {
        # The processing time at a British passport station is given by a random time
        # in the uniform distribution between tmb and tmb+trb
        british_queues[[1]] <- c(british_queues[[1]], round(runif(1, min = tmb, max = tmb + trb)))
        # Similarly for a French station, it is uniformly distributed between
        # tmf and tmf+trf
        # we are taking a round number because we are simulating over each second
        french_queue_lengths[i] <- round(runif(1, min = tmf, max = tmf + trf))
      }
    }
    
    # Updating British queues: this is similar as the French queue updating
    british_queue_lengths <- sapply(british_queues, length)
    for (i in 1:mb) {
      if (british_queue_lengths[i] > 0) {
        british_queue_lengths[i] <- british_queue_lengths[i] - 1
      }
    }
    new_queue_british <- which(british_queue_lengths == 0)
    
    for (i in new_queue_british) {
      british_queues[[i]] <- british_queues[[i]][-1]
    }
    
    # Generating a ector to represent car arrivals based on the given 
    # arrival rate. This is done with the help of random uniform distribution 
    # function.
    if (runif(1) < a.rate) {
      # The car selects the shortest queue by checking the minimum queue length
      shortest_french_queue <- which.min(french_queue_lengths)
      french_queues[[shortest_french_queue]] <- c(french_queues[[shortest_french_queue]], round(runif(1, min = tmf, max = tmf + trf)))
      french_queue_lengths[shortest_french_queue] <- round(runif(1, min = tmf, max = tmf + trf))
    }
    
    # Here we store the average french queue length at each second
    #nf[second] <- sum(french_queue_lengths)/mf
    # And the average british queue length at each second
    #nb[second] <- sum(british_queue_lengths)/mb
    # We are also storing the average expected waiting time for a car at the 
    # start of the french queue for each second
    #eq[second] <- nf[second] * (tmf + trf)/2
    
    # Calculate nf, nb, and eq
    nf[second] <- sum(sapply(french_queues, length))
    nb[second] <- sum(sapply(british_queues, length))
    eq[second] <- nf[second] * (tmf + trf) / 2
    
    
    if (second == check_in_closing_time) {
      # Closing check-in 30 minutes before departure so the probability now becomes 0
      a.rate <- 0
    }
  }
  
  # Creating a data frame for easier visualisation
  result_df <- data.frame(Time = 1:simulation_time, nf=nf,nb=nb,eq=eq)
  
  return(result_df)
}

# Getting the results with the given parameters:
result_df <- qsim(mf=5,mb=5,a.rate=0.1,trb=40,trf=40,tmb=30,tmf=30,maxb=20)

# Thank you for going through our project. 

#################
#   THANK YOU   #
#################