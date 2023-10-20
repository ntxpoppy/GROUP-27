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

# Define a function, qsim, with various parameters to simulate the queue system
qsim <- function(mf,mb,a.rate,trb,trf,tmb,tmf,maxb) {
  # Initialize variables for simulation of 2 hours
  # To track the average French and British queue length at each time step
  # And to track the average expected waiting time for a car at the 
  # start of the french queue for each time step
  simulation_time <- 7200
  check_in_closing_time <- 5400 #Check-in closes 30 min before departure
  nf <- numeric(simulation_time)
  nb <- numeric(simulation_time)
  eq <- numeric(simulation_time)
  total_french_queue <- 0
  total_british_queue <- 0
  french_queues <- rep(0, mf)
  british_queues <- rep(0, mb)
  total_british_cars <- 0
  time <- 0
  
  # Generating a binary vector to represent car arrivals based on the given 
  # arrival rate. This is done with the help of random binomial distribution 
  # function, which will only take 0 and 1 values.
  car_arriving <- c(rbinom(check_in_closing_time, size = 1, prob = a.rate))
  # Identifying the time when a car arrives, i.e. when the above vector is 1
  arrival_time <- which(car_arriving == 1)
  # The total number of cars arriving in the time-window.
  n_cars <- length(arrival_time)
  # The processing time at a British passport station is given by a random time
  # in the uniform distribution between tmb and tmb+trb
  process_time_b <- round(runif(n_cars, min = tmb, max = tmb + trb))
  # Similarly for a French station, it is uniformly distributed between
  # tmf and tmf+trf
  # we are taking a round number because we are simulating over each second
  process_time_f <- round(runif(n_cars, min = tmf, max = tmf + trf))
  # Hence, the depart-time from French station becomes its arrival time + 
  # processing time + waiting time, which we will add later
  # Adding a wait time vector to track the waiting time
  wait_time_f <- rep(0,n_cars)
  wait_time_b <- rep(0,n_cars)
  #depart_time_from_f <- arrival_time + process_time_f
  #depart_time_from_b <- depart_time_from_f + process_time_b
  depart_time_from_f <- rep(0,n_cars)
  depart_time_from_b <- rep(0,n_cars)
  # Calculating the total time spent by a car in the system
  #total_time_by_car <- depart_time_from_b - arrival_time
  
  # Iterating through each second in the time simulation period
  for (second in 1:simulation_time) {
    for (i in 1:n_cars) {
      # If the car arrives at this second, then it goes into the french queue
      if (second <= check_in_closing_time) {
        # We have to check if it arrived before the check-in closing time
        if (second == arrival_time[i]) {
          # The car selects the shortest queue by checking the minimum queue length
          french_queues[which.min(french_queues)] <- french_queues[which.min(french_queues)] + 1
          total_french_queue <- total_french_queue + 1 
        }
      }
    }
    
    for (i in 1:mf) {
      if (french_queues[i] > 0) {
        # the car has to wait for other cars in front of the queue to be processed
        depart_time_from_f[i] <- (french_queues[i]-1)*((2*tmf+trf)/2)
        if (second == depart_time_from_f[i]) {
          # Checking if a car can leave the french queue to join the British 
          # queue after it is done processing
          # There is a maximum number of cars, a British queue can hold,
          # which is given by maxb
          if (total_british_cars < maxb) {
            # If the condition is satisfied, car leaves the French queue
            french_queues[i] <- french_queues[i] - 1
            total_french_queue <- total_french_queue - 1
            # and joins the british queue, again by selecting the shortest one
            british_queues[which.min(british_queues)] <- british_queues[which.min(british_queues)] + 1
            total_british_cars <- total_british_cars + 1
            total_british_queue <- total_british_queue + 1
            } else {
            # The depart time from f increases by 1 for every second it has to wait
            depart_time_from_f[i] <- depart_time_from_f[i] + 1
          }
        }
      }
    }
    
    # When the car is processed from a British queue, it can leave the 
    # queue for its ferry ahead
    for (i in 1:mb) {
      if (british_queues[i] > 0) {
        depart_time_from_b[i] <- (british_queues[i]-1)*((2*tmb+trb)/2)
        if (second == depart_time_from_b[i]) {
          british_queues[i] <- british_queues[i] - 1
          total_british_cars <- total_british_cars - 1
          total_british_queue <- total_british_queue - 1
        }
      }
    }
    
    # Here we store the average french queue length at each second
    nf[second] <- total_french_queue
    # And the average british queue length at each second
    nb[second] <- total_british_queue
    # We are also storing the average expected waiting time for a car at the 
    # start of the french queue for each second
    #eq[second] <- mean(sapply(1:mf, function(i) sum(french_queues[1:french_queues[i]])))
    eq[second] <- (nf[second]*0.5*(tmf+trf)) +(nb[second]*0.5*(tmb +trb))
  }
  
  # Return results as a list
  result <- list(nf=nf,nb=nb,eq=eq)
  return(result)
}

# Running the simulation
results <- qsim(mf=5,mb=5,a.rate=0.1,trb=40,trf=40,tmb=30,tmf=30,maxb=20)

# 4 panel 2 row, 2 column plot
results_tmb_40 <- qsim(mf=5,mb=5,a.rate=0.1,trb=40,trf=40,tmb=40,tmf=30,maxb = 20)
par(mfrow = c(2, 2))

# Plot 1: French and British queue lengths change over time - Default Simulation
#Using plot() we first plot the change of French queue first
plot(results$nf, type = 'l', xlab = 'Time (s)', ylab = 'Queue Length', main = 'Default Simulation - French and British Queue Length', col = 'purple')
#Now plotting change in British queue length
lines(results$nb, col = 'brown')
legend('topright', legend = c('French Queue', 'British Queue'), col = c('purple', 'brown'), lty = 1)

# Plot 2: Expected queuing time changes over time - Default Simulation
#We use eq vector to plot this
plot(results$eq, type = 'l', xlab = 'Time (s)', ylab = 'Expected Queuing Time', main = 'Default Simulation - Expected Queuing Time', col = 'green')

# Plot 3: French and British queue lengths for the tmb set to 40 seconds simulation
#First plotting change in French Queue length with new tmb=40
plot(results_tmb_40$nf, type = 'l', xlab = 'Time (s)', ylab = 'Queue Length', main = 'tmb = 40s Simulation - French and British Queue Length', col = 'purple')
#Now plotting change in British queue length with new tmb=40
lines(results_tmb_40$nb, col = 'brown')
legend('topright', legend = c('French Queue', 'British Queue'), col = c('purple', 'brown'), lty = 1)

# Plot 4: Expected queuing time changes over time - tmb set to 40 seconds 
plot(results_tmb_40$eq, type = 'l', xlab = 'Time (s)', ylab = 'Expected Queuing Time', main = 'tmb = 40s Simulation - Expected Queuing Time', col = 'green')


# redefining function with 100 runs to calculate the probability of at least one car missing the ferry departure:
probability_sim <- function(mf, mb, a.rate, trb, trf, tmb, tmf, maxb, qsim_running = 100) {
  
  #initializing the variable of number of cars that have missed the ferry departure as zero.
  ferry_missed <- 0
  
  #starting the simulation runs(100)
  for (i in 1:qsim_running) {
    #storing the initial results of the simulation
    result_sim <- qsim(mf, mb, a.rate, trb, trf, tmb, tmf, maxb)
    
    # checking if a car has missed the ferry in each run of the for-loop w.r.t to the simulation results.
    #using the max. function to satisfy the atleast condition 
    #while checking the queues with maximum no. of cars (considering the last car of those in the british queues).
    if (max(result_sim$nb) > 0) {
      #missed(last) car will be counted if the above condition is met.
      ferry_missed <- ferry_missed + 1
    }
  }
  #no. of cars that misses ferry out of 100 simulation runnings.
  probability_sim <- (ferry_missed / qsim_running)
  
  return(probability_sim)
}

# incorporating the defined function into variable 'missed_ferry_probability'.
missed_ferry_probability <- probability_sim(mf = 5, mb = 5, a.rate = 0.1, trb = 40, trf = 40, tmb = 30, tmf = 30, maxb = 20)

#displaying the calculated probability
cat("Probability of at least 1 car missing the ferry departure (for 100 runs consideration):", missed_ferry_probability, "\n")

# Thank you for going through our project. 

#################
#   THANK YOU   #
#################

