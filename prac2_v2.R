#--------------------#
#    Practical 2     #
#--------------------#

# This practical involves simulating the flow of cars through French 
# and British passport control at a French ferry terminal in the context 
# of managing queues. The simulation is designed to model a 2-hour period 
# before a ferry departure.

# Define a function, qsim, with various parameters to simulate the queue system
qsim <- function(mf,mb,a_rate,trb,trf,tmb,tmf,maxb) {
  # Initialize variables for simulation of 2 hours
  # To track the average French and British queue length at each time step
  # And to track the average expected waiting time for a car at the 
  # start of the french queue for each time step
  simulation_time <- 700
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
  car_arriving <- c(rbinom(simulation_time, size = 1, prob = a_rate))
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
  # processing time
  depart_time_from_f <- arrival_time + process_time_f
  depart_time_from_b <- depart_time_from_f + process_time_b
  # Calculating the total time spent by a car in the system
  total_time_by_car <- depart_time_from_b - arrival_time
  wait_time <- total_time_by_car - process_time_f - process_time_b
  
  # Iterating through each second in the time simulation period
  for (second in 1:simulation_time) {
    for (i in 1:n_cars) {
      # If the car arrives at this second, then it goes into the french queue
      if (second == arrival_time[i]) {
        # The car selects the shortest queue by checking the minimum queue length
        french_queues[which.min(french_queues)] <- french_queues[which.min(french_queues)] + 1
        total_french_queue <- total_french_queue + 1
      }
    }
    
    for (i in 1:mf) {
      if (french_queues[i] > 0) {
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
          }
        }
      }
    }
    
    # When the car is processed from a British queue, it can leave the 
    # queue for its ferry ahead
    for (i in 1:mb) {
      if (british_queues[i] > 0) {
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
    eq[second] <- mean(sapply(1:mf, function(i) sum(french_queues[1:french_queues[i]])))
  }
  
  # Return results as a list
  result <- list(nf=nf,nb=nb,eq=eq)
  return(result)
}

# Running the simulation
results <- qsim(mf=5,mb=5,a_rate=0.1,trb=40,trf=40,tmb=30,tmf=30,maxb=20)

result_df <- data.frame(results)

