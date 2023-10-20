#Navya's part for Practical-2 
#Calculating Eq and probability of atleast 1 car missing teh ferry departure 
#based on 100 qsim runs.

# Getting the results with the given parameters:
result_df <- qsim(mf=5,mb=5,a.rate=0.1,trb=40,trf=40,tmb=30,tmf=30,maxb=20)

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
