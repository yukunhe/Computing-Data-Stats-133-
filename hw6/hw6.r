# Homework 6
# Stat 133, Lec 2, Spring 2015
# Due : Friday March 20th by 5 pm

# Review the slides on simulations for this assignment.

# Consider the following model on use of a new drug:
# We have a population of doctors, population size : <n.doctors>
# Every doctor has either adopted the use of a new drug, or not (0/1 status)
# Now write a function that runs a simulation for a period of :
# <n.days> where
# - every day exactly two _random_ doctors meet
# - if one has adopted but the other one has not then the
#   holdout adopts the use of the drug with probability p
# Return a matrix that shows for each day which doctors have adopted
# the use of the drug.

# Input varibles are
# <n.days> : the number of days the simulation should be run for
# <n.doctors> : total number of doctors 
# <initial.doctors> : a 0/1 vector of length <n.doctors>, 1 for adopters
# <p> : the probability that the non-adopter adopts the drug.

# Ouput variable
# <has_adopted> : matrix with <n.doctors> rows and <n.days> columns
#                 i.e. one row for each doctor
#                 the entries are 0 for days where the doctor is a
#                 non-adopter, else 1 (so once a row turns to 1 it stays as 1).

sim.doctors <- function(initial.doctors, n.doctors, n.days, p){
  has_adopted <- matrix(initial.doctors, ncol=1)
  for (i in 2:n.days) {
    two_random_doctors = sample(n.doctors, size=2, replace = T)
    if (initial.doctors[two_random_doctors[1]] != initial.doctors[two_random_doctors[2]]){
      if (initial.doctors[two_random_doctors[1]]==0) {
        initial.doctors[two_random_doctors[1]]=sample(c(0,1), 1, prob=c(1-p,p))
       }
      if (initial.doctors[two_random_doctors[2]]==0){
        initial.doctors[two_random_doctors[2]]=sample(c(0,1), 1, prob=c(1-p,p))
      }
      }
    has_adopted = cbind(has_adopted,initial.doctors)
    }
  return (has_adopted)
  }

  # Set up the output variable, define it as a matrix then use initial.doctors
  # to set the first column (day)

  # Run a simulation for <n.days> (use a for loop).  In the loop:
  # 1) pick two random doctors
  # 2) check if one has adopted the other hasn't
  # 3) convert the non-adopter with probability p

  # return the output



# When you test your function you have to generate <initial.doctors> and
# pick values for the other input parameters.

set.seed(42)
# Generate a value for <initial.doctors> that has 10% 1s and 90% 0s.
# Run your function for at least 5 different values of <p> and plot
# on x-axis: days,
# on y-axis : the number of doctors that have already adopted the drug, on that day
# Put all 5 lines in one figure (e.g. use first plot() then lines() for the subsequent lines)

initial.doctors = rep(c(0,1),c(90,10))

a = sim.doctors(initial.doctors, 100, 100,1/2)
plot(1:100,colSums(a),type="l",xlab="days",ylab="number of doctors adopted the drug")

b = sim.doctors(initial.doctors, 100, 100,1/4)
plot(1:100,colSums(b),type="l",col="blue",xlab="days",ylab="number of doctors adopted the drug")

c = sim.doctors(initial.doctors, 100, 100,1/5)
plot(1:100,colSums(c),type="l",col="green",xlab="days",ylab="number of doctors adopted the drug")

d = sim.doctors(initial.doctors, 100, 100,1/8)
plot(1:100,colSums(d),type="l",col="yellow",xlab="days",ylab="number of doctors adopted the drug")

e = sim.doctors(initial.doctors, 100, 100,1/9)
plot(1:100,colSums(e),type="l",col="red",xlab="days",ylab="number of doctors adopted the drug")

plot(1:100,colSums(a),type="l",xlab="days",ylab="number of doctors adopted the drug")+
  lines(1:100,colSums(b),col="blue")+
  lines(1:100,colSums(c),col="green")+
  lines(1:100,colSums(d), col="yellow")+
  lines(1:100,colSums(e), col="red")
