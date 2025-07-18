
library(simmer)
library(magrittr)

set.seed(1234)

lambda <- 7
mu <- 10
rho <- lambda/mu 

log_ <- function(traj, message) {
  traj %>% timeout(function() {
    msg <- if (is.function(message)) message() else message
    cat(msg, "\n")
    0
  })
}

customer <-
  trajectory("Customer's path") %>%
  log_("Here I am") %>%
  timeout(9) %>%
  log_("I must leave")
  
  cloud_kitchen <-
  simmer("cloud_kitchen") %>%
  add_generator("Customer", customer, at(6))

cloud_kitchen %>% run(until = 100)

cloud_kitchen %>% get_mon_arrivals()
library(simmer)

#A customer arriving at random

set.seed(10212)

customer <-
  trajectory("Customer's path") %>%
  log_("Here I am") %>%
  timeout(9) %>%
  log_("I must leave")

 cloud_kitchen<-
  simmer("cloud_kitchen") %>%
  add_generator("Customer", customer, at(rexp(1, 1/6)))

cloud_kitchen %>% run(until = 100)
cloud_kitchen%>% get_mon_arrivals()

#More customers

library(simmer)

# Function to specify a series of waiting times in a loop
loop <- function(...) {
  time_diffs <- c(...)
  i <- 0
  function() {
    if (i < length(time_diffs)) {
      i <<- i+1
    } else {
      i <<- 1
    }
    return(time_diffs[i])
  }
}

customer <-
  trajectory("Customer's path") %>%
  log_("Here I am") %>%
  timeout(loop(9, 10, 15)) %>%
  log_("I must leave")


cloud_kitchen <-
  simmer("cloud_kitchen") %>%
  add_generator("Customer", customer, at(6, 10, 12))

cloud_kitchen %>% run(until = 400)
cloud_kitchen %>% get_mon_arrivals()


library(simmer)

set.seed(1289)

customer <-  trajectory("Customer's path") %>%
  log_("Here I am") %>%
  timeout(function(){rnorm( n=1,mean = 10, sd = 4)}) %>%
  log_("I must leave")

cloud_kitchen <-
  simmer("cloud_kitchen") %>%
  add_generator("Customer", customer, function() {c(0, rexp(10, 1/10), -1)})

cloud_kitchen %>% run(until = 400)
cloud_kitchen %>% get_mon_arrivals()



library(simmer)

set.seed(1234)

cloud_kitchen <- simmer()

customer <-
  trajectory("Customer's path") %>%
  log_("Here I am") %>%
  set_attribute("start_time", function() {now(cloud_kitchen)}) %>%
  seize("counter") %>%
  log_(function() {paste("Waited: ", now(cloud_kitchen) - get_attribute(cloud_kitchen, "start_time"))}) %>%
  timeout(9) %>%
  release("counter") %>%
  log_(function() {paste("Finished: ", now(cloud_kitchen))})

cloud_kitchen <-
  simmer("cloud_kitchen") %>%
  add_resource("counter") %>%
  add_generator("Customer", customer, function() {c(0, rexp(20, 1/10), -1)})

cloud_kitchen %>% run(until = 400)


  cloud_kitchen %>% 
  get_mon_arrivals()%>%
transform(waiting_time = end_time - start_time - activity_time)


library(simmer)

set.seed(1269)

customer <-
  trajectory("Customer's path") %>%
  log_("Here I am") %>%
  set_attribute("start_time", function() {now(cloud_kitchen)}) %>%
  seize("counter") %>%
  log_(function() {paste("Waited: ", now(cloud_kitchen) - get_attribute(cloud_kitchen, "start_time"))}) %>%
  # timeout(rexp(1, 1/12)) would generate a single random time and use it for
  # every arrival, whereas the following line generates a random time for each
  # arrival
  timeout(function() {rexp(1, 1/12)}) %>%
  release("counter") %>%
  log_(function() {paste("Finished: ", now(cloud_kitchen))})

cloud_kitchen <-
  simmer("cloud_kitchen") %>%
  add_resource("counter") %>%
  add_generator("Customer", customer, function() {c(0, rexp(4, 1/10), -1)})

cloud_kitchen %>% run(until = 400)
cloud_kitchen %>%
  get_mon_arrivals() %>%
  transform(waiting_time = end_time - start_time - activity_time)

