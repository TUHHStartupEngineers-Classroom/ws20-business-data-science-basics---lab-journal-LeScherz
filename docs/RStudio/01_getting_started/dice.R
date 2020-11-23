# function to roll two dice
roll2 <- function(faces = 1:6) {
  dice <- sample(faces, size = 2, replace = TRUE)
  sum(dice)
}

# function to roll "number_of_dice" dice with "faces" many faces 
roll2 <- function(faces = 1:6, number_of_dice = 2) {
  dice <- sample(faces, size = number_of_dice, replace = TRUE)
  sum(dice)
}

# function to calculate economic order quantity
calc_EOQ <- function(D = 1000)  {
  K <- 5
  h <- 0.25
  Q <- sqrt(2*D*K/h)
  Q
}
# get help => "?fcn_name"
# get exaples => "example(fcn_name)"
# keyword search => "??keyword"

# function to roll "number_of_dice" dice with "faces" many faces 
roll3 <- function(faces = 1:6, number_of_dice = 1) {
  probability_vector <- c(1/10, 1/10, 1/10, 1/10, 1/10, 1/2)
  dice <- sample(faces, size = number_of_dice, replace = TRUE, prob = probability_vector)
  sum(dice)
}
results <- replicate( n = 100, expr = roll3(), simplify = TRUE)
hist(results)
