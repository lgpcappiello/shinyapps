## Coin flip simulation to demonstrate consistency

# Set total number of samples:
n <- 1000
# Set population proportion (of heads):
p <- 0.5

# Flip coin n times
coinflips <- rbinom(n, 1, p)

# This function calculates the proportion (of heads) in a sample
prop <- function(i, flips){
  sum(flips[1:i])/i
}

# Calculate the proportion after each flip
p.hat <- sapply(1:n, prop, flips=coinflips)

# Plot the sample proportion at each trial
# The red line indicates the true sample propotion
plot(p.hat, type='l', ylim=c(0,1),
     xlab="Trial", ylab="Sample Proportion"); abline(h=p, col='red')

# Do the whole thing a bunch of times and add to plot:
for(i in 1:25){
  # Flip coin n times
  coinflips <- rbinom(n, 1, p)
  
  # This function calculates the proportion (of heads) in a sample
  prop <- function(i, flips){
    sum(flips[1:i])/i
  }
  
  # Calculate the proportion after each flip
  p.hat <- sapply(1:n, prop, flips=coinflips)
  
  # Plot the sample proportion at each trial
  # The red line indicates the true sample propotion
  points(p.hat, type='l')
}