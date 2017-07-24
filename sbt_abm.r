# Load libraries
library(dplyr)   # for more efficient data handling
library(ggplot2) # for plotting
library(cowplot) # not required, but makes the plots nicer
library(viridis) # for neater colours when plotting

# Set global parameters
n <- 100	  					# number of individuals
var1 <- .25						# variance of frist distribution (accessible without trait)
var2 <- 0.5						# variance of second distribution (accessible with trait)
gamma <- 1/n 					# death rate
# s <- 0.51							# skew
# mu <- 0.01						# mutation rate
trait_freq <- 0.5			# initial frequency of trait
rounds <- 1000				# rounds of the game
games <- 20				  	# number of games
trait_t <- 1:games 	  # to record trait frequency


## Defining the simulation loop as a function; 
# S: skew, v2: payoff variance of the trait
game <- function(S,v2){ 
  # looping over k number of games
	for(k in 1:games){ 
	# Initiate vector to store payoffs, fitness, presence of trait
	payoffs <- rep(NA, n)
	fitness <- rep(NA, n)
	trait <- rep(F, n)
	trait[1:(n*trait_freq)] <- T
	
  	# looping over t number of rounds
  	for(t in 1:rounds){
  	# Forage
  		## Foragers w/o trait
  		n_t <- sum(trait)
  		  # Single foraging bout per round
    		payoffs[!trait] <- rnorm(n=n-n_t, mean=0.5, sd=var1) #/ (n-n_t)
  		  # Multiple foraging bouts per round
    		# payoffs[!trait] <- rnorm(n=(n-n_t)*10, mean=0.5, sd=var1) %>% matrix(., ncol=(n-n_t)) %>% colMeans() %>% hist()
  		## Foragers with the trait
    		# Sinle foraging bout per round
    		payoffs[ trait] <- rnorm(n=  n_t   , mean=0.5, sd=v2  ) #/ (  n_t)
    		# Multiple foraging bouts per round
    		# payoffs[ trait] <- rnorm(n=  n_t*10, mean=0.5, sd=v2) %>% matrix(., ncol=(n-n_t)) %>% colMeans()
  		# Set payoffs in [0,1]
  		payoffs[payoffs<0] <- 0
  		payoffs[payoffs>1] <- 1
  	
  		
  	# Choose individuals to die
  		dead <- sample(1:n, size=gamma*n, replace=F) #death rate
  		# Delete payoff of dead individual
  		payoffs[dead] <- 0
  		
  	# Calculate fitness
  		fitness <- payoffs/sum(payoffs)
  	
  	# Choose individuals to reproduce (exclude dead individuals)
  		# For skew define who is eligible
  		noreproduce <- -c(dead, which(payoffs<S))
  		# noreproduce <- -c(dead, which(payoffs < quantile(x=payoffs,probs=S)))
  		reproduce <- sample((1:n)[noreproduce], size=length(dead), replace=F, prob=fitness[noreproduce]) #death rate
  	
  	# Set trait for new individual
  		trait[dead] <- trait[reproduce]
  	}
  	trait_t[k] <- mean(trait)
  }
	return(c(mean(trait_t), S, v2))
}


# Set values
grid <- expand.grid(sk=seq(0.1,.9,0.1), va2=seq(0,.25,0.025))

# Run simulations
res <- lapply(1:nrow(grid), function(x) game(S=grid$sk[x], v2=grid$va2[x]))
# Collect data
res_d <- do.call(rbind, res) %>% as.data.frame()
names(res_d) <- c("trait", "skew", "variance2")
# Plot results
p <- ggplot(res_d, aes(skew, variance2)) + geom_tile(aes(fill = trait)) + scale_fill_viridis(); p
# save_plot(p, filename="~/Desktop/tst.png", base_width=5)

# ggplot(data.frame(x1=rnorm(n=10^5, mean=.5,sd=.25), x2=rnorm(n=10^5, mean=.5,sd=.01))) + geom_density(aes(x1), fill="blue",alpha=.5) + geom_density(aes(x2), fill="red", alpha=.5) + scale_x_continuous(limits=c(0,1))
