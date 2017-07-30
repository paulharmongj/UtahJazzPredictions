#read in the data
setwd("C:/Users/Paul/Documents/UtahJazzPredictions")
player <- read.csv("Player_Stats.csv", header = TRUE)
names(player)[2] <- c("Name")

sea <- read.csv("Per_Season.csv", header = TRUE)

#simulates Gordon's replacement with half the scoring production
#how do I handle replacing Hill with Rubio?

#simulate points per 


#function has to be fed a player matrix with variable PTS
simulate.season <- function (player, num.season){
  
  ppm <- player$PTS/36 #simulates the number of points per minute per player
  
  
  
  
  #think of a prior distribution: 
  ##Year to year variation could lead some players to be better or worse than the year before: 
  #prior distribution of N(0,2)
  
  prior <- rnorm(nrow(player),0,2)
  
  sigma.sq <- var(ppm) #variance of the likelihood
  
  
  wins <- c()
  total.wins <- c()
  for(szns in 1:num.season){ 
    for (games in 1:82){
      
      #sample the posterior distributions
      num.sims <- 1000
      pred.points <- matrix(0,ncol = nrow(player), nrow = num.sims)
      
      #par(mfrow = c(3,5))
      #samples distributions for each player
      for (j in 1:nrow(player)){
        pred.points[,j] <- rnorm(num.sims,mean = (2*ppm[j]/(sigma.sq/15 + 2)),sd = (.5 + nrow(player)/sigma.sq)^-.5)  
        #plot(pred.points[,j], main = player$Name[j], type = "l");abline(h = mean(pred.points[,j]), col = "tomato")
      }
      
      #no issues with convergence or anything like that thankfully
      mean.pred <- apply(pred.points,2,mean) #posterior points per minute
      
      #distribute per number of minutes
      num.mins <- rep(0,nrow(player)) #simulates the number of minutes with little variation 
      for (j in 1:nrow(player)){
        num.mins[j] <- rnorm(1, sea$MP[j],2)
      }
      
      
      
      
      predicted.ppg <- num.mins * mean.pred
      
      #sample index from uniform between 4:7
      reserves <- sample(4:7, 1) #samples the number of reserves that play in a game
      index <- sample(6:15,reserves,replace = FALSE)
      scored <- sum(predicted.ppg[1:5],predicted.ppg[index]) 
      #take the sampled distribution for each player and multiply by the expected minutes 
      
      ##OK but there's a problem! Only a handful of players actually make it into each game. The bottom few may not play
      #more than a couple games...
      wins[games] <- ifelse(scored > 96.8, 1,0)
      
    }
    total.wins[szns] <- sum(wins[])
  }
  
  return(list(wintotal = total.wins, numseasons = num.season, goJazz = c("GO JAZZ GO! #takenote")))
}



season.1 <- simulate.season(player, num.season = 20)







