#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)#library(grDevices)

#define the function used
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

###ACTUAL APP:

# Define UI for application that predicts Utah's wins in 2017-18 season: 
ui <- fluidPage(theme = shinytheme("superhero"),
  
  # Application title
  titlePanel("Predicted 2017 Season Wins:"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("gobert", label = h3("Gobert ppg"), 
                min = 0, max = 30, value = 14),
    sliderInput("ingles", label = h3("Ingles ppg"), 
              min = 0, max = 30, value = 12),
  sliderInput("hoodsg", label = h3("Hood ppg"), 
            min = 0, max = 30, value = 17),
sliderInput("faves", label = h3("Favors ppg"), 
          min =0, max = 30,value = 12),
sliderInput("rubio", label = h3("Rubio ppg"), 
          min = 0, max = 30, value = 15)
),
    # Show a plot of the generated distribution
    mainPanel(
      
      plotOutput("distPlot"),
      img(src = "www/jazzlogo.png", align = "right")
    )
  )
)

player <- readRDS("player.rds") 
sea <- readRDS("season.rds") 

# Define server logic required to estimate predicted wins
server <- function(input, output) {
  
  
player <- player[c(1,2,5,10,6,8,4,11,12,13,14,15),]  
  
   output$distPlot <- renderPlot({
     
     
     player[1,"PTS"] <- 10 #+ as.numeric(input$gobert)
     player[2,"PTS"] <- 10 #+ as.numeric(input$ingles)
     player[3,"PTS"] <- 10 #+ as.numeric(input$hoodsg)
     player[4,"PTS"] <- 10 #+ as.numeric(input$faves)
     player[5,"PTS"] <- 10 #+ as.numeric(input$rubio)
      # generate bins based on input$bins from ui.R
      wins <- simulate.season(player, num.season = 10)
      
     #### OUTPUT PLOT: #### 
      # draw the histogram with the specified number of bins
      plot(density(wins$wintotal), col = rgb(0,43,92, maxColorValue = 255), lwd = 2, main = "Expected Wins in 2018")
      
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

