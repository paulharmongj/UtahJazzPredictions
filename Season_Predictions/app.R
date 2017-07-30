#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny);library(grDevices)

# Define UI for application that predicts Utah's wins in 2017-18 season: 
ui <- fluidPage(theme = shinytheme("superhero"),
  
  # Application title
  titlePanel("Predicted 2017 Season Wins:"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      sliderInput("ingles",
                  "SF (Ingles/Hood):",
                  min = -10,
                  max = 25,
                  value = 0),
      
      sliderInput("gobert",
                  "Rudy:",
                  min = -10,
                  max = 25,
                  value = 0),
      
      sliderInput("faves",
                  "Favors:",
                  min = -10,
                  max = 25,
                  value = 0),
      
      sliderInput("rubio",
                  "Rubio Pts:",
                  min = -10,
                  max = 25,
                  value = 0),
      
      sliderInput("hoodsg",
                  "Hood/SG:",
                  min = -10,
                  max = 25,
                  value = 0)
    ),
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("Density Plot of Jazz Wins")
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  player <- readRDS("player.rds")
  sea <- readRDS("season.rds")
  
player <- player[c(1,2,5,10,6,8,4,11,12,13,14,15),]  
  
   player[1,"PTS"] <- player[1,"PTS"] + input$gobert
   player[2,"PTS"] <- player[1,"PTS"] + input$ingles
   player[3,"PTS"] <- player[1,"PTS"] + input$hoodsg
   player[4,"PTS"] <- player[1,"PTS"] + input$faves
   player[5,"PTS"] <- player[1,"PTS"] + input$rubio
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      wins <- simulate.season(player, num.season = 100)
      
      # draw the histogram with the specified number of bins
      plot(density(wins$wintotal), col = rgb(0,43,92, maxColorValue = 255), lwd = 2, main = "Expected Wins in 2018")
      abline(v = sort(wins$wintotal[which.max(table(wins$wintotal))],decreasing = TRUE))
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

