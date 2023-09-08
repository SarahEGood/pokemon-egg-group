# Import libraries
library(shiny)
library(dplyr)
source("data_manip.R")

ui <- fluidPage(
  tags$script(src = "https://kit.fontawesome.com/7b969bf8cd.js"),
  titlePanel("Pokemon Breeding Path Generator"),
  #sidebarLayout(
    sidebarPanel(
        selectInput(
            "generation",
            "Select Generation/Game:",
            c("Gen II"="GenII", "Gen II"="GenIII", "Gen IV"="Gen IV",
            "Gen V"="GenV", "Gen VI"= "GenVI",
            "Sun & Moon (GenVII)" = "SunMoon",
            "Omega Ruby/Alpha Sapphire (GenVII)"="ORAS",
            "Sword & Shield (Gen VIII)"="SwSh",
            "Brilliant Diamond/Shining Pearl (Gen VIII)"="BDSP",
            "Scarlet/Violet (Gen IX)"="SV"),
            selected = "Gen II"
        ),
      selectInput(
        "start_pkmn",
        "Select starting Pokemon",
        choices = NULL
      ),
      selectInput(
        "finish_pkmn",
        "Select end Pokemon",
        choices = NULL
      )
    )
  #)
  ,
  mainPanel(
    paste("test")
  )
)

server <- function(input, output, session) {
  # Init data
  pkmn_list <- read.csv(paste0('gen_data/', 'GenII.csv'))
  pkmn_list <- paste(pkmn_list$Nat, pkmn_list$Pokemon, sep=": ")
  
  observeEvent(input$generation, {
    pkmn_list <- read.csv(paste0('gen_data/', input$generation, '.csv'))
    pkmn_list <- paste(pkmn_list$Nat, pkmn_list$Pokemon, sep=": ")
    updateSelectInput(session, "start_pkmn",
                      choices = pkmn_list)
    updateSelectInput(session, "finish_pkmn",
                      choices = pkmn_list)
  })
}

shinyApp(ui, server)