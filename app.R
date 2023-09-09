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
      ),
      actionButton("generate", "Generate")
    )
  #)
  ,
  mainPanel(
    uiOutput("test")
  )
)

server <- function(input, output, session) {
  # Init data
  df <- read.csv('pokemon.csv')
  pkmn_list <- read.csv(paste0('gen_data/', 'GenII.csv'))
  pkmn_list <- pkmn_list$Pokemon
  # Create placeholder for path
  pkmn_path <- ""
  
  # Upon selecting a generation, get new list of valid Pokemon
  observeEvent(input$generation, {
    pkmn_list <- read.csv(paste0('gen_data/', input$generation, '.csv'))
    pkmn_list <- pkmn_list$Pokemon
    updateSelectInput(session, "start_pkmn",
                      choices = pkmn_list)
    updateSelectInput(session, "finish_pkmn",
                      choices = pkmn_list)
  })

  observeEvent(input$generate, {
    if (is.null(input$start_pkmn) | is.null(input$finish_pkmn)) {
        showModal(modalDialog(
            title = "Error",
            "You must select two Pokemon to generate a path",
            easyClose = TRUE,
            footer = NULL
        ))
    } else {
        # Filter to selected generation
        df_generation <- filter_pkmn_data(df, input$generation)
        
        # Get HTML for pkmn paths
        result_html <- return_steps_as_text(df_generation, input$start_pkmn,
                                            input$finish_pkmn)
        output$test <- renderUI({
          HTML(result_html)
        })

    }
  })
}

shinyApp(ui, server)