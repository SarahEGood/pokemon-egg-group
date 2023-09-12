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
            c("Gen II"="GenII", "Gen III"="GenIII", "Gen IV"="GenIV",
            "Gen V"="GenV", "Gen VI"= "GenVI",
            "Sun & Moon (GenVII)" = "SunMoon",
            "Omega Ruby/Alpha Sapphire (GenVII)"="ORAS",
            "Sword & Shield (Gen VIII)"="SwSh",
            "Brilliant Diamond/Shining Pearl (Gen VIII)"="BDSP",
            "Scarlet/Violet (Gen IX)"="SV"),
            selected="GenII"
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
      conditionalPanel(
        condition = "!(['GenII','GenIII','GenIV'].includes(input.generation))",
        selectInput(
                "hidden_ability",
                "Select hidden ability (if applicable)",
                choices = c(NULL),
                selected = NULL
          )
      ),
      actionButton("generate", "Generate")
    )
  #)
  ,
  mainPanel(
    fluidRow(
      uiOutput("test")
    )
    
  )
)

server <- function(input, output, session) {
  # Init data
  df <- read.csv('pokemon.csv')
  pkmn_frame <- read.csv(paste0('gen_data/', 'GenII.csv'))
  ha_list <- c(NULL)
  pkmn_list <- pkmn_frame$Pokemon
  # Create placeholder for path
  pkmn_path <- ""
  
  # Upon selecting a generation, get new list of valid Pokemon
  observeEvent(input$generation, {
    pkmn_frame <- read.csv(paste0('gen_data/', input$generation, '.csv'))
    ha_list <- sort(unique(pkmn_frame$HiddenAbility))
    ha_list <- c(NULL, ha_list)
    pkmn_list <- pkmn_frame$Pokemon
    updateSelectInput(session, "start_pkmn",
                      choices = pkmn_list)
    updateSelectInput(session, "finish_pkmn",
                      choices = pkmn_list)
    updateSelectInput(session, "hidden_ability",
                      choices = ha_list)
  })

  # When selecting hidden ability, filter to only Pkmn with that ability
  observeEvent(input$hidden_ability, {
    pkmn_frame <- read.csv(paste0('gen_data/', input$generation, '.csv'))
    ha_list <- sort(unique(pkmn_frame$HiddenAbility))
    ha_list <- c(NULL, ha_list)
    if (!is.null(input$hidden_ability) & input$hidden_ability != "") {
      print(input$hidden_ability)
      print(isTruthy(input$hidden_ability))
      pkmn_list <- pkmn_frame[pkmn_frame$HiddenAbility == input$hidden_ability, 'Pokemon']
    } else {
      pkmn_list <- pkmn_frame$Pokemon
    }
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
        df_generation <- filter_pkmn_data(df, input$generation,
                                            hidden_ability = input$hidden_ability)
        
        # Get HTML for pkmn paths
        result_html <- return_steps_as_text(df_generation, input$start_pkmn,
                                            input$finish_pkmn,
                                            hidden_ability = input$hidden_ability)
        output$test <- renderUI({
          HTML(result_html)
        })

    }
  })
}

shinyApp(ui, server)