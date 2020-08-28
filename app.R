library(shiny)
library(tidyverse)
library(dplyr)
library(knitr)
library(kableExtra)
library(readr)
library(DT)

historic_runners <- read_csv("sbelo_historic_runners.csv")
historic_pitchers <- read_csv("sbelo_historic_pitchers.csv")
historic_catchers <- read_csv("sbelo_historic_catchers.csv")

# Define UI for random distribution app ----
ui <- fluidPage(
    
    # App title ----
    titlePanel("sbELO"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            # Input: Slider for the number of observations to generate ----
            sliderInput("szn",
                        "Seasons:",
                        value = c(1978, 2019),
                        min = 1978,
                        max = 2019,
                        sep = ""),
            br(),
            radioButtons("agg",
                         "View:",
                         c("Individual Seasons" = "indiv",
                           "Combined Seasons" = "comb"))
            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            
            # Output: Tabset w/ plot, summary, and table ----
            tabsetPanel(type = "tabs",
                        tabPanel("Runners", dataTableOutput("run")),
                        tabPanel("Catchers", dataTableOutput("catch")),
                        tabPanel("Pitchers", dataTableOutput("pitch"))
            )
            
        )
    )
)

# Define server logic for random distribution app ----
server <- function(input, output) {
    # Generate an HTML table view of the data ----
    output$run <- renderDataTable({
        if(input$agg == "indiv"){
            historic_runners %>%
                filter(ELO > 0,
                       Season >= input$szn[1],
                       Season <= input$szn[2]) %>%
                arrange(desc(ELO)) %>%
                mutate(ELO = round(ELO)) %>%
                select(Name, Season, SB, CS, ELO)
        }else{
            historic_runners %>%
                filter(ELO > 0,
                       Season >= input$szn[1],
                       Season <= input$szn[2]) %>%
                group_by(id, Name) %>%
                summarize(Years = n(),
                          ELO = sum(ELO),
                          SB = sum(SB),
                          CS = sum(CS)) %>%
                as.data.frame() %>%
                mutate(EAA = round(ELO - 1000*Years),
                       ELO = round(ELO)) %>%
                arrange(desc(EAA)) %>%
                select(Name, SB, CS, ELO, EAA)
        }
    })
    output$pitch <- renderDataTable({
        if(input$agg == "indiv"){
            historic_pitchers %>%
                filter(ELO > 0,
                       Season >= input$szn[1],
                       Season <= input$szn[2]) %>%
                arrange(desc(ELO)) %>%
                mutate(ELO = round(ELO)) %>%
                select(Name, Season, SB, CS, ELO)
        }else{
            historic_pitchers %>%
                filter(ELO > 0,
                       Season >= input$szn[1],
                       Season <= input$szn[2]) %>%
                group_by(id, Name) %>%
                summarize(Years = n(),
                          ELO = sum(ELO),
                          SB = sum(SB),
                          CS = sum(CS)) %>%
                as.data.frame() %>%
                mutate(EAA = round(ELO - 1000*Years),
                       ELO = round(ELO)) %>%
                arrange(desc(EAA)) %>%
                select(Name, SB, CS, ELO, EAA)
        }
    })
    output$catch <- renderDataTable({
        if(input$agg == "indiv"){
            historic_catchers %>%
                filter(ELO > 0,
                       Season >= input$szn[1],
                       Season <= input$szn[2]) %>%
                arrange(desc(ELO)) %>%
                mutate(ELO = round(ELO)) %>%
                select(Name, Season, SB, CS, ELO)
        }else{
            historic_catchers %>%
                filter(ELO > 0,
                       Season >= input$szn[1],
                       Season <= input$szn[2]) %>%
                group_by(id, Name) %>%
                summarize(Years = n(),
                          ELO = sum(ELO),
                          SB = sum(SB),
                          CS = sum(CS)) %>%
                as.data.frame() %>%
                mutate(EAA = round(ELO - 1000*Years),
                       ELO = round(ELO)) %>%
                arrange(desc(EAA)) %>%
                select(Name, SB, CS, ELO, EAA)
        }
    })
}

# Create Shiny app ----
shinyApp(ui, server)
