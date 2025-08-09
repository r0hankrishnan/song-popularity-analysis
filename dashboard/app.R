#Load libraries
library(shinydashboard)
library(tidyverse)
library(ggthemes)
library(randomForest)
library(DT)
library(plotly)
library(scales)

#Import data, remove id, create choice vectors for non-numeric variables
songs <- read.csv("~/Documents/GitHub/song-popularity/data/clean_data.csv")
songs <- songs %>% select(-id)

keyChoice <- sort(as.vector(unique(songs$key)))
timeChoice <- sort(as.vector(unique(songs$time_signature)))
genreChoice <- as.vector(unique(songs$track_genre))


ui <- dashboardPage(
  skin = "blue",
  #Header
  dashboardHeader(title = "Song Popularity Prediction", titleWidth = 250),
  
  #Sidebar
  dashboardSidebar(
    #Add sidebar scroll
    tags$head(tags$style(".wrapper {overflow: visible !important;}")),
    
    sidebarMenu(
      menuItem("Prediction", tabName = "prediction")
    ),
    
    #Song name input
    textInput(inputId = "song_name", label = "Song Name",
              width = "100%"),
    
    #Input variables
    numericInput(inputId = "duration_ms", label = "Song Duration (ms)",
                min = 0, max = 1000000, value = 0),
    
    checkboxInput(inputId = "explicit", label = "Is the Song Explicit?",
                  value = FALSE),
    
    numericInput(inputId = "danceability", 
                 label = "Song Danceability Score (0 to 1)",
                min = 0, max = 1.00, value = 0),
    
    numericInput(inputId = "energy", label = "Song Energy Score (0 to 1)",
                 min = 0, max = 1.00, value = 0),
    
    selectInput(inputId = "key", label = "What is the Song's Key?",
                choices = keyChoice, selected = NULL, width = "100%"),
    
    numericInput(inputId = "loudness", label = "Song Loudness (0 to -100)", 
                 min = -100, max = 0, value = 0),
    
    selectInput(inputId = "mode", label = "Song Mode", 
                choices = c(0,1), selected = 0, width = "100%"),
    
    numericInput(inputId = "speechiness", label = "Song Speechiness (0 to 1)",
                 min = 0, max = 1, value = 0),
    
    numericInput(inputId = "acousticness", label = "Song Acousticness (0 to 1)",
                 min = 0, max = 1, value = 0),
    
    numericInput(inputId = "instrumentalness", 
                 label = "Song Instrumentalness (0 to 1)",
                 min = 0, max = 1, value = 0),
    
    numericInput(inputId = "liveness", 
                 label = "Song Liveness (0 to 1)",
                 min = 0, max = 1, value = 0),
    
    numericInput(inputId = "valence", 
                 label = "Song Valence (0 to 1)",
                 min = 0, max = 1, value = 0),
    
    numericInput(inputId = "tempo", 
                 label = "Song Tempo",
                 min = 0, max = 300, value = 0),
    
    selectInput(inputId = "time_signature",
                label = "Song Time Signature",
                choices = timeChoice, selected = NULL, width = "100%"),
    
    selectInput(inputId = "track_genre",
                label = "What is the Song's Genre?",
                choice = genreChoice, selected = NULL, width = "100%"),
    
    actionButton(inputId = "action",
                 label = "Run Popularity Prediction")
  ),
  
  #Body
  dashboardBody(
    
    tags$head(tags$style(HTML('
      .content-wrapper{
        background-color: #fff;
      }
      
      #row1{
        padding-left: 5px;
        padding-right: 5px;
      }
        
      .small-box{
        border-radius: 10px;
      }
    '
    ))),
    #Prediction tab
    tabItems(
      tabItem(tabName = "prediction",
              fluidRow(id = "row1",
                valueBoxOutput("text_result", width = "100%")
                ),
              fluidRow(id = "row1",
                box(plotlyOutput("plot"),
                    width = 12)
              ),
              fluidRow(id = "row1",
                box(tags$div(id = 'placeholder') ,
                  dataTableOutput("table"),
                  width = 12)
              )
        )
      )
    )
)

server <- function(input, output) {
  
  #Create reactive object for name
  name <- eventReactive(input$action, {
    name <- str_to_title(input$song_name)
  })
  
  #Create reactive test data from inputs after button is clicked
  testData <- eventReactive(input$action, {
    newData <- data.frame(
      duration_ms = input$duration_ms,
      explicit = input$explicit,
      danceability = input$danceability,
      energy = input$energy,
      key = as.numeric(input$key),
      loudness = input$loudness,
      mode = as.numeric(input$mode),
      speechiness = input$speechiness,
      acousticness = input$acousticness,
      instrumentalness = input$instrumentalness,
      liveness = input$liveness,
      valence = input$valence,
      tempo = input$tempo,
      time_signature = as.numeric(input$time_signature),
      track_genre = input$track_genre
    )
  })
  
  #Create popularity prediction from RF model & test data
  popularity <- eventReactive(input$action, {
    rf_model <- readRDS("randomforest.RDS")
    
    
    pred <- predict(rf_model, testData(), type = "response")
    
    predNum <- round(pred,2)
  })
  
  #Create value box output for predicted popularity
  output$text_result <- renderValueBox({
    
    valueBox(value = popularity(),
             subtitle = paste(name(), "Predicted Popularity"),
             color = "blue",
             width = "100%")
  })
  
  #Create test data set with popularity and other features
  full_test_data <- eventReactive(input$action,{
    pData <- cbind(popularity(), testData())
    
    pData %>% rename(popularity = "popularity()")
  })
  
  #Create plot
  output$plot <- renderPlotly({
    
    #Create combined data set for graph (include indicator for test data)
    graphData <- rbind(songs %>% mutate(isTest = FALSE, name = NA),
                       full_test_data() %>% mutate(isTest = TRUE, name = name())
                       )
    
    #Create label variable in graphData
    graphData$label <- paste("Song Name: ", ifelse(is.na(graphData$name), "NA", graphData$name),
                             "<br>Popularity: ", round(graphData$popularity,2), "<br>Duration(ms): ", graphData$duration_ms,
                             "<br>Explicit: ", ifelse(graphData$explicit, "Yes", "No"), "<br>Genre: ", str_to_title(graphData$track_genre))
    #Create ggplot object
    p <- graphData %>%
      ggplot(aes(x = duration_ms, y = popularity, color = isTest)) + 
      geom_point(aes(text = label)) + 
      scale_x_continuous(labels = label_number()) + 
      labs(title = paste("Duration vs Popularity (" ,name(), " in blue)", sep = ""),
                         x = "Duration (ms)", y = "Popularity",
                         color = "New Data") +
      scale_color_manual(values = c("black", "blue")) + 
      ggthemes::theme_few() +
      theme(legend.position = "none") 
      
    
    #Create plotly object
    plot <- ggplotly(p, tooltip = "text")
  })
  
  #Create table
  observeEvent(input$action, {
    insertUI(
      selector = '#placeholder',
      ## wrap element in a div with id for ease of removal
      ui = tags$div(
        tags$h1(paste(name(),": Predicted Popularity & Inputted Characteristics", sep = "")), 
        id = "tableHeader"
      )
    )
  })
  
  output$table <- renderDataTable({
    full_test_data() %>%
      gather(key = "key", value = "value", popularity:track_genre) %>% 
      mutate(key = str_replace(key,"_", " ") %>% str_to_title()) %>%
      rename(Attribute = "key", Value = "value") %>%
      datatable(options = list(pageLength = 16))
  })
}

shinyApp(ui, server)
