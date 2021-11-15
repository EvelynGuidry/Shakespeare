library(shiny)
library(tidyverse)
library(wordcloud)
library(ggplot2)
library(shinythemes)
library(RColorBrewer)
library(dplyr)
library(tidytext)
library(rsconnect)

books <- list("A Mid Summer Night's Dream" = "summer",
              "The Merchant of Venice" = "merchant",
              "Romeo and Juliet" = "romeo")

# task4: add in getFreq function for pre-processing

getFreq <- function(book, stopwords = TRUE) {
  # check that only one of three books is selected
  if (!(book %in% books))
    stop("Unknown book")
  
  text <-  tibble(text = readLines(sprintf("./data/%s.txt", book), encoding="UTF-8"))
  
  # could also pass column of text/character instead
  text <- text %>%
    unnest_tokens(word, text) %>%
    count(word, sort = TRUE) 
  
  if(stopwords){
    text <- text %>%
      anti_join(stop_words)
  }
  
  return(text)
}

# task6: add in shinythemes function

ui <- fluidPage(
  theme = shinytheme("lumen"),
  titlePanel("Shakespeare's Plays Word Frequencies"), # Application title
  
  # task1: add in the sidebarLayout with sidebarPanel and mainPanel
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "choices", 
                  label = "Choose a book:", 
                  choices = books),
      checkboxInput(inputId = "stopwords", 
                    label = "Stop words",
                    value = TRUE),
      actionButton(inputId = "rerun",
                   label = "Rerun"),
      hr(),
      h3("Word Cloud Settings"),
      sliderInput(inputId = "maxwords",
                  label = "Max # of words:",
                  min = 10, max = 200, value = 100, step = 10),
      sliderInput(inputId = "bigwords",
                  label = "Size of largest words:",
                  min = 1, max = 8, value = 4),
      sliderInput(inputId = "smallwords",
                  label = "Size of smallest words:",
                  min = 0.1, max = 4, value = 0.5),
      hr(),
      h3("Word Count Settings"),
      sliderInput(inputId = "minwords",
                  label = "Minumum words for Counts Chart:",
                  min = 10, max = 100, value = 25),
      sliderInput(inputId = "fontsize",
                  label = "Word size for Counts Chart:",
                  min = 8, max = 30, value = 14)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(title = "Word Cloud",
                 plotOutput("cloud", height = "600px" )
        ),
        tabPanel(title = "Word Counts",
                 plotOutput("freq", height = "600px" )
        )
      )
    )
  # task2: add in the inputs in the sidebarPanel
  
  # task1: within the mainPanel, create two tabs (Word Cloud and Frequency)
  
  # task3: add in the outputs in the sidebarPanel
  
  # task6: and modify your figure heights
)
)
server <- function(input, output) {
  
  # task5: add in reactivity for getFreq function based on inputs
  
  freq <- eventReactive(input$rerun, { 
    withProgress({ 
      setProgress(message = "Processing corpus...")
      getFreq(input$choices, input$stopwords) # ... = replace with the two inputs from Task 2
    })
    
  })
  
  output$cloud <- renderPlot({
    v <- freq()
    pal <- brewer.pal(8,"Dark2")
    
    v %>% 
      with(
        wordcloud(
          word, 
          n, 
          scale = c(input$bigwords, input$smallwords),
          random.order = FALSE, 
          max.words = input$maxwords, 
          colors=pal))
  })
  
  output$freq <- renderPlot({
    v <- freq() 
    v %>%
      filter(n >input$minwords)%>%
      ggplot(aes(x = reorder(word, n), y=n)) +
      geom_col() +
      coord_flip() + 
      theme(text = element_text(size = input$fontsize),
            axis.title.x = element_blank(),
            axis.title.y =  element_blank())
  })
  
}

shinyApp(ui = ui, server = server)
