library(shiny)
library(shinythemes)
library(tidytext)
library(tibble)
library(broom)
library(dbplyr)
library(forcats)
library(haven)
library(dplyr)
library(RColorBrewer)
library(wordcloud)
library(quanteda)
library(quanteda.textstats)
library(tidyverse)
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



books <- list("A Mid Summer Night's Dream" = "summer",
              "The Merchant of Venice" = "merchant",
              "Romeo and Juliet" = "romeo")

ui <- fluidPage( theme = shinytheme("cerulean"),
  titlePanel("Shakespeare's Plays Word Frequencies"),# Application title
  
 
  

  sidebarLayout(
    sidebarPanel
    (selectInput(inputId = 'book_name',label = 'book name',choices=books), 
      checkboxInput(inputId = 'stopwords',label = "To remove stopwords",value = ),actionButton("button", "To start"),
      hr(),h3("Word Cloud Settings"),
      sliderInput(inputId = "maxwords", label = "Max of words",min=10,max=200,step = 10,value = 100),
      sliderInput(inputId ="largest_words",label="Largest words",min=1,max=8,value=4),
      sliderInput(inputId = "smallest_words",label="smallest words",min=0.1,max=8,value=0.5),
      hr(),h3("Word Count Settings"),
      sliderInput(inputId = "min_word_counts",label="Minimum word count",min=10,max=100,value = 25),
      sliderInput(inputId = "fontsize",label="fontsize",min=8,max=30,value=14))
  , mainPanel(
    
    tabsetPanel(
      tabPanel("Word Cloud"
               , plotOutput("cloud",height="600px")),
      tabPanel("Word Counts",plotOutput("freq",height="600px"))
  ))
  
)
)
server <- function(input, output) {
  
  # task5: add in reactivity for getFreq function based on inputs
  action<- eventReactive(input$button,
    {withProgress({
    setProgress(message = "Processing corpus...")
    getFreq(input$book_name,input$stopwords) # ... = replace with the two inputs from Task 2
    })})

  output$cloud <- renderPlot({
  v <- action()
  pal <- brewer.pal(8,"Dark2")
  v %>% 
    with(
      wordcloud(
        word, 
        n, 
        scale = c(input$smallest_words,input$largest_words),
        random.order = FALSE, 
        max.words = input$maxwords, 
        colors=pal))
})
  output$freq <- renderPlot({
    v1 <-  action()
    v1 %>%
      filter(n > input$min_word_counts) %>%
      ggplot(aes(x = reorder(word, n), y = n))+
      geom_col()+theme(text = element_text(size = input$fontsize),axis.title=element_blank())+coord_flip()
     
  })



}
shinyApp(ui = ui, server = server)
