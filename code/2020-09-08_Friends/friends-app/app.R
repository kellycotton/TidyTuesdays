# Load packages ----
library(shiny)
library(friends)
library(tidyverse)
library(tidytext)
library(shinythemes)

# Data----
stop_words <- stop_words
data <- friends::friends
data$speaker <- str_squish(data$speaker)
data <- data %>% 
    filter(speaker != "Scene Directions")

# User interface ----
ui <- fluidPage(theme = shinytheme("flatly"),
                titlePanel("friends"),
                
                sidebarLayout(
                    sidebarPanel(
                        helpText("Select a character to see their top words."),
                        
                        selectInput("char", 
                                    label = "Select a character",
                                    choices = c("All",
                                                "Monica Geller",
                                                "Chandler Bing ",
                                                "Ross Geller",
                                                "Phoebe Buffay",
                                                "Joey Tribbiani",
                                                "Rachel Green"
                                    ), 
                                    selected = 1),
                        br(),
                        numericInput("limit", 
                                     label = "Enter a minimum frequency:", 
                                     value = 500,
                                     min = 1),
                        helpText("Note: if there are no words above your selected minimum, no plot will render."),
                        br(),
                        radioButtons("ngram",
                                     label = "Select one:",
                                     choices = c("Word" = 1,
                                                 "Bigram" = 2,
                                                 "Trigram" = 3),
                                     selected = 1),
                        submitButton("Pivot!", icon("refresh"))
                    ),
                    mainPanel(plotOutput("plot", height = "700px"))
                )
)

# Server logic
server <- function(input, output) {
    
    words <- reactive({
        if(input$char != "All") {
            ngrams <- data %>%
                filter(speaker == input$char) %>%
                unnest_tokens(word, text, token = "ngrams", n = as.integer(input$ngram)) %>%
                filter(!is.na(word)) 
        } else {
            ngrams <- data %>%
                unnest_tokens(word, text, token = "ngrams", n = as.integer(input$ngram)) %>%
                filter(!is.na(word)) 
        }
        
        if(as.integer(input$ngram) == 2) {
            ngrams <- ngrams %>%
                separate(word, c("word1", "word2"), sep = " ") %>%  
                filter(!word1 %in% stop_words$word) %>% 
                filter(!word2 %in% stop_words$word) %>% 
                unite(word, word1, word2, sep = " ")
        } else if(as.integer(input$ngram) == 3){
            ngrams <- ngrams %>%
                separate(word, c("word1", "word2", "word3"), sep = " ") %>%  
                filter(!word1 %in% stop_words$word) %>% 
                filter(!word2 %in% stop_words$word) %>% 
                filter(!word3 %in% stop_words$word) %>% 
                unite(word, word1, word2, word3, sep = " ") 
        } else {
            ngrams <- ngrams %>%
                anti_join(stop_words, by = "word")
        }
        
        ngrams %>% 
            count(word, sort = TRUE) %>%
            filter(n > input$limit)
    })
    output$plot <- renderPlot({
        ggplot(words(), aes(x=reorder(word,n),y=n)) +
            geom_bar(stat="identity", fill = "#1979a9", color = "white") + coord_flip() +
            theme_minimal() +
            theme(
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                panel.grid.major.y = element_blank(),
                panel.grid.minor = element_blank()
            )
    })
}

# Run the app
shinyApp(ui, server)
