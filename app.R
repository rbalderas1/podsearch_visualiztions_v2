#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(fresh)
library(tidyverse)
library(remotes)
library(wordcloud)
library(wordcloud2)
library(RColorBrewer)
library(tm)

podsearch_df <- read_csv("podsearch_df_complete_04_08_2023_v1.csv")

# want to create year column
podsearch_df <- podsearch_df %>% 
  mutate(year = substr(birthday, 12, 16)) %>% 
  mutate(year = as.numeric(year))

summary(podsearch_df)
year_list <- unique(podsearch_df$year)
year_list <- na.omit(year_list) 
year_list <- sort(year_list)

genre_list <- c("Horror", "True Crime", "Education")

zodiac_list <- unique(podsearch_df$zodiac)

# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$head(tags$style(HTML('* {
                            font-family: "Space Mono", monospace;
                            color: #291440;
                            background-color: #F2EDF9;
                            }
                            .shiny-input-container {
                            color: #291440;
                            }
                            .js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {
                            background: #A64EFF;
                            }
                            h1 {
                            background-color: #A64EFF;
                            color: #F2EDF9;
                            padding: 15px
                            }
                            h3 {
                            font-weight: bold;
                            text-align: justify;
                            }
                            h5 {
                            text-align: left;
                            font-size: 1.15em;
                            }
                            .btn, button {
                            display: block;
                            margin: 20px auto;
                            height: 50px;
                            width: 100px;
                            border-radius: 50%;
                            border: 2px solid #A64EFF;
                            }'))),
  titlePanel(h1("PodSearch Visuals")),
  fluidRow(
    column(2,
           # Added slider input alternative
           sliderInput("number_episodes_slider",
                       "Select range of episodses:",
                       min = 1, max = 160,
                       value = c(1, 160)),
           selectInput("explicit",
                       "Explicit:",
                       c("None",
                         c("explicit", "not explicit"))),
           selectInput("zodiac",
                       "Zodiac:",
                       c("None",
                         c("Aries", "Taurus", "Gemini", "Cancer", "Leo", "Virgo", "Libra", "Scorpio", "Sagittarius", "Capricorn", "Aquarius", "Pisces"))),
           selectInput("genre",
                       "Genre/Category:",
                       c("None",
                         c("art", "business", "christianity", "comedy", "education", "fiction", "health", "history", "kids", "leisure", "music", "news", "religion", "science", "society", "spirituality", "sports", "technology", "tv")))
           ),
    mainPanel(column(12, 
                     (tabsetPanel(type="tabs",
                                  tabPanel("Timeline",
                                           h4("Select Zodiac to Highlight in Timeline"),
                                             plotOutput(outputId = "time_plot", width = "100%"),
                                           plotOutput(outputId = "genre_time_plot", width = "100%")
                                  ),
                                  tabPanel("Episode Distribution",
                                           plotOutput(outputId = "ep_plot", width = "100%")
                                           ),
                                  tabPanel("Podcast Distribution",
                                           h3("Genre/Category Zodiac Distribution"),
                                           h4(textOutput(outputId = "genre_amount")),
                                           plotOutput(outputId = "genre_dist_plot", width = "100%"),
                                           h3("Total Zodiac Podcast Distrubtion"),
                                           plotOutput(outputId = "zodiac_plot", width = "100%")
                                           ),
                                  tabPanel("Word Cloud Zodiac",
                                           h3("Zodiac Word Cloud"),
                                           wordcloud2Output(outputId = "word_plot", width = "100%")
                                  ),
                                  tabPanel("Word Cloud Genre/Category",
                                           h3("Genre/Category Word Cloud"),
                                           wordcloud2Output(outputId = "word_plot_2", width = "100%")
                                  )
                                  
                     ))))))

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$time_plot <- renderPlot({
    
    filtered_df <- podsearch_df %>% 
      group_by(zodiac, year) %>% 
      summarise(counts = n())
    
    highlighted_line <- filtered_df %>% 
      filter(zodiac == input$zodiac)
    
    filtered_df %>% 
      ggplot(aes(x = year,
                 y =counts,
                 group = zodiac)) +
      geom_line(color = "grey", size = 1) +
      geom_point(data = highlighted_line,
                 color = "purple", size = 3) +
      geom_line(data = highlighted_line,
                color = "purple", size = 1) +
      theme(text = element_text(size = 15, family = "mono", face = "bold")) +
      labs(title = "How many Podcasts were released per year",
           x = "Podcast Release Year",
           y = "Number of Podcasts") +
      scale_x_discrete(limits = c(2007, 2009, 2011, 2013, 2015, 2017, 2019, 2021, 2023))
    
  })
  
  output$genre_time_plot <- renderPlot({
    
    validate(
      need(input$genre != "None", "Please select Genre/Category")
    )
    
    filtered_df <- podsearch_df %>% 
      filter(grepl(input$genre, categories)) %>% 
      group_by(year) %>% 
      summarise(counts = n())
    
    filtered_df %>% 
      ggplot(aes(x = year,
                 y =counts)) +
      geom_line(color = "purple", size = 1) +
      theme(text = element_text(size = 15, family = "mono", face = "bold")) +
      labs(title = "How many Podcasts were released per year",
           x = "Podcast Release Year",
           y = "Number of Podcasts") +
      scale_x_discrete(limits = c(2007, 2009, 2011, 2013, 2015, 2017, 2019, 2021, 2023))
    
  })
  
  output$genre_amount <- renderText({
    genre_df <- podsearch_df %>% 
      filter(grepl(input$genre, categories))
    
    paste("There are", nrow(genre_df), "Podcasts in the", toupper(input$genre)," Genre/Category.")
  })
  
  output$genre_dist_plot <- renderPlot({
    
    genre_df <- podsearch_df %>% 
      filter(grepl(input$genre, categories))
    
    genre_df <- genre_df %>% 
      group_by(zodiac) %>% 
      summarise(counts = n())
    
    genre_df %>% 
      ggplot(aes(y = counts,
                 x = reorder(zodiac, -counts),
                 fill = zodiac,
                 group = zodiac)) +
      geom_bar(stat = "identity") +
      theme(text = element_text(size = 15, family = "mono", face = "bold")) +
      labs(title = "How many podcasts belong to each Zodiac",
           x = "Zodiac",
           y = "Number of Podcasts")
    
  })
  
  output$zodiac_plot <- renderPlot({
    
    zodiac_df <- podsearch_df %>% 
      group_by(zodiac) %>% 
      summarise(counts = n())
    
    zodiac_df %>% 
      ggplot(aes(y = counts,
                 x = reorder(zodiac, -counts),
                 fill = zodiac,
                 group = zodiac)) +
      geom_bar(stat = "identity") +
      theme(text = element_text(size = 15, family = "mono", face = "bold")) +
      labs(title = "How many podcasts belong to each Zodiac",
           x = "Zodiac",
           y = "Number of Podcasts")
    
  })
  
  output$ep_plot <- renderPlot({
    podsearch_df %>% 
      ggplot(aes(x = number_episodes)) +
      geom_histogram(fill = "#A64EFF",
                     color = "#F2EDF9") +
      labs(title = "How many episodes does the average Podcast have?",
           x = "Number of Episodes",
           y = "Number of Podcasts") +
      theme(text = element_text(size = 15, family = "mono", face = "bold"))
  })
  
  output$word_plot <- renderWordcloud2({
    
    validate(
      need(input$zodiac != "None", "Please select Zodiac")
    )
    
    word_df <- podsearch_df %>% 
      filter(zodiac == input$zodiac)
    
    word_col <- Corpus(VectorSource(word_df$description))
    
    word_col <- word_col %>% 
      tm_map(removeNumbers) %>% 
      tm_map(removePunctuation) %>% 
      tm_map(stripWhitespace)
    
    word_col <- tm_map(word_col, content_transformer(tolower))
    word_col <- tm_map(word_col, removeWords, stopwords("english"))
    
    word_count <- TermDocumentMatrix(word_col)
    matrix <- as.matrix(word_count)
    words <- sort(rowSums(matrix), decreasing = TRUE)
    df <- data.frame(word = names(words), freq = words)
    
    #wordcloud(words = df$word, freq = df$freq, min.freq = 1, max.words = 200, random.order = FALSE, rot.per = 0.35, colors = brewer.pal(8, "Dark2"))
    
    wordcloud2(data = df, size = 1.6, color = "random-light", backgroundColor = "#291440")
  })
  
  output$word_plot_2 <- renderWordcloud2({
    
    validate(
      need(input$genre != "None", "Please select Genre/Category")
    )
    
    word_df <- podsearch_df %>% 
      filter(grepl(input$genre, categories))
    
    word_col <- Corpus(VectorSource(word_df$description))
    
    word_col <- word_col %>% 
      tm_map(removeNumbers) %>% 
      tm_map(removePunctuation) %>% 
      tm_map(stripWhitespace)
    
    word_col <- tm_map(word_col, content_transformer(tolower))
    word_col <- tm_map(word_col, removeWords, stopwords("english"))
    
    word_count <- TermDocumentMatrix(word_col)
    matrix <- as.matrix(word_count)
    words <- sort(rowSums(matrix), decreasing = TRUE)
    df <- data.frame(word = names(words), freq = words)
    
    #wordcloud(words = df$word, freq = df$freq, min.freq = 1, max.words = 200, random.order = TRUE, rot.per = 0.35, colors = brewer.pal(8, "Dark2"))
    
    wordcloud2(data = df, size = 1.6, color = "random-light", backgroundColor = "#291440")
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
