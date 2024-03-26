# libs
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(quanteda)
library(wordcloud)

# Define UI
ui <- fluidPage(
  titlePanel("Airbnb Data Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput("property_type", "Select Property Type", choices = unique(airbnb_all$property_type))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Word Cloud",
                 plotOutput("wordcloud")),
        tabPanel("Word Frequency",
                 dataTableOutput("word_freq")),
        tabPanel("Price Distribution",
                 plotOutput("price_dist")),
        tabPanel("Average Price by Property Type",
                 plotOutput("avg_price")),
        tabPanel("TF-IDF Word Importance",
                 plotOutput("tf_idf_plot"))
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Word Cloud
  output$wordcloud <- renderPlot({
    tidy_data <- airbnb_all %>%
      filter(property_type == input$property_type) %>%
      unnest_tokens(word, text) %>%
      anti_join(stop_words) %>%
      count(word)
    
    wordcloud(words = tidy_data$word, freq = tidy_data$n, max.words = 100)
  })
  
  # Word Frequency
  output$word_freq <- renderDataTable({
    tidy_data <- airbnb_all %>%
      filter(property_type == input$property_type) %>%
      unnest_tokens(word, text) %>%
      anti_join(stop_words) %>%
      count(word, sort = TRUE)
    
    tidy_data
  })
  
  # Price Distribution
  output$price_dist <- renderPlot({
    price_freq <- airbnb_all %>%
      group_by(price) %>%
      summarise(frequency = n())
    
    ggplot(price_freq, aes(x = price, y = frequency)) +
      geom_point(color = "skyblue") +
      labs(title = "Distribution of Prices", x = "Price", y = "Frequency") +
      theme_minimal()
  })
  
  # Average Price by Property Type
  output$avg_price <- renderPlot({
    avg_price_by_property <- airbnb_all %>%
      group_by(property_type) %>%
      summarise(avg_price = mean(price, na.rm = TRUE)) %>%
      arrange(avg_price)
    
    ggplot(avg_price_by_property, aes(x = reorder(property_type, avg_price), y = avg_price)) +
      geom_bar(stat = "identity", fill = "skyblue", color = "black") +
      labs(title = "Average Price by Property Type", x = "Property Type", y = "Average Price") +
      theme_minimal() +
      coord_flip() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # TF-IDF Word Importance
  output$tf_idf_plot <- renderPlot({
    property_words <- airbnb_words %>%
      bind_tf_idf(word, property_type, n)
    
    property_words %>%
      arrange(desc(tf_idf)) %>%
      mutate(word = factor(word, levels = rev(unique(word)))) %>%
      group_by(property_type) %>%
      top_n(15) %>%
      ungroup() %>%
      ggplot(aes(word, tf_idf, fill = property_type)) +
      geom_col(show.legend = FALSE) +
      labs(x = NULL, y = "tf-idf") +
      facet_wrap(~property_type, ncol = 2, scales = "free") +
      coord_flip() +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)
# END
