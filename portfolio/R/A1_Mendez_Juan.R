#Importing necessary libraries
library(shiny)
library(shinydashboard)
library(dplyr)
library(tidytext)
library(ggplot2)
library(mongolite)
library(tidytuesdayR)
library(stringr)
library(tidyr)
library(scales)
library(topicmodels)

#Creating the connection with the mongo server
connection_string <- 'mongodb+srv://juan:juan@cluster0.hrd26wt.mongodb.net/'
airbnb_collection <- mongo(collection="listingsAndReviews", db="sample_airbnb", url=connection_string)

#Importing airbnb data as airbnb_all
airbnb_all <- airbnb_collection$find()

#Creating a list of countries in the data
countries <- c("Portugal", "Brazil", "United States", "Turkey", 
               "Canada", "Spain", "Australia", "France", 
               "Ireland", "Georgia", "Mexico", "Jordan", 
               "Jersey", "China", "Italy", "Japan", 
               "Switzerland", "Belgium", "Indonesia", "Russia",
               "Uruguay", "Germany", "Brunswick", "Singapore", "Kenya")

#Creating the UI part of the shinyapp
ui <- dashboardPage(
  dashboardHeader(title = "Airbnb Post Description Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Sentiment Analysis", tabName = "sentiment", icon = icon("smile")), #Sentiment Analysis tab
      menuItem("TF-IDF Analysis", tabName = "tfidf", icon = icon("font")),         #TF-IDF tab
      menuItem("Topic Modeling", tabName = "topics", icon = icon("tags")),         #Topic Modeling tab
      menuItem("Word Correlogram", tabName = "correlogram", icon = icon("cloud"))  #Word Correlogram tab
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "sentiment",
              
              #Adding inputs
              selectInput("countrySelect", "Select a country:",
                          choices = c("Portugal", "Brazil", "United States", "Turkey", 
                                      "Canada", "Spain", "Australia", "France", 
                                      "Ireland", "Georgia", "Mexico", "Jordan", 
                                      "Jersey", "China", "Italy", "Japan", 
                                      "Switzerland", "Belgium", "Indonesia", "Russia",
                                      "Uruguay", "Germany", "Brunswick", "Singapore", "Kenya"), 
                          selected = "United States"),
              plotOutput("sentimentPlot")),
      tabItem(tabName = "tfidf",
              
              #Adding inputs
              selectInput("countrySelectTFIDF1", "Select a country:",
                          choices = c("Portugal", "Brazil", "United States", "Turkey", 
                                      "Canada", "Spain", "Australia", "France", 
                                      "Ireland", "Georgia", "Mexico", "Jordan", 
                                      "Jersey", "China", "Italy", "Japan", 
                                      "Switzerland", "Belgium", "Indonesia", "Russia",
                                      "Uruguay", "Germany", "Brunswick", "Singapore", "Kenya"),
                          selected = "United States"),
              
              #Adding inputs
              selectInput("countrySelectTFIDF2", "Select a country:",
                          choices = c("Portugal", "Brazil", "United States", "Turkey", 
                                      "Canada", "Spain", "Australia", "France", 
                                      "Ireland", "Georgia", "Mexico", "Jordan", 
                                      "Jersey", "China", "Italy", "Japan", 
                                      "Switzerland", "Belgium", "Indonesia", "Russia",
                                      "Uruguay", "Germany", "Brunswick", "Singapore", "Kenya"),
                          selected = "Canada"),
              plotOutput("tfidfPlot")),
      tabItem(tabName = "topics",
              
              #Adding inputs
              sliderInput("kValue", "Number of Topics (k):", 
                          min = 2, max = 5, value = 5),
              plotOutput("topicPlot")),
      tabItem(tabName = "correlogram", 
              
              #Adding inputs
              selectInput("countrySelectCorrelogram1", "Select a country:",
                          choices = c("Portugal", "Brazil", "United States", "Turkey", 
                                      "Canada", "Spain", "Australia", "France", 
                                      "Ireland", "Georgia", "Mexico", "Jordan", 
                                      "Jersey", "China", "Italy", "Japan", 
                                      "Switzerland", "Belgium", "Indonesia", "Russia",
                                      "Uruguay", "Germany", "Brunswick", "Singapore", "Kenya"),
                          selected = "Canada"),
              
              #Adding inputs
              selectInput("countrySelectCorrelogram2", "Select a country:",
                          choices = c("Portugal", "Brazil", "United States", "Turkey", 
                                      "Canada", "Spain", "Australia", "France", 
                                      "Ireland", "Georgia", "Mexico", "Jordan", 
                                      "Jersey", "China", "Italy", "Japan", 
                                      "Switzerland", "Belgium", "Indonesia", "Russia",
                                      "Uruguay", "Germany", "Brunswick", "Singapore", "Kenya"),
                          selected = "Brazil"),
              plotOutput("correlogramPlot"))
    )
  )
)

#Instantiating the server part of the shinyapp
server <- function(input, output) {
  
  #Organizing and preparing the data
  
  #Creating a price group column, that groups according to price quantiles
  airbnb_all <- airbnb_all %>%
    mutate(price_group = cut(price,
                             breaks = quantile(price, probs = seq(0, 1, by = 0.2)),
                             include.lowest = TRUE,
                             labels = c("1st Quintile", "2nd Quintile", "3rd Quintile", "4th Quintile", "5th Quintile")))
  
  #Changing the description column name to text
  colnames(airbnb_all)[5] <- 'text'
  
  #Creating a seperate df with the host location (actual location in the world) information
  host_info <- airbnb_all$host$host_location
  
  #Creating a column named location with the hosts location (again, actual location in the world)
  airbnb_all['location'] <- host_info
  
  #Applying unnest_tokens to the location column, to extract the countries
  countries_airbnb <- airbnb_all %>%
    unnest_tokens(country, location)
  
  #Filtering out words that are not countries in the country column
  data <- countries_airbnb %>%
    filter(tolower(country) %in% tolower(countries) | tolower(country) == "united" | tolower(country) == "us")
  
  #Changing the values 'united' and 'us' to 'united states'
  data$country[data$country %in% c('united', 'us')] <- 'united states'
  
  #Applying unnest_tokens ti the text column to extract tokens (words)
  tidy_data <- data %>%
    unnest_tokens(word, text)
  
  #Eliminating stop words
  airbnb_nostop <- tidy_data %>%
    anti_join(stop_words) %>%
    filter(!tolower(word) %in% c('de', 'da', 'el', 'del',"a", "b", 
                                 "c", "d", "e", "f", "g", "h", "i", 
                                 "j", "k", "l", "m", "n", "o", "p", 
                                 "q", "r", "s", "t", "u", "v", "w", 
                                 "x", "y", "z", 'la', 'à','1','2','3',
                                 '4','5','6','7','8','9'))
  
  ### Sentiment Analysis
  output$sentimentPlot <- renderPlot({
    selected_country <- input$countrySelect
    
    #filterinf for the selected country
    country_data <- airbnb_nostop %>%
      filter(tolower(country) == tolower(selected_country))
    
    #Getting Afinn coefficients
    afinn <- country_data %>%
      inner_join(get_sentiments("afinn")) %>%
      summarise(sentiment = sum(value)) %>%
      mutate(method = "AFINN")
    
    #Getting Bing and NRC coefficients
    bing_and_nrc <- bind_rows(
      country_data %>%
        inner_join(get_sentiments("bing")) %>%
        mutate(method = "Bing et al."),
      country_data %>%
        inner_join(get_sentiments("nrc") %>%
                     filter(sentiment %in% c("positive", "negative"))) %>%
        mutate(method = "NRC")) %>%
      count(method, sentiment) %>%
      spread(sentiment, n, fill = 0) %>%
      mutate(sentiment = positive - negative)
    
    #Separating the data to plot
    plot_data <- bind_rows(afinn, bing_and_nrc)
    
    #Defining cool colors pallet
    cool_colors <- c("AFINN" = "#5599ff", "Bing et al." = "#33cccc", "NRC" = "#99ccff")
    
    #Creating the plot
    ggplot(plot_data, aes(method, sentiment, fill = method)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~method, ncol = 1, scales = "free_y") +
      scale_fill_manual(values = cool_colors)  # Apply the cool color palette
  })
  
  ### TF-IDF
  output$tfidfPlot <- renderPlot({
    req(input$countrySelectTFIDF1, input$countrySelectTFIDF2)
    
    #Selected countries for TF-IDF analysis
    selected_countries <- tolower(c(input$countrySelectTFIDF1, input$countrySelectTFIDF2))
    
    #Creating counts of words
    data_tokens <- airbnb_nostop %>%
      count(country, word, sort = TRUE) %>%
      ungroup()
    
    #Total words
    total_words <- data_tokens %>%
      group_by(country) %>%
      summarize(total = sum(n))
    
    #Filtering for selected countries
    data_words <- left_join(data_tokens, total_words) %>%
      filter(country %in% selected_countries)
    
    #Applying bind_tf_idf function
    country_words <- data_words %>%
      bind_tf_idf(word, country, n)
    
    #Creating the plot
    country_words %>%
      arrange(desc(tf_idf)) %>%
      mutate(word = factor(word, levels = rev(unique(word)))) %>%
      group_by(country) %>%
      top_n(15) %>%
      ungroup() %>%
      ggplot(aes(word, tf_idf, fill = country)) +
      geom_col(show.legend = FALSE) +
      labs(x = NULL, y = "tf-idf") +
      facet_wrap(~country, ncol = 2, scales = "free") +
      coord_flip()
  })
  
  ###Topic Modeling
  output$topicPlot <- renderPlot({
    req(input$kValue) 
    
    #Creating price groups
    airbnb_dtm <- airbnb_nostop   %>%
      count(price_group, word, sort = TRUE) %>%
      cast_dtm(price_group, word, n )
    
    #Applying LDA fucntion with k = input
    airbnb_lda <- LDA(airbnb_dtm, k = input$kValue, control = list(seed = 98))
    
    #Extracting topics
    airbnb_topics <- tidy(airbnb_lda, matrix = "beta")
    
    #Finding top terms for each topic
    top_terms <- airbnb_topics %>%
      group_by(topic) %>%
      top_n(10, beta) %>%
      ungroup() %>%
      arrange(topic, -beta)
    
    #Customizing topic labels to reflect "Price Group" and the topic number
    top_terms$topic <- paste("Price Group", top_terms$topic)
    
    #Plotting
    ggplot(top_terms, aes(x = reorder_within(term, beta, topic), y = beta, fill = topic)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~ topic, scales = "free", ncol = 2) +
      coord_flip() +
      scale_x_reordered() +
      labs(x = NULL, y = "Beta")+
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  ###Word Correlogram
  output$correlogramPlot <- renderPlot({
    req(input$countrySelectCorrelogram1, input$countrySelectCorrelogram2)
    
    #Defining user inputs
    selected_countries_correlogram <- tolower(c(input$countrySelectCorrelogram1, input$countrySelectCorrelogram2))
    
    #Creating tidy text format for United States descriptions
    tidy_country_1 <- airbnb_nostop %>%
      filter(country == "united states")
    
    #Creating a tidy format for country 1 descriptions
    tidy_country_2 <- airbnb_nostop %>%
      filter(country == tolower(input$countrySelectCorrelogram1))
    
    #Creating a tidy format for country 2 descriptions
    tidy_country_3 <- airbnb_nostop %>%
      filter(country == tolower(input$countrySelectCorrelogram2))
    
    #Combine all data sets and do frequency
    frequency <- bind_rows(mutate(tidy_country_1, author = "united states"),
                           mutate(tidy_country_2, author =  tolower(input$countrySelectCorrelogram1)),
                           mutate(tidy_country_3, author = tolower(input$countrySelectCorrelogram2))
    ) %>% 
      mutate(word=str_extract(word, "[a-z']+")) %>%
      count(author, word) %>%
      group_by(author) %>%
      mutate(proportion = n/sum(n)) %>%
      select(-n) %>%
      spread(author, proportion) %>%
      gather(author, proportion, tolower(input$countrySelectCorrelogram1), tolower(input$countrySelectCorrelogram2))
    
    #Plotting the correlograms
    ggplot(frequency, aes(x = proportion, y = `united states`, 
                          color = abs(`united states`- proportion)))+
      geom_abline(color = "grey40", lty = 2)+
      geom_jitter(alpha = .1, size = 2.5, width = 0.3, height = 0.3)+
      geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
      scale_x_log10(labels = percent_format())+
      scale_y_log10(labels = percent_format())+
      scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
      facet_wrap(~author, ncol=2)+
      theme(legend.position = "none")+
      labs(y = "United States", x = NULL)
    
  })
}

#Run this to run the shinyapp
shinyApp(ui, server)
