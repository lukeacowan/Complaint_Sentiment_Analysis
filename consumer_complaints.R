
library(ggplot2)
library(dplyr)
library(tidyverse)
library(readxl)
library(lubridate)
library(wordcloud)
library(sentimentr)
library(tidytext)
library(stringr)
library(plotly)
library(shiny)
library(DT)
library(zoo)
rm(list = ls())

setwd("C:/Users/lukec/OneDrive/Documents/data332/sentiment_analysis_project/data")

raw_data <- readRDS(file = "Consumer_Complaints.rds")

#brought in columns deemed my and made naming conventions consistent - left out tags, zip codes, and complaint IDs; also left out date received because date sent appeared to be the more accurate column
#also deselected consumer consent, company public response, submission type, and consumer dispute columns
my_data <- raw_data%>%
  dplyr::select("Product", "Sub.product", "Issue", "Sub.issue", "Consumer.complaint.narrative",
                "Company", "State",  "Date.sent.to.company", 
                "Company.response.to.consumer", "Timely.response.") %>%
  dplyr::rename("Timely.response" = "Timely.response.")

#identifying potential invalid states
states <- my_data %>%
  select(State) %>%
  distinct()

#removing invalid states
valid_data <- my_data %>%
  dplyr::filter(State != "AE",
                State != "MH",
                State != "AP",
                State != "FM",
                State != "MP",
                State != "PW",
                State != "AA")

complaint_analysis <- valid_data %>%
  #creating column for all words in narrative column to be stored separately
  unnest_tokens(word, Consumer.complaint.narrative) %>%
  count(word) %>%
  #sorting words from most to least common
  arrange(desc(n)) %>%
  #filtering out insignificant words such as "of" or "the"
  anti_join(stop_words) %>%
  #removing censored information and mispellings
  dplyr::filter(word != "xxxx",
                word != "xx",
                word != "n't",)

#creating wordcloud visualization to display the 50 words that appear most frequently in customers' comments
wordcloud(
  words = complaint_analysis$word,
  freq = complaint_analysis$n,
  max.words = 50,
  colors = "grey40"
)

narrative_text <- valid_data %>% 
  #storing individual words from consumer comments separately
  unnest_tokens(word, Consumer.complaint.narrative) %>% 
  #removing stop words
  anti_join(stop_words)

sentiment_consumer <- narrative_text %>%
  #appending the NRC dictionary and filtering for negative, sadness, and anger sentiments
  inner_join(get_sentiments("nrc")) %>% 
  filter(sentiment %in% c("negative", "sadness", "anger")) %>%
  #counting by word and sentiment and taking the top 10 of each
  count(word, sentiment) %>% 
  group_by(sentiment) %>% 
  top_n(10, n) %>% 
  ungroup() %>% 
  #creating a column that has each word ordered by the count
  mutate(word2 = fct_reorder(word, n))

#making a visualization regarding undesirable sentiments
ggplot(sentiment_consumer, aes(x = word2, y = n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  #creating a separate facet for each sentiment with free axes
  facet_wrap(~ sentiment, scales = "free") +
  coord_flip() +
  #titling the plot "Sentiment Word Counts" with "Words" for the x-axis
  labs(
    title = "Sentiment Word Counts",
    x = "Words",
    y = "Occurrences"
  )

company_sentiment <- narrative_text %>%
  #appending the bing sentiment dictionary
  inner_join(get_sentiments("bing")) %>% 
  #counting by company and sentiment
  count(Company, sentiment) %>% 
  #spreading the sentiment and count columns
  spread(sentiment, n) %>% 
  #computing overall_sentiment
  mutate(overall_sentiment = positive - negative) %>%
  arrange(overall_sentiment) %>%
  head(n=10)

#creating a bar plot concerning the ten companies with the lowest/most negative overall sentiment
ggplot(
  company_sentiment, 
  aes(x = Company, y = overall_sentiment, fill = as.factor(Company))
) +
  geom_col(show.legend = FALSE) +
  coord_flip() + 
  #titling and labeling the plot
  labs(
    title = "Overall Sentiment by Company",
    subtitle = "Most Negative Sentiments",
    y = "Overall Sentiment"
  )

product_sentiment <- narrative_text %>%
  #appending the bing sentiment dictionary
  inner_join(get_sentiments("bing")) %>% 
  #counting by product and sentiment
  count(Product, sentiment) %>% 
  #spreading the sentiment and count columns
  spread(sentiment, n) %>% 
  #compute overall_sentiment
  mutate(overall_sentiment = positive - negative) %>%
  arrange(overall_sentiment) 

#creating a bubble chart for the overall sentiment of all 12 products
ggplot(
  product_sentiment, 
  aes(x = Product, y = overall_sentiment, size = negative/positive)
) +
  geom_point(color = "orange", alpha=0.8) +
  scale_size(range = c(.1, 20), name="Negativity Ratio") +
  coord_flip() +
  #titling and labeling the plot
  labs(
    title = "Overall Sentiment by Product",
    subtitle = "Most Negative Sentiments",
    y = "Overall Sentiment"
  )

state_sentiment <- narrative_text %>%
  #appending the bing sentiment dictionary
  inner_join(get_sentiments("bing")) %>% 
  #counting by state and sentiment
  count(State, sentiment) %>% 
  #spreading the sentiment and count columns
  spread(sentiment, n) %>% 
  #computing overall_sentiment 
  mutate(overall_sentiment = positive - negative) %>%
  arrange(overall_sentiment) 

#creating an interactive map that displays the overall sentiment by state
plot_geo(state_sentiment, locationmode = 'USA-states') %>%
  add_trace(
    z = state_sentiment$overall_sentiment, locations = state_sentiment$State)

response_sentiment <- narrative_text %>%
  #appending the bing sentiment dictionary
  inner_join(get_sentiments("bing")) %>% 
  #counting by product and sentiment
  count(Company.response.to.consumer, sentiment) %>% 
  #spreading the sentiment and count columns
  spread(sentiment, n) %>% 
  #compute overall_sentiment
  mutate(overall_sentiment = positive - negative) %>%
  arrange(overall_sentiment) 

ui<-fluidPage( 
  
  titlePanel(title = "Explore Consumer Complaints Dataset"),
  h4('Complaints by day'),
  
  fluidRow(
    column(2,
           selectInput('Company Response', 'Choose Response', response_sentiment$Company.response.to.consumer)
    ),
    column(4,plotOutput('plot_01')),
    column(6,DT::dataTableOutput("table_01", width = "100%"))
  )
  
  
)

server<-function(input,output){
  
  output$plot_01 <- renderPlot({
    ggplot(response_sentiment, aes(x = Company.response.to.consumer, y = overall_sentiment, fill = Company.response.to.consumer)) +
      geom_col(data = subset(response_sentiment, Company.response.to.consumer == input$Company.response.to.consumer))
  })
  
  output$table_01<-DT::renderDataTable(valid_data[,c(input$X,input$Y,input$Splitby)],options = list(pageLength = 4))
}

shinyApp(ui=ui, server=server) 




