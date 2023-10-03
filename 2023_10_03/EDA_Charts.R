#install.packages("tidytuesdayR")

library(tidytuesdayR)
library(tidyverse)
library(tidytext)
library(RColorBrewer)
library(wordcloud)
library(wordcloud2)
library(tm)


# Loading data
tuesdata <- tidytuesdayR::tt_load('2023-10-03')


# split grants and opportunity details

grants <- tuesdata$grants
grant_opportunity_details <- tuesdata$grant_opportunity_details

## Number of grants per agency (Top10) in 2023



grants %>%
  filter(posted_date >= '2023-01-01') %>%
  group_by(agency_name) %>%
  count() %>%
  arrange(desc(n)) %>%
  head(., n=10) %>%
  ggplot()+
    geom_bar(aes(x=n, y=fct_reorder(agency_name, n)), 
             stat = "identity", 
             position = "dodge",
             fill = "#009999") +
  theme_minimal()+
  ylab("US Agencies") +
  xlab("Number of Grants") +
  ggtitle("Top 10 US agencies by number of Grants in 2023")


## Wordcloud process

#Create a vector containing only the text
grant_op_2023 <- grant_opportunity_details %>%
  filter(posted_date >= '2023-01-01')

text <- grant_op_2023$funding_opportunity_title

# Create a corpus  
docs <- Corpus(VectorSource(text))

# Clean the data
docs <- docs %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english"))

# Generate term document matrix

dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df_words <- data.frame(word = names(words),freq=words)

# Wordcloud
set.seed(1234) # for reproducibility 

# One wordcloud
wordcloud(words = df_words$word, freq = df_words$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

# Another wordcloud

wordcloud2(data=df_words, size = 0.7, shape = 'diamond')


# Estimated total program funding vs number of grants for category education

categories <- names(grant_opportunity_details)[44:67]


# Create a Vector with Columns
columns = c("category","number","total") 

#Create a Empty DataFrame with 0 rows and n columns
df_categories = data.frame(matrix(nrow = 0, ncol = length(columns))) 

# Assign column names
colnames(df_categories) = columns


for(category in new_categories){
  print(category)
  total_funding <- grant_opportunity_details %>%
    filter(category == TRUE) %>%
    select(estimated_total_program_funding) %>%
    drop_na(.) %>%
    summarise(total = sum(estimated_total_program_funding))
  
  print(total_funding$total)
  
  number_grants <- grant_opportunity_details %>%
    filter(category == TRUE) %>%
    select(estimated_total_program_funding) %>%
    drop_na(.) %>%
    count()
 
  df_temp <- data.frame(category = category, number = number_grants$n, total = total_funding$total)
  
  df_categories <- rbind(df_categories,df_temp)
  
}


