#install.packages("tidytuesdayR")

library(tidytuesdayR)
library(tidyverse)
library(tidytext)

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

  
 

