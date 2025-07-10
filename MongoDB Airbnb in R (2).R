#installing and loading the mongolite library to download the Airbnb data
install.packages("mongolite") #need to run this line of code only once and then you can comment out
library(mongolite)

#Loading required libraries  
library(dplyr)
library(ggplot2)
library(tidyverse) 
library(tidytext) 
library(tidyr)
library(stringr)


# This is the connection_string. You can get the exact url from your MongoDB cluster screen
#replace the <<user>> with your Mongo user name and <<password>> with the mongo password
#lastly, replace the <<server_name>> with your MongoDB server name
connection_string <- 
airbnb_collection <- mongo(collection="listingsAndReviews", db="sample_airbnb", url=connection_string)


#Here's how you can download all the Airbnb data from Mongo
## keep in mind that this is huge and you need a ton of RAM memory

airbnb_all <- airbnb_collection$find()

write.csv(airbnb_all, "airbnb_all.csv", row.names = FALSE)


#######################################################
#if you know or want to learn MQL (MongoQueryLanguage), that is a JSON syntax, feel free to use the following:::
######################################################
#1 subsetting your data based on a condition:
mydf <- airbnb_collection$find('{"bedrooms":2, "price":{"$gt":50}}')

#2 writing an analytical query on the data::
mydf_analytical <- airbnb_collection$aggregate('[{"$group":{"_id":"$room_type", "avg_price": {"$avg":"price"}}}]')


############################################################
########Step1:Data processing  #############################
############################################################

# Installing the 'textcat' package to find the language. 
library(tidytext)
library(dplyr)
library(readr)

install.packages("textcat")
library(textcat)

# filtering for English descriptions
data_rev_lang <- airbnb_all %>%
  mutate(description_language = textcat(description)) %>%
  filter(description_language == "english")

# Tokenize and clean text for analysis removing stop words
tokenized_descriptions <- data_rev_lang %>%
  unnest_tokens(word, description) %>%
  anti_join(stop_words, by = "word") %>%
  count(word, sort = TRUE) 


# Token frequency histogram for the top 10 most frequent description words
freq_hist <- tokenized_descriptions %>%
  top_n(10, n) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  xlab(NULL) +
  labs(y = "Frequency", title = "Top 10 Most Frequent Words in Descriptions")

print(freq_hist)

# Analyzing the amenities column. 
library(stringr)

# Ensuring my amenities column is a character vector
data_rev_lang <- data_rev_lang %>%
  mutate(amenities = as.character(amenities))

# splitting on the amenities column
data_rev_lang <- data_rev_lang %>%
  mutate(amenities_list = strsplit(amenities, ",\\s*"))  

# Unnest the amenities into a long format, one row per amenity
tokenized_amenities <- data_rev_lang %>%
  unnest(amenities_list) %>%
  filter(amenities != "") %>%  # Remove any empty strings 
  unnest_tokens(amenity, amenities) %>%
  count(amenity, sort = TRUE) 

# Token frequency histogram for the top 10 most frequent amenities
freq_hist_amenities <- tokenized_amenities %>%
  top_n(10, n) %>%
  mutate(amenities = reorder(amenity, n)) %>%
  ggplot(aes(x = amenities, y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  xlab(NULL) +
  labs(y = "Frequency", title = "Top 10 Most Frequent Amenities")

print(freq_hist_amenities)


##########################################################
#####STEP6: Performing sentiment analysis ################
##########################################################

# Get the sentiments from the lexicons
afinn <- get_sentiments("afinn")
nrc <- get_sentiments("nrc")
bing <- get_sentiments("bing")

# Combine all lexicons into one data frame
sentiments <- bind_rows(
  mutate(afinn, lexicon = "afinn"),
  mutate(nrc, lexicon = "nrc"),
  mutate(bing, lexicon = "bing")
)

# Join with tokenized descriptions and calculate sentiment
sentiment_analysis <- tokenized_descriptions %>%
  inner_join(sentiments, by = "word")

# Filter for each lexicon and perform analysis
bing_sentiment_analysis <- sentiment_analysis %>%
  filter(lexicon == "bing") %>%
  count(word, sentiment, sort = TRUE)

afinn_sentiment_analysis <- sentiment_analysis %>%
  filter(lexicon == "afinn") %>%
  count(word, value, sort = TRUE)

nrc_sentiment_analysis <- sentiment_analysis %>%
  filter(lexicon == "nrc") %>%
  count(word, sentiment, sort = TRUE)

# Calculate sentiment score for each description using AFINN
afinn_sentiment <- sentiment_analysis %>%
  filter(lexicon == "afinn") %>%
  group_by(word) %>%
  summarize(sentiment_score = sum(value, na.rm = TRUE)) %>%
  ungroup()

# Calculate sentiment score for each description using Bing
bing_sentiment <- sentiment_analysis %>%
  filter(lexicon == "bing") %>%
  group_by(word) %>%
  summarize(sentiment_score = sum(ifelse(sentiment == "positive", 1, -1),
                                  na.rm = TRUE)) %>%
  ungroup()

# Calculate sentiment score for each description using NRC
nrc_sentiment <- sentiment_analysis %>%
  filter(lexicon == "nrc") %>%
  group_by(word) %>%
  summarize(sentiment_score = sum(ifelse(sentiment == "positive", 1, -1),
                                  na.rm = TRUE)) %>%
  ungroup()

# Combine the sentiment scores from different lexicons 
combined_sentiment_scores <- afinn_sentiment %>%
  rename(afinn_score = sentiment_score) %>%
  inner_join(bing_sentiment %>% rename(bing_score = sentiment_score), 
             by = "word") %>%
  inner_join(nrc_sentiment %>% rename(nrc_score = sentiment_score), 
             by = "word")

# Display the combined sentiment scores
print(combined_sentiment_scores)


#############################################
###### N-grams  #############################
#############################################

# Load necessary libraries
library(dplyr)
library(tidytext)
library(tidyr)

# Create bigrams from the `description` column
bigrams <- data_rev_lang %>%
  unnest_tokens(bigram, description, token = "ngrams", n = 2)

# Separating the bigrams into individual words
bigrams_separated <- bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

# Filtering out stop words from both columns
data("stop_words")
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# Counting the bigrams and sorting them by frequency
bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)

# View the bigram counts
print(bigram_counts)

# Create quadgrams from the `description` column
quadgrams <- data_rev_lang %>%
  unnest_tokens(quadgram, description, token = "ngrams", n = 4)

# Separate the quadgrams into individual words
quadgrams_separated <- quadgrams %>%
  separate(quadgram, c("word1", "word2", "word3", "word4"), sep = " ")

# Filter out stop words from all four columns
quadgrams_filtered <- quadgrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word) %>%
  filter(!word4 %in% stop_words$word)

# Count the quadgrams and sort them by frequency
quadgram_counts <- quadgrams_filtered %>%
  count(word1, word2, word3, word4, sort = TRUE)

# View the quadgram counts
print(quadgram_counts)


# Analysis 

##########################################################
###### correlation of frequency by country ###############
##########################################################

# Load necessary libraries
library(dplyr)
library(tidytext)
library(ggplot2)
library(tidyr)

# Tokenizing descriptions per country (USA, Brazil, Australia)
tidy_usa <- data_rev_lang %>%
  filter(address$country == "United States") %>%
  unnest_tokens(word, description) %>%
  anti_join(stop_words, by = "word")

tidy_brazil <- data_rev_lang %>%
  filter(address$country == "Brazil") %>%
  unnest_tokens(word, description) %>%
  anti_join(stop_words, by = "word")

tidy_australia <- data_rev_lang %>%
  filter(address$country == "Australia") %>%
  unnest_tokens(word, description) %>%
  anti_join(stop_words, by = "word")

# Combine all datasets and calculate frequencies
frequency <- bind_rows(
  mutate(tidy_usa, author = "United States"),
  mutate(tidy_brazil, author = "Brazil"),
  mutate(tidy_australia, author = "Australia")
) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n / sum(n)) %>%
  ungroup() %>%
  pivot_wider(names_from = author, values_from = proportion, 
              values_fill = list(proportion = 0)) %>%
  gather(key = "author", value = "proportion", `United States`,
         Brazil, Australia)

# Plotting the comparison
library(ggplot2)
library(scales)

# filter for the top 10 most frequent words in each country
top_words_by_country <- frequency %>%
  group_by(author) %>%
  top_n(10, proportion) %>%
  ungroup()

# Plot the most frequent words for each country
ggplot(top_words_by_country, aes(x = reorder(word, proportion),
                                 y = proportion, fill = author)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +  
  labs(x = NULL, y = "Proportion", title = 
         "Top 10 Most Frequent Words by Country") +
  facet_wrap(~author, scales = "free_y") + 
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))  

# Correlation tests
cor_test_usa_brazil <- cor.test(frequency[frequency$author == "Brazil",]
                                $proportion, frequency[frequency$author == 
                                                         "United States",]
                                $proportion, method = "pearson")
cor_test_usa_australia <- cor.test(frequency[frequency$author == "Australia",]
                                   $proportion, frequency[frequency$author == 
                                                            "United States",]
                                   $proportion, method = "pearson")

# Print correlation test results
print(cor_test_usa_brazil)
print(cor_test_usa_australia)



########################################################
###### Word Frequencies for descriptions ###############
########################################################
library(dplyr)
library(wordcloud)
library(RColorBrewer)


# creating a new dataframe 'word_frequencies' to store the word and their count
word_frequencies <- tokenized_descriptions %>%
  group_by(word) %>%
  summarise(count = sum(n)) %>%
  arrange(desc(count))

# Now we generate the word cloud using the 'wordcloud' library
wordcloud(words = word_frequencies$word, freq = word_frequencies$count,
          max.words = 100, random.order = FALSE, colors = 
            brewer.pal(8, "Dark2"))

write.csv(word_frequencies, "word_frequencies.csv", row.names = FALSE)

# Grouping by the column and summarising
word_amenities <- tokenized_amenities %>%
  group_by(amenity) %>%
  summarise(count = sum(n)) %>%
  arrange(desc(count))

# Generate the word cloud using the correct dataframe and column
wordcloud(words = word_amenities$amenity, freq = word_amenities$count,
          max.words = 100, random.order = FALSE, colors = 
            brewer.pal(8, "Dark2"))

#####################################################
###### Distribution of Property Types ###############
#####################################################

library(ggplot2)
library(dplyr)

# Calculating the count of each property type
property_type_distribution <- data_rev_lang %>%
  count(property_type) %>%
  arrange(desc(n))

# Selecting only the top 10 property types
top_property_types <- head(property_type_distribution, 10)

# Plotting the distribution of the top 10 property types
ggplot(top_property_types, aes(x = reorder(property_type, n), y = n, 
                               fill = property_type)) +
  geom_bar(stat = "identity") +
  coord_flip() +  
  labs(x = "Property Type", y = "Count", 
       title = "Top 10 Distribution of Property Types") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  
        legend.position = "none")  

#############################################
###### Price vs property_type ###############
#############################################

# Load necessary libraries
library(readr)
library(dplyr)
library(ggplot2)

# Clean the data: remove rows with NA values in price or property_type
data_rev_lang <- data_rev_lang %>%
  filter(!is.na(price) & !is.na(property_type))

# Identify the top 10 most frequent property types
top_property_types <- data_rev_lang %>%
  count(property_type) %>%
  arrange(desc(n)) %>%
  slice_head(n = 10)

# Filter data to only include the top 10 property types
data_rev_lang_top <- data_rev_lang %>%
  filter(property_type %in% top_property_types$property_type)

# Basic statistics for price by property type among the top 10 types
price_summary_top <- data_rev_lang_top %>%
  group_by(property_type) %>%
  summarise(
    Count = n(),
    Mean = mean(price, na.rm = TRUE),
    Median = median(price, na.rm = TRUE),
    Min = min(price, na.rm = TRUE),
    Max = max(price, na.rm = TRUE)
  ) %>%
  arrange(desc(Mean))

# Print the summary statistics
print(price_summary_top)

# Visualizing the distribution of prices for the top 10 most frequent property
ggplot(data_rev_lang_top, aes(x = property_type, y = price)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Price Distribution by Property Type for Top 10 Types", 
       x = "Property Type", y = "Price")

# Calculate and plot average price by property type for the top 10 types
average_price_plot_top <- data_rev_lang_top %>%
  group_by(property_type) %>%
  summarise(Average_Price = mean(price, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(property_type, Average_Price), y = Average_Price)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Average Price by Property Type for Top 10 Types", 
       x = "Property Type", y = "Average Price")

# Print the average price plot
print(average_price_plot_top)
