# Load the necessary libraries
library(pdftools)
library(tm)
library(topicmodels)
library(syuzhet)
library(tokenizers)
library(here)

# Specify the folder containing the PDFs
pdf_folder <- "/Users/andrewruiz/ex4"

# Read all PDF files from the folder
file_list <- list.files(path = pdf_folder, pattern = "*.pdf", full.names = TRUE)

# Extract text from each PDF
text_data <- lapply(file_list, pdf_text)

# Combine the text into one character vector, one element per PDF
text_data_combined <- sapply(text_data, paste, collapse = " ")

# Create a corpus from the combined text
docs <- Corpus(VectorSource(text_data_combined))

# Define the initial cleaning function
clean_corpus_initial <- function(corpus){
  original_length <- length(corpus)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  corpus <- tm_map(corpus, stripWhitespace)
  
  non_empty_docs <- sapply(corpus, function(x) length(unlist(strsplit(as.character(x), " "))) > 0)
  corpus_cleaned <- corpus[non_empty_docs]
  
  cat("Original number of documents:", original_length, "\n")
  cat("Number of documents after cleaning:", length(corpus_cleaned), "\n")
  cat("Number of documents dropped:", original_length - length(corpus_cleaned), "\n")
  
  retained_docs <- names(corpus_cleaned)
  cat("Documents retained:", paste(retained_docs, collapse = ", "), "\n")
  
  return(corpus_cleaned)  # Return the cleaned corpus
}

# Apply initial cleaning
docs_cleaned_initial <- clean_corpus_initial(docs)

# Document-Term Matrix for initial analysis
dtm_initial <- DocumentTermMatrix(docs_cleaned_initial)
dtm_initial <- dtm_initial[rowSums(as.matrix(dtm_initial)) > 0, ]

# Initial topic modeling
k_initial <- 8
lda_model_initial <- LDA(dtm_initial, k = k_initial, control = list(seed = 4321))
topics_initial <- terms(lda_model_initial, 8)
print("Initial topics with potential names included:")
print(topics_initial)

# Extract the topic for each document for the initial model
topic_probabilities_initial <- posterior(lda_model_initial)$topics
doc_topics_initial <- apply(topic_probabilities_initial, 1, which.max)

# Create a data frame for the document-topic associations for the initial model
doc_topics_df_initial <- data.frame(Document = names(doc_topics_initial), MostLikelyTopic = doc_topics_initial)

# View the first few rows of the document-topic association for the initial model
head(doc_topics_df_initial)


# Extend the stopwords list with common names for refined cleaning
custom_stopwords <- c(stopwords("en"), "mary", "michael", "david", "john", "susan", "patricia")

# Refined cleaning function that includes removal of first names
clean_corpus_refined <- function(corpus) {
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, custom_stopwords)
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}

# Apply refined cleaning
docs_cleaned_refined <- clean_corpus_refined(docs)

# DTM for refined analysis
dtm_refined <- DocumentTermMatrix(docs_cleaned_refined)
dtm_refined <- dtm_refined[rowSums(as.matrix(dtm_refined)) > 0, ]


# Refined topic modeling
k_refined <- 8
lda_model_refined <- LDA(dtm_refined, k = k_refined, control = list(seed = 4321))
topics_refined <- terms(lda_model_refined, 8)
print("Refined topics without first names:")
print(topics_refined)

# Extract the topic for each document for the refined model
topic_probabilities_refined <- posterior(lda_model_refined)$topics
doc_topics_refined <- apply(topic_probabilities_refined, 1, which.max)

# Create a data frame for the document-topic associations for the refined model
doc_topics_df_refined <- data.frame(Document = names(doc_topics_refined), MostLikelyTopic = doc_topics_refined)

# View the first few rows of the document-topic association for the refined model
head(doc_topics_df_refined)


# Extract the topic for each document for the refined model
topic_probabilities_refined <- posterior(lda_model_refined)$topics
doc_topics_refined <- apply(topic_probabilities_refined, 1, which.max)

# Create a data frame for the document-topic associations for the refined model
doc_topics_df_refined <- data.frame(Document = names(doc_topics_refined), MostLikelyTopic = doc_topics_refined)

# View the first few rows of the document-topic association for the refined model
head(doc_topics_df_refined)

# Perform sentiment analysis on the combined text data
sentiment_scores <- get_sentiment(text_data_combined, method = "bing")

# View the sentiment scores
head(sentiment_scores)

# Create a vector of PDF document names
pdf_document_names <- basename(file_list)

# Create a data frame with document names and sentiment scores
sentiment_df <- data.frame(DocumentName = pdf_document_names, SentimentScore = sentiment_scores)

# Print the first few rows of the data frame to see the mapping
head(sentiment_df)
print(sentiment_df)

# Assuming 'text_data_combined' contains your text data
nrc_data <- get_nrc_sentiment(text_data_combined)

# Access the data frame columns for emotions and sentiments
anger_items <- which(nrc_data$anger > 0)
joy_items <- which(nrc_data$joy > 0)

# Print sentences associated with specific emotions
print(text_data_combined[anger_items])
print(text_data_combined[joy_items])

# View the entire sentiment data frame
print(nrc_data)

# View only the positive and negative valence columns
print(nrc_data[, c("negative", "positive")])

document_sentiment <- data.frame(DocumentName = pdf_document_names, 
                                 Negative = nrc_data$negative, 
                                 Positive = nrc_data$positive)

# Print the data frame with document names and sentiment scores
print(document_sentiment)

# Create a bar graph of emotions
barplot(
  sort(colSums(prop.table(nrc_data[, 1:8]))), 
  horiz = TRUE, 
  cex.names = 0.7, 
  las = 1, 
  main = "Emotions in Text", 
  xlab = "Percentage"
)
