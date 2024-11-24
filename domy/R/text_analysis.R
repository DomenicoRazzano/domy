#' Text analysis
#'
#' This function reads text files, tokenizes them, counts word frequency, perform sentiment analysis and creates a word cloud.
#'
#' @param file_paths A vector of file paths
#' @return A list with word frequency and total sentiment score
#' @export

text_analysis <- function(file_paths) {
  
  # Empty list
  results <- list()
  
  for (file_path in file_paths) {
    cat("\nText:", basename(file_path), "\n")
    
    # File reading
    text_data <- data.frame(text = readLines(file_path), stringsAsFactors = FALSE)
    
    # Tokenization
    data(stop_words)
    tokenized_words <- text_data %>%
      unnest_tokens(word, text)
    
    # Word frequencies
    word_frequencies <- tokenized_words %>%
      anti_join(stop_words) %>%
      count(word, sort = TRUE)
    
    cat("10 most used words:\n")
    print(head(word_frequencies, 10))
    
    # Sentiment analysis
    afinn <- get_sentiments("afinn")
    sentiment_score <- tokenized_words %>%
      inner_join(afinn, by = "word") %>%
      summarise(total_score = sum(value, na.rm = TRUE)) %>%
      pull()
    
    cat(paste0("\nSentiment score:\n", sentiment_score, "\n"))
    
    # Word cloud
    wordcloud_plot <- tokenized_words %>%
      anti_join(stop_words) %>%
      count(word) %>%
      ggplot(aes(label = word, size = n)) +
      geom_text_wordcloud() +
      theme_minimal() +
      ggtitle(paste("Word Cloud for:", basename(file_path)))
    
    print(wordcloud_plot)
    
    results[[basename(file_path)]] <- list(
      word_frequencies = word_frequencies,
      sentiment_score = sentiment_score
    )
  }
  
  return(results)
}


