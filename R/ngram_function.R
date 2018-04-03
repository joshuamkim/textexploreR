#' Function outputs top n-grams in a data frame
#'
#' @param dataframe input for the function
#' @param ngram_number number of n-grams desired (i.e 2 = bigram, 3 = trigram)
#' @return data.frame() count of n-grams
#'
#' @author Joshua Kim
#'
#' @examples
#' ngram_function(dataframe, 3)
#'
#' @export
#'
ngram_function <-
function(dataframe, ngram_number) {
  
  text_df2 <- data_frame(text = dataframe$text)
  
  number_grams <- text_df2 %>% 
    unnest_tokens(gram, text, token = "ngrams", n = ngram_number)
  
  n_grams <- number_grams %>% count(gram, sort = TRUE)
  View(n_grams)
}
