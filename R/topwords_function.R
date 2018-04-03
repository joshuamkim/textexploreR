#' Function outputs a graph of top words
#'
#' @param dataframe input for the function
#' @param top_n_number number of words as output
#' @return data.frame() graph of the top words in descending order
#'
#' @author Joshua Kim
#'
#' @examples
#' topwords_function(dataframe, 30)
#'
#' @export
topwords_function <-
function(dataframe, top_n_number){
  
  
  text_df <- data_frame(text = dataframe$text)
  text_df <- text_df %>% unnest_tokens(word, text) %>% anti_join(stop_words)
  
  top_words_graph <- text_df %>% count(word, sort = TRUE) %>%
    top_n(top_n_number) %>% ## make this argument part of the function
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n)) +
    geom_col(fill= "dodgerblue") +
    xlab(NULL) +
    coord_flip()
  
  top_words_graph
}
