
term_frequencies <- function(data, extra_stopwords){
    
    require(dplyr)
    require(tm)
    require(tibble)

    if(is.vector(data)){
        corpus <- data %>% 
            VectorSource() %>% 
            VCorpus()
    # } else if {
    #     is.data.frame(data){
    #         corpus <- data %>% 
    #             
    #     }
    }

    ## Clean the corpus
    clean_corpus <- corpus %>% 
        tm_map(removePunctuation, preserve_intra_word_dashes=TRUE) %>% 
        tm_map(removeNumbers) %>% 
        tm_map(content_transformer(tolower)) %>% 
        tm_map(removeWords, c(stopwords("en"), extra_stopwords)) %>% 
        tm_map(stripWhitespace)
    
    TermDocumentMatrix(clean_corpus) %>% 
        as.matrix %>% 
        rowSums(titles_mat) %>% 
        enframe(name = 'word', value = 'freqs') %>% 
        arrange(desc(freqs)) %>% 
        return()
    
}
