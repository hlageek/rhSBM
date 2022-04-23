#' Run as pipeline
#'
#' @name pipeline
#' @export

pipeline <- function(titles = "titles.txt",
                     corpus = "corpus.txt",
                     ud_model = "english-ewt-ud-2.5-191206.udpipe",
                     stopwords_sample = NULL,
                     shapiro_sample = 50,
                     shapiro_threshold = 0.05,
                     replace_from = NULL,
                     replace_to = NULL,
                     collocations = TRUE,
                     min_nchar = NULL,
                     remove_punct = TRUE,
                     remove_symbols = TRUE,
                     remove_numbers = TRUE,
                     remove_url = TRUE,
                     remove_separators = TRUE,
                     split_hyphens = FALSE,
                     padding = FALSE,
                     stopwords = quanteda::stopwords("en"),
                     min_termfreq = NULL,
                     max_termfreq = NULL,
                     termfreq_type = c("count", "prop", "rank", "quantile"),
                     min_docfreq = NULL,
                     max_docfreq = NULL,
                     docfreq_type = c("count", "prop", "rank", "quantile")) {
  check_python()

  lemmatized <- run_udpipe(corpus, ud_model)

  cat("Corpus was lemmatized.\n")

  if (!is.null(stopwords_sample)) {

    corpus_sample <- make_sample(lemmatized, stopwords_sample = stopwords_sample)

    processed_sample <- process_text(src_docs = NULL,
                              src_texts = lemmatized,
                              replace_from = replace_from,
                              replace_to = replace_to,
                              collocations = collocations,
                              min_nchar = min_nchar,
                              remove_punct = remove_punct,
                              remove_symbols = remove_symbols,
                              remove_numbers = remove_numbers,
                              remove_url = remove_url,
                              remove_separators = remove_separators,
                              split_hyphens = split_hyphens,
                              padding = padding,
                              stopwords = stopwords,
                              min_termfreq = min_termfreq,
                              max_termfreq = max_termfreq,
                              termfreq_type = termfreq_type,
                              min_docfreq = min_docfreq,
                              max_docfreq = max_docfreq,
                              docfreq_type = docfreq_type
    )

    empirical_stopwords <- customize_stopwords(corpus = processed_sample$corpus_processed,
                                               shapiro_sample = shapiro_sample,
                                               shapiro_threshold = shapiro_threshold)
    cat("Custom stopwords helper model removed.\n")

  } else {empirical_stopwords <- NULL}

  processed <- process_text(titles,
    lemmatized,
    replace_from = replace_from,
    replace_to = replace_to,
    collocations = collocations,
    min_nchar = min_nchar,
    remove_punct = remove_punct,
    remove_symbols = remove_symbols,
    remove_numbers = remove_numbers,
    remove_url = remove_url,
    remove_separators = remove_separators,
    split_hyphens = split_hyphens,
    padding = padding,
    stopwords = c(stopwords, empirical_stopwords),
    min_termfreq = min_termfreq,
    max_termfreq = max_termfreq,
    termfreq_type = termfreq_type,
    min_docfreq = min_docfreq,
    max_docfreq = max_docfreq,
    docfreq_type = docfreq_type
  )

  cat("Corpus was processed.\n")

  model <- run_hsbm(processed$titles_processed, processed$corpus_processed)

  convert_model(model)

  cat("Pipeline succesfully completed.\n")
}
