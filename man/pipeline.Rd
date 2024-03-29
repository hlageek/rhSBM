% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pipeline.R
\name{pipeline}
\alias{pipeline}
\title{Run as pipeline}
\usage{
pipeline(
  titles = "titles.txt",
  corpus = "corpus.txt",
  ud_model = "english-ewt-ud-2.5-191206.udpipe",
  stopwords_sample = NULL,
  stopwords_threshold = 0.05,
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
  docfreq_type = c("count", "prop", "rank", "quantile")
)
}
\description{
Run as pipeline
}
