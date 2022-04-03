#' Run as pipeline
#'
#' @name make_sample
#' @export

make_sample <- function(corpus, stopwords_sample, corpus_sample = "corpus_sample.txt"){

  lines <- readLines(corpus)

  sample_size <- round(length(lines) * stopwords_sample)
  sample_index <- sample(lines, sample_size)

  write.table(sample_index, corpus_sample, quote = FALSE, row.names = FALSE, col.names = FALSE)

  return(corpus_sample)

}
