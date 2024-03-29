#' Lemmatize corpus with udpipe
#'
#' @name run_udpipe
#' @export


run_udpipe <- function(corpus, ud_model = "english-ewt-ud-2.5-191206.udpipe") {
  if (!file.exists(ud_model)) {
    language <- gsub("(.*)-ud.*", "\\1", ud_model)
    # https://github.com/jwijffels/udpipe.models.ud.2.5/tree/master/inst/udpipe-ud-2.5-191206
    udpipe::udpipe_download_model(language = language)
  }

  n_cores <- future::availableCores() - 1
  if (n_cores < 1) {
    n_cores <- 1
  }

  get_lemma <- function(text, ud_model) {
    paste(udpipe::udpipe(text, object = ud_model) %>%
            dplyr::filter(!is.na(lemma)) %>%
            dplyr::pull(lemma), collapse = " ")
  }

  lines <- readLines(corpus)

  future::plan(future::multisession, workers = n_cores)

  res <- furrr::future_map_chr(lines, get_lemma, ud_model, .options = furrr::furrr_options(seed = TRUE))

  corpus_lemmatized <- paste0(gsub("\\.txt", "", basename(corpus)), "_lemmatized.txt")
  write.table(res, corpus_lemmatized, quote = FALSE, row.names = FALSE, col.names = FALSE)
  return(corpus_lemmatized)
}
