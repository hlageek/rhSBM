#' Extract empirical stopwords
#'
#' @name customize_stopwords
#' @export

customize_stopwords <- function(corpus_sample = "corpus_sample.txt", stopwords_threshold) {

  model_file <- run_hsbm(src_docs = NULL, corpus_sample)

  model <- reticulate::py_load_object(model_file)

  # file.remove(c("sample_c.txt", model_file))

  docs_df <- tibble::as_tibble(t(model$get_groups(l=0L)[["p_tw_d"]]),
                    .name_repair = ~paste0("topic_", seq_len(length(.x))))

  stop_topics <- docs_df %>%
    furrr::future_map_dfr(test_norm, .options = furrr::furrr_options(seed = TRUE)) %>%
    tidyr::pivot_longer(
      cols = everything(),
      names_to = "topic",
      values_to = "value"
    ) %>%
    dplyr::filter(value > stopwords_threshold) %>%
    dplyr::pull(topic) %>%
    stringr::str_replace_all("topic_", "") %>%
    as.integer()

  empirical_stopwords <- c()

  for (i in stop_topics) {
    empirical_stopwords <- c(
      empirical_stopwords,
      model$topics(0L, model$get_V())[[i]] %>%
        purrr::transpose() %>%
        purrr::pluck(1) %>%
        unlist()
    )
  }

  if (is.null(empirical_stopwords)) empirical_stopwords <- ""

  cat("Writting custom stopwords.\n")

  writeLines(empirical_stopwords, "custom_stopwords.txt")

  return(empirical_stopwords)
}
