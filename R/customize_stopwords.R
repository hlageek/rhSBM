#' Extract empirical stopwords
#'
#' @name customize_stopwords
#' @export

customize_stopwords <- function(corpus, stopwords_sample, stopwords_threshold) {

  lines <- readLines(corpus)
  sample_size <- round(length(lines) * stopwords_sample)
  sample_index <- sample(lines, sample_size)

  write.table(sample_index, "sample_c.txt", quote = FALSE, row.names = FALSE, col.names = FALSE)

  model_file <- run_hsbm(src_docs = NULL, "sample_c.txt")

  model <- reticulate::py_load_object(model_file)

  file.remove(c("sample_c.txt", model_file))

  make_df_doc <- function(list_src) {
    data.frame(
      topic = paste0("topic_", (purrr::pluck(list_src, 1) + 1)),
      weight = purrr::pluck(list_src, 2)
    ) %>%
      tidyr::pivot_wider(names_from = topic, values_from = weight)
  }


  docs_df <- furrr::future_map_dfr(
    (seq_along(model$documents) - 1),
    function(x) {
      model$topicdist(as.integer(x), 0L) %>%
        purrr::map(make_df_doc) %>%
        dplyr::bind_cols()
    }, .options = furrr::furrr_options(seed = TRUE)
  )

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

  cat("Writting custom stopwords.\n")

  writeLines(empirical_stopwords, "custom_stopwords.txt")

  return(empirical_stopwords)
}
