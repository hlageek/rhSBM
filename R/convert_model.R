#' Covert topic model to table
#'
#' @name convert_model
#' @export

convert_model <- function(model_src, level = NULL) {
  make_df_doc <- weight <- topic <- freq <- NULL
  # model_src <- "topic_model_202204011237.pickle"
  # level <- NULL

  n_cores <- future::availableCores() - 1
  if (n_cores < 1) {
    n_cores <- 1
  }
  future::plan(future::multisession, workers = n_cores)

  model <- reticulate::py_load_object(model_src)

  if (is.null(level)) {
    levels_length <- model$L
    levels <- seq(0, levels_length - 1)
  } else {
    levels <- level - 1
  }

  make_df <- function(list_src) {
    data.frame(
      term = purrr::pluck(list_src, 1),
      weight = purrr::pluck(list_src, 2)
    )
  }

  make_df_inside <- function(list_item) {
    furrr::future_map(list_item, make_df, .options = furrr::furrr_options(seed = TRUE)) %>% dplyr::bind_rows()
  }

  for (i in levels) {
    topics_df <- model$topics(as.integer(i), model$get_V()) %>%
      furrr::future_map(make_df_inside, .options = furrr::furrr_options(seed = NULL)) %>%
      dplyr::bind_rows(.id = "topic") %>%
      dplyr::mutate(topic = paste0("topic_", (as.integer(topic) + 1))) %>%
      dplyr::mutate(level = i + 1) %>%
      dplyr::relocate(level, .before = topic)

    cat(paste0("Writing word-topic distributions at level ", i + 1, ".\n"))

    vroom::vroom_write(topics_df, paste0(gsub("\\.pickle", "", basename(model_src)), "_topics_level_", i + 1, ".tsv"))
  }

  make_df_doc <- function(list_src) {
    data.frame(
      topic = paste0("topic_", (purrr::pluck(list_src, 1) + 1)),
      weight = purrr::pluck(list_src, 2)
    ) %>%
      tidyr::pivot_wider(names_from = topic, values_from = weight)
  }

  bind_df <- function(x) {
    furrr::future_map(x, make_df_doc, .options = furrr::furrr_options(seed = NULL)) %>%
      dplyr::bind_cols()
  }

  doc_seq <- (seq_along(model$documents) - 1)
  model_fun <- model$topicdist

  for (i in levels) {

    doc_top <- furrr::future_map(doc_seq, ~model_fun(as.integer(.x), as.integer(i)),
                                 .options = furrr::furrr_options(seed = NULL))

    docs_df <- furrr::future_map(
      doc_top, bind_df,
      .options = furrr::furrr_options(seed = TRUE)
    ) %>%
      dplyr::bind_rows(.id = "doc_id") %>%
      dplyr::mutate(level = i + 1) %>%
      dplyr::relocate(level, .after = doc_id)

    cat(paste0("Writing document-topic distributions at level ", i + 1, ".\n"))

    vroom::vroom_write(docs_df, paste0(gsub("\\.pickle", "", basename(model_src)), "_documents_level_", i + 1, ".tsv"))
  }

  doc_ids_df <- tibble::enframe(model$documents, name = "doc_id", value = "title")

  cat(paste0("Writing document IDs.\n"))

  vroom::vroom_write(doc_ids_df, paste0(gsub("\\.pickle", "", basename(model_src)), "_documents_id.tsv"))
}
