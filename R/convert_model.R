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


  for (i in levels) {

    topics_df <- tibble::as_tibble(model$get_groups(l = 0L)[["p_w_tw"]],
      .name_repair = ~ paste0("topic_", seq_len(length(.x)))
    ) %>%
      dplyr::bind_cols(
        term = model$words,
        level = i + 1
      ) %>%
      dplyr::relocate(term, 1) %>%
      dplyr::relocate(level, 1)


    cat(paste0("Writing word-topic distributions at level ", i + 1, ".\n"))

    vroom::vroom_write(topics_df,
                       paste0(gsub("\\.pickle", "", basename(model_src)), "_topics_level_", i + 1, ".tsv"))
  }


  for (i in levels) {
    docs_df <- tibble::as_tibble(t(model$get_groups(l = as.integer(i))[["p_tw_d"]]),
      .name_repair = ~ paste0("topic_", seq_len(length(.x))) %>%
        dplyr::bind_cols(doc = model$documents) %>%
        dplyr::relocate(doc, 1)
    )

    cat(paste0("Writing document-topic distributions at level ", i + 1, ".\n"))

    vroom::vroom_write(docs_df, paste0(gsub("\\.pickle", "", basename(model_src)), "_documents_level_", i + 1, ".tsv"))
  }

  # doc_ids_df <- tibble::enframe(model$documents, name = "doc_id", value = "title")
  #
  # cat(paste0("Writing document IDs.\n"))
  #
  # vroom::vroom_write(doc_ids_df, paste0(gsub("\\.pickle", "", basename(model_src)), "_documents_id.tsv"))
}
