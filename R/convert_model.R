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

    i <- as.integer(i)

     topics_df <- tibble::as_tibble(model$get_groups(l = i)[["p_w_tw"]],
      .name_repair = ~ paste0("topic_", seq_len(length(.x)))
    ) %>%
      dplyr::bind_cols(
        term = model$words,
        level = i + 1
      ) %>%
      dplyr::relocate(term, 1) %>%
      dplyr::relocate(level, 1)


    message(paste0("Writing word-topic distributions at level ", i + 1, ".\n"))

    arrow::write_feather(topics_df,
                       paste0(gsub("\\.pickle", "", basename(model_src)), "_topics_level_", i + 1, ".feather"))
  }


  for (i in levels) {
    i <- as.integer(i)

    docs_df <- tibble::as_tibble(t(model$get_groups(l = i)[["p_tw_d"]]),
      .name_repair = ~ paste0("topic_", seq_len(length(.x)))) %>%
        dplyr::bind_cols(doc = model$documents) %>%
        dplyr::relocate(doc, 1)


    message(paste0("Writing document-topic distributions at level ", i + 1, ".\n"))

    arrow::write_feather(docs_df, paste0(gsub("\\.pickle", "", basename(model_src)), "_documents_level_", i + 1, ".feather"))
  }

    for (i in levels) {
    i <- as.integer(i)

    clusters <- tibble::as_tibble(t(model$get_groups(l = i)[["p_td_d"]]),
    .name_repair = ~ paste0("cluster_", seq_len(length(.x))))

    clusters$cluster <- colnames(clusters)[max.col(clusters,ties.method="first")]
    
    clusters_df <- clusters |> 
    dplyr::bind_cols(doc = model$documents) |> 
    dplyr::select(doc, cluster)

    message(paste0("Writing document clusters at level ", i + 1, ".\n"))

    arrow::write_feather(clusters_df, paste0(gsub("\\.pickle", "", basename(model_src)), "_clusters_level_", i + 1, ".feather"))
  }

}
