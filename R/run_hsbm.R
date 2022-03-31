#' Run hSBM
#'
#' @name run_hsbm
#' @export

run_hsbm <- function(src_docs,
                     src_texts,
                     n_min = 1L,
                     seed = 32L) {

  # src_docs = "titles_processed.txt"
  # src_texts = "corpus_processed.txt"
  # n_min = 1L
  # seed = 32L



  reticulate::use_condaenv("rhsbm_env")

  reticulate::source_python("https://raw.githubusercontent.com/martingerlach/hSBM_Topicmodel/master/sbmtm.py")
  reticulate::py_run_string("import matplotlib")
  reticulate::py_run_string("matplotlib.use('Agg')")

  if (!is.null(src_docs)) {
  docs <- readLines(src_docs)
  } else (docs <- NULL)

  texts <- readLines(src_texts)

  texts <- lapply(texts, rhSBM::splitter)

  model <- sbmtm()

  model$make_graph(texts, docs, n_min = n_min)

  gt <- reticulate::import("graph_tool", "gt")

  gt$seed_rng(seed)

  model$fit()

  model_file_name <- paste0("./topic_model_", format(Sys.time(), "%Y%m%d%H%M"), ".pickle")
  reticulate::py_save_object(model, paste0("./", model_file_name))

  cat(paste("Model run finished.\nModel saved as", model_file_name, "\n"))

  return(model_file_name)
}
