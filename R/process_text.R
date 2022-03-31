#' Preprocessing of raw or lemmatized text data
#'
#' @name process_text
#' @importFrom utils write.table
#' @export

process_text <- function(src_docs,
                         src_texts,
                         target_docs = "titles_processed.txt",
                         target_texts = "corpus_processed.txt",
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
                         split_tags = FALSE,
                         padding = FALSE,
                         stopwords = NULL,
                         min_termfreq = NULL,
                         max_termfreq = NULL,
                         termfreq_type = c("count", "prop", "rank", "quantile"),
                         min_docfreq = NULL,
                         max_docfreq = NULL,
                         docfreq_type = c("count", "prop", "rank", "quantile")) {
  # # for dev purposes
  # src_docs = "titles.txt"
  # src_texts = "corpus.txt"
  # target_docs = "titles_p.txt"
  # target_texts = "corpus_p.txt"
  # replace_from = NULL
  # replace_to = NULL
  # collocations = TRUE
  # min_nchar = NULL
  # remove_punct = TRUE
  # remove_symbols = TRUE
  # remove_numbers = TRUE
  # remove_url = TRUE
  # remove_separators = TRUE
  # split_hyphens = FALSE
  # split_tags = FALSE
  # padding = FALSE
  # stopwords = NULL
  # min_termfreq = NULL
  # max_termfreq = NULL
  # termfreq_type = c("count", "prop", "rank", "quantile")
  # min_docfreq = NULL
  # max_docfreq = NULL
  # docfreq_type = c("count", "prop", "rank", "quantile")

  docs <- readLines(src_docs)
  lines <- readLines(src_texts)

  names(lines) <- docs

  stopifnot("replace_from is not the same length as replace_to" = length(replace_from) == length(replace_to))

  if (!is.null(replace_from)) {
    for (i in seq_along(replace_from)) {
      lines <- stringr::str_replace_all(lines, replace_from[i], replace_to[i])
    }
  }

  corpus <- quanteda::tokens(lines,
    remove_punct = remove_punct,
    remove_symbols = remove_symbols,
    remove_numbers = remove_numbers,
    remove_url = remove_url,
    remove_separators = remove_separators,
    split_hyphens = split_hyphens,
    split_tags = split_tags,
    padding = padding
  ) %>%
    quanteda::tokens_tolower() %>%
    quanteda::tokens_remove(
      min_nchar = min_nchar
    ) %>%
    quanteda::tokens_remove(
      pattern = stopwords
    )

  if (collocations) {

    detected_2collocations <- corpus %>%
      quanteda.textstats::textstat_collocations() %>%
      dplyr::pull(collocation)
    compounds_list <- strsplit(detected_2collocations, " ")

    detected_3collocations <- corpus %>%
      quanteda.textstats::textstat_collocations(size = 3) %>%
      dplyr::pull(collocation)

    compounds_list <- c(compounds_list, strsplit(detected_3collocations, " "))
    corpus <- corpus %>%
      quanteda::tokens_compound(compounds_list, join = FALSE)


  }

  texts_dfm <- quanteda::dfm(corpus) %>%
    quanteda::dfm_trim(
      min_termfreq = min_termfreq,
      max_termfreq = max_termfreq,
      termfreq_type = c("count", "prop", "rank", "quantile"),
      min_docfreq = min_docfreq,
      max_docfreq = max_docfreq,
      docfreq_type = c("count", "prop", "rank", "quantile")
    )

  texts_df <- texts_dfm %>% quanteda::convert("data.frame", docid_field = "doc_id")
  col_sel <- setdiff(colnames(texts_df), "doc_id")

  texts_df <- texts_df %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~ dplyr::na_if(., 0))) %>%
    tidyr::pivot_longer(
      cols = tidyselect::all_of(col_sel),
      names_to = "word",
      values_to = "freq",
      values_drop_na = TRUE
    ) %>%
    dplyr::filter(freq > 0) %>%
    dplyr::group_by(doc_id) %>%
    dplyr::summarise(text = paste0(rep(word, freq), collapse = " "))

  write.table(texts_df$text, target_texts, quote = FALSE, row.names = FALSE, col.names = FALSE)
  write.table(texts_df$doc_id, target_docs, quote = FALSE, row.names = FALSE, col.names = FALSE)

  return(list(titles_processed = target_docs,
              corpus_processed = target_texts))

  #               remove = stopwords("english"),
  #               stem = TRUE, remove_punct = TRUE)
  #
  #
  #
  #
  #   mutate.(text = str_replace_all(text, "(\\w)\\-+(\\w)", "\\1SPECIALREGEX\\2")) %>%
  #   mutate.(text = str_replace_all(text, "[[:punct:]]", " ")) %>%
  #   mutate.(text = str_replace_all(text, "SPECIALREGEX", "-")) %>%
  #   mutate.(text = str_replace_all(text, "´s|´", " ")) %>%
  #   mutate.(text = str_replace_all(text, "<.*>", " ")) %>%
  #   mutate.(text = str_replace_all(text, "[[:digit:]]+(th|rd|nd|st) ", " ")) %>%
  #   mutate.(text = str_replace_all(text, "\\|", " ")) %>%
  #   mutate.(text = str_replace_all(text, "[[:digit:]]+ ", " ")) %>%
  #   mutate.(text = str_replace_all(text, "[[:space:]]{2,}", " "))
  #
}
