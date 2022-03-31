#' Split tokens
#'
#' @name splitter
#' @export
splitter <- function(x) {

  vec_input <-  unlist(strsplit(x, split = "\\s+"))
  vec_output <- vec_input[vec_input != ""]
  return(vec_output)

}

#' Test normality
#'
#' @name test_norm
#' @export
test_norm <- function(x) {
  broom::tidy(shapiro.test(x)) %>% dplyr::pull(p.value)
}

