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
  set.seed(123)
  if (sum(x) != 0) {
  broom::tidy(shapiro.test(x)) %>% dplyr::pull(p.value)
  } else {1}
}

