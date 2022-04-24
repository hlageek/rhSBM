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
test_norm <- function(x, shapiro_sample) {
  set.seed(123)
  x <- sample(x, shapiro_sample)
  if (sum(x) != 0) {
  broom::tidy(shapiro.test(x)) %>% dplyr::pull(p.value)
  } else {1}
}

