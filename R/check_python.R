#' Checks of Python requirements
#'
#' If Python requirements are not met, user must install hSBM requirements manually.
#'
#' @name check_python
#' @export

check_python <- function(env_name = "rhsbm_env") {

  stopifnot('Install reticulate package: install.packages("reticulate")' = require(reticulate))

  if(sum(reticulate::conda_list()$name == env_name) == 0) {
    reticulate::conda_create(env_name)
  }
  suppressWarnings(suppressMessages(reticulate::use_condaenv(env_name, required = TRUE)))

  if(!reticulate::py_module_available("numpy")) suppressWarnings(suppressMessages(reticulate::conda_install(env_name,"numpy")))

  if(!reticulate::py_module_available("pickle")) suppressWarnings(suppressMessages(reticulate::conda_install(env_name, "pickle")))

  if(!reticulate::py_module_available("matplotlib")) suppressWarnings(suppressMessages(reticulate::conda_install(env_name, "matplotlib")))

  if(!reticulate::py_module_available("pandas")) suppressWarnings(suppressMessages(reticulate::conda_install(env_name, "pandas")))

  if(!reticulate::py_module_available("graph_tool.all")) suppressWarnings(suppressMessages(reticulate::conda_install(env_name, "graph-tool")))

cat("Python and required dependencies are available.\n")

}
