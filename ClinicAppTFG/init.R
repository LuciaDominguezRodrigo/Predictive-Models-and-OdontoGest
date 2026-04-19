my_packages <- c('shiny', 'DBI', 'pool', 'bcrypt', 'shinyjs', 'jsonlite', 'bslib', 'toastui', 'shinyWidgets', 'xgboost', 'caret', 'plotly', 'dotenv', 'digest', 'mailR', 'sodium', 'RMariaDB')

install_if_missing <- function(p) {
  if (!p %in% rownames(installed.packages())) {
    install.packages(p, dependencies = TRUE, repos = "https://cloud.r-project.org/")
  }
}

invisible(sapply(my_packages, install_if_missing))