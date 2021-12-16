# For Ubuntu 18.04 (also known as Bionic)
options(repos = c(
  REPO_NAME = "https://packagemanager.rstudio.com/all/__linux__/bionic/latest",
  getOption("repos")
))

# For Ubuntu 20.04 (also known as Focal)
options(repos = c(
  REPO_NAME = "https://packagemanager.rstudio.com/all/__linux__/focal/latest",
  getOption("repos")
  ))

if (interactive()) {
  source("renv/activate.R")
  suppressMessages(require(devtools))
  suppressMessages(require(testthat))
  suppressMessages(require(usethis))
}
