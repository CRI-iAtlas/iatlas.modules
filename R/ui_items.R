#' Title Box
#'
#' @param title a string
#'
#' @export
titleBox <- function(title) {
  shiny::fluidRow(
    shinydashboard::box(
      width = 12,
      background = "yellow",
      shiny::span(shiny::strong(title), style = "font-size:24px")
    )
  )
}

#' Sub Title Box
#'
#' @param title  a string
#'
#' @export
subTitleBox <- function(title) {
  shiny::fluidRow(
    shinydashboard::box(
      width = 12,
      shiny::span(shiny::strong(title),style = "font-size:16px")
    )
  )
}

#' Section Box
#'
#' @param ... arguments to shinydashboard::box
#' @param title  a string
#'
#' @export
sectionBox <- function(..., title) {
  shiny::fluidRow(
    shinydashboard::box(
      ...,
      width = 12,
      title = title,
      solidHeader = TRUE,
      status = "warning",
      collapsible = TRUE
    )
  )
}

#' Options Box
#'
#' @param ... arguments to shinydashboard::box
#'
#' @export
optionsBox <- function(...) {
  shinydashboard::box(..., background = "navy")
}

#' Plot Box
#'
#' @param ... arguments to shinydashboard::box
#'
#' @export
plotBox <- function(...) {
  shinydashboard::box(..., status = "warning")
}

#' Table Box
#'
#' @param ... arguments to shinydashboard::box
#'
#' @export
tableBox <- function(...) {
  shinydashboard::box(..., status = "warning")
}

#' Text Box
#'
#' @param ... arguments to shinydashboard::box
#'
#' @export
textBox <- function(...) {
  shinydashboard::box(..., status = "success")
}

#' Message Box
#'
#' @param ... arguments to shinydashboard::box
#'
#' @export
messageBox <- function(...) {
  shinydashboard::box(..., status = "danger", background = "green")
}

#' Img Link Box
#'
#' @param ... arguments to shinydashboard::box
#' @param linkId a string
#' @param title a string
#' @param imgSrc a string
#' @param boxText a string
#' @param linkText a string
#'
#' @export
imgLinkBox <- function(..., linkId, title, imgSrc, boxText, linkText) {
  shinydashboard::box(
    ...,
    title = shiny::span(title, style = "font-size:15px"),
    solidHeader = TRUE, status = "primary",
    shiny::fluidRow(
      shiny::column(
        width = 4,
        shiny::img(src = imgSrc, width = "100%")
      ),
      shiny::column(
        width = 8,
        shiny::p(boxText),
        shiny::actionButton(inputId = linkId, label = linkText)
      )
    )
  )
}
