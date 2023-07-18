get_html_for_url <- function(url){

  remDr <- get_selenium_session()

  remDr$navigate(url)

  remDr$getPageSource() |> dplyr::first() |> xml2::read_html()
}

