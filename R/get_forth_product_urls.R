get_forth_product_urls <- function(){

  list_url <- "https://www.forthwithlife.co.uk/health-tests/"

  list_html <- get_html_for_url(list_url)

  list_html |> 
    rvest::html_nodes("a[href^='https://www.forthwithlife.co.uk/health-tests/']") |> 
    rvest::html_attr("href") |> unique() |>
    purrr::keep(function(x){stringr::str_detect(x, "^https://www.forthwithlife.co.uk/health-tests/.+/.+/$")}) |>
    rvest::url_absolute(list_url)
}
