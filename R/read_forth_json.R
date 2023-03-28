read_forth_json <- function(url){
  httr::GET(url, 
    httr::add_headers(
    #  Origin = "https://shop.forthwithlife.co.uk", 
      Referer = "https://shop.forthwithlife.co.uk/"
    ),
    httr::content_type("application/json"),
    httr::user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:111.0) Gecko/20100101 Firefox/111.0")
  ) |>
  httr::content(as="parsed")
}