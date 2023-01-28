complete_exchange_format <- function(products_in_exchange_format){
  list(
    provider = list(
      name = jsonlite::unbox("Forth"),
      url = jsonlite::unbox("https://www.forthwithlife.co.uk")
    ),
    products = products_in_exchange_format  ,
    last_updated = lubridate::now() |> lubridate::format_ISO8601(usetz=TRUE) |> jsonlite::unbox()
  )
}
