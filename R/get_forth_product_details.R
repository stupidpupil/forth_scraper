get_forth_product_details <- function(forth_product_url){

  tryCatch({
  handle <- forth_product_url |> 
    stringr::str_match("health-tests/([a-z0-9_-]+/[a-z0-9_-]+)/$") |> (\(x) x[,2])()

  forth_product_html <- get_html_for_url(forth_product_url)

  title <- forth_product_html |> rvest::html_node("h1") |> rvest::html_text() |>
    stringr::str_extract("^(.+?)( Blood)?( Test)?$", group=1)

  price_pence <- forth_product_html |>
    rvest::html_nodes(xpath='//*[contains(@id,"price-normal")]') |>
    rvest::html_text() |> 
    stringr::str_trim() |>
    stringr::str_extract("^£\\s*(\\d+(\\.\\d+)?)$", group=1) |>
    as.numeric() |> na.omit() |> dplyr::first() |> (\(x) as.integer(x*100))()

  available <- TRUE # HACK

  biomarker_names <-  forth_product_html |> 
    rvest::html_nodes(css=".what-gets-tested-modal .biomarker-content a") |> 
    rvest::html_text() |> unique()

  biomarkers <- biomarker_names |> 
    normalise_biomarker_name()

  venous_available <- forth_product_html |> 
    rvest::html_node(css=".collection-methods-modal") |> rvest::html_text() |> 
    stringr::str_detect("([Pp]hlebotomy|Superdrug|partner clinic)") |> any()

  venous_only <- venous_available &
    !(forth_product_html |> rvest::html_node(css=".collection-methods-modal") |> rvest::html_text() |> stringr::str_detect("Finger ?prick") |> any())

  list(
    title = title,
    handle = handle,
    biomarkers = biomarkers,
    price_pence = price_pence,
    available = available,
    venous_available = venous_available,
    venous_only = venous_only
  )
  },
  error = function(cond) {
    list(
      title = "Error",
      handle = "Error",
      biomarkers = c(),
      price_pence = 99999,
      available = FALSE,
      venous_available = FALSE,
      venous_only = FALSE
    )
  }
  )
}