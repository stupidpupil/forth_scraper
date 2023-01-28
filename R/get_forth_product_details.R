get_forth_product_details <- function(forth_product_url){

  handle <- forth_product_url |> 
    stringr::str_match("health-tests/([a-z0-9_-]+/[a-z0-9_-]+)/$") |> (\(x) x[,2])()

  forth_product_html <- rvest::read_html(forth_product_url)

  title <- forth_product_html |> rvest::html_node("h1") |> rvest::html_text()

  price_pence <- forth_product_html |>
    rvest::html_nodes("div") |> 
    rvest::html_text() |> 
    stringr::str_match("^£(\\d+)$") |> (\(x) x[,2])() |> 
    as.numeric() |> na.omit() |> dplyr::first() |> (\(x) as.integer(x*100))()

  available <- forth_product_html |> 
    rvest::html_nodes("span") |> rvest::html_text2() |> 
    stringr::str_detect("^IN STOCK$") |> any()

  biomarker_names <-  forth_product_html |> 
    rvest::html_nodes(".elementor-widget-container") |> 
    purrr::keep( \(x) x |> rvest::html_text() |> 
    stringr::str_detect("gets tested")) |> rvest::html_nodes("li") |> rvest::html_text() |> unique()

  biomarkers <- biomarker_names |> 
    stringr::str_to_lower() |> 
    stringr::str_replace_all("[\\(\\)\\s,-]","_") |> 
    stringr::str_replace_all("_+","_") |> 
    stringr::str_replace_all("(^_|_$)","")


  venous_available <- forth_product_html |> rvest::html_nodes("span") |> rvest::html_text() |> stringr::str_detect("Phlebotomy kit") |> any()

  venous_only <- venous_available &
    !(forth_product_html |> rvest::html_nodes("span") |> rvest::html_text() |> stringr::str_detect("Finger ?prick") |> any())

  list(
    title = title,
    handle = handle,
    biomarkers = biomarkers,
    price_pence = price_pence,
    available = available,
    venous_available = venous_available,
    venous_only = venous_only
  )


}