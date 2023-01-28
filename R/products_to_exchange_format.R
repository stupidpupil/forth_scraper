products_to_exchange_format <- function(products){

  biomarkers_map <- readr::read_csv("data-raw/biomarker_snomed_map.csv", col_types="cc")

  products <- products |>
    purrr::keep(function(prod){
      prod$available
    }) |>
    purrr::map(function(prod){
      prod$available = NULL

      prod$name = jsonlite::unbox(prod$title)
      prod$title = NULL

      prod$url = jsonlite::unbox(paste0("https://www.forthwithlife.co.uk/health-tests/", prod$handle))
      prod$handle = NULL

      prod$price_pence = jsonlite::unbox(prod$price_pence)

      prod$biomarkers <- tibble::tibble(biomarker_handle = prod$biomarkers) |>
        dplyr::left_join(biomarkers_map,  by="biomarker_handle") |>
        dplyr::pull("sctid") |>
        na.omit() |>
        unique()

      return(prod)
    }) |>
    purrr::keep(function(prod){
      length(prod$biomarkers) > 0
    })


  fingerprick <- products |>
    purrr::keep(function(prod) {
      !is.na(prod$venous_only) & !prod$venous_only
    }) |>
    purrr::map(function(prod){
      prod$venous_only = NULL
      prod$venous_available = NULL

      prod$name = jsonlite::unbox(paste0(prod$name, " with fingerprick"))

      prod$sampling_procedure = jsonlite::unbox("fingerprick")

      return(prod)
    })

  venous <- products |>
    purrr::keep(function(prod) {
      !is.na(prod$venous_available) & prod$venous_available
    }) |>
    purrr::map(function(prod){
      prod$venous_only = NULL
      prod$venous_available = NULL
      
      prod$name = jsonlite::unbox(paste0(prod$name, " with venous sample"))

      prod$sampling_procedure = jsonlite::unbox("venous")
      prod$price_pence = jsonlite::unbox(prod$price_pence + (30*100))

      return(prod)
    })


  return(c(fingerprick, venous))
}