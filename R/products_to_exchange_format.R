products_to_exchange_format <- function(products){

  biomarkers_map <- readr::read_csv("data-raw/biomarker_snomed_map.csv", col_types="cc")

  products <- products |>
    purrr::keep(function(prod){
      !is.na(prod$available) &
      prod$available &
      !is.na(prod$title) &
      !is.na(prod$handle) &
      !is.na(prod$price_pence)
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


  stopifnot(length(products) > 5)

  stopifnot(products |> 
    purrr::keep(function(prod){prod$name |> stringr::str_detect("\\b[Tt]estosterone\\b")}) |> 
    purrr::every(function(prod){"997161000000108" %in% prod$biomarkers}))

  stopifnot(products |> 
    purrr::keep(function(prod){prod$name |> stringr::str_detect("\\b[Oo]?[Ee]stradiol\\b")}) |> 
    purrr::every(function(prod){"1010521000000102" %in% prod$biomarkers}))

  stopifnot(products |> 
    purrr::keep(function(prod){prod$name |> stringr::str_detect("\\b[Oo]?[Ee]strogen\\b")}) |> 
    purrr::every(function(prod){"1010521000000102" %in% prod$biomarkers}))

  stopifnot(products |> 
    purrr::keep(function(prod){prod$name |> stringr::str_detect("\\b[Ff]emale [Hh]ormone")}) |> 
    purrr::every(function(prod){"1010521000000102" %in% prod$biomarkers}))

  stopifnot(products |> 
    purrr::keep(function(prod){prod$name |> stringr::str_detect("menopaus")}) |> 
    purrr::every(function(prod){"1010521000000102" %in% prod$biomarkers}))

  stopifnot(products |> 
    purrr::keep(function(prod){prod$name |> stringr::str_detect("\\b[Ll]iver\\b")}) |> 
    purrr::every(function(prod){"1000861000000106" %in% prod$biomarkers}))

  stopifnot(products |> 
    purrr::keep(function(prod){prod$name |> stringr::str_detect("\\b[Ll]iver\\b")}) |> 
    purrr::every(function(prod){"1018251000000107" %in% prod$biomarkers}))


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

  venous_sampling_price_pence <- get_forth_venous_sampling_price_pence()

  venous <- products |>
    purrr::keep(function(prod) {
      !is.na(prod$venous_available) & prod$venous_available
    }) |>
    purrr::map(function(prod){
      prod$venous_only = NULL
      prod$venous_available = NULL
      
      prod$name = jsonlite::unbox(paste0(prod$name, " with venous sample"))

      prod$sampling_procedure = jsonlite::unbox("venous")
      prod$price_pence = jsonlite::unbox(prod$price_pence + venous_sampling_price_pence)

      return(prod)
    })


  return(c(fingerprick, venous))
}