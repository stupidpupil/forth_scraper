get_forth_bespoke_info <- function(){

  kits_json <- jsonlite::read_json("https://api.forthwithlife.co.uk//kit-pricing/get-kit-prices")


  # Assume we can only access the first ?base kit
  kit_price_pence <- as.integer(as.numeric(kits_json[[1]]$kitPrice)*100)
  venous_sampling_price_pence <- get_forth_venous_sampling_price_pence()

  biomarkers_map <- readr::read_csv("data-raw/biomarker_snomed_map.csv", col_types="cc")

  building_blocks_json <- jsonlite::read_json("https://api.forthwithlife.co.uk/bespoke-test/get-plus-tests/1?")

  building_blocks <- building_blocks_json |> 
    purrr::map(function(x){

      list(
        name = jsonlite::unbox(x$testName),
        price_pence = jsonlite::unbox(as.integer(as.numeric(x$testPrice) * 100)),
        biomarkers = x$biomarkers |> purrr::map_chr(function(b){
          bs <- b$biomarkerName |> normalise_biomarker_name()
          bs <- tibble::tibble(biomarker_handle = bs) |>
            dplyr::left_join(biomarkers_map, by="biomarker_handle") |>
            dplyr::pull(sctid)

          return(bs)
        }) |> unname() |> na.omit()

      )
    })

  list(
    kit_price_pence = jsonlite::unbox(kit_price_pence),
    venous_sampling_price_pence = jsonlite::unbox(venous_sampling_price_pence),
    building_blocks = building_blocks,
    last_updated = lubridate::now() |> lubridate::format_ISO8601(usetz=TRUE) |> jsonlite::unbox()
  )
}