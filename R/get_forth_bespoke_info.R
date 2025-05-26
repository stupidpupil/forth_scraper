get_forth_bespoke_info <- function(){



  #kits_json <- read_forth_json("https://api.forthwithlife.co.uk//kit-pricing/get-kit-prices")


  # Assume we can only access the first ?base kit
  kit_price_pence <- 3700#as.integer(as.numeric(kits_json[[1]]$kitPrice)*100)
  venous_sampling_price_pence <- get_forth_venous_sampling_price_pence()

  biomarkers_map <- readr::read_csv("data-raw/biomarker_snomed_map.csv", col_types="cc")


  remDr <- get_selenium_session()

  remDr$navigate("https://build.forthwithlife.co.uk/bespoke-test/0")
  Sys.sleep(3.0)

  articles <- remDr$findElements(using="xpath", "//article")


  building_blocks <- articles |> 
    purrr::imap(function(x, i){

      header <- tryCatch({
         x$findChildElement(using="xpath", ".//header")
      },
        error= function(cond){ NULL }
      )

      if(is.null(header)){
        return(list())
      }


      remDr$executeScript(paste0('document.getElementsByTagName("article")[', i-1, '].getElementsByTagName("button")[0].click()'))

      Sys.sleep(0.1)

      biomarkers <- remDr$findElements(using="xpath", "//div[@id=\'biomarkersItems\']//h2")
      biomarkers <- biomarkers |> purrr::map(function(b){
        b$getElementText()
      })

      biomarkers <- biomarkers |> unlist() |> purrr::map_chr(function(b){
          bs <- b |> normalise_biomarker_name()
          bs <- tibble::tibble(biomarker_handle = bs) |>
            dplyr::left_join(biomarkers_map, by="biomarker_handle") |>
            dplyr::pull(sctid)

          return(bs)
        }) |> unname() |> na.omit()

      remDr$executeScript('document.getElementsByClassName("btn-close")[0].click()')

      price_pence <- x$getElementText() |> 
          stringr::str_extract("£\\s*(\\d+(\\.\\d+)?)", group=1) |>
          as.numeric() |> na.omit() |> dplyr::first() |> (\(x) as.integer(x*100))()

      list(
        name = header$getElementText() |> unlist() |> jsonlite::unbox(),
        price_pence = price_pence |> jsonlite::unbox(),
        biomarkers = biomarkers
      )
    })

  building_blocks <- building_blocks |> purrr::keep(\(x) any(!is.na(x$biomarkers)))

  list(
    kit_price_pence = jsonlite::unbox(kit_price_pence),
    venous_sampling_price_pence = jsonlite::unbox(venous_sampling_price_pence),
    building_blocks = building_blocks,
    last_updated = lubridate::now() |> lubridate::format_ISO8601(usetz=TRUE) |> jsonlite::unbox()
  )
}