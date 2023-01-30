normalise_biomarker_name <- function(biomarker_name){
  biomarker_name |>
    stringr::str_to_lower() |> 
    stringr::str_replace_all("[\\(\\)\\s,-]","_") |> 
    stringr::str_replace_all("_+","_") |> 
    stringr::str_replace_all("(^_|_$)","")
}