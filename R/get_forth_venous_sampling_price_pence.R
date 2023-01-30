get_forth_venous_sampling_price_pence <- function(){
  phlebotomy_json <- jsonlite::read_json("https://api.forthwithlife.co.uk/phlebotomy/get-plebotomy-prices")

  venous_sampling_entry <- phlebotomy_json |> 
    purrr::keep(\(x) x$phlebotomyName == "Partner clinic") 

  stopifnot(length(venous_sampling_entry) == 1)

  ret <- as.integer(as.numeric(venous_sampling_entry[[1]]$phlebotomyPrice) * 100)

  return(ret)
}