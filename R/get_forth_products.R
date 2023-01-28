get_forth_products <- function() {
  get_forth_product_urls() |>
    purrr::map(get_forth_product_details)
}