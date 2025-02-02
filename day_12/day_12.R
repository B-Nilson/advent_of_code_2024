# Generic function for outputing answer files
write_answer <- function(x, part) {
  out_file <- paste0("answer_", part, ".txt")
  x |>
    as.character() |>
    writeLines(out_file)
}

# Part 1 ---------------------------------------

input <- "input.txt" |>
  readLines() |>
  stringr::str_split("", simplify = TRUE)

garden <- input |>
  matrix(ncol = ncol(input)) |> 
  terra::rast(crs = "local")

fence_polygons = garden |>
  terra::as.polygons() |>
  sf::st_as_sf() |>
  sf::st_cast("POLYGON") |>
  terra::vect()

fences <- data.frame(
  perim = terra::perim(fence_polygons),
  area = terra::expanse(fence_polygons)
) |>
  dplyr::mutate(
    cost = perim * area
  )

sum(fences$cost) |> write_answer(part = 1)

# Part 2 ---------------------------------------

# write_answer(part = 2)
