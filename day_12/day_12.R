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
  dplyr::rename(plant = lyr.1) |> 
  dplyr::group_by(plant) |>
  dplyr::mutate(id = 1:dplyr::n()) |>
  terra::vect()

fences <- data.frame(
  plant = fence_polygons$plant,
  id = fence_polygons$id,
  perim = terra::perim(fence_polygons),
  area = terra::expanse(fence_polygons)
) |>
  dplyr::mutate(
    cost = perim * area
  )

sum(fences$cost) |> write_answer(part = 1)

# Part 2 ---------------------------------------

side_counts <- fence_polygons |>
  terra::as.points() |>
  sf::st_as_sf() |>
  data.frame() |>
  dplyr::group_by(plant, id) |>
  dplyr::summarise(sides = dplyr::n(), .groups = "drop")

fences = fences |>
  dplyr::select(-dplyr::any_of("sides")) |>
  dplyr::left_join(side_counts, by = c("plant", "id")) |>
  dplyr::mutate(
    cost_discount = sides * area
  )

  sum(fences$cost_discount) |>  write_answer(part = 2)
