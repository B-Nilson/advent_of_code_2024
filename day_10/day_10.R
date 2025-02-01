# Generic function for outputing answer files
write_answer <- function(x, part) {
  out_file <- paste0("answer_", part, ".txt")
  x |>
    as.character() |>
    writeLines(out_file)
}

# Part 1 ---------------------------------------

input <- "input.txt" |>
  readLines()

topography <- input |>
  stringr::str_split("") |>
  sapply(as.integer) |>
  t()

trailheads <- which(topography == 0, arr.ind = TRUE)
path_finder <- function(topography, position) {
  # Get all possible directions including center
  locations <- data.frame(
    direction = c(NA, 1:4) |> factor(labels = c("N", "E", "S", "W")),
    row = position[1] + c(0, 1, 0, -1, 0),
    col = position[2] + c(0, 0, 1, 0, -1)
  ) |>
    # Drop any that are out of bounds
    dplyr::filter(
      row > 0 & row <= nrow(topography),
      col > 0 & col <= ncol(topography)
    )
  # If center is 9, we're done
  center = topography[locations[1, 2], locations[1, 3]]
  if(center == 9) {
    index = (position[2] * nrow(topography) + position[1]) |>
      unname()
    return(index)
  }
  # Get any directions that are 1 step higher than center
  next_steps <- locations[-1, ] |>
    apply(1, \(nbr) {
      nbr <- as.numeric(nbr[2:3])
      topography[nbr[1], nbr[2]] == (center + 1)
    }) |>
    setNames(locations$direction[-1]) |>
    unlist()
  next_steps <- next_steps[next_steps]

  if(length(next_steps) == 0) {
    return(NULL)
  }

  names(next_steps) |> lapply(\(step) {
    step_coords <- locations[-1, ][locations[-1, ]$direction == step, 2:3] |>
      unlist()
    path_finder(topography, step_coords)
  }) |> setNames(names(next_steps)) |>
    unlist()
}

paths = trailheads |>
  apply(1, path_finder, topography = topography)


trailheads = trailheads |>
  data.frame() |>
  dplyr::mutate(score = sapply(paths, \(x) length(unique(x))))

sum(trailheads$score) |> write_answer(part = 1)

# Part 2 ---------------------------------------

# write_answer(part = 2)
