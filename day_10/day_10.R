# Generic function for outputing answer files
write_answer <- function(x, part) {
  out_file <- paste0("answer_", part, ".txt")
  x |>
    as.character() |>
    writeLines(out_file)
}

path_finder <- function(topography, position) {
  # Get all possible neigbours
  neighbours <- data.frame(
    direction = c("N", "E", "S", "W"),
    row = position[1] + c(1, 0, -1, 0),
    col = position[2] + c(0, 1, 0, -1)
  ) |>
    dplyr::filter(
      row > 0 & row <= nrow(topography),
      col > 0 & col <= ncol(topography)
    )

  # If center is 9, return its index
  center <- topography[t(position)]
  if (center == 9) {
    index <- unname(position[2] * nrow(topography) + position[1])
    return(index)
  }

  # Get any directions that are 1 step higher than center
  next_steps <- neighbours |>
    apply(1, \(nbr) {
      loc <- as.numeric(nbr[2:3])
      topography[t(loc)] == (center + 1)
    }) |>
    setNames(neighbours$direction) |>
    unlist()
  next_steps <- next_steps[next_steps]

  if (length(next_steps) == 0) {
    return(NULL) # end path if no next steps
  } else {
    # Iterate through the next steps until path ends
    # return vector of path ending indices named N.S.S.S.E.E.E.N.W (for example)
    names(next_steps) |>
      setNames(names(next_steps)) |>
      lapply(\(step) {
        row <- neighbours$direction == step
        unlist(neighbours[row, 2:3]) |>
          path_finder(topography = topography)
      }) |>
      unlist()
  }
}

# Part 1 ---------------------------------------

topography <- "input.txt" |>
  readLines() |>
  stringr::str_split("") |>
  sapply(as.integer) |>
  t()

paths <- (topography == 0) |>
  which(arr.ind = TRUE) |>
  apply(1, path_finder, topography = topography)

scores <- paths |>
  sapply(\(x) length(unique(x)))

sum(scores) |> write_answer(part = 1)

# Part 2 ---------------------------------------

ratings <- sapply(paths, length)

sum(ratings) |> write_answer(part = 2)
