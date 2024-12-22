# Generic function for outputing answer files
write_answer <- function(x, part) {
  out_file <- paste0("answer_", part, ".txt")
  x |>
    as.character() |>
    writeLines(out_file)
}

# Part 1 ---------------------------------------
# How many unique locations within the bounds of the map contain an antinode?

freq_map <- "input.txt" |>
  readLines() |>
  stringr::str_split("", simplify = TRUE)
freq_map[freq_map == "."] = NA

frequencies = freq_map |>
  as.vector() |>
  unique() |>
  sort()

get_antinodes = function(frequencies, freq_map) {
  max_dims = dim(freq_map)
  frequencies |> lapply(\(frequency){
    locs = (freq_map == frequency) |>
      which(arr.ind = TRUE) |>
      as.data.frame() |>
      dplyr::mutate(id = 1:dplyr::n())
    locs |> apply(1, \(loc){
      other_locs = locs[locs$id != loc[3], ]
      other_locs$row = loc[1] + 2 * (other_locs$row - loc[1])
      other_locs$col <- loc[2] + 2 * (other_locs$col - loc[2])
      return(other_locs[, 1:2])
    }) |>
      dplyr::bind_rows() |>
      dplyr::filter(
        row > 0, row <= max_dims[2],
        col > 0, col <= max_dims[1] 
      )
  }) |> setNames(frequencies)
}

antinodes = frequencies |> 
  get_antinodes(freq_map)

antinodes |>
  dplyr::bind_rows() |>
  dplyr::distinct() |> 
  nrow() |>
  write_answer(part = 1)


# Part 2 ---------------------------------------

# write_answer(part = 2)
