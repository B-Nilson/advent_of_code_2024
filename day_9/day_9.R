# Generic function for outputing answer files
write_answer <- function(x, part) {
  out_file <- paste0("answer_", part, ".txt")
  x |>
    as.character() |>
    writeLines(out_file)
}

# Part 1 ---------------------------------------

disk_map <- "input.txt" |>
  readLines() |>
  stringr::str_split("", simplify = TRUE) |>
  as.integer()

get_blocks <- function(disk_map) {
  block_sizes <- disk_map[seq(1, length(disk_map), 2)]
  empty_sizes <- disk_map[seq(2, length(disk_map), 2)]
  if (length(empty_sizes) < length(block_sizes)) {
    empty_sizes <- c(empty_sizes, 0)
  }
  1:length(block_sizes) |>
    sapply(\(id) rep(id - 1, block_sizes[id]) |>
      c(rep(NA, empty_sizes[id]))
    ) |>
    unlist()
}

sort_blocks <- function(blocks) {
  get_last_digit <- \(x) which(!is.na(x)) |> dplyr::last()
  get_first_space <- \(x) which(is.na(x)) |> dplyr::first()
  is_sorted <- get_last_digit(blocks) < get_first_space(blocks)

  while (!is_sorted) {
    last_digit <- blocks |> get_last_digit()
    first_space <- blocks |> get_first_space()
    cat("Gap =", last_digit - first_space, "\n")
    blocks[first_space] <- blocks[last_digit]
    blocks[last_digit] <- NA
    is_sorted <- last_digit < first_space
  }
  return(blocks)
}

get_checksum <- function(blocks) {
  blocks = blocks[!is.na(blocks)]
  sum(blocks * ((1:length(blocks)) - 1))
}

sorted_blocks = disk_map |>
  get_blocks() |>
  sort_blocks()

sorted_blocks |> 
  get_checksum() |>
  write_answer(part = 1)

# Part 2 ---------------------------------------

# write_answer(part = 2)
