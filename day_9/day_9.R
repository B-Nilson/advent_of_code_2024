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

get_blocks <- function(disk_map, unlist = TRUE) {
  block_sizes <- disk_map[seq(1, length(disk_map), 2)]
  empty_sizes <- disk_map[seq(2, length(disk_map), 2)]
  if (length(empty_sizes) < length(block_sizes)) {
    empty_sizes <- c(empty_sizes, 0)
  }
  blocks <- 1:length(block_sizes) |>
    sapply(\(id) rep(id - 1, block_sizes[id]) |>
      c(rep(NA, empty_sizes[id])))
  if (unlist) {
    blocks <- blocks |>
      unlist()
  }
  return(blocks)
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
  blocks <- blocks[!is.na(blocks)]
  sum(blocks * ((1:length(blocks)) - 1))
}

sorted_blocks <- disk_map |>
  get_blocks() |>
  sort_blocks()

sorted_blocks |>
  get_checksum() |>
  write_answer(part = 1)

# Part 2 ---------------------------------------

sort_full_blocks <- function(blocks) {
  get_gap_width <- function(blocks) {
    blocks |> sapply(\(block) sum(is.na(block)))
  }
  names(blocks) <- (1:length(blocks)) - 1
  for (id in as.character(1518:0)) {
    print(id)
    gap_widths <- get_gap_width(blocks)
    width <- sum(blocks[[id]] == id, na.rm = TRUE)
    values = blocks[[id]][!is.na(blocks[[id]])]
    new_pos = which(gap_widths >= width)[1]
    if(is.na(new_pos)) next
    blocks[[new_pos]] = c(
      blocks[[new_pos]][!is.na(blocks[[new_pos]])],
      values,
      blocks[[new_pos]][is.na(blocks[[new_pos]])][-(1:width)]
    )
    blocks[[id]] = blocks[[id]][blocks[[id]] != id]
  }

  return(blocks)
}

sorted_blocks <- disk_map |>
  get_blocks(unlist = FALSE) |>
  sort_full_blocks()

sorted_blocks |>
  unlist() |>
  get_checksum() |>
  write_answer(part = 2)
