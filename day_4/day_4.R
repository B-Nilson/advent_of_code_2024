# Functions ---------------------------------------

# Generic function for outputing answer files
write_answer <- function(x, part) {
  out_file <- paste0("answer_", part, ".txt")
  x |>
    as.character() |>
    writeLines(out_file)
}

# Part 1 ---------------------------------------
## Look for "XMAS" in a word search

word_search <- "input.txt" |>
  readLines() |>
  stringr::str_split("", simplify = TRUE)


# Rotate a matrix 90 degrees multiple times
rotate <- function(matrix, times = 1) {
  times <- times %% 4
  if (times == 0) {
    return(matrix)
  }
  rot <- \(x) t(apply(x, 2, rev))
  for (i in 1:times) matrix <- rot(matrix)
  return(matrix)
}

# Get diagonals in a single direction for a matrix
get_diagonals <- function(matrix) {
  rows <- nrow(matrix)
  seq(-rows + 1, rows - 1) |>
    sapply(\(i) {
      if (i >= 0) {
        diag <- (1:(rows - i)) |>
          sapply(\(j) matrix[j, j + i])
      } else {
        diag <- (1:(rows + i)) |>
          sapply(\(j) matrix[j - i, j])
      }
      diag |> paste(collapse = "")
    })
}

# Get all vertical, horizontal, and diagonal text
verticals <- word_search |>
  cbind(rotate(word_search, 2)) |>
  apply(2, paste, collapse = "")

horizontals <- word_search |>
  rbind(rotate(word_search, 2)) |>
  apply(1, paste, collapse = "")

diagonals <- 0:3 |>
  lapply(\(times) word_search |>
    rotate(times) |>
    get_diagonals()) |>
  unlist()

# Combine and count occurences of "XMAS"
c(verticals, horizontals, diagonals) |>
  stringr::str_count("XMAS") |>
  sum() |>
  write_answer(part = 1)

# Part 2 ---------------------------------------
## Look for "X-MAS" (as in two "MAS" shaped like an "X")

# Check if a matrix location matches a X-MAS pattern
check_if_x_mas <- function(matrix, x, y) {
  # Predefined X-MAS patterns
  patterns <- list(
    c("M.S", ".A.", "M.S"),
    c("M.M", ".A.", "S.S"),
    c("S.S", ".A.", "M.M"),
    c("S.M", ".A.", "S.M")
  )
  # Handle edge cases
  rows <- (x - 1):(x + 1)
  cols <- (y - 1):(y + 1)
  invalid_rows <- rows < 0 | rows > nrow(matrix)
  invalid_cols <- cols < 0 | cols > ncol(matrix)
  if (any(invalid_rows | invalid_cols)) {
    return(FALSE)
  }
  # Check if this location matches any patterns
  sample <- matrix[rows, cols] |> paste(collapse = "")
  patterns <- patterns |> sapply(paste, collapse = "")
  patterns |>
    sapply(\(pattern) sample |> stringr::str_detect(pattern)) |>
    any()
}

# Count locations centered on "A" with matches to X-MAS patterns
which(word_search == "A", arr.ind = TRUE) |>
  apply(1, \(a_loc) word_search |>
    check_if_x_mas(a_loc[1], a_loc[2])) |>
  sum() |>
  write_answer(part = 2)
