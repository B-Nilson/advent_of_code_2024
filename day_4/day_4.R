write_answer <- function(x, part) {
  out_file <- paste0("answer_", part, ".txt")
  x |>
    as.character() |>
    writeLines(out_file)
}

get_diagonals <- function(matrix) {
  ndim <- nrow(matrix)
  offsets <- seq(-ndim + 1, ndim - 1)
  offsets |>
    lapply(\(i) {
      if (i >= 0) { # Extract diagonal above or on the main diagonal
        (1:(ndim - i)) |>
          sapply(\(j) matrix[j, j + i]) |>
          paste(collapse = "")
      } else { # Extract diagonal below the main diagonal
        (1:(ndim + i)) |>
          sapply(\(j) matrix[j - i, j]) |>
          paste(collapse = "")
      }
    }) |>
    unlist()
}

rotate <- function(matrix, times = 1) {
  times <- times %% 4
  if (times == 0) {
    return(matrix)
  }
  rot <- \(x) t(apply(x, 2, rev))
  for (i in 1:times) matrix <- rot(matrix)
  return(matrix)
}

# Part 1 ---------------------------------------

input <- "input.txt" |>
  readLines()

word_search <- input |>
  stringr::str_split("", simplify = TRUE)

verticals <- word_search |>
  # Include right -> left
  cbind(word_search[nrow(word_search):1, ]) |>
  apply(2, paste, collapse = "")

horizontals <- word_search |>
  # Include bottom -> top
  rbind(word_search[, ncol(word_search):1]) |>
  apply(1, paste, collapse = "")

diagnols <- 0:3 |> # Get all 4 sets of diagnols
  lapply(\(times) word_search |>
    rotate(times) |> 
    get_diagonals()) |>
  unlist()

# Combine and count occurences of "XMAS"
c(verticals, horizontals, diagnols) |>
  stringr::str_count("XMAS") |>
  sum() |>
  write_answer(part = 1)

# Part 2 ---------------------------------------
