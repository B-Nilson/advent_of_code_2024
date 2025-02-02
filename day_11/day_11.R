# Generic function for outputing answer files
write_answer <- function(x, part) {
  out_file <- paste0("answer_", part, ".txt")
  x |>
    as.character() |>
    writeLines(out_file)
}

ndigits <- function(x) {
  x <- log10(x) %/% 1 + 1
  x[x < 1] <- 1
  return(x)
}

split_numbers <- function(numbers, digits) {
  denom <- 10^(digits - digits / 2)
  start <- as.integer(numbers / denom)
  end <- numbers - start * denom
  c(start, end)
}

combine_counts <- function(counts) {
  values <- names(counts)
  is_duplicated <- values %in% values[duplicated(values)]
  if (all(!is_duplicated)) {
    return(counts)
  }
  # combine duplicates
  duplicates <- counts[is_duplicated]
  values <- names(duplicates)
  unique(values) |>
    sapply(\(value) sum(duplicates[values == value])) |>
    c(counts[!is_duplicated])
}

blink_stones <- function(counts) {
  if (is.null(names(counts))) counts <- table(counts)
  values <- names(counts) |> as.numeric()
  digits <- ndigits(values)
  zeroes <- values == 0 # rule 1
  evens <- digits %% 2 == 0 # rule 2
  others <- !zeroes & !evens # rule 3
  # apply rule 1 and 3
  new_counts <- counts
  names(new_counts)[zeroes] <- 1
  names(new_counts)[others] <- values[others] * 2024
  if (!any(evens)) {
    return(new_counts)
  }
  # handle rule 2 if needed
  split_values <- values[evens] |>
    split_numbers(digits[evens])
  split_counts <- rep(counts[evens], 2) |>
    setNames(split_values)
  old_entries <- names(counts) %in% values[evens]
  new_counts[!old_entries] |>
    c(split_counts) |>
    combine_counts()
}

# Part 1 ---------------------------------------

input <- "input.txt" |>
  readLines() |>
  stringr::str_split(" ", simplify = TRUE) |>
  as.integer()

blinks <- 25
stones <- input
for (blink in 1:blinks) {
  stones <- blink_stones(stones)
}

sum(stones) |> write_answer(part = 1)

# Part 2 ---------------------------------------

blinks <- 75
stones <- input
for (blink in 1:blinks) {
  stones <- blink_stones(stones)
}

sum(stones) |> write_answer(part = 2)
