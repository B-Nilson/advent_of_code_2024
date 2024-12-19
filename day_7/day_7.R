# Functions ------------------------------------

# Generic function for outputing answer files
write_answer <- function(x, part) {
  out_file <- paste0("answer_", part, ".txt")
  x |>
    as.character() |>
    writeLines(out_file)
}

split <- function(x, sep, ...) {
  x |> stringr::str_split(sep, simplify = TRUE, ...)
}

eval_math <- function(x) {
  parse(text = paste0(
    "c(",
    x |> stringi::stri_c(collapse = ", "),
    ")"
  )) |>
    eval()
}

handle_concat <- function(x, concat = "\\|\\|") {
  has_concat <- x |> stringr::str_detect(concat)
  if (!any(has_concat)) {
    return(x)
  }
  # Eval sides of concat entries
  sides <- x[has_concat] |> split(concat)
  sides[, 1] <- sides[, 1] |>
    paste0(")") |>
    eval_math()
  sides[, 2] <- sides[, 2] |>
    stringr::str_remove_all("\\)$|^ ")
  # Insert pasted entries
  x[has_concat] <- sides[, 1] |>
    paste0(sides[, 2])
  return(x)
}

check_equation <- function(answer, values, operators) {
  add_brackets <- function(x) {
    paste0("(", x, ")") # so evals from left -> right
  }
  # Init results with first value
  results <- data.frame(
    expression = values[1] |> add_brackets()
  )
  # Handle easy cases
  if (values[1] > answer) {
    return(FALSE)
  }
  # For all other values
  for (i in 2:length(values)) {
    results <- results |>
      # Expand out all combos of operators
      dplyr::mutate(next_value = values[i]) |>
      tidyr::expand(
        tidyr::nesting(expression, next_value),
        operator = operators
      ) |>
      # Add to the expressions
      dplyr::mutate(
        expression = expression |>
          paste(operator, next_value) |>
          add_brackets() |>
          handle_concat(),
        total = expression |> eval_math()
      ) |> 
      # Drop any that exceed the expected total 
      dplyr::filter(total <= answer)
  }
  # Evaluate if answer possible
  answer %in% results$total
}

# Part 1 ---------------------------------------
## Determine which equations could possibly be true.
## What is their total calibration result?

input <- "input.txt" |>
  readLines()

operators <- c("+", "*")
valid_values <- input |>
  sapply(\(line){
    inputs <- line |>
      split(": | ") |>
      as.numeric()
    check_equation(
      answer = inputs[1],
      values = inputs[-1],
      operators
    ) |> ifelse(inputs[1], 0)
  })

valid_values |>
  sum() |>
  write_answer(part = 1)

# Part 2 ---------------------------------------

operators <- c("+", "*", "||")
valid_concat_values <- input[valid_values == 0] |>
  sapply(\(line){
    inputs <- line |>
      split(": | ") |>
      as.numeric()
    check_equation(
      answer = inputs[1],
      values = inputs[-1],
      operators
    ) |> ifelse(inputs[1], 0)
  })

(sum(valid_concat_values) + sum(valid_values)) |>
  write_answer(part = 2)
