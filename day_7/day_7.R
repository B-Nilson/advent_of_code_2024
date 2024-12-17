# Generic function for outputing answer files
write_answer <- function(x, part) {
  out_file <- paste0("answer_", part, ".txt")
  x |>
    as.character() |>
    writeLines(out_file)
}

# Part 1 ---------------------------------------
## Determine which equations could possibly be true. 
## What is their total calibration result?

input <- "input.txt" |>
  readLines()

check_equation = function(answer, values, operators) {
  add_brackets = function(x) {
    paste0("(", x, ")") # so evals from left -> right
  }
  # Init results with first value
  results = data.frame(
    expression = values[1] |> add_brackets()
  )
  # Handle first value larger than answer
  if(values[1] > answer){
    return(FALSE)
  }
  # For all other values
  for(i in 2:length(values)){
    results = results |>
      # Expand out all combos of operators
      dplyr::mutate(next_value = values[i]) |>
      tidyr::expand(
        tidyr::nesting(expression, next_value),
        operator = operators) |>
      # Add to the expression(s) and retotal
      dplyr::mutate(
        expression = expression |>
          paste(operator, next_value) |>
          add_brackets(),
        total = expression |>
          sapply(\(e) eval(parse(text = e)))
    ) |> 
      # Drop any that exceed the expected total 
      dplyr::filter(total <= answer)
  }
  # Evaluate if answer possible
  answer %in% results$total
}

operators = c("+", "*")
input |>
  sapply(\(line){
    inputs = line |>
      stringr::str_split(": | ", simplify = TRUE) |>
      as.numeric()
    is_valid = check_equation(
      answer = inputs[1], 
      values = inputs[-1], 
      operators
    )
    is_valid |> ifelse(inputs[1], 0)
  }) |>
  sum() |>
  write_answer(part = 1)

# Part 2 ---------------------------------------

# write_answer(part = 2)
