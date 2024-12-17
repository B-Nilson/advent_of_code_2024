# Generic function for outputing answer files
write_answer <- function(x, part) {
  out_file <- paste0("answer_", part, ".txt")
  x |>
    as.character() |>
    writeLines(out_file)
}

eval_math = function(x){
  parse(text = paste0(
       "c(",
       x |> stringi::stri_c(collapse = ", "),
       ")"
      )) |> 
    eval()
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
    expression = values[1] |> add_brackets(),
    full_expresion = values[1] |> add_brackets()
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
        tidyr::nesting(expression, full_expresion, next_value),
        operator = operators) |>
      # Add to the expression(s) and retotal
      dplyr::mutate(
        full_expresion = full_expresion |>
          paste(operator, next_value) |>
          add_brackets(),
        expression = expression |>
          paste(operator, next_value) |>
          add_brackets() |>
          handle_concat()
      )
    results = results |>
      dplyr::mutate(total = expression |> eval_math()) |> 
      # Drop any that exceed the expected total 
      dplyr::filter(total <= answer)
    if(answer %in% results$total) {
      break
    }
  }
  # Evaluate if answer possible
  answer %in% results$total
}

operators = c("+", "*")
valid_values = input |>
  sapply(\(line){
    cat(line, "\n")
    inputs = line |>
      stringr::str_split(": | ", simplify = TRUE) |>
      as.numeric()
    is_valid = check_equation(
      answer = inputs[1], 
      values = inputs[-1], 
      operators
    )
    is_valid |> ifelse(inputs[1], 0)
  })
valid_values |>
  sum() |>
  write_answer(part = 1)

# Part 2 ---------------------------------------

handle_concat = function(x, concat = "\\|\\|") {
  has_concat = stringr::str_detect(x, concat)
  if(!any(has_concat)) {
    return(x)
  }
  # Eval sides of concat entries
  sides = x[has_concat] |>
    stringr::str_split(concat, simplify = TRUE)
  sides[, 1] = sides[, 1] |> 
    paste0(")") |>
    eval_math()
  sides[, 2] = sides[, 2] |> 
    stringr::str_remove_all("\\)$|^ ")
  # Insert pasted entries
  x[has_concat] = paste0(
    sides[, 1],
    sides[, 2]
  )
  return(x)
}

operators = c("+", "*", "||")
valid_concat_values = input[valid_values == 0] |>
  sapply(\(line){
    cat(line, "\n")
    inputs = line |>
      stringr::str_split(": | ", simplify = TRUE) |>
      as.numeric()
    if(sum(inputs[-1] > inputs[1])) return(0)
    is_valid = check_equation(
      answer = inputs[1], 
      values = inputs[-1], 
      operators
    )
    is_valid |> ifelse(inputs[1], 0)
  })

(sum(valid_concat_values) + sum(valid_values)) |>
  write_answer(part = 2)
