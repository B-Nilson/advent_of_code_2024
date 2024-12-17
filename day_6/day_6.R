# Generic function for outputing answer files
write_answer <- function(x, part) {
  out_file <- paste0("answer_", part, ".txt")
  x |>
    as.character() |>
    writeLines(out_file)
}

# Part 1 ---------------------------------------
## Predict the path of the guard.
## How many distinct positions will the guard visit
##   before leaving the mapped area?

# Load map as matrix of individual character

starting_map <- "input.txt" |>
  readLines() |>
  stringr::str_split("", simplify = TRUE)

get_guard_loc <- function(map, guards) {
  (map %in% guards) |>
    array(dim(map)) |>
    which(arr.ind = TRUE)
}

get_path_forward <- function(map, guard, guard_loc) {
  if (guard == "^") {
    map[guard_loc[1]:1, guard_loc[2]]
  } else if (guard == ">") {
    map[guard_loc[1], guard_loc[2]:ncol(map)]
  } else if (guard == "v") {
    map[guard_loc[1]:nrow(map), guard_loc[2]]
  } else if (guard == "<") {
    map[guard_loc[1], guard_loc[2]:1]
  }
}

updated_path_walked <- function(map, guard, guard_loc_old, guard_loc) {
  if (guard == "^") {
    rows = guard_loc_old[1]:guard_loc[1]
    map[rows, guard_loc[2]] <- 1:length(rows)
  } else if (guard == ">") {
    map[guard_loc[1], guard_loc_old[2]:guard_loc[2]] <- values
  } else if (guard == "v") {
    map[guard_loc_old[1]:guard_loc[1], guard_loc[2]] <- values
  } else if (guard == "<") {
    map[guard_loc[1], guard_loc_old[2]:guard_loc[2]] <- values
  }
  return(map)
}

get_next_guard <- function(guard, guards) {
  next_guard <- (which(guards == guard) + 1) %% length(guards)
  guards[ifelse(next_guard == 0, length(guards), next_guard)]
}

move_guard <- function(map) {
  # Get current coords of guard and direction she faces
  guards <- c("^", ">", "v", "<")
  guard_loc <- map |> get_guard_loc(guards)
  if (nrow(guard_loc) == 0) stop("The guard has left the map.")
  guard <- map[guard_loc]
  # Determine nearest obstruction in direction she faces
  path_forward <- map |>
    get_path_forward(guard, guard_loc)
  obstructions <- "#"
  nearest_obstruction <- which(path_forward == obstructions)[1]
  if (is.na(nearest_obstruction)) {
    # Handle guard exiting
    nearest_obstruction <- length(path_forward) + 1
    next_guard <- "X"
  } else {
    # If guard is still on map, rotate her 90 degrees
    next_guard <- guard |> get_next_guard(guards)
  }

  # Move guard to just before nearest obstruction, replace path with "X"
  guard_loc_old <- guard_loc
  if (guard == "^") {
    guard_loc[1] <- guard_loc[1] - nearest_obstruction + 2
  } else if (guard == ">") {
    guard_loc[2] <- guard_loc[2] + nearest_obstruction - 2
  } else if (guard == "v") {
    guard_loc[1] <- guard_loc[1] + (nearest_obstruction - 2)
  } else if (guard == "<") {
    guard_loc[2] <- guard_loc[2] - nearest_obstruction + 2
  }
  map <- map |> updated_path_walked(guard, guard_loc_old, guard_loc)
  map[guard_loc] <- next_guard

  return(map)
}

travel_map <- starting_map
while (TRUE) {
  travel_map <- move_guard(travel_map)
}

sum(travel_map %in% as.character(1:(10^6))) |>
  write_answer(part = 1)

# View paths
raster::raster(travel_map == "X") |> raster::plot()

# Part 2 ---------------------------------------
## You need to get the guard stuck in a loop by adding a single new obstruction.
## How many different positions could you choose for this obstruction?

# Find all rectangles numbers on the travel map
# Filter to sequential rectangles
# 1. look for all but 1 value (the end) being sequential
# Identify start/stop point directions
# 1. use increasing numbers instead of X for path
# 2. look for corner in rectangle with non-sequential numbers
# 3. sequential direction is ending dir, other is starting
# Mark if loop possible
# 1. only possible if 

# write_answer(part = 2)
