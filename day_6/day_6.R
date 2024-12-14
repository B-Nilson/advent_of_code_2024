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

starting_map <- "input.txt" |>
  readLines() |>
  stringr::str_split("", simplify = TRUE)


move_guard <- function(map) {
  guards <- c("^", ">", "v", "<")
  obstructions <- "#"
  guard_loc <- which(array(map %in% guards, dim(map)), arr.ind = TRUE)
  if (nrow(guard_loc) == 0) stop("The guard has left the map.")
  guard <- map[guard_loc]
  guard_loc_old <- guard_loc
  if (guard == "^") {
    path_forward <- map[guard_loc[1]:1, guard_loc[2]]
    nearest_obstruction <- which(path_forward == obstructions)[1]
    if (is.na(nearest_obstruction)) nearest_obstruction <- length(path_forward) + 1
    guard_loc[1] <- guard_loc[1] - nearest_obstruction + 2
    map[guard_loc_old[1]:guard_loc[1], guard_loc[2]] <- "X"
  } else if (guard == ">") {
    path_forward <- map[guard_loc[1], guard_loc[2]:ncol(map)]
    nearest_obstruction <- which(path_forward == obstructions)[1]
    guard_loc[2] <- guard_loc[2] + nearest_obstruction - 2
    map[guard_loc[1], guard_loc_old[2]:guard_loc[2]] <- "X"
  } else if (guard == "v") {
    path_forward <- map[guard_loc[1]:nrow(map), guard_loc[2]]
    nearest_obstruction <- which(path_forward == obstructions)[1]
    guard_loc[1] <- guard_loc[1] + (nearest_obstruction - 2)
    map[guard_loc_old[1]:guard_loc[1], guard_loc[2]] <- "X"
  } else if (guard == "<") {
    path_forward <- map[guard_loc[1], guard_loc[2]:1]
    nearest_obstruction <- which(path_forward == obstructions)[1]
    guard_loc[2] <- guard_loc[2] - nearest_obstruction + 2
    map[guard_loc[1], guard_loc_old[2]:guard_loc[2]] <- "X"
  }

  if (any(guard_loc == 1) | guard_loc[1] == nrow(map) | guard_loc[2] == ncol(map)) {
    return(map)
  }
  next_guard <- (which(guards == guard) + 1) %% 4
  map[guard_loc] <- guards[ifelse(next_guard == 0, 4, next_guard)]
  return(map)
}

travel_map <- starting_map
for (i in 1:1400) {
  travel_map <- move_guard(travel_map)
}

sum(travel_map == "X") |>
  write_answer(part = 1)

# View paths
raster::raster(travel_map == "X") |> raster::plot()

# Part 2 ---------------------------------------

# write_answer(part = 2)
