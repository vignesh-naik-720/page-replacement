
initialize_frames <- function(num_frames) {
  vector("list", num_frames)
}

is_page_in_frames <- function(page, frames) {
  page %in% frames
}


find_optimal_index <- function(frames, reference_string, current_index) {

  future_usage <- sapply(frames, function(frame) {

    next_occurrence <- which(reference_string[(current_index + 1):length(reference_string)] == frame)


    if (length(next_occurrence) == 0) return(Inf)


    return(next_occurrence[1] + current_index)
  })

  return(which.max(future_usage))
}



draw_frames <- function(frames, step, current_page, is_fault, reason) {
  cat("\nStep:", step, "| Current Page:", current_page, "| Page Fault:", ifelse(is_fault, "Yes", "No"), "\n")
  if (is_fault) cat("Reason:", reason, "\n")

  for (frame in frames) cat("+-----+ ", sep = "")
  cat("\n")


  for (frame in frames) {
    if (!is.null(frame)) cat(sprintf("| %-3d | ", frame), sep = "")
    else cat("|     | ", sep = "")
  }
  cat("\n")


  for (frame in frames) cat("+-----+ ", sep = "")
  cat("\n")
  Sys.sleep(0.5)
}

simulate_optimal <- function(reference_string, num_frames) {
  frames <- initialize_frames(num_frames)
  page_faults <- 0
  for (i in seq_along(reference_string)) {
    current_page <- reference_string[i]
    reason <- ""
    is_fault <- FALSE

    if (is_page_in_frames(current_page, frames)) {
      reason <- "Page already in frames."
    } else {
      page_faults <- page_faults + 1
      is_fault <- TRUE

      if (any(sapply(frames, is.null))) {
        empty_index <- which(sapply(frames, is.null))[1]
        frames[[empty_index]] <- current_page
        reason <- paste("Inserted page", current_page, "into an empty frame (frame", empty_index, ").")
      } else {
        optimal_index <- find_optimal_index(frames, reference_string, i)
        optimal_page <- frames[[optimal_index]]
        frames[[optimal_index]] <- current_page
        reason <- paste("Replaced page", optimal_page, "(frame", optimal_index, ") with page", current_page, "using Optimal algorithm.")
      }
    }

    draw_frames(frames, step = i, current_page = current_page, is_fault = is_fault, reason = reason)
  }

  cat("\nSimulation Complete\n")
  cat("Total Page Faults:", page_faults, "\n")
}
optimal_info <- function() {
  cat("Optimal Page Replacement Algorithm\n")
  cat("--------------------------------\n")
  cat("Description: The optimal page replacement algorithm replaces the page that will not be used for the longest period of time in the future. It gives the best possible page replacement decision, but it requires knowledge of the future reference string, which is usually not available.\n")
  cat("Invented by: The optimal page replacement algorithm was first introduced by Belady in 1966.\n")
  cat("Algorithm Steps:\n")
  cat("1. When a page fault occurs, look ahead at the future reference string to determine which page will not be used for the longest time.\n")
  cat("2. Replace the page that will be used the farthest in the future.\n")
  cat("3. If there is space available in memory, simply load the new page into memory.\n")
}

