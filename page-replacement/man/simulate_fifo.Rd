\name{simulate_fifo}
\alias{simulate_fifo}
\title{Simulate FIFO Page Replacement Algorithm}
\description{
  This function simulates the FIFO (First-In-First-Out) page replacement algorithm.
  It takes a reference string and a number of frames and simulates the page loading process, updating the memory frames according to the FIFO policy.
}
\usage{
simulate_fifo(reference_string, num_frames)
}
\arguments{
  \item{reference_string}{A vector of integers representing the reference string (the sequence of page accesses).}
  \item{num_frames}{An integer specifying the number of frames (memory slots) available.}
}
\value{
  The function returns the total number of page faults that occurred during the simulation.
  The frames are displayed step by step, with page faults highlighted.
}
\details{
  The FIFO algorithm works by replacing the page that has been in memory the longest when a new page needs to be loaded.
  In case a page is already in memory, no replacement occurs.
  The function uses a queue to manage the pages in memory and handles both the case where there are empty frames and the case where all frames are filled.
}
\examples{
reference_string <- c(7, 0, 1, 2, 0, 3, 0, 4, 2, 3, 0, 3, 2, 1, 2, 0, 1, 7, 0, 1)
num_frames <- 3
simulate_fifo(reference_string, num_frames)
}
\seealso{
  \code{\link{fifo_info}}
}
