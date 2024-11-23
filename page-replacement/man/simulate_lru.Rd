\name{simulate_lru}
\alias{simulate_lru}
\title{Simulate LRU Page Replacement Algorithm}
\description{
  This function simulates the LRU (Least Recently Used) page replacement algorithm.
  It takes a reference string and a number of frames and simulates the page loading process, updating the memory frames according to the LRU policy.
}
\usage{
simulate_lru(reference_string, num_frames)
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
  The LRU algorithm replaces the page that has not been used for the longest period of time when a new page needs to be loaded.
  The function keeps track of the order of page accesses and, when a page fault occurs, replaces the least recently used page.
  The simulation continues until all pages in the reference string have been processed.
}
\examples{
reference_string <- c(7, 0, 1, 2, 0, 3, 0, 4, 2, 3, 0, 3, 2, 1, 2, 0, 1, 7, 0, 1)
num_frames <- 3
simulate_lru(reference_string, num_frames)
}
\seealso{
  \code{\link{lru_info}}
}
