% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/monty_hall_scripts.R
\name{play_game}
\alias{play_game}
\title{Monty Hall game simulation.}
\usage{
play_game()
}
\arguments{
\item{...}{no arguments are used by the function.}
}
\value{
This function returns a data frame containing the strategy (switch or stay) and the game
outcome (win or lose) as a result of the strategy. .
}
\description{
\code{play_game()} simulates an entire Monty Hall game with both stay and switch strategies.
}
\details{
This function plays the Monty Hall game by generating the doors, a contestant selecting
a door, the host opening a goat door, the contestant choosing a stay or switch strategy,
and determining whether the contestant has won based on their strategy.
}
\examples{
play_game()

}
