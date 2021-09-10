#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors 
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two 
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous 
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies. 
#'
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function( )
{
  a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
  return( a.game )
}



#' @title
#' Contestant selects a door.
#'
#' @description
#' `select_door()` generates a random door position to open.
#'
#' @details
#' In the Monty Hall Problem there are three doors. A car is behind one door and goats are behind
#' the other two doors. The contestant must pick a door to open. This function simulates the
#' contestant selecting one of the three doors.
#'
#' @param ... no arguments are used by the function.
#'
#' @return
#' This function returns a single object containing the position of the selected door: 1, 2, or 3.
#'
#' @examples
#' select_door()
#'
#' @export
select_door <- function( )
{
  doors <- c(1,2,3)
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#' Host opens one of the goat doors.
#'
#' @description
#' `open_goat_door()` simulates the game show host opening a door that the contestant has not
#' opened and that has a goat behind it.
#'
#' @details
#' In the Monty Hall Problem, once the contestant has opened a door, the game show host opens
#' one of the closed doors that has a goat behind it.
#'
#' @param game
#' A vector containing one "car" and two "goat" strings in any configuration.
#' @param a.pick
#' The position of the selected door, a number between 1 and 3.
#'
#' @return
#' This function returns a single object containing the position of the opened goat door: 1, 2, or 3.
#'
#' @examples
#' game <- create_game()
#' a.pick <- select_door()
#' open_goat_door(game, a.pick)
#'
#' @export
open_goat_door <- function( game, a.pick )
{
  doors <- c(1,2,3)
  # if contestant selected car,
  # randomly select one of two goats
  if( game[ a.pick ] == "car" )
  {
    goat.doors <- doors[ game != "car" ]
    opened.door <- sample( goat.doors, size=1 )
  }
  if( game[ a.pick ] == "goat" )
  {
    opened.door <- doors[ game != "car" & doors != a.pick ]
  }
  return( opened.door ) # number between 1 and 3
}



#' @title
#' Contestant stays or switches.
#'
#' @description
#' `change_door()` simulates the contestant choosing to either stay with their initial door
#' selection or switching.
#'
#' @details
#' In the Monty Hall Problem, once the contestant has opened a door and the host has opened
#' one of the door with a goat behind it, the contestant must choose to either stay with their
#' original door selection or switch to the unopened door.
#'
#' @param stay
#' A boolean object representing TRUE if the contestant stays with their initial door pick or
#' FALSE if the contestant switches to the unopened door.
#' @param opened.door
#' The position of the door opened by the host with a goat behind it, a number between 1 and 3.
#' @param a.pick
#' The position of the contestant's initial door select, a number between 1 and 3.
#'
#' @return
#' This function returns the position of the contestant's final choice after either staying
#' or switching: 1, 2, or 3.
#'
#' @examples
#' game <- create_game()
#' a.pick <- select_door()
#' opened.door <- open_goat_door(game, a.pick)
#' change_door( stay = T, opened.door, a.pick)
#'
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
  doors <- c(1,2,3)

  if( stay )
  {
    final.pick <- a.pick
  }
  if( ! stay )
  {
    final.pick <- doors[ doors != opened.door & doors != a.pick ]
  }

  return( final.pick )  # number between 1 and 3
}



#' @title
#' Game outcome is determined.
#'
#' @description
#' `determine_winner()` simulates whether the contestant has won or lost the game.
#'
#' @details
#' In the Monty Hall Problem, once the contestant has opened a door and the host has opened
#' one of the doors with a goat behind it, the contestant must choose to either stay with their
#' original door selection or switch to the unopened door. If the final selection has a "car"
#' behind it then the contestant has won. If the final selection has a "goat" behind it, then the
#' contestant has lost.
#'
#' #' @param final.pick
#' The position of the contestant's final choice after either staying or switching, a number 1-3.
#' @param game
#' A vector containing one "car" and two "goat" strings in any configuration.
#'
#' @return
#' This function returns a string indicating contestant's outcome in the game: "WIN" or "LOSE".
#'
#' @examples
#' game <- create_game()
#' a.pick <- select_door()
#' opened.door <- open_goat_door(game, a.pick)
#' final.pick <- change_door( stay = T, opened.door, a.pick)
#' determine_winner(final.pick, game)
#'
#' @export
determine_winner <- function( final.pick, game )
{
  if( game[ final.pick ] == "car" )
  {
    return( "WIN" )
  }
  if( game[ final.pick ] == "goat" )
  {
    return( "LOSE" )
  }
}



#' @title
#' Monty Hall game simulation.
#'
#' @description
#' `play_game()` simulates an entire Monty Hall game with both stay and switch strategies.
#'
#' @details
#' This function plays the Monty Hall game by generating the doors, a contestant selecting
#' a door, the host opening a goat door, the contestant choosing a stay or switch strategy,
#' and determining whether the contestant has won based on their strategy.
#'
#' @param ... no arguments are used by the function.
#'
#' @return
#' This function returns a data frame containing the strategy (switch or stay) and the game
#' outcome (win or lose) as a result of the strategy. .
#'
#' @examples
#' play_game()
#'
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )
  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )
  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )

  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}



#' @title
#' Monty Hall Problem game simulation of "n" games
#'
#' @description
#' `play_n_games()` simulates the Monty Hall Problem game for a series of 'n' games.
#'
#' @details
#' This function plays the Monty Hall Problem game for as many iterations as the user enters.
#'
#' @param
#' n An integer representing the number of games to play.
#'
#' @return
#' This function returns a table of the game winning and losing probabilities based on the
#' number of game plays and the results of each round in a dataframe.
#'
#' @examples
#' play_n_games(n = 150)
#'
#' @export
play_n_games <- function( n=100 )
{

  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome
    loop.count <- loop.count + 1
  }

  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>%
    prop.table( margin=1 ) %>%  # row proportions
    round( 2 ) %>%
    print()

  return( results.df )

}
