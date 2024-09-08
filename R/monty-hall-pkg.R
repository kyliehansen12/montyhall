#' @title
#' Create a new Monty Hall Problem game.
#' @description
#' `create_game()` generates a new game that consists of two doors 
#'  with goats behind them, and one with a car.
#' @details
#' The game setup replicates the game on the TV show "Let's
#' Make a Deal" where there are three doors for a contestant
#' to choose from, one of which has a car behind it and two 
#' have goats. The contestant selects a door, then the host
#' opens a door to reveal a goat, and then the contestant is
#' given an opportunity to stay with their original selection
#' or switch to the other unopened door. There was a famous 
#' debate about whether it was optimal to stay or switch when
#' given the option to switch, so this simulation was created
#' to test both strategies. 
#' @param ... no arguments are used by the function.
#' @return The function returns a length 3 character vector
#' indicating the positions of goats and the car.
#' @examples
#' create_game()
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
} 



#' @title
#' Simulate a contestant's selection of the first door.
#' @description
#' `select_door` utilizes 'sample' to select a number between 1 and 3 
#' that would correspond with the number of doors available to the contestant 
#' to choose from.
#' @details
#' This function is used to represent the contestants first choice of 1 door
#' out of the 3 available doors presented.
#' @param ... no arguments are used by the function.
#' @return The function returns an integer between 1 and 3.
#' @examples
#' select_door()
#' @export
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#' Simulate the host's selection of one other door containing a goat.
#' @description
#' `open_goat_door` utilizes other R functions to randomly select 1 of the 
#' remaining two doors not selected by the contestant to reflect the host's
#' choice of door.
#' @details
#' This function is used to represent the host's choice of 1 door
#' out of the 2 remaining doors presented, ensuring only 'goat' doors are 
#' eliminated by host.
#' @param game Character vector
#' @param a.pick Numeric integer
#' @return The function returns a numeric vector
#' @examples
#' open_goat_door(1:3, 2)
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
#' Determines whether a contestant will decide to choose to keep the same door
#' or choose to "switch" doors.
#' @description
#' `change_door` will filter out the original contestant's door choice and the
#' door selected by the game host and leaves the remaining door, and will 
#' provide the contestant the option to keep their door choice or switch.
#' @details
#' This is the step in the game that will determine whether the contestant
#' wins or loses the game.
#' @param 
#' `opened.door` Numeric integer
#' `a.pick` Numeric integer
#' `doors` Character vector
#' `final.pick` Numeric integer
#' @return 
#' The function will return a character string of either "car" or "goat"
#' @examples
#' change_door(1:3, 2, 1:2, "SWITCH")
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
#' Will provide the results of the game as either a win or loss of the game for 
#' the contestant.
#' @description
#' `determine_winner` will indicate what the contestants final door choice 
#' resulted in - a win or a loss. 
#' @details
#' Checks whether the final value of the contestants choice is "car".
#' @param 
#' `final.pick` Numeric integer
#' @return 
#' A character string or "WIN" or "LOSE"
#' @examples
#' determine_winner("car")
#' determine_winner("goat")
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
#' Runs through game twice, once where contestant keeps original door choice
#' and another game where contestant switches their door choice.
#' @description
#' `play_game()` generates all game elements to determine if switching or 
#' keeping the door results in a win.
#' @details
#' Function imports game outputs and extracts the outcome variable from the 
#' resulting list to indicate whether contestant won or lost.
#' @param 
#' ...no arguments are used by the function.
#' @return List with results of keeping or switching door.
#' @examples
#' play_game()
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
#' Game simulation function.
#' @description
#' `play_n_games` performs a simulation to play through the game and switch or
#' keep the door multiple times to produce an output table to indicate % of 
#' wins or losses for each strategy.
#' @details
#' The function loops through play_game() multiple times and returns data frames
#' of outcomes and propotion of wins and losses for each strategy
#' @param 
#' `n` Integer for number of simulations
#' @return 
#' Proportion table and data frame of outcomes
#' @examples
#' play_n_games(10)
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
