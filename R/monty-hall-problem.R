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
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
} 



#' @title
#'   The contestant selects a door. 
#'   
#' @description
#'   `select_door()` generates a door selection and the contestant     
#'   randomly chooses from.
#'
#' @details
#'    This function creates a vector of doors numbered 1, 2, 3. 
#'    Three doors that one contestant can choose from. Then the 
#'    contestant randomly selects ONE of the doors. The function 
#'    returns the selection that the contestant has made, the door
#'    number between 1 and 3.   
#' 
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a length 1 numeric vector
#'   indicating the door position or number that one contestant 
#'   initially selects.
#'   
#' @examples
#'   select_door()
#' 
#' @export
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}




#' @title
#'   The host opens a goat door. 
#'   
#' @description
#'   `open_goat_door()` returns the position of a goat door that the host 
#'    reveals after the contestant's initial random decision. The host will 
#'    always open a door with a goat behind it. But the door can't be the one
#'    the contestant has selected.
#'
#' @details
#'    This function first creates a vector of doors numbered 1, 2, 3. Then if 
#'    the contestant selects a car door, the function will return the position 
#'    of a goat door selected by the host from any of the two remaining 
#'    doors that contain the goat. If the contestant selects a non-car door 
#'    at the first place, the function will return the only goat door 
#'    position that left for the host.
#' 
#' @param game character vector, length 3, any combination of one "car" and 
#'   two "goat". 
#' @param a.pick numeric vectors, length 1, between 1 and 3.
#' 
#' @return The function returns a length 1 numeric vector
#'   indicating the goat door position that the host reveals. 
#'   
#' @examples
#'   open_goat_door( c("goat","goat","car"), 3 )
#'   open_goat_door( c("car","goat","goat"), 1 )
#'   open_goat_door( c("goat","car","goat"), 2 )
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
#'   Change the door. 
#'   
#' @description
#'   `change_door()` returns the final contestant's decision on the door
#'    position.
#'    
#' @details
#'    The contestant is given the option to change from their initial 
#'    selection to the other door that is still closed. This function first 
#'    creates a vector of doors numbered 1, 2, 3. Then if the contestant 
#'    decides to stay, the function will return their final selection of the 
#'    position on the door to open, which is the same as their initial choice. 
#'    If the contestant switches, the returned final selection from this 
#'    function would be the door that has not yet been opened nor has been not 
#'    selected by the contestant.
#' 
#' @param stay logical vector, default to true.
#' @param opened.door numeric vectors, length 1, between 1 and 3, cannot 
#'    equal to a.pick.
#' @param a.pick numeric vectors, length 1, between 1 and 3, cannot 
#'    equal to opened.door.
#' 
#' @return The function returns a length 1 numeric vector
#'   indicating the contestant's final selection. 
#'   
#' @examples
#'   change_door( T, 1, 2 )
#'   change_door( F, 1, 2 )
#'   change_door( T, 2, 3 )
#'   change_door( F, 2, 3 )
#' 
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c( 1, 2, 3 ) 
   
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
#'   Determine if the contestant has won. 
#'   
#' @description
#'   `determine_winner()` returns the "WIN" or "LOSE" game result. 
#'    
#' @details
#'    If the contestant's final selection contains the car, they win. 
#'    The function will return "WIN"; otherwise they lost/ if a goat 
#'    behind their final door selection. The function will return "LOSE"
#'    for this scenario.
#' 
#' @param final.pick Numeric vectors, length 1, between 1 and 3.
#' @param game character vector, length 3, any combination of one "car" and 
#'   two "goat". 
#' 
#' @return The function returns a length 1 character vector
#'   indicating the game result. 
#'   
#' @examples
#'   determine_winner( 1, c("goat","goat","car") )
#'   determine_winner( 2, c("goat","goat","car") )
#'   determine_winner( 3, c("goat","goat","car") )
#'   determine_winner( 1, c("car","goat","goat") )
#'   determine_winner( 2, c("car","goat","goat") )
#'   determine_winner( 3, c("car","goat","goat") )
#'   determine_winner( 1, c("goat","car","goat") )
#'   determine_winner( 2, c("goat","car","goat") )
#'   determine_winner( 3, c("goat","car","goat") )
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
#'   Play the Monty Hall Problem game.
#'
#' @description
#'   `play_game()` packages all above functions together and plays 
#'   the game from the beginning to the end.
#'
#' @details
#'   The function will execute all functions by order, generate a random 
#'   game setting, play around, and returns the result. 
#'
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a length 1 character vector
#'   indicating the result of the game.
#'
#' @examples
#'   play_game()
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
#'   Simulate the Monty Hall Problem game.
#'
#' @description
#'   `play_n_games()` simulates the game by n rounds/ default at 100 and 
#'   generates the strategy table with the probability of stay or switch to 
#'   the respective chance on win or lose.
#'
#' @details
#'   The function will create a list to document the game result with a loop 
#'   by n rounds, append each additional game result to the list according 
#'   to the index position. and transform the list into a table with row 
#'   proportions rounded to the second decimal digits. 
#'
#' @param n numeric vectors, length 1, default at 100
#' 
#' @return The function returns a 2 by 2 data frame
#'   indicating the probability of win or lose for stay 
#'   and switch strategies.
#'
#' @examples
#'   play_n_games()
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
