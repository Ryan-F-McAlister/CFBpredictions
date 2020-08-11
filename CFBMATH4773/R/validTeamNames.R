#' Valid team names
#'
#' A method to see all of FBS team names according to sports reference
#'
#' @return a character vector of all valid team names
#' @export
#'
#' @examples validTeamNames()
validTeamNames<-function(){
  unique(as.character(MCAL4773MCAL0002::teamNames$SpRef))
}
