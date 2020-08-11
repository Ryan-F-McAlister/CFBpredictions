#' Summary
#'
#' Summary function for cfbTeam object.
#'
#' @param team team object to be summarized
#'
#' @return a list of the teams current record, reported as "Nwins-Nlosses", the teams CBS rank and the
#' defense and offense scores assigned by odds shark.
#' @export
#'
#' @examples CFB<-data(CFB19wk13)
#' OSU<-team("Ohio State", CFB)
#' summary(OSU)
summary.cfbTeam<-function(team){
  return(list(cfbRecord=team$cfbRecord, rank=team$rank, defScore=team$defScore, offScore=team$offScore))
}
