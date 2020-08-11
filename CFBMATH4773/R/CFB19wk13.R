#'CFB19wk13
#'
#'A data set containing results and ranking of the 2019 season as of week 13.
#'
#'@format A data frame containing 1428 observations of 15 variables
#'\describe{
#' \item{Opponent}{The (sports reference) team against which the game was played}
#' \item{Rk}{unique identifier for every game. Each Rk shows twice, once for each team playing}
#' \item{Wk}{The week during which the game was played}
#' \item{Date}{Date on which the game was played in the form "Mon DD, YYYY"}
#' \item{Time}{The time of kickoff in the form "XX:YY ZM"}
#' \item{Day}{Day of the week on which the game was played in the form "Sun", "Sat",...}
#' \item{Team}{The Sports Referance name of the team playing the game}
#' \item{Pts}{number of points scored by the team of reference}
#' \item{Away}{dummy variable, 0 if a home game, 1 if an away game}
#' \item{OppPoints}{number of points scored by the opposing team}
#' \item{Notes}{A character vector of notes added by sports reference}
#' \item{win}{dummy variable, 1 if the team won, 0 if they lost}
#' \item{OppRank}{Character vector of the rank of the team according to CBS. If not an FBS team then "Non-FBS"}
#' \item{OppDef}{Character vector of the defense's score according to OddsShark. If not an FBS team then "Non-FBS"}
#' \item{OppOff}{Character vector of the offense's score according to OddsShark. If not an FBS team then "Non-FBS"}
#'}
#'@source \url{https://www.sports-reference.com/cfb/years/2019-schedule.html},\url{https://www.cbssports.com/college-football/rankings/cbs-sports-ranking/},\url{https://www.oddsshark.com/ncaaf/defensive-stats}
"CFB19wk13"
