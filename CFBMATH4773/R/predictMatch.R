#' Predict Match
#'
#' Prints out a predicted score based on prior performance of the teams.
#'
#' @param homeTeam a `team` object of the home team (or just team one if played at a neutral location).
#' @param awayTeam a `team` object of the away team (or just team two if played at a neutral location).
#' @param neutralLocation a boolean, True if the match to be pedicted will not be played at the home stadium of either team.
#'
#' @return a list of length two the first element, `teamOnePts`, is the predicted amount of points for the first team. `teamTwoPts` is the predicted amount for the second team.
#' @export
#'
#' @examples CFB<-scrapeCurrentSeason()
#' @examples OU<-team("Oklahoma", CFB)
#' @examples OSU<-team("Ohio State", CFB)
#' @examples predictMatch(Oklahoma, Ohio State)
#'
predictMatch<-function(homeTeam, awayTeam, neutralLocation=FALSE){
  teamOneAway=0
  teamTwoAway=1
  if(neutralLocation){
    teamOneAway=0.5
    teamTwoAway=0.5
  }
  teamOne.df=data.frame(OppRank=awayTeam$rank, OppDef=awayTeam$defScore, OppOff=awayTeam$offScore, Away=teamOneAway)
  teamTwo.df=data.frame(OppRank=homeTeam$rank, OppDef=homeTeam$defScore, OppOff=homeTeam$offScore, Away=teamTwoAway)
  teamOnePts1=predict.lm(homeTeam$ptsFor.mod, newdata = teamOne.df, interval="prediction", se.fit = TRUE)
  teamTwoPts1=predict.lm(homeTeam$ptsAgainst.mod, newdata = teamOne.df, interval="prediction", se.fit = TRUE)
  teamOnePts2=predict.lm(awayTeam$ptsAgainst.mod, newdata = teamTwo.df, interval="prediction", se.fit = TRUE)
  teamTwoPts2=predict.lm(homeTeam$ptsFor.mod, newdata = teamTwo.df, interval="prediction", se.fit = TRUE)

  teamOneStd1=sqrt((teamOnePts1$se.fit)^2+(summary(homeTeam$ptsFor.mod)$sigma)^2)
  teamTwoStd1=sqrt((teamTwoPts1$se.fit)^2+(summary(homeTeam$ptsAgainst.mod)$sigma)^2)
  teamOneStd2=sqrt((teamOnePts2$se.fit)^2+(summary(awayTeam$ptsAgainst.mod)$sigma)^2)
  teamTwoStd2=sqrt((teamTwoPts2$se.fit)^2+(summary(awayTeam$ptsFor.mod)$sigma)^2)

  teamOneMu1=teamOnePts1$fit[1]
  teamTwoMu1=teamOnePts2$fit[1]
  teamOneMu2=teamTwoPts2$fit[1]
  teamTwoMu2=teamTwoPts2$fit[1]

  teamOneTs1=(c(0:100)-teamOneMu1)/teamOneStd1
  teamTwoTs1=(c(0:100)-teamTwoMu1)/teamTwoStd1
  teamOneTs2=(c(0:100)-teamOneMu2)/teamOneStd2
  teamTwoTs2=(c(0:100)-teamTwoMu2)/teamTwoStd2

  teamOneDf1=teamOnePts1$df
  teamTwoDf1=teamTwoPts1$df
  teamOneDf2=teamOnePts2$df
  teamTwoDf2=teamTwoPts2$df

  teamOneDist1=c(pt(teamOneTs1[1],df=teamOneDf1))
  teamTwoDist1=c(pt(teamTwoTs1[1],df=teamTwoDf1))
  teamOneDist2=c(pt(teamOneTs2[1],df=teamOneDf2))
  teamTwoDist2=c(pt(teamTwoTs2[1],df=teamTwoDf2))

  for(i in 2:101){
    teamOneDist1[i]=pt(teamOneTs1[i], df=teamOneDf1)-pt(teamOneTs1[i-1], df=teamOneDf1)
    teamTwoDist1[i]=pt(teamTwoTs1[i], df=teamTwoDf1)-pt(teamTwoTs1[i-1], df=teamTwoDf1)
    teamOneDist2[i]=pt(teamOneTs2[i], df=teamOneDf2)-pt(teamOneTs2[i-1], df=teamOneDf2)
    teamTwoDist2[i]=pt(teamTwoTs2[i], df=teamTwoDf2)-pt(teamTwoTs2[i-1], df=teamTwoDf2)
  }

  teamOneDist1[102]=1-pt(teamOneTs1[101],df=teamOneDf1)
  teamTwoDist1[102]=1-pt(teamTwoTs1[101],df=teamTwoDf1)
  teamOneDist2[102]=1-pt(teamOneTs2[101],df=teamOneDf2)
  teamTwoDist2[102]=1-pt(teamTwoTs2[101],df=teamTwoDf2)

  teamOneDist=(teamOneDist1+teamOneDist2)/2
  teamTwoDist=(teamTwoDist2+teamTwoDist2)/2

  p=0.0
  for(i in 1:102){
    p=p+(teamOneDist[i]*sum(teamTwoDist[1:i-1]))
  }

  teamOnePts=(teamOnePts1$fit[1]+teamOnePts2$fit[1])/2
  teamTwoPts=(teamTwoPts2$fit[1]+teamTwoPts2$fit[1])/2
  print(paste(homeTeam$name, "-", teamOnePts))
  print(paste(awayTeam$name, "-", teamTwoPts))
  print(paste("Probability that ", homeTeam$name, " wins: ", p))

  return(invisible(list(teamOnePts=teamOnePts,teamTwoPts=teamTwoPts)))
}
