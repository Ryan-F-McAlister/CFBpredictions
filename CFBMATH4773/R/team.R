#' cfbTeam constructor
#'
#' Constructor function for the cfbTeam object which encompasses the data for an FBS team useable in this package.
#'
#' @param SpRefTeamName  The name of the team according to sports reference
#' @param season.df The season data from which the team data will be extracted
#'
#' @return a cfbTeam object
#' @export
#'
#' @examples CFB<-data(CFB19wk13)
#' OSU<-team("Ohio State", CFB)
#'
team<-function(SpRefTeamName,season.df){
  if(dim(season.df)[2]!=15|names(season.df)[1]!="Opponent"){
    stop("season.df must be of the form provided in this package. Consider using CFB19wk13 or scrapeCurrentSeason()")
  }
  df<-season.df[season.df$Team==SpRefTeamName& season.df$OppRank!="Non-FBS",]
  if(dim(df)[1]==0){
    stop(paste(SpRefTeamName, " does not match any Sports Reference Team Names. For list of valid team names use validTeamNames()"))
  }
  df$OppRank<-as.numeric(df$OppRank)
  df$OppDef<-as.numeric(df$OppDef)
  df$OppOff<-as.numeric(df$OppOff)
  ptsAgainst.mod<-lm(OppPts~OppRank+OppOff+Away, data=df)
  ptsFor.mod<-lm(Pts~OppRank+OppDef+Away, data=df)
  cfbRecord=paste0(sum(df$win),"-",sum(1-df$win))
  statind=which(season.df$Opponent==SpRefTeamName)[1]
  rank=as.numeric(season.df$OppRank[statind])
  DefScore=as.numeric(season.df$OppDef[statind])
  OffScore=as.numeric(season.df$OppOff[statind])
  out<-list(team.df = df, ptsAgainst.mod=ptsAgainst.mod, ptsFor.mod=ptsFor.mod, cfbRecord=cfbRecord, rank=rank, defScore=DefScore, offScore=OffScore, name=SpRefTeamName)
  class(out)<-append(class(out), "cfbTeam")
  return(invisible(out))
}
