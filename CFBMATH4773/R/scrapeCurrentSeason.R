
#' scrapeCurrentSeason
#'
#' scrapes the web to get the most up to date rankings and scores of the current college football season for all 130 CFB teams
#'
#' @return a dataframe of up to date information, in the same form as the CFB19wk13 dataframe
#' @export
#'
#' @examples CFB.df<-scrapeCurrentSeason()
#'
scrapeCurrentSeason<-function(){
  require(rvest)
  require(RCurl)
  if(!is.character(getURL("www.google.com"))){
    stop("Cannot scrape current data - no internet connection")
  }
  url='https://www.sports-reference.com/cfb/years/2019-schedule.html'
  webpage = read_html(url)
  sched=html_table(
    xml_child(
      xml_child(
        xml_child(
          xml_child(
            xml_child(
              xml_child(
                xml_child(webpage, 2),
                1),
              5),
            3),
          2),
        1),
      1))

  #Rename Columns

  colnames(sched)<-c("Rk","Wk","Date","Time","Day", "Team", "Pts", "Away", "Opponent","OppPts","Notes")

  #Filter for Relevant Rows

  sched<-sched[sched$Rk !="Rk",]
  sched<-sched[sched$Pts!="",]

  #Remove numbers from winner names

  WinnerNamesOnly=c()
  for(i in sched$Team){
    if(substring(i,1,1)=="("){
      x=strsplit(i,"\\)",perl=T)
      name=substring(x[[1]][length(x[[1]])],first=2)
      WinnerNamesOnly[length(WinnerNamesOnly)+1]=name
    }
    else{
      WinnerNamesOnly[length(WinnerNamesOnly)+1]=i
    }
  }
  sched$Team=WinnerNamesOnly

  #Remove numbers from loser names

  LoserNamesOnly=c()
  for(i in sched$Opponent){
    if(substring(i,1,1)=="("){
      x=strsplit(i,"\\)",perl=T)
      name=substring(x[[1]][length(x[[1]])],first=2)
      LoserNamesOnly[length(LoserNamesOnly)+1]=name
    }
    else{
      LoserNamesOnly[length(LoserNamesOnly)+1]=i
    }
  }
  sched$Opponent=LoserNamesOnly

  #Mark a win

  sched$win=1

  #Mark away games

  awaydummy=c()
  for(i in sched$Away){
    if(i == "@"){
      awaydummy[length(awaydummy)+1]=1
    }
    else{
      awaydummy[length(awaydummy)+1]=0
    }
  }
  sched$Away=awaydummy

  #Double to df for all the losers data

  losersched=data.frame(Rk=sched$Rk, Wk=sched$Wk, Date=sched$Date, Time=sched$Time, Day=sched$Day, Team=sched$Opponent, Pts=sched$OppPts,
                        Away=1-sched$Away, Opponent=sched$Team, OppPts=sched$Pts,Notes=sched$Notes, win=1-sched$win)

  #Make one big ass df of all of them

  CFB=rbind(sched,losersched)

  #Get CBS Rankings

  url2='https://www.cbssports.com/college-football/rankings/cbs-sports-ranking/'
  webpage2=read_html(url2)
  ranks=html_table(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(webpage2,
                                                                                                                                 2),
                                                                                                                       5),
                                                                                                             2),
                                                                                                   1),
                                                                                         1),
                                                                               2),
                                                                     1),
                                                           1),
                                                 2),
                                       1),
                             1))
  ranks<-ranks[,c("Rank","Team")]

  #Fix the team names

  onlyTeamNames=c()
  for(i in ranks$Team){
    x=strsplit(i,"\n")[[1]][1]
    onlyTeamNames[length(onlyTeamNames)+1]=x
  }
  ranks$Team=onlyTeamNames

  #Build Translated df-add CBS rank

  rankScore=merge(MCAL4773MCAL0002::teamNames,ranks,by.x=c("CBS"), by.y=c("Team"))

  #Scrape defense Stats

  url3='https://www.oddsshark.com/ncaaf/defensive-stats'
  webpage3=read_html(url3)
  defense<-html_table(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(webpage3,
                                                                                                                          2),
                                                                                                                3),
                                                                                                      1),
                                                                                            2),
                                                                                  2),
                                                                        4),
                                                              1),
                                                    1),
                                          2),
                                1))
  defense<-defense[defense$Team!="League Average", c("Team","Score")]

  #add defense data to translated df

  rankScore=merge(rankScore,defense,by.x="OddShark",by.y="Team")
  names(rankScore)[5]<-"defenseScore"

  #Scrape Offense data

  url4='https://www.oddsshark.com/ncaaf/offensive-stats'
  webpage4=read_html(url4)
  offense=html_table(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(webpage4,
                                                                                                                         2),
                                                                                                               3),
                                                                                                     1),
                                                                                           2),
                                                                                 2),
                                                                       4),
                                                             1),
                                                   1),
                                         2),
                               1))
  offense<-offense[offense$Team!="League Average", c("Team","Score")]

  #add that to the translation df

  rankScore=merge(rankScore,offense,by.x="OddShark",by.y="Team")
  names(rankScore)[6]<-"offenseScore"

  #link translated variables to schedule

  CFB=merge(CFB, rankScore[,c("SpRef","Rank","defenseScore","offenseScore")], by.x="Opponent", by.y="SpRef", all.x=TRUE)

  #Rename and fill in NAs

  names(CFB)[c(13,14,15)]<-c("OppRank","OppDef","OppOff")
  CFB$OppRank[is.na(CFB$OppRank)]="Non-FBS"
  CFB$OppDef[is.na(CFB$OppDef)]="Non-FBS"
  CFB$OppOff[is.na(CFB$OppOff)]="Non-FBS"
  return(CFB)
}

