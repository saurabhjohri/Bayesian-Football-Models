library(rstan)
library(rjags)
cat("Stan version: ",stan_version(),"\n")

# match data from laliga - 6 columns:
# laliga$Season
# laliga$Week
# laliga$HomeTeam
# laliga$AwayTeam
# laliga$HomeGoals
# laliga$AwayGoals

set.seed(12345)
load("~/Downloads//rasmus_baath_user_13_data_analysis_contest//laliga.RData")
# 1 for positive no.
# 0 for zero number
# -1 for negative number
laliga$MatchResult<-sign(laliga$HomeGoals-laliga$AwayGoals)

d<-na.omit(laliga)
teams<-unique(c(d$HomeTeam, d$AwayTeam))
seasons<-unique(d$Season)

# data to fit to model
data_list<- list(HomeGoals=d$HomeGoals,
                 AwayGoals=d$AwayGoals, 
                 HomeTeam=as.numeric(factor(d$HomeTeam,levels=teams)),
                 AwayTeam=as.numeric(factor(d$AwayTeam,levels=teams)),
                 Season=as.numeric(factor(d$Season,levels=seasons)),
                 n_teams=length(teams),
                 n_games=nrow(d),
                 n_seasons=length(seasons))

# convenience function to generate the type of column names that Jags output
col_name<-function(name,...){
  paste0(name, "[",paste(...,sep=","),"]")
}
browser()
m1_string<-"model{
  #loop for simulaint goals
  for(i in 1:n_games){
    HomeGoals[i]~dpois(lambda_home[HomeTeam[i],AwayTeam[i]])
    AwayGoals[i]~dpois(lambda_away[HomeTeam[i],AwayTeam[i]])
  }
  #loop to set up scoring rate for home and away teams based on priors
  for(home_i in 1:n_teams){
    for(away_i in 1:n_teams){
      lambda_home[home_i,away_i]<-exp(baseline + skill[home_i] - skill[home_i])
      lambda_away[home_i,away_i]<-exp(baseline + skill[away_i] - skill[home_i])
    }
  }

  skill[1]<-0
  for(j in 2:n_teams){
    skill[j] ~ dnorm(group_skill, group_tau)
  }

  group_skill ~ dnorm(0,0.0625)
  group_tau <- 1/pow(group_sigma,2)
  group_sigma ~ dunif(0,3)
  baseline ~ dnorm(0, 0.0625)

}"

m1 <- jags.model(textConnection(m1_string), data = data_list, n.chains = 3,n.adapt = 5000)


# load files - downloaded from www.football-data.co.uk
# build_week_data<-function(unique_dates){
# }
# 
# season12_13<-read.csv("~/Dropbox/Scripts/SPORTS_MODELLING/SP1_12_13.csv")
# col1<-rep("2012/2013",dim(season12_13)[1])# season
# col2<-vector()#Week
# 
# # match dates per season
# unique_dates<-unique(season12_13$Date)
# build_week_data(unique_dates)
# 
# 
# col3<-vector()#HomeTeam
# col4<-vector()#AwayTeam
# col5<-vector()#HomeGoals
# col6<-vector()#AwayGoals
# 
# 
# # season11_12<-read.csv("~/Dropbox/Scripts/SPORTS_MODELLING/SP1_11_12.csv")
# # season10_11<-read.csv("~/Dropbox/Scripts/SPORTS_MODELLING/SP1_10_11.csv")
# # season09_10<-read.csv("~/Dropbox/Scripts/SPORTS_MODELLING/SP1_09_10.csv")
# # season08_09<-read.csv("~/Dropbox/Scripts/SPORTS_MODELLING/SP1_08_09.csv")
# 
# # csv file => 70 columns:
# #four_seasons_df<-as.data.frame(rbind(season08_09,season09_10,season10_11,season11_12))
# #season_vect<-four_seasons_df$Date
# 
# 
# #season_vect<-
# #unlist(strsplit(as.character(four_seasons_df$Date),"/",perl=TRUE))
# # for(i in 1:length(four_seasons_df$Date)){
# #   date<-four_seasons_df$Date[i]
# #   cat("date = ",date," ",four_seasons_df$Date[i],"\n")
# # }
# 
