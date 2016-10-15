library(rvest)
library(data.table)

# English Premier League (EPL) --------------------------------------------

EPL_table <- read_html("http://www.bbc.co.uk/sport/football/premier-league/table")
EPL_fixtures <- read_html("http://www.bbc.co.uk/sport/football/premier-league/fixtures")

# EPL functions -----------------------------------------------------------

# Fetch fields from EPL html
EPL_fetch <- function(x)
{
  EPL_table %>%
  html_nodes(paste("#last-ten-games-key+ .full-table-wide .team",x)) %>%
  html_text()
}

# Cast fields containing numbers to numeric type
EPL_tonum <- function(x) if (length(grep("[1-9]",unique(x)))>0) {as.numeric(as.character(x))} else {as.character(x)}

# EPL Table creation ------------------------------------------------------

EPL_fields <- c(".team-name",".played",".won",".drawn",".lost",".for",".against",".goal-difference",".points")
EPL_stats <- as.data.frame(sapply(EPL_fields,EPL_fetch))
EPL_stats <- data.frame(lapply(EPL_stats,EPL_tonum))
names(EPL_stats) <- toupper(sub('.','',names(EPL_stats)))
EPL_stats <- data.table(EPL_stats)
EPL_stats$FOR_AVG <- EPL_stats$FOR / EPL_stats$PLAYED
EPL_stats$AGAINST_AVG <- EPL_stats$AGAINST / EPL_stats$PLAYED
EPL_stats$DIFF_AVG <- EPL_stats$GOAL.DIFFERENCE / EPL_stats$PLAYED


# EPL Fixtures Predictions ------------------------------------------------

home_team <- EPL_fixtures %>%
              html_nodes(".table-stats~ .table-stats .team-home a") %>%
              html_text()

away_team <- EPL_fixtures %>%
              html_nodes(".table-stats~ .table-stats .team-away a") %>%
              html_text()

fixtures <- data.table(Home=home_team,Away=away_team)

figures <- setdiff(names(EPL_stats),"TEAM.NAME")

fixtures$Home_Goals <- 0
fixtures$Away_Goals <- 0
fixtures$Home_Expect <- 0
fixtures$Away_Expect <- 0

for (i in 1:nrow(fixtures)){
  figdiffs <- EPL_stats[TEAM.NAME==fixtures$Home[i],figures,with=F] - EPL_stats[TEAM.NAME==fixtures$Away[i],figures,with=F]

fixtures$Home_Expect[i] <- 
                  max(
                  # Expected FOR deteriorated by an opposer's good defence (Away team against goals low)
                  EPL_stats[TEAM.NAME==fixtures$Home[i],FOR_AVG]*(EPL_stats[TEAM.NAME==fixtures$Away[i],AGAINST_AVG]/5)+
                  # The differenciation factor is the gap in goals difference (half applied on Home, half on away)
                  EPL_stats[TEAM.NAME==fixtures$Away[i],AGAINST_AVG]+figdiffs$DIFF_AVG/2,
                  0)

fixtures$Away_Expect[i] <- 
                  max(
                  # Formulas are the symmetrical to the ones applied for Home team
                  EPL_stats[TEAM.NAME==fixtures$Away[i],FOR_AVG]*(EPL_stats[TEAM.NAME==fixtures$Home[i],AGAINST_AVG]/5)+
                  EPL_stats[TEAM.NAME==fixtures$Home[i],AGAINST_AVG]-figdiffs$DIFF_AVG/2,
                  0)


fixtures$Home_Goals[i] <- floor(fixtures$Home_Expect[i])
fixtures$Away_Goals[i] <- floor(fixtures$Away_Expect[i])
}

write.csv(fixtures[,1:4,with=F],"Desktop/EPL_Predictions_FC.csv",row.names = F)
