## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

```{r cars}
library(ggimage)
library(ggplot2)
library(ggrepel)
library(tidyverse)
library(cfbscrapR)
library(cfbfastR)
library(gridExtra)
library(ggpubr)
library(gt)
library(gtExtras)
library(scales)
library(reactable)
library(reactablefmtr)
#API key
Sys.setenv(CFBD_API_KEY = "hLRjjxwqE8qF/a/iBD/dly4S/F2c36iEiffnV8pHvV05bvMtAVR7K1yY2KTb/eF0")

#load pbp data
pbp <- data.frame()
seasons <- 2021
progressr::with_progress({
  future::plan("multisession")
  pbp <- cfbfastR::load_cfb_pbp(seasons, season_type = "regular")
})
names(cfbteams_data)[names(cfbteams_data) == "logos[0]"] <- "logos1"
names(cfbteams_data)[names(cfbteams_data) == "logos[1]"] <- "logos2"

#get talent from 247
talent <- cfbd_team_talent(2021)

#get game winner and losers
winners <- pbp %>%
  group_by(game_id) %>% 
  summarise(
    week = first(week),
    sd = last(pos_score_diff),
    winner = ifelse(last(pos_score_diff) < 0, last(def_pos_team), last(pos_team)),
    loser = ifelse(last(pos_score_diff) <= 0, last(pos_team), last(def_pos_team)),
    winner_diff = last(abs(pos_score_diff)),
    loser_diff = ifelse(sd < 0, sd, -1*sd),
  ) 

#defensive stats by game
def <- pbp %>% 
  group_by(def_pos_team, game_id) %>% 
  summarise(
    def_epa = sum(EPA, na.rm=T),
    yards_allowed = sum(yards_gained, na.rm=T)
  )

#offensive stats by game and combine dfs
teams <- pbp %>% 
  group_by(pos_team, game_id) %>% 
  summarise(
    off_epa = sum(EPA, na.rm=T),
    yards_gained = sum(yards_gained, na.rm=T),
  ) %>% 
  left_join(def, by = c("pos_team" = "def_pos_team", "game_id" = "game_id")) %>% 
  left_join(cfbteams_data, by = c("pos_team" = "school")) %>% 
  left_join(talent, by = c("pos_team" = "school")) %>% 
  left_join(winners, by = c("game_id" = "game_id"))
  
#get cum sums
teams$csum_defepa <- ave(teams$def_epa, teams$pos_team, FUN=cummean)
teams$csum_ya <- ave(teams$yards_allowed, teams$pos_team, FUN=cummean)
teams$csum_offepa <- ave(teams$off_epa, teams$pos_team, FUN=cummean)
teams$csum_yg <- ave(teams$yards_gained, teams$pos_team, FUN=cummean)

#calculate team score
teams$ydpt <- (teams$csum_yg-teams$csum_ya)/100*7
teams$season_score <- (teams$csum_offepa + teams$csum_defepa + (teams$ydpt) + (teams$talent/6))
teams$opp <- ifelse(teams$winner == teams$pos_team, teams$loser, teams$winner)

#get scores
scores <- teams %>% 
  group_by(pos_team, game_id) %>% 
  summarise(
    opp_score = season_score
  )

#get opp team seasons score in df
teams <- teams %>% 
  left_join(scores, by = c("opp" = "pos_team", "game_id" = "game_id")) 
teams$season_score[is.na(teams$season_score)] <- 0

x <- teams
teams <- x

teams$rating <- (teams$season_score)+1000
teams$opp_score <- (teams$opp_score)+1000
teams$change <- 0
teams$win <- 0
teams$loss <- 1
teams$before <- 0
teams$last_score <- teams$season_score
teams$rating[is.na(teams$rating)] <- mean(teams$rating, na.rm=T)
teams$opp_score[is.na(teams$opp_score)] <- mean(teams$opp_score, na.rm=T)


#set up elo system
# selo <- teams %>% 
#   arrange(week) %>% 
#   mutate(sc = season_score) %>% 
#   mutate(r1 = 10^(season_score/400)) %>% 
#   mutate(r2 = 10^(opp_score/400)) %>% 
#   mutate(e1 = r1/(r1+r2)) %>% 
#   mutate(s = ifelse(pos_team == winner, 1, 0)) %>% 
#   mutate(season_score = replace(season_score, season_score == sc, sc + (32*(s-e1)))) %>% 
#   group_by(pos_team, game_id) %>%
#   summarise(
#     change = 32*(s-e1),
#     rating = season_score + (32*(s-e1)),
#     season_score = rating
#   ) %>% 
#   left_join(winners, by = c("game_id"="game_id"))

for(i in 1:nrow(teams)){
  r1 = 10^(teams$rating[i]/400)
  r2 = 10^(teams$opp_score[i]/400)
  e1 = r1/(r1+r2)
  e2 = r2/(r1+r2)
  s1 = ifelse(teams$pos_team[i] == teams$winner[i], 1, 0)
  if(teams$pos_team[i] == teams$winner[i]){teams$win[i] = 1}
  if(teams$pos_team[i] == teams$winner[i]){teams$loss[i] = 0}
  s2= ifelse(teams$pos_team[i] == teams$winner[i], 0, 1)
  diff = ifelse(teams$pos_team[i] == teams$winner[i], teams$winner_diff[i]/3, teams$loser_diff[i]/3)
  teams$change[i] <- 100*(s1-e1) + diff + (teams$season_score[i]-teams$last_score[i])
  teams$before[i] <- teams$rating[i]
  teams$last_score[i] <- teams$season_score[i]
  teams$rating[i] <- teams$rating[i] + teams$change[i]
  teams$opp_score[i] <- teams$opp_score[i] + (100*(s2-e2) + (teams$loser_diff[i]/3)) + (teams$season_score[i]-teams$last_score[i])
  k = i+1
  for(j in k:nrow(teams)){
    if(i != 1692){
      if(teams$pos_team[j] == teams$pos_team[i]){teams$rating[j] = teams$rating[i]}
      if(teams$opp[j] == teams$opp[i]){teams$opp_score[j] = teams$opp_score[i]}
      if(teams$pos_team[j] == teams$pos_team[i]){teams$last_score[j] = teams$season_score[i]}
    }
  }
}


teams$wins <- ave(teams$win, teams$pos_team, FUN=cumsum)
teams$losses <- ave(teams$loss, teams$pos_team, FUN=cumsum)

teams$Team <- teams$pos_team
  
final <- teams %>% 
  group_by(pos_team) %>% 
  summarise(
    ` ` = first(logos1),
    Rating = round((last(rating))),
    Record = paste(last(wins), last(losses), sep="-"),
    Change = round(last(change))
  )
names(final)[names(final) == "pos_team"] <- "Team"
final$Rating[final$Team == "New Mexico State"] <- 1071
final$Change[is.na(final$Change)] <- -10

final$Rank <- NA
order.scores<-order(-final$Rating,final$Team)
final$Rank[order.scores] <- 1:nrow(final)

#final <- final[,c(4, 2,1,3)]


#make predictions
schedule <- cfbd_game_media(year = 2021) %>% 
  left_join(final, by = c("home_team" = "Team")) %>% 
  left_join(final, by = c("away_team" = "Team"))
  
#home team data
home <- schedule %>% 
  mutate(r1 = 10^(Rating.x/400)) %>% 
  mutate(r2 = 10^(Rating.y/400)) %>% 
  mutate(e1 = r1 / (r1+r2)) %>% 
  mutate(e2 = r2 / (r1+r2)) %>% 
  group_by(home_team, game_id) %>% 
  summarise(
    week = first(week),
    prob = e1,
  )
home$prob[is.na(home$prob)] <- .3

#away team data
away <- schedule %>% 
  mutate(r1 = 10^(Rating.y/400)) %>% 
  mutate(r2 = 10^(Rating.x/400)) %>% 
  mutate(e1 = r1 / (r1+r2)) %>% 
  mutate(e2 = r2 / (r1+r2)) %>% 
  group_by(away_team, game_id) %>% 
  summarise(
    week = first(week),
    prob = e1,
  )
away$prob[is.na(away$prob)] <- .3
names(away)[names(away) == "away_team"] <- "home_team"


#combined season
season <- rbind(home, away)
season <- season %>% 
  group_by(home_team) %>% 
  summarise(
    probs <- list(prob), .groups = "drop",
  )
names(season)[names(season) == "probs <- list(prob)"] <- "probs"

# organize
n_game <- unlist(lapply(season$probs, length))
season$n_game <- n_game
season <- season[n_game >= 7, ]


# single season function for losses
FUN_szn <- function(p) {
  wins <- sum(sapply(p, function(x) rbinom(1, 1, x)))
  return(wins)
}

# full simulation for losses
FUN_sim <- function(n, p) {
  s <- replicate(n = n, FUN_szn(p))
  losses <- length(p) - s
  return(mean(losses <= 2))
}
# get proabilities for cfp
playoff_prob <- lapply(season$probs, function(x) {
                        FUN_sim(n = 1000, p = x)
                      })
playoff_prob <- unlist(playoff_prob)
season$playoff_prob <- playoff_prob
season[order(-playoff_prob), ]

# single season function for wins
FUN_szn <- function(p) {
  wins <- sum(sapply(p, function(x) rbinom(1, 1, x)))
  return(wins)
}

# full simulation for wins
FUN_sim <- function(n, p) {
  s <- replicate(n = n, FUN_szn(p))
  return(mean(s >= 7))
}
# get proabilities for bowl game
bowl_prob <- lapply(season$probs, function(x) {
  FUN_sim(n = 1000, p = x)
})
bowl_prob <- unlist(bowl_prob)
season$bowl_prob <- bowl_prob
season[order(-bowl_prob), ]

#final table
final <- final %>% 
  left_join(season, by = c("Team" = "home_team"))

final <- final[-c(7, 8)]
final <- final[,c(6, 2, 1, 4, 5, 3, 7, 8)]

names(final)[names(final) == "bowl_prob"] <- "Make Bowl"
names(final)[names(final) == "playoff_prob"] <- "CFP Bid"
final$`CFP Bid`[is.na(final$`CFP Bid`)] <- .01
final$`Make Bowl`[is.na(final$`Make Bowl`)] <- .01
final$`Make Bowl`[final$`Make Bowl` == 1] <- .99
final$`CFP Bid` <- round((final$`CFP Bid`),2)
final$`Make Bowl` <- round((final$`Make Bowl`),2)


#########

transparent <- function(img) {
  image_fx(img, expression = "0.5*a", channel = "alpha")
}

teams$rating[is.na(teams$rating)] <- mean(teams$rating, na.rm=T)

max <- teams %>% 
  group_by(week) %>% 
  summarise(
    max = max(rating)
  ) %>% 
  filter(week < 14)

min <- teams %>% 
  group_by(week) %>% 
  summarise(
    min = min(rating)
  )%>% 
  filter(week < 14)

team <- teams %>% 
  filter(pos_team == "Miami")%>% 
  filter(week < 14) %>% 
  arrange(week)


#progression table
ggplot() +
  ylim(600, 1750)+
  geom_hline(yintercept = 1112, color = "black")+
  #geom_area(aes(x=max$week, y=max$max), fill="gray", alpha=0.35) +
  geom_ribbon(aes(max$week, ymin = min$min, ymax = max$max), alpha = 0.4) +
  geom_line(aes(x=max$week, y=max$max), color="black", size=.5) + 
  #geom_area(aes(x=min$week, y=min$min), fill="gray", alpha=0.35) +
  geom_image(aes(x = 6.5, y = 1112), image = team$logos1[1], 
              size =.2, asp = 16/8, image_fun = transparent) +
  geom_line(aes(x=min$week, y=min$min), color="black", size=.5) +
  geom_line(aes(x=team$week, y=team$rating), color=team$color, size=1,) +
  geom_point(aes(x = last(team$week), y=last(team$rating)), shape=21, size=3, 
             color=team$color[1], fill="white",) +
  theme_bw()+
  xlim(1, 13)+
  
  #theme_ipsum
  labs(
    x = "Week",
    y = "Rating",
    title = glue::glue("{unique(team$pos_team)} Team Rating"),
  )+
  geom_text(mapping = aes(x = 1.5, y = max$max[1]+6), label = "NCAAF Best", size=1.8)+
  geom_label(mapping = aes(x = 1.5, y = 1112), label = "Average", size=1.8)+
  geom_text(mapping = aes(x = 1.5, y = min$min[1]-13), label = "Worst", size=1.8)+
  theme(plot.title = element_text(size = 15, hjust = 0.47, face = "bold"),
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        axis.text = element_text(size = 9, hjust=.5),
        axis.title = element_text(size = 10, hjust=0.47),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        panel.border = element_blank(),
        text = element_text(color = "black", family = "montserrat"),
  )+
  scale_x_continuous(breaks = c(4, 8, 12))

final <- final %>% 
  arrange(Rank)
final$`Make Bowl`[final$`Make Bowl` == 1] <- .99

tab <- reactable(final,
          showSortIcon = FALSE,
          theme = fivethirtyeight(),
          searchable = TRUE,
          language = reactableLang(
          searchPlaceholder = "SEARCH FOR A TEAM..."),
          columns = list(
            Rank = colDef(show = T),
            ` ` = colDef(
              name = " ",
              align = "center",
              maxWidth = 50,
              cell = embed_img(height = "30", width = "30")
            ),
            Team = colDef(show = T, maxWidth = 150),
            Record = colDef(show = T, maxWidth = 100),
            Change = colDef(
              show = T,
              maxWidth = 63,
              style = color_scales(final, colors = c("#fd84a9", "white", "#42c2ca")),
            ),
            Rating = colDef(
              show = T,
              maxWidth = 60,
              style = color_scales(final, colors = c("#fd84a9", "white", "#42c2ca"))
            ),
            `CFP Bid` = colDef(
              show = T,
              maxWidth = 60,
              style = color_scales(final, colors = c("#fd84a9", "white", "#42c2ca"))),
            `Make Bowl` = colDef(
              show = T,
              maxWidth = 60,
              style = color_scales(final, colors = c("#fd84a9", "white", "#42c2ca"))
            )
          )
)

```

## Including Plots

You can also embed plots, for example:

```{r pressure, echo=FALSE}
tab
```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
