library(googlesheets4)
library(rvest)
library(plotly)
library("ggimage")
gs4_deauth()
transfer <- "https://docs.google.com/spreadsheets/d/1ol-SqXnZtN_cotE6PqpF34svAO7Co_AtNOGcr3Qk12I/edit#gid=0" |>
  read_sheet() |> mutate(Income = as.double(Income)) |>
  rename(Team = Club) |>
  mutate(new = ifelse(grepl(" FC", Team, fixed=TRUE), str_replace(Team, " FC", ""), Team)) |>
  select (new, Expenditure, Arrivals, Income, Departures) |>
  mutate(new=ifelse(grepl("AFC ", new, fixed=TRUE), str_replace(new, "AFC ", ""), new)) |>
  mutate(new=ifelse(grepl("&", new, fixed=TRUE), str_replace(new, "&", "and"), new)) |>
  rename(Team = new)
  #Imports my google sheet and turns it into a tibble
images = tibble(Team = c("Arsenal", "Aston Villa", "Bournemouth", "Brentford", "Brighton and Hove Albion",
                         "Chelsea", "Crystal Palace", "Everton", "Fulham", "Leeds United", "Leicester City",
                         "Liverpool", "Manchester City", "Manchester United", "Newcastle United",
                         "Nottingham Forest", "Southampton", "Tottenham Hotspur", "West Ham United", 
                         "Wolverhampton Wanderers"), 
                Badges = c("https://upload.wikimedia.org/wikipedia/en/thumb/5/53/Arsenal_FC.svg/1200px-Arsenal_FC.svg.png", 
                "https://a.espncdn.com/combiner/i?img=/i/teamlogos/soccer/500/362.png",
                "https://upload.wikimedia.org/wikipedia/en/thumb/e/e5/AFC_Bournemouth_%282013%29.svg/1200px-AFC_Bournemouth_%282013%29.svg.png",
                "https://upload.wikimedia.org/wikipedia/en/thumb/2/2a/Brentford_FC_crest.svg/640px-Brentford_FC_crest.svg.png",
                "https://upload.wikimedia.org/wikipedia/en/thumb/f/fd/Brighton_%26_Hove_Albion_logo.svg/1200px-Brighton_%26_Hove_Albion_logo.svg.png",
                "https://upload.wikimedia.org/wikipedia/en/thumb/c/cc/Chelsea_FC.svg/1200px-Chelsea_FC.svg.png",
                "https://upload.wikimedia.org/wikipedia/en/thumb/a/a2/Crystal_Palace_FC_logo_%282022%29.svg/1200px-Crystal_Palace_FC_logo_%282022%29.svg.png",
                "https://upload.wikimedia.org/wikipedia/en/thumb/7/7c/Everton_FC_logo.svg/1200px-Everton_FC_logo.svg.png",
                "https://upload.wikimedia.org/wikipedia/en/thumb/e/eb/Fulham_FC_%28shield%29.svg/1200px-Fulham_FC_%28shield%29.svg.png",
                "https://upload.wikimedia.org/wikipedia/en/thumb/5/54/Leeds_United_F.C._logo.svg/800px-Leeds_United_F.C._logo.svg.png",
                "https://upload.wikimedia.org/wikipedia/en/thumb/2/2d/Leicester_City_crest.svg/1200px-Leicester_City_crest.svg.png",
                "https://upload.wikimedia.org/wikipedia/en/thumb/0/0c/Liverpool_FC.svg/1200px-Liverpool_FC.svg.png",
                "https://upload.wikimedia.org/wikipedia/en/thumb/e/eb/Manchester_City_FC_badge.svg/1200px-Manchester_City_FC_badge.svg.png",
                "https://upload.wikimedia.org/wikipedia/en/thumb/7/7a/Manchester_United_FC_crest.svg/1200px-Manchester_United_FC_crest.svg.png",
                "https://upload.wikimedia.org/wikipedia/en/thumb/5/56/Newcastle_United_Logo.svg/1200px-Newcastle_United_Logo.svg.png",
                "https://a.espncdn.com/combiner/i?img=/i/teamlogos/soccer/500/393.png",
                "https://upload.wikimedia.org/wikipedia/en/thumb/c/c9/FC_Southampton.svg/1200px-FC_Southampton.svg.png",
                "https://upload.wikimedia.org/wikipedia/en/thumb/0/05/Spurs_2017_badge.svg/170px-Spurs_2017_badge.svg.png",
                "https://upload.wikimedia.org/wikipedia/en/thumb/c/c2/West_Ham_United_FC_logo.svg/1200px-West_Ham_United_FC_logo.svg.png",
                "https://upload.wikimedia.org/wikipedia/en/thumb/f/fc/Wolverhampton_Wanderers.svg/1200px-Wolverhampton_Wanderers.svg.png"))


images 
table <- "https://www.skysports.com/premier-league-table/2022" |> read_html() |> html_element("table") |> 
  html_table() |> select(`#`, Team, W, D, L, GD, Pts) |> rename(Rank = `#`, Points = Pts)

info <- table |> left_join(transfer)|> mutate(balance = Income - Expenditure) |>
  left_join(images, join_by(Team))
activity_pts <- info |> ggplot(aes(x=balance, y=Points)) + geom_image(aes(image = Badges), size = .04) +
  scale_x_continuous(labels = scales::dollar_format(suffix="€",prefix="")) + theme_classic() +
  labs(title = "Transfer Activity Balance vs Points for Premier League Teams in the 2022/23 Season",
       subtitle = "Under Todd Boehly, Chelsea are an extreme outlier",
       x = "Transfer Activity Balance",
       y = "Points in 2022/23 Season")
        
write_rds(activity_pts, "activity_pts.rds")