library(rstanarm)
library(googlesheets4)
library(rvest)
library(plotly)
library("ggimage")
library(tidyverse)
library(ggplot2)
library(dplyr)
gs4_deauth()
transfer <- "https://docs.google.com/spreadsheets/d/1ol-SqXnZtN_cotE6PqpF34svAO7Co_AtNOGcr3Qk12I/edit#gid=0" |>
  read_sheet() |> mutate(Income = as.double(Income)) |>
  rename(Team = Club) |>
  mutate(new = ifelse(grepl(" FC", Team, fixed=TRUE), str_replace(Team, " FC", ""), Team)) |>
  select (new, Expenditure, Arrivals, Income, Departures) |>
  mutate(new=ifelse(grepl("AFC ", new, fixed=TRUE), str_replace(new, "AFC ", ""), new)) |>
  mutate(new=ifelse(grepl("&", new, fixed=TRUE), str_replace(new, "&", "and"), new)) |>
  rename(Team = new) |> mutate(balance = Income - Expenditure)
#Imports my google sheet and turns it into a tibble


table_21 <- "https://www.skysports.com/premier-league-table/2021" |> read_html() |> html_element("table") |>
  html_table() |> select(Team, Pts) |> rename(Points21 = Pts)
table_22 <- "https://www.skysports.com/premier-league-table/2022" |> read_html() |> html_element("table") |>
  html_table() |> select(Team, Pts) |> rename(Points22 = Pts)

info <- table_22 |> left_join(transfer) |> left_join(table_21) |> drop_na() |> 
  mutate(ptChange = Points22-Points21) |> select (balance, ptChange)

fit <- stan_glm(ptChange ~ balance, data = info, refresh = 0)
obs <- tibble(balance = -500000000)
pp <- posterior_predict(fit, newdata = obs) |> as_tibble()
plot <- pp |> ggplot(aes(x=`1`)) + geom_histogram(aes(y=after_stat(count/sum(count))), fill = "#034694", bins=25) +
  theme_classic() + scale_y_continuous(labels = scales::percent_format()) +
  scale_x_continuous(breaks=c(seq(-100, 100, 10))) +
  labs(x= "Points Gained/Lost", y= "Probability", title = "Posterior for Expected Point Gain",
       caption = "Source: Transfermarkt & Skysports.com")

write_rds(plot, "Chelsea_prediction.RDS")

info2 <- table_22 |> left_join(transfer) |> left_join(table_21) |> drop_na() |> 
  mutate(ptChange = Points22-Points21) |> filter(!Team == "Chelsea") |> 
  select(balance, ptChange)
fit <- stan_glm(ptChange ~ balance, data = info, refresh = 0)
obs <- tibble(balance = -500000000)
pp <- posterior_predict(fit, newdata = obs) |> as_tibble()
plot2 <- pp |> ggplot(aes(x=`1`)) + geom_histogram(aes(y=after_stat(count/sum(count))), fill = "#034694", bins=25) +
  theme_classic() + scale_y_continuous(labels = scales::percent_format()) +
  scale_x_continuous(breaks=c(seq(-100, 100, 10))) +
  labs(x= "Points Gained/Lost", y= "Probability", 
       title = "Posterior for Expected Point Gain WITHOUT Considering Chelsea",
       caption = "Data From: Transfermarkt & Skysports.com")
write_rds(plot2, "Chelsea_prediction2.RDS")