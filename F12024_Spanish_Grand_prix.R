library(tidyverse)
library(gganimate)

# Datasets used for the first part of the project
race_results <- read_csv("./Datasets/F1_2024_Results/Formula1_2024season_raceResults.csv")
sprint_results <- read_csv("./Datasets/F1_2024_Results/Formula1_2024season_sprintResults.csv")
quali_results <- read_csv("./Datasets/F1_2024_Results/Formula1_2024season_qualifyingResults.csv")

# Datasets used for the last part of the project
races <- read_csv("./Datasets/F1_2024_Spanish_race_data/races.csv")
lap_times <- read_csv("./Datasets/F1_2024_Spanish_race_data/lap_times.csv")
drivers <- read_csv("./Datasets/F1_2024_Spanish_race_data/drivers.csv")

#official F1 team colors from https://www.reddit.com/r/formula1/comments/1avhmjb/f1_2024_hex_codes/
teams_cols <- c("Alpine Renault" = "#FF87BC", "Aston Martin Aramco Mercedes" = "#229971", "Ferrari" = "#E8002D",
                "Haas Ferrari" = "#B6BABD", "Kick Sauber Ferrari" = "#52E252", "McLaren Mercedes" = "#FF8000",
                "Mercedes" = "#27F4D2", "RB Honda RBPT" = "#6692FF", "Red Bull Racing Honda RBPT" = "#3671C6",
                "Williams Mercedes" = "#64C4FF")

drivers_cols <- c("Charles Leclerc" = "#E8002D", "Lando Norris" = "#FF8000", "George Russell" = "#27F4D2", 
                  "Max Verstappen" = "#3671C6")

## Removing data from GPs that take place after the Spanish Grand Prix
race_results <- race_results |>
  filter(Track != "Spain" & Track != "Austria" & Track != "Great Britain" & Track != "Hungary" & Track != "Belgium")

sprint_results <- sprint_results |>
  filter(Track != "Austria")

# Lets create a dataframe that tracks the points gained during the season
race_points <- race_results |>
  select(Driver, Team, Points) |>
  rename(Race_points = Points)

sprint_points <- sprint_results |>
  select(Driver, Team, Points) |>
  rename(Sprint_points = Points)

# Let's determine a Grand Prix order to the results (based on event dates)
ordered_race_results <- race_results |>
  mutate(GP_num = case_when(Track == "Bahrain" ~ 1,
                            Track == "Saudi Arabia" ~ 2,
                            Track == "Australia" ~ 3,
                            Track == "Japan" ~ 4,
                            Track == "China" ~ 5,
                            Track == "Miami" ~ 6,
                            Track == "Emilia Romagna" ~ 7,
                            Track == "Monaco" ~ 8,
                            Track == "Canada" ~ 9,
                            Track == "Spain" ~ 10,
                            Track == "Austria" ~ 11,
                            Track == "Great Britain" ~ 12,
                            Track == "Hungary" ~ 13,
                            Track == "Belgium" ~ 14))

ordered_sprint_results <- sprint_results |>
  mutate(GP_num = case_when(Track == "Bahrain" ~ 1,
                            Track == "Saudi Arabia" ~ 2,
                            Track == "Australia" ~ 3,
                            Track == "Japan" ~ 4,
                            Track == "China" ~ 5,
                            Track == "Miami" ~ 6,
                            Track == "Emilia Romagna" ~ 7,
                            Track == "Monaco" ~ 8,
                            Track == "Canada" ~ 9,
                            Track == "Spain" ~ 10,
                            Track == "Austria" ~ 11,
                            Track == "Great Britain" ~ 12,
                            Track == "Hungary" ~ 13,
                            Track == "Belgium" ~ 14))

# Select the data from the Spain Grand Prix for Qualifying analysis
q3_results <- quali_results |>
  filter(Track == "Spain" & (!is.na(quali_results$Q3)) & quali_results$Q3 != "DNF") |>
  select(Driver, Team, Q3)

# Add drivers name abbreviation for qualifying analysis
q3_results <- q3_results |>
  mutate(Driver = case_when(Driver == "Lando Norris" ~ "NOR", 
                            Driver == "Max Verstappen" ~ "VER",
                            Driver == "Lewis Hamilton" ~ "HAM",
                            Driver == "George Russell" ~ "RUS",
                            Driver == "Charles Leclerc" ~ "LEC",
                            Driver == "Carlos Sainz" ~ "SAI",
                            Driver == "Pierre Gasly" ~ "GAS",
                            Driver == "Sergio Perez" ~ "PER",
                            Driver == "Esteban Ocon" ~ "OCO"))

# Select the 2024 Spanish Grand Prix from the races list dataset
spanish_grand_prix <- races |>
  filter(year == 2024 & name == "Spanish Grand Prix") |>
  select(raceId)

# Retrieve the raceId corresponding to the 2024 Spanish Grand Prix
target_race_id <- spanish_grand_prix$raceId

# Obtaining the 2024 spanish race lap times from the race lap times dataset
race_lap_times <- lap_times |>
  filter(raceId == target_race_id)

# Let's transform the drivers list in order to mantain the same data format of the results datasets
drivers <- drivers |>
  mutate(driver = paste(forename, surname, sep = " ")) |>
  select(driverId, driver)

# Removing unused columns
race_lap_times <- race_lap_times |>
  select(driverId, lap, milliseconds)

# Join the race lap times with drivers
race_lap_times <- race_lap_times %>%
  inner_join(drivers)

# Adding "team" attribute to the dataframe
race_lap_times <- race_lap_times |>
  select(driver, lap, milliseconds) |>
  mutate(team = case_when(driver == "Max Verstappen" ~ "Red Bull Racing Honda RBPT",
                          driver == "Sergio Pérez" ~ "Red Bull Racing Honda RBPT",
                          driver == "Lewis Hamilton" ~ "Mercedes",
                          driver == "George Russell" ~ "Mercedes",
                          driver == "Charles Leclerc" ~ "Ferrari",
                          driver == "Carlos Sainz" ~ "Ferrari",
                          driver == "Lando Norris" ~ "McLaren Mercedes",
                          driver == "Oscar Piastri" ~ "McLaren Mercedes",
                          driver == "Fernando Alonso" ~ "Aston Martin Aramco Mercedes",
                          driver == "Lance Stroll" ~ "Aston Martin Aramco Mercedes",
                          driver == "Pierre Gasly" ~ "Alpine Renault",
                          driver == "Esteban Ocon" ~ "Alpine Renault",
                          driver == "Yuki Tsunoda" ~ "RB Honda RBPT",
                          driver == "Daniel Ricciardo" ~ "RB Honda RBPT",
                          driver == "Nico Hülkenberg" ~ "Haas Ferrari",
                          driver == "Kevin Magnussen" ~ "Haas Ferrari",
                          driver == "Alexander Albon" ~ "Williams Mercedes",
                          driver == "Logan Sargeant" ~ "Williams Mercedes",
                          driver == "Valtteri Bottas" ~ "Kick Sauber Ferrari",
                          driver == "Guanyu Zhou" ~ "Kick Sauber Ferrari"))


### Drivers standings

# Summing up the points based on drivers 
race_drivers_standings <- race_points |>
  group_by(Driver, Team) |>
  summarize(
    Race_points = sum(Race_points),
    .groups = "drop"
  )

sprint_drivers_standings <- sprint_points |>
  group_by(Driver) |>
  summarize(
    Sprint_points = sum(Sprint_points)
  )

# In Formula 1 there are two types of races: classic races and sprint races (in a less number). Sprint races are long 1/3 of a classical race length and they award a lower number of points. So it's needed to compute the sum of the points awarded from both race types.
drivers_standings <- race_drivers_standings %>%
  full_join(sprint_drivers_standings)

# There is a driver that raced in a classic race but not in any sprint race, so it's needed to set at zero the points awarded in sprint races instead of NA
drivers_standings[is.na(drivers_standings)] <- 0

# Summing up the overall points
drivers_standings <-  drivers_standings |>
  mutate(
    Points = Race_points + Sprint_points
  )

# Plotting the Drivers Standings
ggplot(
  drivers_standings,
  aes(x = Points, y = reorder(Driver, Points), fill = Team)
) +
  geom_bar(stat = "Identity", color = "black") +
  geom_text(aes(label = Points), nudge_x = 15, nudge_y = 0.1) +
  scale_fill_manual(values = teams_cols) +
  labs(
    title = "Drivers standings",
    x = "Points", y = "Driver"
  )


### Constructors Standings

# Summing up the points based on drivers 
race_constructors_standings <- race_points |>
  group_by(Team) |>
  summarize(
    Race_points = sum(Race_points)
  )

sprint_constructors_standings <- sprint_points |>
  group_by(Team) |>
  summarize(
    Sprint_points = sum(Sprint_points)
  )

# Joining classic races points and sprint races points
constructors_standings <- race_constructors_standings %>%
  full_join(sprint_constructors_standings)

# Summing up the overall points
constructors_standings <-  constructors_standings |>
  mutate(
    Points = Race_points + Sprint_points
  )

# Plotting the Constructors Standings
ggplot(
  constructors_standings,
  aes(x = Points, y = reorder(Team, Points))
) +
  geom_bar(stat = "Identity", fill = teams_cols, color = "black") +
  geom_text(aes(label = Points), nudge_x = 20) +
  labs(
    title = "Constructors standings",
    x = "Points", y = "Team"
  )

### Drivers performance classes

drivers_rank <- drivers_standings |>
  select(Driver, Points)

# Group drivers into performance classes 
drivers_rank = arrange(drivers_rank, desc(Points))
D = dist(drivers_rank$Points)
cc = hclust(D, method = "average")
#plot(cc)
#clusters.list = rect.hclust(cc, k = 6, border="blue")

## Visualize the driver ranking highlighting the performance classes with different colors
clusters = cutree(cc, k = 6)
drivers_rank = mutate(drivers_rank, cluster = as.factor(clusters))
ggplot(drivers_rank, aes(x = 1, y = Points, color = cluster)) +
  geom_text(aes(label = Driver), show.legend = FALSE) + 
  labs(y = "Rating") +
  theme_minimal() + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

### Teams performance classes

constructors_rank <- constructors_standings |>
  select(Team, Points)

# Group teams into performance classes 
constructors_rank = arrange(constructors_rank, desc(Points))
D = dist(constructors_rank$Points)
cc = hclust(D, method = "average")

# Visualize the team ranking highlighting the performance classes with different colors
clusters = cutree(cc, k = 4)
constructors_rank = mutate(constructors_rank, cluster = as.factor(clusters))
ggplot(constructors_rank, aes(x = 1, y = Points, color = cluster)) +
  geom_text(aes(label = Team), show.legend = FALSE) + 
  labs(y = "Rating") +
  theme_minimal() + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

### Drivers title contenders Trend Analysis

# Using the ordered results for this block
ordered_race_results <- ordered_race_results|>
  select(GP_num, Driver, Team, Points)

ordered_sprint_results <- ordered_sprint_results|>
  select(GP_num, Driver, Team, Points)

ordered_race_results <-  rename(ordered_race_results, Race_points = Points)

ordered_sprint_results <-  rename(ordered_sprint_results, Sprint_points = Points)

# Join the classical races and the sprint races results
ordered_results <- ordered_race_results %>%
  full_join(ordered_sprint_results)

ordered_results[is.na(ordered_results)] <- 0

# Summing up the overall points
drivers_points <-  ordered_results |>
  mutate(
    Points = Race_points + Sprint_points
  ) |>
  select(GP_num, Driver, Team, Points)

# In this section we consider just the points of the four "top teams" "number one" drivers (drivers that have more points compared to his teammate)
drivers_title_contenders_points <- drivers_points |>
  filter(Driver == "Max Verstappen" | Driver == "Charles Leclerc" | Driver == "Lando Norris" | Driver == "George Russell")

# Compute the cumulative points of target drivers
cumulative_points_ver <- drivers_title_contenders_points |>
  filter(Driver == "Max Verstappen") |>
  mutate(Cumulative_points = cumsum(Points))

cumulative_points_lec <- drivers_title_contenders_points |>
  filter(Driver == "Charles Leclerc") |>
  mutate(Cumulative_points = cumsum(Points))

cumulative_points_nor <- drivers_title_contenders_points |>
  filter(Driver == "Lando Norris") |>
  mutate(Cumulative_points = cumsum(Points))

cumulative_points_rus <- drivers_title_contenders_points |>
  filter(Driver == "George Russell") |>
  mutate(Cumulative_points = cumsum(Points))

# Concatenate the cumulative points of the four target drivers
cumulative_points <- rbind(cumulative_points_ver, cumulative_points_nor, cumulative_points_lec, cumulative_points_rus)

# Tidying the data on the Grand Prix dates
cumulative_points <- cumulative_points |>
  arrange(GP_num, Driver)

# Visualize the title contenders drivers trend
drivers_trend <- ggplot(
  cumulative_points, 
  aes(x = GP_num, y = Cumulative_points, color = Driver)) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 2.5) +
  scale_color_manual(values = drivers_cols) +
  transition_reveal(GP_num)  +
  labs(
    title = "Title contenders drivers trend",
    x = "Number of races", y = "Cumulative points"
  ) + 
  scale_x_continuous(breaks = 1:9)

animate(
  drivers_trend,
  renderer = av_renderer()
)

## Constructors title contenders trend analysis

# Group the points based on teams
constructors_title_contenders_points <- drivers_points |>
  select(GP_num, Team, Points) |>
  group_by(GP_num, Team) |>
  summarise(Points = sum(Points),
            .groups = "drop"
  )

# In this section we consider just the points of the four "top teams"
constructors_title_contenders_points <- constructors_title_contenders_points |>
  filter(Team == "Red Bull Racing Honda RBPT" | Team == "McLaren Mercedes" | Team == "Ferrari" | Team == "Mercedes")

# Compute the cumulative points of target teams
cumulative_points_rb <- constructors_title_contenders_points |>
  filter(Team == "Red Bull Racing Honda RBPT") |>
  mutate(Cumulative_points = cumsum(Points))

cumulative_points_mcl <- constructors_title_contenders_points |>
  filter(Team == "McLaren Mercedes") |>
  mutate(Cumulative_points = cumsum(Points))

cumulative_points_fer <- constructors_title_contenders_points |>
  filter(Team == "Ferrari") |>
  mutate(Cumulative_points = cumsum(Points))

cumulative_points_mer <- constructors_title_contenders_points |>
  filter(Team == "Mercedes") |>
  mutate(Cumulative_points = cumsum(Points))

# Concatenate the cumulative points of the four target drivers
teams_cumulative_points <- rbind(cumulative_points_rb, cumulative_points_mcl, cumulative_points_fer, cumulative_points_mer)

# Tidying the data on the Grand Prix dates
teams_cumulative_points <- teams_cumulative_points |>
  arrange(GP_num, Team)

# Visualize the title contenders teams trend
teams_trend <- ggplot(
  teams_cumulative_points, 
  aes(x = GP_num, y = Cumulative_points, color = Team)) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 2.5) +
  scale_color_manual(values = teams_cols) +
  transition_reveal(GP_num) +
  labs(
    title = "Title contenders teams trend",
    x = "Number of races", y = "Cumulative points"
  ) + 
  scale_x_continuous(breaks = 1:9)

animate(
  teams_trend,
  renderer = av_renderer()
)

### Are qualifying sessions worth to be considered?

# Retrieve the final race position and the starting grid position 
race_results <- race_results |>
  select(Position, `Starting Grid`)

sprint_results <- sprint_results |>
  select(Position, `Starting Grid`)

# Concatenate the data of classical races and sprint races
results <- rbind(race_results, sprint_results)

# Let's remove the non classified and disqualified drivers
results <- results |>
  filter(Position != "NC" & Position != "DQ")

results$Position <- as.integer(results$Position)

# Visualize a scatterplot to find a correlation between final position and starting position
ggplot(
  results,
  aes(x = Position, y = `Starting Grid`)
) +
  geom_point() + 
  geom_abline() 

# Compute the Pearson coefficient correlation between starting position and final position
r = cor(results$Position, results$`Starting Grid`, , method = "pearson")
r

## Qualifying performances

# Converting lap time from String type to double
q3_results <- q3_results |>
  mutate(Time_ms = (60 + (as.double(str_split_i(Q3, ":", -1)))))

# Visualize the qualifying lap time gap to the pole sitter
ggplot(
  q3_results,
  aes(x = reorder(Driver, Time_ms), y = Time_ms - min(Time_ms), fill = Team)
) + 
  geom_bar(stat = "Identity", color = "black", show.legend = FALSE) +
  geom_text(aes(label = round(Time_ms - min(Time_ms), digits = 3)), nudge_y = 0.03) +
  scale_fill_manual(values = teams_cols) +
  labs(
    title = "Qualifying lap time",
    x = "Drivers", y = "Delta to pole position (seconds)"
  )

## Race lap times performances

# In this section we consider just the points of the four "top teams" "number one" drivers (drivers that have more points compared to his teammate)
title_contenders_spanish_lap_times <- race_lap_times |>
  filter(driver == "Max Verstappen" | driver == "Lando Norris" | driver == "Charles Leclerc" | driver == "George Russell")

# Visualize the line chart of the title contenders drivers lap times
ggplot(
  title_contenders_spanish_lap_times,
  aes(x = lap, y = milliseconds, color = driver)
) + 
  geom_line() +
  scale_color_manual(values = drivers_cols)

## Race lap time distribution

# Visualize the box plot of the title contenders drivers lap times
ggplot(
  title_contenders_spanish_lap_times,
  aes(x = driver, y = milliseconds, color = driver)
) + 
  geom_boxplot(outlier.shape = NA) +
  scale_color_manual(values = drivers_cols) +
  coord_cartesian(ylim = c(77000, 84500)) +
  labs(
    title = "Title contenders drivers lap times distribution",
    x = "Driver", y = "Lap time"
  )

## Race gaps to the race winner

# Computing the average lap times
average_lap_times <- race_lap_times |>
  group_by(driver, team) |>
  summarise(
    average_lap_time = mean(milliseconds),
    .groups = "drop"
  )

# Visualize the final gaps respect to the race winner
ggplot(
  average_lap_times,
  aes(x = average_lap_time - min(average_lap_time), y = reorder(driver, average_lap_time), fill = team)
) + 
  geom_bar(stat = "Identity", color = "black", show.legend = FALSE) +
  scale_fill_manual(values = teams_cols) +
  labs(
    title = "Final gaps",
    x = "Final gap to the race winner", y = "Driver"
  )

# Computing the mean of average lap times of drivers of the same team
average_team_lap_times <- average_lap_times |>
  group_by(team) |>
  summarise(
    average_lap_time = mean(average_lap_time)
  )

# Visualize the average gaps respect to the fastest average lap times team
ggplot(
  average_team_lap_times,
  aes(x = average_lap_time - min(average_lap_time), y = reorder(team, average_lap_time), fill = team)
) + 
  geom_bar(stat = "Identity", color = "black", show.legend = FALSE) +
  scale_fill_manual(values = teams_cols) +
  labs(
    title = "Average team lap times",
    x = "Average team lap times", y = "Gap to the fastest average team"
  )

## Spanish Grand Prix performance classes

# Group drivers into performance classes 
average_lap_times <- arrange(average_lap_times, average_lap_time)
D = dist(average_lap_times$average_lap_time)
cc = hclust(D, method = "average")

# Visualize the driver ranking highlighting the performance classes with different colors
clusters = cutree(cc, k = 6)
average_lap_times = mutate(average_lap_times, cluster = as.factor(clusters))
ggplot(average_lap_times, aes(x = 1, y = desc(average_lap_time), color = cluster)) +
  geom_text(aes(label = driver), show.legend = FALSE) + 
  labs(y = "Rating") +
  theme_minimal() + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

# Group teams into performance classes 
average_team_lap_times <- arrange(average_team_lap_times, average_lap_time)
D = dist(average_team_lap_times$average_lap_time)
cc = hclust(D, method = "average")

# Visualize the team ranking highlighting the performance classes with different colors
clusters = cutree(cc, k = 4)
average_team_lap_times = mutate(average_team_lap_times, cluster = as.factor(clusters))
ggplot(average_team_lap_times, aes(x = 1, y = desc(average_lap_time), color = cluster)) +
  geom_text(aes(label = team), show.legend = FALSE) + 
  labs(y = "Rating") +
  theme_minimal() + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())