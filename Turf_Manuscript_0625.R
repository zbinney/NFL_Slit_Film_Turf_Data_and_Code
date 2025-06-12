# Manuscript Code (cleaning, descriptive, model dataset creation) JYB 11/3/24
# Edited by ZB and JYB 6/12/25

# Install and load necessary packages
pacman::p_load(tidyverse, scales, cowplot, styler, here,  table1, emmeans,haven, glmm, gt, lme4, merTools, purrr, reshape2, knitr, kableExtra)


# Injury dataset for 2018-23
nfl_injs <- read.csv("FO_injuries.csv") |>
  filter(Year >= 2018)

# Games and turf data for 2018-23
games <- read.csv("Game_Data_Rev.csv") |>
  filter(Year >= 2018) |>
  # Drop unnecessary variable
  dplyr::select(-Rk)


# Make team abbreviations in games match those in injury data
games <- games |>
  mutate(
    Tm = case_when(
      Tm == "GNB" ~ "GB",
      Tm == "KAN" ~ "KC",
      Tm == "NWE" ~ "NE",
      Tm == "NOR" ~ "NO",
      Tm == "SDG" & Year < 2017 ~ "SD",
      Tm == "SDG" & Year >= 2017 ~ "LAC",
      Tm == "SFO" ~ "SF",
      Tm == "TAM" ~ "TB",
      Tm == "STL" & Year >= 2016 ~ "LAR",
      Tm %in% c("OAK", "LVR") & Year >= 2020 ~ "LV",
      TRUE ~ Tm
    ),
    Opp = case_when(
      Opp == "GNB" ~ "GB",
      Opp == "KAN" ~ "KC",
      Opp == "NWE" ~ "NE",
      Opp == "NOR" ~ "NO",
      Opp == "SDG" & Year < 2017 ~ "SD",
      Opp == "SDG" & Year >= 2017 ~ "LAC",
      Opp == "SFO" ~ "SF",
      Opp == "TAM" ~ "TB",
      Opp == "STL" & Year >= 2016 ~ "LAR",
      Opp %in% c("OAK", "LVR") & Year >= 2020 ~ "LV",
      TRUE ~ Opp
    )
  )


# Add specific stadium info when same location had multiple stadiums
games <- games |>
  mutate(Stadium = case_when(
    Location == "SF" & Year >= 2014 ~ "SF-L",
    Location == "SF" ~ "SF-C",
    Location == "MIN" & Year >= 2016 ~ "MIN-USB",
    Location == "MIN" & Year >= 2014 ~ "MIN-TCF",
    Location == "MIN" ~ "MIN-HHM",
    Location == "ATL" & Year >= 2017 ~ "ATL-MB",
    Location == "ATL" ~ "ATL-GD",
    Location %in% c("LAR", "LAC") & Year >= 2020 ~ "LA-Sofi",
    Location == "LAR" ~ "LA-Col",
    Location == "LAC" ~ "LA-Dig",
    Location %in% c("NYG", "NYJ") ~ "NYC",
    TRUE ~ Location
  )) |>
  # Create manuscript's artificial turf categories
  mutate(
    Surface_detailed = case_when(
      Surface == "Unknown Non-Slit-Film" ~ NA_character_, # NOR 2023 season
      Location == "NOR" ~ "UBU/Turfnation", # All other NOR seasons are UBU/Turfnation
      Surface == "a_turf" ~ "A-Turf",
      turf_spec %in% c("FieldTurf Vertex CORE", "FieldTurf CORE", "FieldTurf Revolution 360") ~ "FieldTurf CORE/Revolution 360",
      turf_spec == "FieldTurf Classic HD" ~ "FieldTurf Classic HD",
      Surface == "ubu/turfnation" ~ "UBU/Turfnation",
      Surface == "actglobal" ~ "Act Global",
      Surface == "momentumturf" ~ "Shaw Momentum",
      Surface == "matrixturf" ~ "Hellas Matrix",
      Surface %in% c("dessograss", "sisgrass", "hybridgrass") ~ "Hybrid",
      Surface == "grass" ~ "Natural Grass",
      Surface == "Unknown Non-Slit-Film" ~ NA_character_
    ),
    Surface_3cat = case_when(
      Surface_detailed == "Natural Grass" ~ "Natural Grass",
      Surface_detailed == "Hybrid" ~ NA,
      Surface_detailed %in% c("FieldTurf Classic HD", "UBU/Turfnation", "Act Global", "Shaw Momentum") ~ "Artificial Slit-Film",
      Surface_detailed %in% c("A-Turf", "FieldTurf CORE/Revolution 360", "Hellas Matrix") | Surface == "Unknown Non-Slit-Film" ~ "Other Artificial"
    ),
    Surface_2cat = case_when(
      Surface_detailed == "Natural Grass" ~ "Natural Grass",
      Surface_detailed == "Hybrid" ~ NA,
      TRUE ~ "Artificial Turf"
    )
  )


# Create dataset of LE injuries 2018-23, regular season, grouped by team-game
nfl_injs_le <- nfl_injs |>
  filter(
    # Restrict to injuries that occurred in regular season before final week of season
    wk_occur > 0,
    ((Year <= 2020 & wk_occur < 17) | (Year >= 2021 & wk_occur < 18)),

    # Restrict to lower extremity injuries
    INJURY_LOCATION_1 %in% c("Groin", "Knee", "Ankle", "Achilles", "Thigh", "Foot", "Hip", "Hamstring", "Quadriceps", "Leg", "Calf", "Buttocks"),
    
    # To run 3-level model for foot and ankle injuries only, comment out the preceding line
    # and use the line below instead
    # INJURY_LOCATION_1 %in% c("Ankle", "Foot"),
    
    # Restrict to injuries resulting in at least one missed game
    gmissed != 0
  )

# Summarize counts of LE injuries that occurred for each team for each week of each season
# Team-games without any injuries will not appear in this dataset
inj_mod <- nfl_injs_le |>
  group_by(Team, wk_occur, Year) |>
  summarize(n_injs = n())

# Join count of LE injuries to the game and surface data
model_data_final <- games |>
  left_join(inj_mod, by = c("Tm" = "Team", "Year" = "Year", "Week" = "wk_occur")) |>
  # If nothing was joined it means there were no injuries; set to 0
  mutate(n_injs = coalesce(n_injs, 0))

rm(inj_mod)

# Add a new variable splitting out natural grass types
model_data_final <- model_data_final |>
  mutate(Surface_all = case_when(
    Stadium %in% c("ARI", "BAL", "CAR", "CHI", "JAX", "KC", "LA-Col", "LA-Dig", "LV", "MIA", "OAK", "PHI", "SF-L", "TB", "TEN", "WAS") ~ "Bermuda",
    Stadium %in% c("CLE", "DEN", "PIT", "CHI") ~ "Kentucky Bluegrass",
    Stadium == "MEX" ~ "Kikuyu",
    TRUE ~ as.character(Surface_detailed) # handle any remaining levels as character strings
  ))

# Create new variable for team-year
model_data_final$TeamYear <- paste(model_data_final$Tm, model_data_final$Year)


# Export final model data we can share publicly on Github
# write.csv(model_data_final, "./model_data_final.csv")



# Create descriptive tables for game turfs and injuries

library(table1)

# Create labels
label(nfl_injs_le$INJURY_LOCATION_1) <- "Injury Location"
label(nfl_injs_le$side) <- "Offense vs. Defense vs. Special Teams"
label(nfl_injs_le$wk_occur) <- "Week Occurred"
label(nfl_injs_le$gmissed) <- "Games Missed"

# Create inj table
inj_table <- table1(~ INJURY_LOCATION_1 + side + wk_occur + gmissed,
  data = nfl_injs_le,
  caption = "Injury Characteristics"
)

inj_table


label(games$Surface_detailed) <- "Detailed Surface Type"
label(games$Surface_3cat) <- "3-Level Surface Type"

# Create game/surface table
game_table <- table1(~ Surface_detailed + Surface_3cat,
  data = games,
  caption = "Game Turf Characteristics"
)

game_table









# Make natural grass the reference level for modeling purposes
model_data_final <- model_data_final %>% 
  mutate(Surface_3cat = fct_relevel(Surface_3cat, "Natural Grass", "Other Artificial"),
         Surface_detailed = fct_relevel(Surface_detailed, "Natural Grass"))


# Table Steps 
# 3 cat
## 3 cat crude

  injs_3cat <- model_data_final |>
    group_by(Surface_3cat) |> # Numerators
    summarize(inj = sum(n_injs))
  #
  tempstad_3cat <- games |>
    group_by(Surface_3cat) |>
    summarize(tg = n())
  #
  ir_table_3cat <- injs_3cat |>
    left_join(tempstad_3cat, by = "Surface_3cat") |>
    mutate(
      rate = inj / tg,
      rate_ll = (inj - 1.96 * sqrt(inj)) / tg,
      rate_ul = (inj + 1.96 * sqrt(inj)) / tg
    )
  
  # 3 category model:
  m3cat <- glmer(
    n_injs ~ 1 + Surface_3cat +
      (1 | TeamYear) + (1 | Stadium),
    data = model_data_final, family = poisson(link = "log")
  )
  summary(m3cat)
  exp(fixef(m3cat))
  confint(m3cat, method = "Wald")
  #confint(m3cat)
  
  model_means <- emmeans(m3cat, ~Surface_3cat, type = "response")
  cat_mod_sum <- as.data.frame(summary(model_means)) |>
    rename(model_ll = asymp.LCL, model_ul = asymp.UCL, mod_rate = rate) |>
    dplyr::select(-SE, -df)
  cat_table <- ir_table_3cat |>
    filter(!is.na(Surface_3cat)) |>
    dplyr::select(-inj, -tg) |>
    rename(crude_rate = rate) |>
    left_join(cat_mod_sum, by = "Surface_3cat")
  
    # 3 cat table 
  cat_table |>
    mutate(
      `Crude Rate (95% CI)` = paste0(
        sprintf("%.2f", crude_rate),
        " (", sprintf("%.2f", rate_ll),
        "–", sprintf("%.2f", rate_ul), ")"
      ),
      `Model Rate (95% CI)` = paste0(
        sprintf("%.2f", mod_rate),
        " (", sprintf("%.2f", model_ll),
        "–", sprintf("%.2f", model_ul), ")"
      )
    ) |>
    dplyr::select(Surface_3cat, `Crude Rate (95% CI)`, `Model Rate (95% CI)`) |>
    gt() |>
    tab_header(
      title = "Injury Rates by Three Category Surface Type",
      subtitle = "Crude and model-adjusted rates per team game"
    ) |>
    cols_label(
      Surface_3cat = "Surface Type",
      `Crude Rate (95% CI)` = "Crude Injury Rate (95% CI)",
      `Model Rate (95% CI)` = "Model-Adjusted Injury Rate (95% CI)"
    ) |>
    cols_align(
      align = "center",
      columns = c(`Crude Rate (95% CI)`, `Model Rate (95% CI)`)
    ) |>
    tab_footnote(
      footnote = "Rates represent injuries per game. Model adjusted for team-year and stadium random effects."
    ) |>
    tab_options(
      table.font.size = 12,
      heading.title.font.size = 14
    )

  # 3-category plot
  
  cat_mod_sum %>% 
    ggplot(aes(x = Surface_3cat, y = mod_rate, color = Surface_3cat)) +
    geom_point() +
    geom_errorbar(aes(ymin = model_ll, ymax = model_ul), width = 0.2) +
    ylim(c(0,NA)) +
    labs(x = "Surface Type", y = "Adjusted Injury Rate per Team-Game") +
    scale_color_manual(values=c("darkgreen", "blue", "red")) +
    theme_minimal() +
    theme(legend.position = "none") +
    coord_flip()
  
    


# detailed table creation
  
  
  injs_det <- model_data_final |>
    group_by(Surface_detailed) |> # Numerators
    summarize(inj = sum(n_injs))
  #
  tempstad_det <- games |>
    group_by(Surface_detailed) |>
    summarize(tg = n())
  #
  ir_table_det <- injs_det |>
    left_join(tempstad_det, by = "Surface_detailed") |>
    mutate(
      rate = inj / tg,
      rate_ll = (inj - 1.96 * sqrt(inj)) / tg,
      rate_ul = (inj + 1.96 * sqrt(inj)) / tg
    )
  set.seed(05282025)
  
  mod_all <- glmer(
    n_injs ~ 1 + Surface_detailed +
      (1 | TeamYear) + (1 | Stadium),
    data = model_data_final, family = poisson(link = "log"),
    control = glmerControl(
      optimizer = "bobyqa",
      optCtrl = list(maxfun = 5e5),
      tolPwrss = 1e-8 # note: getting conversion error, had to change iterations and optimizer per substack
    )
  )
  
  summary(mod_all)
  confint(mod_all, method = "Wald")
  confint(mod_all)
  
  model_means <- emmeans(mod_all, ~Surface_detailed, type = "response")
  mod_sum <- as.data.frame(summary(model_means)) |>
    rename(model_ll = asymp.LCL, model_ul = asymp.UCL, mod_rate = rate) |>
    dplyr::select(-SE, -df)
  det_table <- ir_table_det |>
    filter(!is.na(Surface_detailed)) |>
    dplyr::select(-inj, -tg) |>
    rename(crude_rate = rate) |>
    left_join(mod_sum, by = "Surface_detailed")
  
  
  det_table |>
    mutate(
      `Crude Rate (95% CI)` = paste0(
        sprintf("%.2f", crude_rate),
        " (", sprintf("%.2f", rate_ll),
        "–", sprintf("%.2f", rate_ul), ")"
      ),
      `Model Rate (95% CI)` = paste0(
        sprintf("%.2f", mod_rate),
        " (", sprintf("%.2f", model_ll),
        "–", sprintf("%.2f", model_ul), ")"
      )
    ) |>
    dplyr::select(Surface_detailed, `Crude Rate (95% CI)`, `Model Rate (95% CI)`) |>
    gt() |>
    tab_header(
      title = "Injury Rates by Detailed Surface Type",
      subtitle = "Crude and model-adjusted rates per team game"
    ) |>
    cols_label(
      Surface_detailed = "Surface Type",
      `Crude Rate (95% CI)` = "Crude Injury Rate (95% CI)",
      `Model Rate (95% CI)` = "Model-Adjusted Injury Rate (95% CI)"
    ) |>
    cols_align(
      align = "center",
      columns = c(`Crude Rate (95% CI)`, `Model Rate (95% CI)`)
    ) |>
    tab_footnote(
      footnote = "Rates represent injuries per game. Model adjusted for team-year and stadium random effects."
    ) |>
    tab_options(
      table.font.size = 12,
      heading.title.font.size = 14
    )
  
  # Detailed category plot
  
  mod_sum %>% 
    ggplot(aes(x = Surface_detailed, y = mod_rate, color = Surface_detailed)) +
    geom_point() +
    geom_errorbar(aes(ymin = model_ll, ymax = model_ul), width = 0.2) +
    ylim(c(0,NA)) +
    labs(x = "Surface Type", y = "Adjusted Injury Rate per Team-Game",
         caption = "Red = artificial slit-film turf, Blue = other artificial turf, 
         Green = Natural Grass, Black = Hybrid surface.") +
    scale_color_manual(values=c("darkgreen", "blue", "red", "red", "blue", "blue", "black", "red", "red")) +
    theme_minimal() +
    theme(legend.position = "none") +
    coord_flip()


  
  
  
  
  # 2-category model for plot by year
  
  annual_plot_list <- list()
  
  # For each season 2018-2023...
  for(i in 2018:2023){
  
    #...run 2 category model:
    m2cat <- glmer(
      n_injs ~ 1 + Surface_2cat +
        (1 | TeamYear) + (1 | Stadium),
      data = filter(model_data_final, Year == i), family = poisson(link = "log")
    )
    
    # Get adjusted means and 95% CIs and store results in a list
    model_means <- emmeans(m2cat, ~Surface_2cat, type = "response")
    annual_plot_list[[i-2017]] <- as.data.frame(summary(model_means)) |>
      mutate(Year = i) %>% 
      rename(model_ll = asymp.LCL, model_ul = asymp.UCL, mod_rate = rate) |>
      dplyr::select(-SE, -df)
  }
  
 # Combine yearly model results into one data frame
 annual_plot_combined <- 
   rbind(annual_plot_list[[1]], annual_plot_list[[2]], annual_plot_list[[3]],
        annual_plot_list[[4]], annual_plot_list[[5]], annual_plot_list[[6]])
  
  # 2-category yearly plot
  
  annual_plot_combined %>% 
    ggplot(aes(x = Year, y = mod_rate, color = Surface_2cat)) +
    geom_point() +
    geom_line() +
    geom_errorbar(aes(ymin = model_ll, ymax = model_ul), width = 0.2) +
    ylim(c(0,NA)) +
    labs(x = "Season", y = "Adjusted Injury Rate per Team-Game") +
    scale_color_manual(values=c("blue", "darkgreen")) +
    theme_minimal() +
    theme(legend.position = "bottom", legend.title = element_blank())