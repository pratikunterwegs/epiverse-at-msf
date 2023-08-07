
# a simple script to make figures for a presentation at the MSF Manson unit
pak::pak("epiverse-trace/cfr")

library(cfr)
library(epiparameter)
library(dplyr)
library(tidyr)
library(purrr)
library(covidregionaldata)
library(ggplot2)

# get covid data from OWID
# get data from {owidR}
df_covid <- get_regional_data(country = "India")

# get covid onset to death
onset_to_death_covid <- epidist_db(
  disease = "COVID-19",
  epi_dist = "onset_to_death",
  author = "Linton_etal"
)

# filter for states
# rename the cases and deaths columns using {dplyr}
df_covid <- select(
  df_covid, state, date,
  cases = cases_new, deaths = deaths_new
)

# get the total number of deaths in each country with more than 100,000 deaths
df_covid_cfr <-
  df_covid %>%
  filter(
    date < "2021-06-01" & date > "2020-06-01",
    state %in% c("Kerala", "Maharashtra", "Uttar Pradesh", "West Bengal")
  ) %>%
  nest(.by = state) %>%
  mutate(
    estimate = map(
      .x = data,
      .f = estimate_time_varying,
      correct_for_delays = TRUE,
      epidist = onset_to_death_covid
    )
  ) %>%
  unnest(
    cols = "estimate"
  )

# plot a comparison
p = ggplot(df_covid_cfr) +
    geom_ribbon(
        aes(
            date,
            ymin = severity_lo,
            ymax = severity_hi,
            fill = state
        ),
        alpha = 0.2
    ) +
    geom_line(
        aes(date, severity_me, col = state)
    ) +
    scale_colour_brewer(
        palette = "Dark2",
        name = "State"
    ) +
    scale_fill_brewer(
        palette = "Dark2",
        name = "State"
    ) +
    scale_y_continuous(
        name = "CFR"
    ) +
    scale_x_date(
        date_labels = "%b-%Y"
    ) +
    coord_cartesian(
        ylim = c(0.0, 0.35),
        xlim = c(as.Date("2021-01-01"), NA),
        expand = FALSE
    ) +
    theme_classic() +
    theme(legend.position = "top")

ggsave(
    p, filename = "figures/fig_cfr_covid_india.png",
    dpi = 300, height = 4, width = 5
)
