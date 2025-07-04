# Copyright 2025 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

library(tidyverse)
library(here)
library(conflicted)
library(readxl)
library(fpp3)
library(distributional)
conflicts_prefer(dplyr::filter)
#load the data------------------

rtra_files <- list.files(here("data"), pattern = "lfsstat4digNAICS")
mapping <- read_excel(here("data","industry_mapping_2025_with_stokes_agg.xlsx"))

h_data <- vroom::vroom(here("data", rtra_files))|>
  na.omit()|>
  filter(LF_STAT=="Employed")|>
  #calculate the monthly totals to filter out 0s at end of LFS data
  group_by(SYEAR,SMTH)|>
  mutate(total=sum(`_COUNT_`))|>
  filter(total>0)|>#months with 0 total employment gone.
  select(-total)|>
  inner_join(mapping, by=c("NAICS_5"="naics_5"))|>
  group_by(lmo_detailed_industry, aggregate_industry,  SYEAR, SMTH)|>
  summarise(lfs=sum(`_COUNT_`, na.rm=TRUE))|>
  mutate(when=yearmonth(paste(SYEAR, SMTH, sep="/")))|>
  ungroup()|>
  select(-SYEAR,-SMTH)|>
  tsibble(index=when, key=c(lmo_detailed_industry, aggregate_industry))|>
  aggregate_key(aggregate_industry / lmo_detailed_industry, lfs = sum(lfs))


models <- h_data |>
  filter(ym(when)<max(ym(when))-years(1)) |> #hold back 3 years for accuracy testing
  model(
    ets = ETS(log(lfs)~trend("Ad")),
    tslm = TSLM(log(lfs) ~ trend() + season())
  )

# --- Step 2: Reconcile the models before forecasting ---
# We reconcile each model separately — here using MinT (min_trace)
reconciled_models <- models %>%
  reconcile(
    ets = min_trace(ets, method = "mint_shrink"),
    tslm = min_trace(tslm, method = "mint_shrink")
  )

# --- Step 3: Forecast reconciled models ---
fc_reconciled <- reconciled_models %>%
  forecast(h = "1 years")

ets_fc <- fc_reconciled |>
  as_tibble()|>
  filter(.model == "ets")|>
  select(-.model, -lfs)
tslm_fc <- fc_reconciled |>
  as_tibble()|>
  filter(.model == "tslm")|>
  select(-.model, -lfs)

# Combine point forecasts with equal weights
combined_fc <- full_join(ets_fc, tslm_fc, by = join_by(aggregate_industry, lmo_detailed_industry, when))|>
  mutate(forecast= 0.5 * .mean.x + 0.5 * .mean.y) |>
  select(-.mean.x, -.mean.y)

long <- full_join(h_data, combined_fc)|>
  pivot_longer(c(lfs, forecast), names_to = "series", values_to = "value", values_drop_na = TRUE)

long|>
  filter(is_aggregated(lmo_detailed_industry),
         ym(when)>ym(max(when))-years(10))|>
  ggplot(aes(x=when, y=value, color=series)) +
  geom_line() +
  facet_wrap(~aggregate_industry, scales = "free_y")+
  labs(x=NULL, y=NULL)+
  scale_y_continuous(labels = scales::number_format(scale = 1/1000, suffix = "K", big.mark = ","))

long|>
  filter(!is_aggregated(lmo_detailed_industry),
         ym(when)>ym(max(when))-years(10))|>
  ggplot(aes(x=when, y=value, color=series)) +
  geom_line() +
  facet_wrap(~lmo_detailed_industry, scales = "free_y")+
  labs(x=NULL, y=NULL)+
  scale_y_continuous(labels = scales::number_format(scale = 1/1000, suffix = "K", big.mark = ","))




