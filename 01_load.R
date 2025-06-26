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
conflicts_prefer(dplyr::filter)

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
  summarise(value=sum(`_COUNT_`, na.rm=TRUE))|>
  mutate(when=yearmonth(paste(SYEAR, SMTH, sep="/")))|>
  ungroup()|>
  select(-SYEAR,-SMTH)|>
  tsibble(index=when, key=c(lmo_detailed_industry, aggregate_industry))|>
  aggregate_key(aggregate_industry / lmo_detailed_industry, value = sum(value))


fit <- h_data |>
  filter(ym(when)<max(ym(when))-years(3)) |> #hold back 3 years for accuracy testing
  model(base = ETS(value)) |>
  reconcile(
    bu = bottom_up(base),
    ols = min_trace(base, method = "ols"),
    mint = min_trace(base, method = "mint_shrink")
  )

fc <- fit |>
  forecast(h = "3 years")

fcast_accuracy <- accuracy(fc, h_data)|>
  select(lmo_detailed_industry, aggregate_industry, .model, MAPE, RMSE)

fc |>
  filter(is_aggregated(lmo_detailed_industry)) |>
  autoplot(
    h_data|>filter(ym(when) >= max(ym(when)) - years(10)),
    level = NULL
  ) +
  labs(y = "Employment") +
  facet_wrap(vars(aggregate_industry), scales = "free_y")








