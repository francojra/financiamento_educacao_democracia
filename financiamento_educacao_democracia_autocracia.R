
# Financiamento em educação - Países democratas e autocratas -------------------------------------------------------------------------------
# Autoria do script: Jeanne Franco ---------------------------------------------------------------------------------------------------------
# Data: 06/01/23 ---------------------------------------------------------------------------------------------------------------------------
# Referência: https://ourworldindata.org/financing-education -------------------------------------------------------------------------------

# Sobre os dados ---------------------------------------------------------------------------------------------------------------------------


# Carregar pacotes -------------------------------------------------------------------------------------------------------------------------

library(tidyverse)
library(cols4all)
library(hrbrthemes)
library(ggthemes)

# Carregar dados ---------------------------------------------------------------------------------------------------------------------------

fin_educ <- read.csv("public-education-expenditure-as-share-of-gdp.csv")
view(fin_educ)
names(fin_educ)

# Manipular dados --------------------------------------------------------------------------------------------------------------------------

fin_educ <- fin_educ %>%
  select(-Code) %>%
  rename(fin_educa = Public.Expenditure.on.Education..Tanzi...Schuktnecht..2000..) %>%
  view()

fin_educ1 <- fin_educ %>%
  filter(Entity %in% c("Norway", "France",
                       "United Kingdom", "Japan")) %>%
  group_by(Entity) %>%
  summarise(media = mean(fin_educa),
            sd = sd(fin_educa), n = n(),
            se = sd/sqrt(n)) %>%
  view()
