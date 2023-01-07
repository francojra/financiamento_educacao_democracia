
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

fin_educ2 <- fin_educ %>%
  filter(Entity %in% c("Norway", "France",
                       "United Kingdom", "Japan")) %>%
  view()

# Gráficos ---------------------------------------------------------------------------------------------------------------------------------

c4a("safe", 4)

ggplot(fin_educ1, aes(x = fct_reorder(Entity, media), y = media, fill = Entity)) +
  geom_col(width = 0.9) +
  geom_errorbar(aes(ymin = media - se, ymax = media + se),
                width = 0.2, size = 0.8) +
  scale_y_continuous(expand = expansion(mult = c(0.0))) +
  scale_fill_manual(values = c("#88CCEE", "#CC6677",
                               "#DDCC77", "#117733")) +
  labs(x = "Países", y = "Despesas públicas de educação\n como percentagem do PIB") +
  theme_ipsum(axis_text_size = 14, axis_title_size = 16) +
  theme(legend.position = "none",
        axis.text = element_text(color = "black"))
