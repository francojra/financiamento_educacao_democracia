
# Financiamento em educação - Países democratas e autocratas -------------------------------------------------------------------------------
# Autoria do script: Jeanne Franco ---------------------------------------------------------------------------------------------------------
# Data: 06/01/23 ---------------------------------------------------------------------------------------------------------------------------
# Referência: https://ourworldindata.org/financing-education -------------------------------------------------------------------------------

# Sobre os dados ---------------------------------------------------------------------------------------------------------------------------

### Na maioria dos países, a educação básica não é vista apenas como um direito, mas também como
### um dever - governos basicamente esperam aumentar o acesso da população à educação básica, enquanto
### cidadãos frequentemente são obrigados por lei a atingir um determinado nível educacional.

### Isso nem sempre foi o caso: os avanços dessas ideias iniciou apenas no século 19, quando a maioria
### dos países industrializados de hoje começou a expandir a educação primária, principalmente através
### de financiamentos públicos e intervenção governamental. Dados desse período inicial mostram que
### os fundos governamentais para financiar a expansão da educação vem de diferentes fontes, mas
### especificamente taxas em nível local joga um papel importante. O papel histórico do financiamento
### local para escolas públicas é importante para ajudar-nos a compreender mudanças - ou persistências -
### nas desigualdades regionais.

### A segunda metade do século 20 marcou o início da expansão da educação como um fenômeno global.
### Dados disponíveis mostram que nos anos 90 os gastos do governo com educação como uma porcentagem
### dos rendimentos nacionais em muitos países em desenvolvimento foram próximos a média observada
### em países desenvolvidos.

### A expansão da educação global no século 20 resultou em uma redução histórica na desigualdade da 
### educação em todo o mundo: no período de 1960 a 2010 a desigualdade da educação foi reduzindo a cada ano
### para todas as idades em todas as regiões do mundo. Estimativas recentes da desigualdade educacional
### e tornos de grupos de idades sugerem que reduções futuras na desigualdade são ainda esperadas em países
### desenvolvidos.

### Recentes dados da UNESCO nos diz que o mundo está financiando com gastos governamentais a educação
### hoje, e esses fundos adicionais públicos para educação não são necessariamente à custa de outros 
### setores governamentais. Ainda além dessas tendências globais, existe ainda substancial heterogeneidade
### entre países e entre regiões. Em países de alta renda, por exemplo, as famílias apresentam maiores
### despesas na educação para os níveis superiores do que nos níveis mais baixos, mas em países de baixa
### renda esse não é o caso. Malawui é um exemplo: educação superior é quase completamento subsidiada
### pelo estado, ainda que as famílias contribuam quase 20% dos custos na educação primária.

### Seguindo o acordo dos Objetivos de Desenvolvimento do Milênio, a primeira década do século 21 viu
### um importante aumento no fluxo de financiamento internacional sob o guarda-chuva da assistência de
### desenvolvimento. Estimativas recentes mostram que a assistência ao desenvolvimento para educação tem
### parado de crescer desde 2010, com notável redução do fluxo para educação primária. Estas mudanças
### da priorização da assistência do desenvolvimento para educação entre níveis e regiões, pode ter 
### potencialmente grandes efeitos de distribuição, particularmente dentro de países de baixa renda
### que dependem substancialmente dessas fontes de financiamento para educação básica.

### Ao anaçisar correlatos, determinantes e consequências do consumo da educação, os dados macro indicam
### que despesas sobre educação não explicam bem diferenças entre países nos resultados de aprendizagem.
### Isso é um indicativo de uma complexa 'função da produção de educação' em que para alguns dados níveis
### de despesas, os resultados alcançados dependem crucialmente da mistura de entradas.

### Evidências disponíveis especificamente sobre a importância de entradas para produzir educação,
### sugerem que resultados de aprendizgem podem ser mais sensíveis a aperfeiçoamentos na qualidade
### dos professores, que na melhoria do tamanhos de turmas. Em relação aos investimentos familiares,
### a evidência de um recente experimento sugere que intervenções que aumentam os benefícios na frequência
### escolar (ex. transferência de dinheiro condicionais) são particularmente prováveis para aumentar
### tempo de estudantes na escola; e aqueles que incentivam o esforço acadêmico (ex. bolsas de estudos)
### são prováveis para melhorar resultados de aprendizagem.

### Experimentos políticos tem também mostrado que o investimento pré-escolar em inputs ao lado da procura
### leva a grandes impactos positivos na educação - e em outro importante resultado futuro na vida.
### O ambiente que a criança é exposta no início da vida, apresenta uma importante função em formar
### as habilidades, comportamentos e talentos dela.

# Carregar pacotes -------------------------------------------------------------------------------------------------------------------------

library(tidyverse)
library(cols4all)
library(hrbrthemes)
library(ggthemes)

# Carregar dados ---------------------------------------------------------------------------------------------------------------------------

fin_educ <- read.csv("public-education-expenditure-as-share-of-gdp.csv")
view(fin_educ)
names(fin_educ)

fin_educ_s <- read.csv("total-government-expenditure-on-education-gdp.csv")
view(fin_educ_s)
names(fin_educ_s)

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

fin_educ_s <- fin_educ_s %>%
  select(-Code) %>%
  rename(fin_educa = Government.expenditure.on.education..total....of.GDP.) %>%
  view()

fin_educ1_s <- fin_educ_s %>%
  filter(Entity %in% c("North America", "Japan", "Germany",
                       "China", "Cuba")) %>%
  group_by(Entity) %>%
  summarise(media = mean(fin_educa),
            sd = sd(fin_educa), n = n(),
            se = sd/sqrt(n)) %>%
  view()

fin_educ2_s <- fin_educ_s %>%
  filter(Entity %in% c("North America", "Japan", "Germany",
                       "China", "Cuba")) %>%
  view()

fin_educ3_s <- fin_educ_s %>%
  filter(Entity %in% c("North America", "China", "Brazil")) %>%
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
  scale_x_discrete(labels = c("França", "Japão", "Reino Unido", "Noruega")) +
  labs(x = "Países", y = "Despesas públicas de educação\n como percentagem do PIB") +
  theme_ipsum(axis_text_size = 14, axis_title_size = 16) +
  theme(legend.position = "none",
        axis.text = element_text(color = "black"))

ggplot(fin_educ2, aes(x = factor(Year), y = fin_educa, 
                      group = Entity, color = Entity)) +
  geom_point(shape = 15, size = 2.5) +
  geom_line(size = 1.2) +
  scale_color_manual(values = c("#88CCEE", "#CC6677",
                                "#DDCC77", "#117733")) +
  labs(x = "Tempo (anos)", 
       y = "Despesas públicas de educação\n como percentagem do PIB",
       col = "Países") +
  theme_ipsum(axis_text_size = 14, axis_title_size = 16) +
  theme(axis.text = element_text(color = "black")) 
  
# # ----------------------------------------------------------------------------------------------------------------------------------------

c4a("safe", 5)

ggplot(fin_educ1_s, aes(x = fct_reorder(Entity, media), y = media, fill = Entity)) +
  geom_col(width = 0.9) +
  geom_errorbar(aes(ymin = media - se, ymax = media + se),
                width = 0.2, size = 0.8) +
  scale_y_continuous(expand = expansion(mult = c(0.0))) +
  scale_fill_manual(values = c("#88CCEE", "#CC6677",
                               "#DDCC77", "#117733",
                               "#332288")) +
  scale_x_discrete(labels = c("China", "Japão", "Alemanha", 
                              "América do Norte", "Cuba")) +
  labs(x = "Países", y = "Despesas totais na educação (%)") +
  theme_ipsum(axis_text_size = 14, axis_title_size = 16) +
  theme(legend.position = "none",
        axis.text = element_text(color = "black"))

ggplot(fin_educ2_s, aes(x = Year, y = fin_educa, 
                      group = Entity, color = Entity)) +
  geom_point(shape = 15, size = 2.5) +
  geom_line(size = 1.2) +
  scale_color_manual(values = c("#88CCEE", "#CC6677",
                                "#DDCC77", "#117733",
                                "#332288"),
                     labels = c("China", "Cuba", "Alemanha",
                                "Japão", "América do Norte")) +
  labs(x = "Tempo (anos)", 
       y = "Despesas totais na educação (%)",
       col = "Países") +
  theme_ipsum(axis_text_size = 14, axis_title_size = 16) +
  theme(axis.text = element_text(color = "black")) 

ggplot(fin_educ3_s, aes(x = Year, y = fin_educa, 
                      group = Entity, color = Entity)) +
  geom_line(size = 2) +
  scale_color_manual(values = c('#1B9E77', '#999999','#E69F00'),
                     labels = c("Brasil", "China", "América do Norte")) +
  labs(x = "Tempo (anos)", 
       y = "Despesas totais na educação (%)",
       col = "Países") +
  theme_light() +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(color = "black", size = 15),
        legend.text = element_text(size = 12))
