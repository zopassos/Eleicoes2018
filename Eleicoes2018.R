getwd()

library(dplyr)
library(tidyr)
library(data.table)
library(stringi)
library(stringr)
library(xlsx)
library(reshape2)

#### CRIANDO TABELA COM RESULTADOS ####

base.tse <- fread("bweb_1t_SP_101020182030.csv", encoding="Latin-1")
locais.df <- fread("locais_temp.csv", encoding="UTF-8")

glimpse(base.tse)

base.sp <- base.tse %>%
  filter(DS_CARGO_PERGUNTA=="Presidente") %>%
  filter(NM_MUNICIPIO=="SÃO PAULO")

resultados.df <- base.sp %>%
  mutate(ZONA_SEC = paste(NR_ZONA, NR_SECAO, NR_LOCAL_VOTACAO, sep="-")) %>%
  group_by(ZONA_SEC, NM_VOTAVEL) %>%
  summarise(QT_VOTOS)

abstencoes.df <- base.sp %>%
  mutate(ZONA_SEC = paste(NR_ZONA, NR_SECAO, NR_LOCAL_VOTACAO, sep="-")) %>%
  group_by(ZONA_SEC) %>%
  summarise(QT_APTOS = round(mean(QT_APTOS),0), 
            QT_ABSTENCOES = mean(QT_ABSTENCOES))
  ### separate(ZONA_SEC, c("NR_ZONA", "NR_SECAO")) %>%
  ### mutate(abs_proporcional = round(abstencoes/votantes,3))

resultados.df <- dcast(resultados.df, ZONA_SEC ~ NM_VOTAVEL, value.var = "QT_VOTOS")
resultados.df[is.na(resultados.df)] <- 0
resultados.df <- left_join(resultados.df, abstencoes.df, by="ZONA_SEC") %>%
  separate(ZONA_SEC, c("NR_ZONA", "NR_SECAO", "NR_LOCAL_VOTACAO")) %>%
  mutate(ZONA_SEC = paste(NR_ZONA, NR_SECAO, sep="-"))

locais.df <- locais.df %>%
  mutate(ZONA = as.character(ZONA), 
         SECAO = as.character(SECAO),
         ZONA_SEC = paste(ZONA, SECAO, sep="-")) %>%
  select(-c(ZONA, SECAO))
glimpse(locais.df)

locais.df$SECAO <- as.character(locais.df$SECAO)

resultados.df <- left_join(resultados.df, locais.df, by = "ZONA_SEC") %>%
  resultados.df <- resultados.df %>%  
  select(-ZONA_SEC) %>%
  group_by(BAIRRO, LOCAL, ENDERECO, NR_ZONA, NR_SECAO, NR_LOCAL_VOTACAO) %>%
  summarise_all(sum)

fwrite(resultados.df, file="Resultados_eleicoes_SP.csv")
resultados.df <- fread("Resultados_eleicoes_SP.csv", encoding="UTF-8")
write.xlsx(as.data.frame(resultados.df), file="Resultados_eleicoes_SP.xlsx", row.names = FALSE)

#### GRAFICOS ELEICOES ####

library(ggplot2)
library(data.table)
library(dplyr)
library(tidyr)
library(scales)

tema_br_dados <- function (tam.fonte = 16, fonte = "sans", angle.x = 360, leg_pos = "bottom"){
  
  theme_minimal(base_size = 16, base_family = fonte) + 
    theme(axis.text = element_text(color = "black"),
          legend.text = element_text(),
          legend.position = "top",
          plot.subtitle = element_text(size = 14),
          plot.title = element_text(size = 18, face = "bold"),
          panel.grid = element_blank(),
          panel.grid.major.y = element_line(color = "gray90"),
          axis.line.x = element_line()
    )
}

saladio.df <- fread("ipeadata[14-10-2018-06-49].csv", encoding="UTF-8")

dados.df <- fread("ipeadata[14-10-2018-06-49].csv", encoding="UTF-8")
dados.df[,3] <- NULL
dados.df$SMR <- dados.df[,2]
dados.df[,2] <- NULL
dados.df <- dados.df %>%
  separate(Data, c("Ano", "Mes")) %>%
  mutate(Ano = as.numeric(Ano),
         Mes = as.numeric(Mes))

dados.df <- dados.df %>%
  filter(Ano %in% (1958:2018), Mes=="9")

glimpse(dados.df)

p <- ggplot(dados.df, aes(x=Ano, y=SMR)) + 
  geom_line(size=1.1, alpha=0.9, colour="#00441b") +
  tema_br_dados() +
  scale_x_continuous(breaks = seq(1958, 2018, by = 5)) +
  ###scale_y_continuous(format(y, big.mark=".")) +
  labs(color = NULL, x = NULL, y = "Salário Mínimo Real (R$)",
       caption = "\nFonte: Ipeadata (https://goo.gl/96BZwC)",
       title = "Salário Mínimo Real, 1958-2018",
       subtitle = "Crescimento constante ao longo dos últimos 20 anos")
  ###geom_vline(xintercept="1964")

p
install.packages("Cairo")
Cairo::CairoPNG(filename = "salario_minimo_19582018.png", width = 6.89, height = 4.88, dpi = 72, units = "in")

ggsave(p, file= "plot.png", dpi=300, width=6.89, height=4.88)

glimpse(dados.df)

dados.df <- dados.df %>%
  mutate(DIVIDA_PIB = round(DIVIDA/PIB, 2))
dados.df$DIVIDA_PIB = percent(DIVIDA_PIB)

dados.df <- dados.df %>%
  mutate(DIVIDA_PIB = round(DIVIDA/PIB,2),
         DATA = as.Date(DATA))

percent <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

X=0.14
percent(X)
