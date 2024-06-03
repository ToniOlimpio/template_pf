library(tidyverse)
estat_colors <- c(
  "#A11D21", "#003366", "#CC9900",
  "#663333", "#FF6600", "#CC9966",
  "#999966", "#006606", "#008091", 
  "#041835", "#666666" )

theme_estat <- function(...) {
  theme <- ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(colour = "black", size = 12),
      axis.title.x = ggplot2::element_text(colour = "black", size = 12),
      axis.text = ggplot2::element_text(colour = "black", size = 9.5),
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black"),
      legend.position = "top",
      ...
    )
  
  return(
    list(
      theme,
      scale_fill_manual(values = estat_colors),
      scale_colour_manual(values = estat_colors)
    )
  )
}

#Analise 1: Linha Bivariado
#Analise 3: Coluna Bivariado
#Analise 5: Boxplot

terarm <- bf %>%
  mutate(format = case_when(
    trans %>% str_detect("serie") ~ "serie",
    trans %>% str_detect("crossover") ~ ""
  )) %>%
  group_by(trans, drv) %>%
  summarise(freq = n()) %>%
  mutate(
    freq_relativa = round(freq / sum(freq) * 100,1)
  )
porcentagens <- str_c(trans_drv$freq_relativa, "%") %>% str_replace("
\\.", ",")
legendas <- str_squish(str_c(trans_drv$freq, " (", porcentagens, ")")
)
ggplot(trans_drv) +
  aes(
    x = fct_reorder(trans, freq, .desc = T), y = freq,
    fill = drv, label = legendas
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding =
                                        0)) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, hjust = 0.5,
    size = 3
  ) +
  labs(x = "Transmissão", y = "Frequência")

uy <- seq(1969, max(daf), by = 10)
daf <- 1969:2020

classesCa <- WC %>%
  filter(!is.na(who_caught)) %>%
  count(who_caught) %>%
  mutate(
    freq = n,
    relative_freq = round((freq / sum(freq)) * 100, 1),
    freq = gsub("\\.", ",", relative_freq) %>% paste("%", sep = ""),
    label = str_c(n, " (", freq, ")") %>% str_squish()
  )

ggplot(classesT) +
  aes(x = fct_reorder(setting_terrain, n, .desc=T), y = n, label = label) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  scale_y_continuous(breaks = oy) + 
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, #hjust = .5,
    size = 3
  ) + 
  labs(x = "Terreno", y = "Frequência") +
  theme_estat()
ggsave("colunas-uni-freq.pdf", width = 158, height = 93, units = "mm")

terarm$"trap_work_first" <- bf[, "trap_work_first"]
terarmf <- filter(terarm, trap_work_first == TRUE)


ggplot(WC) +
  aes(x = reorder(who_caught, engagement, FUN = median), y = engagement) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Quem Capturou", y = "Engajamento") +
  theme_estat()
ggsave("box_bi.pdf", width = 158, height = 93, units = "mm")
19


DO <- bf[, "caught_other"]
DV$"engagement" <- bf[, "engagement"]
DO <- filter(DO, caught_other == TRUE)
DV[, "caught_velma"] <- "caught_velma"
WC$'who_caught' <- c(WC$'who_caught', DF$caught_fred)
WC <- data.frame(DC$caught_daphnie)
names(WC) <- c("who_caught", "engagement")
WC <- rbind(WC, DV)

ggplot(mpg) +
  aes(x = reorder(who_caught, engagement, FUN = median), y = engagement) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white") +
  labs(x = "Transmissão", y = "Consumo em Cidade (milhas/galão)") +
  theme_estat()
ggsave("box_bi.pdf", width = 158, height = 93, units = "mm")

ggplot(bf) +
  aes(x = format, y = date_aired, group = format, colour = format) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(x = "Ano", y = "Preço") +
  theme_estat()
ggsave("series_grupo.pdf", width = 158, height = 93, units = "mm")

forimdb <- bf[, 'imdb']
forimdb$'season' <- bf[, 'season']
medidas_co <- filter(forimdb, season == "Crossover")

class(bf$engagement)
as.numeric(WC$engagement)
WC$engagement <- unlist(WC$engagement)
WC$who_caught <- unlist(WC$who_caught)

ggplot(if6) +
  aes(x = date_aired, y = index, group = format, colour = format) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(x = "Ano", y = "Preço") +
  theme_estat()
ggsave("series_grupo.pdf", width = 158, height = 93, units = "mm")

ggplot(DV) +
  aes(x=factor(""), y=engagement) +
  geom_boxplot(fill=c("#A11D21"), width = 0.5) +
  guides(fill=FALSE) +
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="", y="Consumo em Cidade (milhas/galão)")+
  theme_estat()

class(mpg$cty)
class(WC$who_caught)
DV$engagement <- unlist(DV$engagement)

ggplot(WC) +
  aes(x = reorder(who_caught, engagement, FUN = median), y = engagement) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white") +
  labs(x = "Quem capturou o monstro", y = "Frequencia") +
  theme_estat()
ggsave("box_bi.pdf", width = 158, height = 93, units = "mm")

#1-15  -  60-69
#16-156  -  70-79
#157-324  -  80-89
#325-338  -  90-99
#339 - 422  -  00-09
#423-572  -  10-19
#573-603  -  20-21
rm(df8c)
df2 <- bf[573:603, 'date_aired']
df6$'format' <- bf[573:603 , "format"]
df9s <- filter(df9, format == 'Serie')
df9m <- filter(df9, format == 'Movie')
df9c <- filter(df9, format == 'CrossOver')
tt4$"frequency" <- 1:8

dfg <- df0c
tt2 <- rbind(tt2, tt4)
tt2$frequency <- unlist(tt2$frequency)

ggplot(be) +
  aes(x = year, y = n, group = format, colour = format) +
  geom_line(size = 1) +
  scale_y_continuous(breaks = dy) +
  geom_point(size = 2) +
  labs(x = "Ano", y = "Frequencia") +
  theme_estat()
ggsave("series_grupo.pdf", width = 158, height = 93, units = "mm")


dfg$frequency <- unlist(dfg$frequency)
yy <- seq(1960, 2030, by = 10)

tt <- bf[, 'index']
tt$'date_aired' <- bf[, "date_aired"]
tt$'format' <- bf[, 'format']
tt2 <- filter(tt, format == "Serie")
tt3 <- filter(tt, format == "Movie")
tt4 <- filter(tt, format == 'CrossOver')
cdf2c <- count(df2c$format)
cdf2c$'format' <- 'CrossOver'
cdf2c$'year' <- 2020
cdf2c <- data.frame(c(0))
names(cdf2c) <- c("n")

be$year <- unlist(be$year)
