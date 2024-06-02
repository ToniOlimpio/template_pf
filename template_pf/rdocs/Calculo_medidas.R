media <- mean(medidas_co$imdb)
mediana <- median(medidas_co$imdb)
dp <- sd(medidas_co$imdb)
variancia <- var(medidas_co$imdb)
maximo <- max(medidas_co$imdb)
minimo <- min(medidas_co$imdb)
quartis <- quantile(medidas_co$imdb, probs = c(0.25, 0.75))
print(media)
print(desvio_padrao)
print(variancia)
print(minimo)
print(mediana)
print(quartis)
print(maximo)


medidas_co$imdb <- unlist(medidas_co$imdb)
class(medidas_co$imdb)

sum(medidas_mov$imdb)
221.6/46

DV[, 'who_caught'] <- "velmacaught"
