
Practica1 <- read.table("D:/Cursos/00 URJC/01 Primero/07 Estadistica/00 Practicas/PRACTICA 1/hábitos.txt",header=TRUE, stringsAsFactors=TRUE, sep="", na.strings="9", dec=".", strip.white=TRUE)
Practica1 <- within(Practica1, {
  sexo <- factor(sexo, labels=c('Hombre','Mujer'))
})
Practica1 <- within(Practica1, {
  centro <- factor(centro, labels=c('Público','Privado'))
})
Practica1 <- within(Practica1, {
  estudios <- factor(estudios, labels=c('EGB','BUP','FP'))
})
Practica1 <- within(Practica1, {
  hábitat <- factor(hábitat, labels=c('Rural','Urbano'))
})
Practica1 <- within(Practica1, {
  estupadr <- factor(estupadr, labels=c('Sin estudios','Estudios primarios','Bachiller o equivalente','Diplomado, licenciado o equivalente'))
})
Practica1 <- within(Practica1, {
  estumadr <- factor(estumadr, labels=c('Sin estudios','Estudios primarios','Bachiller o equivalente','Diplomado, licenciado o equivalente'))
})
Practica1 <- within(Practica1, {
  univ <- factor(univ, labels=c('Si','No'))
})
Practica1 <- within(Practica1, {
  gustcine <- factor(gustcine, labels=c('Solo','Acompañado','Indiferente'))
})
Practica1 <- within(Practica1, {
  tipocine <- factor(tipocine, labels=c('Amor','Humor','Violencia','Sexo'))
})
Practica1 <- within(Practica1, {
  violen <- factor(violen, labels=c('Activo','Pasivo'))
})
Practica1 <- within(Practica1, {
  impdin <- factor(impdin, labels=c('Muy poca','Poca','Media','Alta','Muy alta'))
})
Practica1 <- within(Practica1, {
  impest <- factor(impest, labels=c('Muy poca','Poca','Media','Alta','Muy alta'))
})
Practica1 <- within(Practica1, {
  físico <- factor(físico, labels=c('Muy poca','Poca','Media','Alta','Muy alta'))
})
Practica1 <- within(Practica1, {
  depor <- factor(depor, labels=c('Muy poca','Poca','Media','Alta','Muy alta'))
})
editDataset(Practica1)
sapply(Practica1, function(x)(sum(is.na(x)))) # NA counts

