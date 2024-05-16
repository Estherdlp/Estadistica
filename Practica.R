
#Código de estadísticos:
###############################################
#Cuestión 1: Distribución porcentual del sexo.#
###############################################
 
    #SEXO: Porcentajes dividido por hombres y mujeres:
local({
  .Table <- with(Practica1, table(sexo))
  cat("\ncounts:\n")
  print(.Table)
  cat("\npercentages:\n")
  print(round(100*.Table/sum(.Table), 2))
})
    #Representación gráfica -> Gráfica de sectores:
library(colorspace, pos=17)
with(Practica1, piechart(sexo, xlab="", ylab="",
  main="Distribucion porcentual por sexo",
  col=rainbow_hcl(2), scale="percent"))
 
 
##################################################################
#Cuestion 2: Promedio de asistencia al cine de hombres y mujeres.#
##################################################################
 
    #CINE: media dividido por sexo:
numSummary(Practica1[,"cine", drop=FALSE], groups=Practica1$sexo, 
  statistics=c("mean"), quantiles=c(0,.25,.5,.75,1))
    #Representacion gráfica -> diagrama de cajas:
Boxplot(cine~sexo, data=Practica1, id=list(method="none"), xlab="Sexo",
ylab="Asistencia anual al cine", main="Asistencia al cine por sexos")
 

################################################################################# 
#Cuestión 3: Variabilidad de asistencia anual a conciertos de hombres y mujeres.#
#################################################################################
    #CONCIERTOS: Desviación típica, rango intercuartílico, coeficiente de variación dividido por sexo:
numSummary(Practica1[,"conciert", drop=FALSE], groups=Practica1$sexo, 
  statistics=c("sd", "IQR", "cv"), quantiles=c(0,.25,.5,.75,1))
    #Representación gráfica -> diagrama de cajas:
Boxplot(conciert~sexo, data=Practica1, id=list(method="none"), 
  xlab="Sexo", ylab="Asistencia anual a conciertos", 
  main="Asistencia a conciertos por sexos")
 
 
########################################################################################
#Cuestión 4: Comparación de la dispersión de las variables futbol e ingresos mensuales.#
########################################################################################
 
    #FUTBOL: Desviación típica, rango intercuartílico, coeficiente de variación:
numSummary(Practica1[,"fútbol", drop=FALSE], statistics=c("sd", "IQR", "cv"),
   quantiles=c(0,.25,.5,.75,1))
    #Representación gráfica -> diagrama de cajas:
Boxplot( ~ fútbol, data=Practica1, id=list(method="none"), 
  ylab="Número de partidos de futbol", main="Asistencia a partidos de futbol")
Boxplot( ~ ingr, data=Practica1, id=list(method="none"), ylab="Euros", main="Ingresos mensuales del núcleo familiar")
 

###########################################################################################################
#Cuestión 5: Diferencias entre la distribución de cuartiles de la calificación media según tipo de centro.#
###########################################################################################################
 
    #CALIFICACION MEDIA: Rango intercuartílico y cuartiles:
numSummary(Practica1[,"califest", drop=FALSE], groups=Practica1$centro, 
  statistics=c("IQR", "quantiles"), quantiles=c(0,.25,.5,.75,1))
    #Representacion gráfica -> diagrama de cajas:
Boxplot(califest~centro, data=Practica1, id=list(method="none"), 
  xlab="Centro", ylab="Calificaciones medias", 
  main="Calificaciones medias segun el centro")
    #Representación gráfica -> diagrama de barras:
with(Practica1, discretePlot(califest, by=centro, scale="frequency", 
  xlab="Calificaciones medias", ylab="Frecuencias", 
  main="Calificaciones medias segun el centro"))
 
 
################################################################################################################
# Cuestión 6: Calculamos la valoración por el físico de las personas a las que les interesa mucho el deporte.  #
################################################################################################################

#Damos orden a las opciones que pueden tomar la variable físico y depor:
Practica1$físico <- with(Practica1, factor(físico, 
  levels=c('Muy poca','Poca','Media','Alta',
  'Muy alta')))
Practica1$depor <- with(Practica1, factor(depor, 
  levels=c('Muy poca','Poca','Media','Alta','Muy alta'),
   ordered=TRUE))
#Creamos una tabla de contingencia absoluta:
local({
  .Table <- xtabs(~físico+depor, data=Practica1)
  cat("\nFrequency table:\n")
  print(.Table)
})
#Creamos una tabla de contingencia relativa (porcentaje):
local({
  .Table <- xtabs(~físico+depor, data=Practica1)
  cat("\nFrequency table:\n")
  print(.Table)
  cat("\nColumn percentages:\n")
  print(colPercents(.Table))
})
 
# Creamos una tabla de distribución marginal absoluta de las personas que tienen una afición alta por el deporte, con la variable físico:
AficionAltaDepor <- subset(Practica1, subset=depor=='Alta', select=c(físico))
# Hacemos un gráfico de barras, de la importancia que le dan al físico las personas que tienen una afición alta por el deporte:
with(AficionAltaDepor, Barplot(físico, xlab="Físico", ylab="Número de personas", main="Valoración por el físico de las personas que tienen una afición alta por el deporte", 
  label.bars=TRUE))
#Obsevamos el resultado matemáticamente:
summary(AficionAltaDepor)
 
#Repetimos el procedimiento anterior creando una tabla de distribución marginal absoluta de las personas que tienen una afición muy alta por el deporte, con la variable físico:
AficionMuyAltaDepor <- subset(Practica1, subset=depor=='Muy alta', select=c(físico))
# Hacemos un gráfico de barras de la importancia que le dan al físico las personas que les gusta mucho el deporte:
with(AficionMuyAltaDepor, Barplot(físico, xlab="Físico", ylab="Número de personas", main="Valoración por el físico de las personas que tienen una afición muy alta por el deporte", 
  label.bars=TRUE))
#Obsevamos el resultado matemáticamente:
summary(AficionMuyAltaDepor)
 
 
##################################################################################################################################
# Cuestión 7: ¿Cuál es la valoración que le dan a los estudios aquellos estudiantes cuyos padres tienen mayor nivel de estudios? #
##################################################################################################################################

#Damos orden a las opciones que pueden tomar la variable estupadr, estumadr y imprest 
Practica1$estumadr <- with(Practica1, factor(estumadr, 
   levels=c('Sin estudios','Estudios primarios', 
  'Bachiller o equivalente', 
  'Diplomado, licenciado o equivalente'), ordered=TRUE)) 
Practica1$estupadr <- with(Practica1, factor(estupadr, 
   levels=c('Sin estudios','Estudios primarios', 
  'Bachiller o equivalente', 
  'Diplomado, licenciado o equivalente'), ordered=TRUE)) 
Practica1$impest <- with(Practica1, factor(impest,  
  levels=c('Muy poca','Poca','Media','Alta','Muy alta'), 
   ordered=TRUE)) 

#Creamos una tabla de contingencia absoluta del nivel de estudios de los padres e importancia dada a los estudios:
local({ 
  .Table <- xtabs(~estupadr+impest, data=Practica1) 
  cat("\nFrequency table:\n") 
  print(.Table) 
}) 
#Creamos una tabla de contingencia reletiva del nivel de estudios de los padres e importancia dada a los estudios:
local({ 
  .Table <- xtabs(~estupadr+impest, data=Practica1) 
  cat("\nFrequency table:\n") 
  print(.Table) 
  cat("\nColumn percentages:\n") 
  print(colPercents(.Table)) 
}) 
#Creamos una tabla de distribución marginal absoluta de las personas cuyos padres son Diplomado, licenciado o equivalente, con la variable estudios:
EstudioPadre <- subset(Practica1, subset=estupadr=='Diplomado, licenciado o equivalente',select=c(impest))
#Creamos una gráfica para observar la valoración que les dan a los estudios:
with(EstudioPadre, Barplot(impest, xlab="Valoración por los estudios", ylab="Número de personas", main="Valoración de los estudios, hijos con padres con nivel alto de estudios", label.bars=TRUE))
#Obsevamos el resultado matemáticamente:
summary(EstudioPadre)
 
#Creamos una tabla de contingencia reletiva del nivel de estudios de las madres e importancia dada a los estudios:
local({ 
  .Table <- xtabs(~estumadr+impest, data=Practica1) 
  cat("\nFrequency table:\n") 
  print(.Table) 
}) 
#Creamos una tabla de contingencia reletiva del nivel de estudios de las madres e importancia dada a los estudios:
local({ 
  .Table <- xtabs(~estumadr+impest, data=Practica1) 
  cat("\nFrequency table:\n") 
  print(.Table) 
  cat("\nColumn percentages:\n") 
  print(colPercents(.Table)) 
}) 
#Creamos una tabla de distribución marginal absoluta de las personas cuyas madres son Diplomado, licenciado o equivalente, con la variable estudios:
EstudioMadre <- subset(Practica1, subset=estumadr=='Diplomado, licenciado o equivalente',select=c(impest))
#Creamos una gráfica para observar la valoración que les dan a los estudios:
with(EstudioMadre, Barplot(impest, xlab="Valoración por los estudios", ylab="Número de personas", main="Valoración de los estudios, hijos con madres con nivel alto de estudios", label.bars=TRUE))
#Obsevamos el resultado matemáticamente:
summary(EstudioMadre)
 
#Creamos una tabla de distribución marginal de las personas cuyos padres (ambos) son Diplomados, licenciados o equivalentes, con la variable estudios:
EstudioPadres <- subset(Practica1,subset=estupadr=='Diplomado, licenciado o equivalente'&estumadr=='Diplomado, licenciado o equivalente', select=c(impest))
#Creamos una gráfica para observar la valoración que les dan a los estudios:
with(EstudioPadres, Barplot(impest, xlab="Valoración por los estudios", ylab="Número de personas", main="Valoración de los estudios, hijos con padres (ambos) con nivel alto de estudios", label.bars=TRUE))
#Obsevamos el resultado matemáticamente:
summary(EstudioPadres)
 
 
######################################################################################################################################
# Cuestión 8: ¿Está asociada linealmente la cantidad de libros que leen al año con las horas que juegan a video juegos semanalmente? #
######################################################################################################################################
#Creamos un modelo de regresión lineal simple con una recta ajustada por mínimos, para comprobar visualmente la linealidad.
scatterplot(lect~vj, regLine=TRUE, smooth=FALSE, boxplots=FALSE, main="Gráfico de dispersión: Horas jugadas a videojuegos semanalmente-libros leídos al año", data=Practica1)
#Obtenemos el coeficiente de correlación matemáticamente (-0.839722), observando una correlación inversa muy alta: 
with(Practica1, cor.test(lect, vj, method="pearson"))
 

##########################################################################################################################################################################################
# Cuestión 9: ¿Es posible predecir la cantidad de libros que leen en un año los estudiantes a partir de las horas semanales que juegan con video juegos?, en caso afirmativo modelízalo. #
##########################################################################################################################################################################################
 
#Calculamos la ecuación de la recta de regresión, con la que predeciremos la cantidad de libros que leen en un año los estudiantes y=53.6332-1.9090x y el coeficiente de determinación (0,7067)
RegModel.3 <- lm(vj~lect, data=Practica1)
summary(RegModel.3)
#Creamos un modelo de regresión lineal simple con una recta ajustada por mínimos cuadrados(modelizado), para hacernos una idea de los posibles resultados.
scatterplot(lect~vj, regLine=TRUE, smooth=FALSE, boxplots=FALSE, main="Gráfico de dispersión: Horas jugadas a videojuegos semanalmente-libros leídos al año", data=Practica1)
 

########################################################################################################################################################################
# Cuestión 10: Si ahora realizas el mismo modelo de 9. pero diferenciado según sexo del estudiante, ¿qué diferencia existe entre el modelo de hombres y el de mujeres? #
########################################################################################################################################################################
 
#Creamos una tabla de distribución marginal absoluta de los hombres con las variables lect y Vj:
HombreLeer <- subset(Practica1, subset=sexo=='Hombre', select=c(lect,vj))
#Creamos un diagrama de dispersión, con una recta de mínimos cuadrados, para poder apreciar la relación lineal entre las dos variables:
scatterplot(lect~vj, regLine=TRUE, smooth=FALSE, boxplots=FALSE, 
  main="Gráfico de dispersión (Hombres): Horas jugadas a video juegos semanalmente-libros leídos al año",
   data=HombreLeer)
#Calculamos la ecuación de la recta de regresión, con la que predeciremos la cantidad de libros que leen en un año los estudiantes (Hombres): y=57.8713-1.9090x y el coeficiente de determinación (0.7113):
RegModel.1 <- lm(vj~lect, data=HombreLeer)
summary(RegModel.1)
#Calculamos el coeficiente de correlación (-0.8433799), para comprobar la relación lineal:
with(HombreLeer, cor.test(lect, vj, alternative="two.sided", method="pearson"))
 
#Creamos una tabla de distribución marginal absoluta de las mujeres con las variables lect y Vj:
MujerLeer <- subset(Practica1, subset=sexo=='Mujer', select=c(lect,vj))
#Creamos un diagrama de dispersión, con una recta de mínimos cuadrados, para poder apreciar la relación lineal entre las dos variables:
scatterplot(lect~vj, regLine=TRUE, smooth=FALSE, boxplots=FALSE, 
  main="Gráfico de dispersión (Mujeres): Horas jugadas a video juegos semanalmente-libros leídos al año",
   data=MujerLeer)
#Calculamos la ecuación de la recta de regresión, con la que predeciremos la cantidad de libros que leen en un año los estudiantes (Mujeres): y=49.1397-1.6385x y el coeficiente de determinación (0.6804):
RegModel.1 <- lm(vj~lect, data=MujerLeer)
summary(RegModel.1)
#Calculamos el coeficiente de correlación (r=-0.8270882), para comprobar la relación lineal:
with(MujerLeer, cor.test(lect, vj, alternative="two.sided", method="pearson"))


  


