dataRatios <- read.csv2('E:/dataSetMarcadoresFinancieros.csv', header=TRUE, dec=',', stringsAsFactors = FALSE, sep = ";")
dataRatios<-dataRatios[dataRatios$anio==2019,]
datos3<-data.frame(empresa=dataRatios$codigoEmpresa,
                   RE1=dataRatios$RE1,
                   RE2=dataRatios$RE2,
                   RE3=dataRatios$RE3,
                   RE4=dataRatios$RE4,
                   RE5=dataRatios$RE5,
                   RE6=dataRatios$RE6,
                   EF2=dataRatios$EF2,
                   EF3=dataRatios$EF3,
                   EF4=dataRatios$EF4,
                   EF5=dataRatios$EF5,
                   PR1=dataRatios$PR1,
                   SF1=dataRatios$SF1,
                   SF2=dataRatios$SF2,
                   SF7=dataRatios$SF7,
                   SF8=dataRatios$SF8,
                   SF10=dataRatios$SF10,
                   SF11=dataRatios$SF11,
                   LI1=dataRatios$LI1,
                   LI2=dataRatios$LI2,
                   LI3=dataRatios$LI3,
                   LI4=dataRatios$LI4,
                   LI5=dataRatios$LI5,
                   LI6=dataRatios$LI6,
                   LI7=dataRatios$LI7,
                   LI8=dataRatios$LI8,
                   LI9=dataRatios$LI9,
                   LI10=dataRatios$LI10,
                   LI11=dataRatios$LI11,
                   LI12=dataRatios$LI12,
                   RO1=dataRatios$RO1,
                   RO2=dataRatios$RO2,
                   RO3=dataRatios$RO3,
                   RO4=dataRatios$RO4,
                   RO5=dataRatios$RO5,
                   decision=dataRatios$decision)

set.seed(130)
dividir <- sample(2, nrow(datos3), replace = TRUE, prob = c(0.8, 0.2))
entrenamiento <- datos3[dividir == 1, ]
prueba <- datos3[dividir == 2, ]
modelo_arbol<-rpart::rpart(formula=decision~., data = entrenamiento)
rpart.plot::rpart.plot(modelo_arbol)
prediccion_arbol <-predict(modelo_arbol, prueba[,-36])
MC<-table(prueba$decision,prediccion_arbol)
MC



