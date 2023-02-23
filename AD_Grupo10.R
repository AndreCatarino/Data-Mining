
rm(list=ls(all=TRUE))



# LER DADOS
wd = ""
setwd(wd)

dados <- read.csv("CompanyFinancialIndicators.csv", header = TRUE)

# remo��o das vari�veis categ�ricas (X0, X43, X50)
dados <- dados[,-1]
dados <- dados[,-43]
dados <- dados[,-49]



View(dados)
summary(dados)


# dados corrigidos (necess�rio para realizar ACP)
corrigir = function(x) { x - mean(x) }
dados_c <- apply(X = dados, MARGIN = 2, FUN = corrigir)
dados_c


# matrizes de covariancia e correlacao
S=cov(dados_c)
round(S,2)

variances_list <- vector(mode = "list", length = 49)


# variancia total
TV=sum(diag(S))
TV


# Verificar se existem vari�veis respons�veis por uma fra��o substancial da vari�ncia total
for(i in 1:49) {
 
  variances_list[i] =  (S[i,i] / TV) * 100
  
}

plot(variances_list, label = "x")

boxplot( variances_list, main="Vari�ncia Relativa das Vari�veis Originais",xlab="Indice da coluna referente as Variaveis originais",ylab="Vari�ncia Relativa, em %")



# Estandardiza��o dos dados 
# (pr�tica necess�ria para n�o haver uma grande influ�ncia das vari�veis com alta vari�ncia nos componentes principais)
standardize=function(x){(x-mean(x))/sd(x)}
dados_std=apply(X=dados_c,MARGIN=2,FUN=standardize)

eigen(cov(dados_std))


#2� Passo: Obten��o das Componentes Principais
# -------------------------------------------------------------------------------------------------

#Componentes Principais
cps=princomp(dados_std)
cps



#Summary Statistics das componentes principais
summary(cps)


# Gr�fico das vari�ncias ("valores pr�prios") 
plot(cps, type="barplot", main = "Componentes Principais", pch=19)


# Pacotes necess�rios para a an�lise paralela de Horn
library(lattice)
library(nFactors)

# valores e vetores proprios da matriz de variancias dos dados estandardizados
EigVal <- eigen(cor(dados_std))
EigVal


# Construcao da matriz de correlacoes dos dados artificiais
# Criar uma matriz com as dimensoes desejadas e com  os desvios padrao desejados na diagonal (desvio padr�o = 1)
dv_cor <- diag(1, ncol(dados_std))

## Gerar dados artificiais e calcular os seus valores e vetores proprios
## usar o comando parallel do package nFactors
paralel_cor <- parallel(subject = nrow(dados_std), var = ncol(dados_std), rep = 100, model = "components", sd = dv_cor)
paralel_cor

# Grafico dos valores proprios dos dados reais vs artificiais
plotParallel(eig = EigVal$values, parallel = paralel_cor, model = "components", xlab= "Componentes", ylab="Valores Pr�prios", main = "An�lise Paralela de Horn")


# valores pr�prios da matriz de variancias dos dados estandardizados
EigVal$values



#4� Passo: Interpretar os Componentes Principais Retidos ( Scores e Loadings)   
# -------------------------------------------------------------------------------------------------

#C�lculo dos Scores
scores_cps <- cps$scores[,1:11]
scores_cps

# Defini��o dos Loadings
loadings_cps <- cor(dados_std, scores_cps)
round(loadings_cps,2)

#Ver Pesos
df <- data.frame(EigVal$vectors)
df <- df[,-c(12:49)]
View(df)

#Exportar Tabela Pesos para excel
library("writexl")
write_xlsx(df,"C:/Users/kakar/OneDrive/Ambiente de Trabalho/ISEG/Mestrado/1� Ano/2� Semestre/An�lise de Dados/Trabalho Grupo\\file name.xlsx")

#####################################################################################################  
#                                        AN�LISE DISCRIMINANTE                                      #                                                        
#####################################################################################################

rm(list=ls(all=TRUE))

setwd("C:/Users/kakar/OneDrive/Ambiente de Trabalho/ISEG/Mestrado/1� Ano/2� Semestre/An�lise de Dados/Trabalho Grupo")

dados <- read.csv("CompanyFinancialIndicators.csv", header = TRUE)
dados

# remo��o das vari�veis categ�ricas (X43, X50)

dados <- dados[,-44]
dados <- dados[,-50]

View(dados)

# 1�) Poder de Discrimina��o das Variaveis Discriminantes

# a) An�lise univariada, Teste-t para verificar se variavel � discriminante

# Se valor-p for >  0.05, para 5%, n�o rejeitamos H0, logo a Variavel n�o tem poder discriminante

P_Value <- data.frame

P_Value  = t.test(X1 ~ X0, data = dados, var.equal=TRUE)$p.value
P_Value[2]  = t.test(X2 ~ X0, data = dados, var.equal=TRUE)$p.value
P_Value[3]  = t.test(X3 ~ X0, data = dados, var.equal=TRUE)$p.value
P_Value[4]  = t.test(X4 ~ X0, data = dados, var.equal=TRUE)$p.value
P_Value[5]  = t.test(X5 ~ X0, data = dados, var.equal=TRUE)$p.value
P_Value[6]  = t.test(X6 ~ X0, data = dados, var.equal=TRUE)$p.value
P_Value[7]  = t.test(X7 ~ X0, data = dados, var.equal=TRUE)$p.value
P_Value[8]  = t.test(X8 ~ X0, data = dados, var.equal=TRUE)$p.value
P_Value[9]  = t.test(X9 ~ X0, data = dados, var.equal=TRUE)$p.value
P_Value[10]  = t.test(X10 ~ X0, data = dados, var.equal=TRUE)$p.value
P_Value[11]  = t.test(X11 ~ X0, data = dados, var.equal=TRUE)$p.value
P_Value[12]  = t.test(X12 ~ X0, data = dados, var.equal=TRUE)$p.value
P_Value[13]  = t.test(X13 ~ X0, data = dados, var.equal=TRUE)$p.value
P_Value[14]  = t.test(X14 ~ X0, data = dados, var.equal=TRUE)$p.value
P_Value[15]  = t.test(X15 ~ X0, data = dados, var.equal=TRUE)$p.value
P_Value[16]  = t.test(X16 ~ X0, data = dados, var.equal=TRUE)$p.value
P_Value[17]  = t.test(X17 ~ X0, data = dados, var.equal=TRUE)$p.value
P_Value[18]  = t.test(X18 ~ X0, data = dados, var.equal=TRUE)$p.value
P_Value[19]  = t.test(X19 ~ X0, data = dados, var.equal=TRUE)$p.value
P_Value[20]  = t.test(X20 ~ X0, data = dados, var.equal=TRUE)$p.value
P_Value[21]  = t.test(X21 ~ X0, data = dados, var.equal=TRUE)$p.value
P_Value[22]  = t.test(X22 ~ X0, data = dados, var.equal=TRUE)$p.value
P_Value[23]  = t.test(X23 ~ X0, data = dados, var.equal=TRUE)$p.value
P_Value[24]  = t.test(X24 ~ X0, data = dados, var.equal=TRUE)$p.value
P_Value[25]  = t.test(X25 ~ X0, data = dados, var.equal=TRUE)$p.value
P_Value[26]  = t.test(X26 ~ X0, data = dados, var.equal=TRUE)$p.value
P_Value[27]  = t.test(X27 ~ X0, data = dados, var.equal=TRUE)$p.value
P_Value[28]  = t.test(X28 ~ X0, data = dados, var.equal=TRUE)$p.value
P_Value[29]  = t.test(X29 ~ X0, data = dados, var.equal=TRUE)$p.value
P_Value[30]  = t.test(X30 ~ X0, data = dados, var.equal=TRUE)$p.value
P_Value[31]  = t.test(X31 ~ X0, data = dados, var.equal=TRUE)$p.value
P_Value[32]  = t.test(X32 ~ X0, data = dados, var.equal=TRUE)$p.value
P_Value[33]  = t.test(X33 ~ X0, data = dados, var.equal=TRUE)$p.value
P_Value[34]  = t.test(X34 ~ X0, data = dados, var.equal=TRUE)$p.value
P_Value[35]  = t.test(X35 ~ X0, data = dados, var.equal=TRUE)$p.value
P_Value[36]  = t.test(X36 ~ X0, data = dados, var.equal=TRUE)$p.value
P_Value[37]  = t.test(X37 ~ X0, data = dados, var.equal=TRUE)$p.value
P_Value[38]  = t.test(X38 ~ X0, data = dados, var.equal=TRUE)$p.value
P_Value[39]  = t.test(X39 ~ X0, data = dados, var.equal=TRUE)$p.value
P_Value[40]  = t.test(X40 ~ X0, data = dados, var.equal=TRUE)$p.value
P_Value[41]  = t.test(X41 ~ X0, data = dados, var.equal=TRUE)$p.value
P_Value[42]  = t.test(X42 ~ X0, data = dados, var.equal=TRUE)$p.value
P_Value[43]  = t.test(X44 ~ X0, data = dados, var.equal=TRUE)$p.value
P_Value[44]  = t.test(X45 ~ X0, data = dados, var.equal=TRUE)$p.value
P_Value[45]  = t.test(X46 ~ X0, data = dados, var.equal=TRUE)$p.value
P_Value[46]  = t.test(X47 ~ X0, data = dados, var.equal=TRUE)$p.value
P_Value[47]  = t.test(X48 ~ X0, data = dados, var.equal=TRUE)$p.value
P_Value[48]  = t.test(X49 ~ X0, data = dados, var.equal=TRUE)$p.value
P_Value[49]  = t.test(X51 ~ X0, data = dados, var.equal=TRUE)$p.value


 plot(P_Value,main="Valor-P associado ao Teste-t da An�lise Univariada",xlab="Indice da Variavel na DataFrame P_value", ylab="Valor-P")
 abline(h=0.05, col="green")
 
 #As variaveis x8, x9,x11,x16, x19, x24, x25, x28, x34, x40, x45, x48, x49 n�o t�em poder discriminante
 
 # b) An�lise Multivariada (para as vari�veis em conjunto, para todas as variaveis originais?): 
 
 #Penso que este teste ser� apenas com as discriminantes, porque com todas as variaveis originais, n�o rejeitamos H0
 
 library(rrcov)
 
 #Teste para todas as variaveis originais
 Wilks.test(X0 ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11 + X12 + X13 + X14 + X15 + X16 + 
              X17 + X18 + X19 + X20 + X21 + X22 + X23 + X24 + X25 + X26 + X27 + X28 + X29 + X30 + X31 +
              X32 + X33 + X34 + X35 + X36 + X37 + X38 + X39 + X40 + X41 + X42 + X44 + X45 + X46 + X47 +
              X48 + X49 + X51 , data=dados)
 
 # Resultado: 
 # Wilks' Lambda = 1.8738
 # Chi2-Value = -611.31
 # DF = 49.00
 # p-value = 1, Como p-value > 0,05, n�o rejeitamos Ho e assim as variaveis em conjunto n�o dividem bem as empresas
 # em 2 grupos
 
 #Teste apenas para variaveis discriminantes
 Wilks.test(X0 ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X10 + X12 + X13 + X14 + X15 + X17 + X18 + X20 + X21 + X22 + 
                X23 + X26 + X27 + X29 + X30 + X31 + X32 + X33 + X35 + X36 + X37 + X38 + X39 + X41 + X42 + X44 + 
                X46 + X47 + X51 , data=dados)
 
 # Resultado: 
 # Wilks' Lambda = 0.046892
 # Chi2-Value = 2998.7
 # DF = 36.0
 # p-value < 2.2e-16, Como p-value < 0,05, rejeitamos Ho e assim as variaveis em conjunto dividem bem as empresas
 # em 2 grupos
 
 # 2�) Estima��o da Fun��o Discriminante
 
 #Iremos utilizar apenas variaveis discriminantes, uma vez que a analise multivariada nos diz que se usarmos todas
 # as variaveis, n�o iremos dividir bem as empresas em 2 grupos
 
 library(MASS)
 func_discrim <- lda(X0 ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X10 + X12 + X13 + X14 + X15 + X17 + X18 + X20 + X21 + X22 + 
                       X23 + X26 + X27 + X29 + X30 + X31 + X32 + X33 + X35 + X36 + X37 + X38 + X39 + X41 + X42 + X44 + 
                       X46 + X47 + X51 , data=dados, CV = FALSE)
 
 func_discrim

 # 3�) Obter Classifica��es 
 
 # Previs�es (Scores) dentro da amostra
 
 func_discrim_pred <- predict(func_discrim, newdata = dados[,c(2:50)])
 
 #Calculo do valor de Corte
 df_new <- cbind(func_discrim_pred$x, dados$X0 )
 mean_group1 = mean(subset(df_new[, 1], df_new[, 2] == 0 ))
 mean_group2 = mean(subset(df_new[, 1], df_new[, 2] == 1 ))
 
 cutoff = (mean_group1 + mean_group2) / 2
 cutoff
 
 plot(func_discrim_pred$x, ylab = "score", col = dados[,1]+1, pch=19)
 abline(cutoff, 0, col = "blue", lty = "dashed")

 
 #Matriz de classifica��o in-sample
 matriz_classif_in <- table(dados[,1], func_discrim_pred$class) 
 matriz_classif_in
 
 #Matriz de classifica��o in-sample em percentagem
 round(prop.table(matriz_classif_in),4)
 
 #Gr�fico de classifica��o in-sample
 plot(as.numeric(func_discrim_pred$class)-1, ylab = "Classifica��o", col = dados[,1]+1, pch=19)
 
 #Exatid�o da classifica��o in-sample
 sum(diag(matriz_classif_in)/sum(matriz_classif_in))
 
 
 #Obter fun��o discriminante com valida��o cruzada
 func_discrim2 <- lda(X0 ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X10 + X12 + X13 + X14 + X15 + X17 + X18 + X20 + X21 + X22 + 
                            X23 + X26 + X27 + X29 + X30 + X31 + X32 + X33 + X35 + X36 + X37 + X38 + X39 + X41 + X42 + X44 + 
                            X46 + X47 + X51 , data=dados, CV=TRUE)
 
 
 #Matriz de classifica��o out-of-sample
 matriz_classif_out <- table(dados[,1], func_discrim2$class)
 matriz_classif_out
 
 #Matriz de classifica��o out-of-sample em percentagem
 round(prop.table(matriz_classif_out),4)
 
 #Gr�fico de classifica��o out-of-sample
 plot(as.numeric(func_discrim2$class)-1, ylab = "Classifica��o", col = dados[,1]+3, pch=19)
 
 #Exatid�o da classifica��o out-of-Sample
 sum(diag(matriz_classif_out)/sum(matriz_classif_out))
 
 
 #####################################################################################################  
 #                                        Regress�o Log�stica                                        #                                                        #
 #####################################################################################################
 
 #Uma vez que a hipotese de normalidade dos dados � violada, devido a existirem variaveis categorias,
 # tudo indica que a Regress�o Logistica dever� ser superior � An�lise Discriminante feita anteriormente
 
 rm(list=ls(all=TRUE))
 
 setwd("C:/Users/kakar/OneDrive/Ambiente de Trabalho/ISEG/Mestrado/1� Ano/2� Semestre/An�lise de Dados/Trabalho Grupo")
 
 dados <- read.csv("CompanyFinancialIndicators.csv", header=TRUE)
 
 # 1�) ESTIMAR MODELO

 my_logit <- glm(X0 ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11 + X12 + X13 + X14 + X15 + X16 + 
                      X17 + X18 + X19 + X20 + X21 + X22 + X23 + X24 + X25 + X26 + X27 + X28 + X29 + X30 + X31 +
                      X32 + X33 + X34 + X35 + X36 + X37 + X38 + X39 + X40 + X41 + X42 + X43 + X44 + X45 + X46 +
                      X47 + X48 + X49 + X50 + X51 , data = dados, family = binomial(link = logit))
 summary(my_logit)
 
 #Falta fazer a significancia individual dos coeficientes
 
 
 # SIGNIFICANCIA GLOBAL DA REGRESSAO: Iremos utilizar o LIKELIHOOD RATIO TEST
 
 my_logit_null <- glm(X0 ~ 1, data = dados, family = binomial(link = logit))
 
 LR <- -2*logLik(my_logit_null) + 2*logLik(my_logit)
 -2*logLik(my_logit_null)
 -2*logLik(my_logit)
 LR
 pchisq(LR, 2, lower.tail = FALSE) #D�-nos o Valor-P do teste
 
 #Utilizando o McFadden
 
 McFadden <- 1 - as.vector(logLik(my_logit) / logLik(my_logit_null))
 
 logLik(my_logit)
 logLik(my_logit_null)
 McFadden
 
 # -------------------------------------------------------------------------------------------------
 #    INTERVALOS DE CONFIANCA PARA OS PARAMETROS
 # -------------------------------------------------------------------------------------------------
 
 confint(my_logit)
 
 # -------------------------------------------------------------------------------------------------
 #    OBTER COEFICIENTES PARA A CHANCE DE UMA EMPRESA SER BEM SUCEDIDA
 # -------------------------------------------------------------------------------------------------
 
 exp(my_logit$coefficients)
 exp(confint(my_logit))
 
 # -------------------------------------------------------------------------------------------------
 #    PROBABILIDADES
 #    Nota: alternativamente pode usar-se my_logit$fitted
 # -------------------------------------------------------------------------------------------------
 
 my_prob <- predict(my_logit, dados, type = "response")
 plot(my_prob, col = dados$X0 + 1, pch = 19)
 abline(0.5, 0, col = "black", lty = "dashed")
 
 # -------------------------------------------------------------------------------------------------
 #    CLASSIFICACAO
 # -------------------------------------------------------------------------------------------------
 
 my_class <- as.integer(my_prob > 0.5)
 my_class
 table(dados$X0, my_class)
 
 #####################################################################################################  
 #                                        An�lise de Clusters                                        #                                                        #
 #####################################################################################################
 
 rm(list=ls(all=TRUE))
 
 setwd("C:/Users/kakar/OneDrive/Ambiente de Trabalho/ISEG/Mestrado/1� Ano/2� Semestre/An�lise de Dados/Trabalho Grupo")
 
 dados <- read.csv("CompanyFinancialIndicators.csv", header=TRUE, sep=",")
 dados
 
 ################################################################################
 #Calculo do melhor m�todo de Agrupamento
 #Quanto mais proximo for de 1, melhor o m�todo de agrupamento
 
 library(factoextra)
 library(cluster)
 
 #define linkage methods
 m <- c( "average", "single", "complete", "ward")
 names(m) <- c( "average", "single", "complete", "ward")
 
 #function to compute agglomerative coefficient
 ac <- function(x) {
   agnes(dados, method = x)$ac
 }
 
 #calculate agglomerative coefficient for each clustering linkage method
 sapply(m, ac)
 ###################################################################
 
 d=dist(as.matrix(dados)) #Calculo da Distancia Euclidiana
 d
 
 hc1=hclust(d,method="ward.D2") #Metodo de Agrupamento Escolhido
 
 
 plot(hc1, xlab="Observa��es",ylab="Dist�ncia Euclidiana", main="Dendrograma") #Representa��o Gr�fica, Dendograma
 hc1
 
 ###################################################################
 #Determinar o n� Optimo de Clusters segundo m�trica Gap Statistic
 
 #calculate gap statistic for each number of clusters (up to 10 clusters)
 gap_stat <- clusGap(dados, FUN = hcut, nstart = 25, K.max = 10, B = 50)
 
 #produce plot of clusters vs. gap statistic
 fviz_gap_stat(gap_stat)
 
 ##################################################################
#Outro metodo de determinar n� optimo de clusters, at� 20
 
 #install.packages("fviz_nbclust")
 
 p1 <- fviz_nbclust(dados, FUN = hcut, method ="wss", k.max=20) + ggtitle("(A) Elbow Method")
 p2 <- fviz_nbclust(dados, FUN = hcut, method ="silhouette", k.max=20) + ggtitle("(B) Silhouette Method")
 p3 <- fviz_nbclust(dados, FUN = hcut, method ="gap_stat", k.max=20) + ggtitle("(C) Gap Statistic")
 
 #Display plots
 gridExtra::grid.arrange(p1,p2,p3, nrow=1)
 
 #################################################################
 
 groups <- cutree(hc1, k=7) # cut tree into k clusters
 
 # draw dendogram with red borders around the k clusters
 rect.hclust(hc1, k=7, border="blue")
 
 #N� de Observa��es por Grupo
 table(groups)
 groups
 
 dados_groups=cbind(dados,groups)
 View(dados_groups)
 
 #Peso de cada Variavel em cada Grupo
 aggregate(dados,list(groups),mean)

 
 