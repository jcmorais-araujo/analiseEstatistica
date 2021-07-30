getwd()
setwd("C:/Users/Morais/Desktop/Lorena/Analise_de_Dados")

#Carregar pacotes
library(readxl)
library(psych)
library(dplyr)
library(FactoMineR)
library(factoextra)

#Ler tabela
read_excel("quimica_solo.xlsx", sheet = 2)
dados <- read_excel("quimica_solo.xlsx", sheet = 2)

View(dados)

###Início das análises

describe(dados, na.rm = T)
describe.by(dados, group = "Matriz")

#Análise de PCA

#Converter dados alfabéticos em núméricos
#dados$Cu <- as.numeric(dados$Cu)
#dados$Sr <- as.numeric(dados$Sr)

#PCA

new_dados <- data.frame(dados[,c(-2,-3)], row.names = "Amostra")

library(imputeTS)

new_dados <- na.interpolation(new_dados, option = "linear")

View(new_dados)

PCA(new_dados, scale.unit = T, graph = T )
res.pca <- PCA(new_dados, scale.unit = T, graph = T )

#sumarizar as informações

summary(res.pca, nbelements = Inf) ####nelements= Inf, serve para mostrar todos os elementos analisados

#Visualização dos Eigenvalues / Variances
#Visualizar Eigenvalues

eig.val <- get_eigenvalue(res.pca) 
eig.val

#Scree plot 
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))

######Variavéis Gráficas#########
var <- get_pca_var(res.pca) 
var

###FAzer o gráfico de "Plot da correlação das variáveis 
fviz_pca_var(res.pca, col.var = "black", repel = T)


#Descrição das dimensões

res.desc <- dimdesc(res.pca, axes = c(1,2), proba = 0.05)

# Description of dimension 1
res.desc$Dim.1

# Description of dimension 2
res.desc$Dim.2
####################################

#####Gráficos dos individuos###########

ind <- get_pca_ind(res.pca)
ind

##Plotar: Contribuição e qualitativa

fviz_pca_ind(res.pca) #Plotar o PCA apenas com meus pontos (QUalitativo)

###############################################################################

####Trabalhando na PCA das Variáveis
#Plotar elementos: pontos, texto e arrow

fviz_pca_var(res.pca, geom.var = c("point", "text"),
             col.circle = "transparent", repel = T,
             pointshape = 21, pointsize = 2,
             fill.var = "orange"
) +
  theme_bw() + theme(element_blank()) +
  labs (
    x = "PC1 (43%)",
    y = "PC2 (17%)",
    title = NULL
  )

#Salvar

ggsave("PCA_Solo_e_Rocha.eps", width = 15, height = 15,
       units = "cm", dpi = 600)

##################################################################
#Fazer PCA dos individuos

fviz_pca_ind(res.pca, repel = T,
             pointshape = 21, pointsize= 3,
             fill.ind = dados$Matriz,
             geom = c("point", "text")
) +
  theme_bw() + theme(element_blank(), 
                     legend.position = "none"
  ) +
  scale_x_continuous(limits = c(-8,8))+
  scale_y_continuous(limits = c(-10,10))+
  labs(
    x = "PC1 (43%)",
    y = "PC2 (17%)",
    title = NULL
  ) 

#Salvar PCA dos Individuos
ggsave("PCA_ind_rocha_e_solo.eps", 
       width = 15, height = 15,
       units = "cm", dpi = 600)

######################################################################

#Interpretação
library(corrplot)
corrplot(var$coord)

#individuos
corrplot(ind$cos2)



#Correlação
library(imputeTS)

dados_inter <- na.interpolation(new_dados)

z<-cor(dados_inter)
View(z)

as.matrix(z)

write.csv(z, file = "correlacao_solo", sep = ";", row.names = T)
