                                                                 #
# Estudo de caso: Bacias                                             #
#####################################################################

# Instala??o das bibliotecas
install.packages('gdata')
install.packages('caret')
install.packages('corrplot')
install.packages('psych')
install.packages("clValid")

# Carregando as bibliotecas
library(gdata)
library(caret)
library(corrplot)
library(psych)
library(tidyverse)
library(factoextra)
library(clValid)
library(rattle)
library(rpart)
library(e1071)
library(readxl)

# Download do dataset
data <- read_excel("D:/Users/cynth/Desktop/FGV/Métodos Matriciais e Análises de Clusters/Trabalho/DadosRioDoce.xlsx", sheet = "2017")
summary(data)

# Separando o Rio da Bacia
data_names <- data %>% select(Rio)
data_cleaned <- data %>% select(-Cod, -Estacao)
data_cleaned %>% Filter(f = is.factor) %>% names

# Analisando somente os dados categóricos
data_categ <- data_names %>% select(Rio)
distinct(data_categ %>% select(Rio))

# Como a coluna Rio é nominal, 
# vamos utilizar o codificador One Hot (ou Dummy Variables)
#dmy <- dummyVars(" ~ .", data = data_cleaned)
#data_onehot <- data.frame(predict(dmy, newdata = data_cleaned))

data_cleaned <- data_cleaned %>% select(-Rio)


# Salvando os dados
#save(data_onehot, file = "data_onehot.Rdata")
save(data_cleaned, file = "data_cleaned.Rdata")
save(data_names, file = "data_names.Rdata")


# Medidas de Similaridade e Distância
dist(data_cleaned, method="euclidean")
simil(data_cleaned, method="euclidean")

# Objeto da biblioteca proxy
pr_DB$get_entry("fJaccard")

# Para ver outras métricas
summary(pr_DB)

# Visualização e descrições estatásticas
corrplot(cor(data_cleaned), order = "hclust")
pairs.panels(data_cleaned, 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

# Visualizando somente algumas variáveis
pairs.panels(data_cleaned[,c( 2,4,5, 9, 11, 13)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

data_cleaned %>%
  gather(Attributes, value, 1:13) %>%
  ggplot(aes(x=value)) +
  #geom_histogram(binwidth=1, alpha=.5, position="identity") +
  geom_density(alpha=.5) +
  facet_wrap(~Attributes, scales="free") +
  labs(x="Values", y="Frequency") 

data_cleaned %>%
  gather(Attributes, value, 1:13) %>%
  ggplot(aes(x=value)) +
  #geom_histogram(binwidth=1, alpha=.5, position="identity") +
  geom_boxplot(alpha=.5) +
  facet_wrap(~Attributes, scales="free") +
  theme_bw() +
  labs(x="Valores (mg/L)", y="Frequencia") 


# Reduzindo dimensionalidade
pca <- prcomp(data_cleaned, scale=TRUE, center=TRUE)
pca
pca_df <- data.frame(x=pca$x[,"PC1"], y=pca$x[,"PC2"])

# Plotando as direções de maior variância
ggplot(data = pca_df, aes(x,y)) + 
  geom_point() + xlab("PC1") + ylab("PC2")



# Adicionando um pouco mais de informação (cores!)
pca <- prcomp(data_cleaned, scale=TRUE, center=TRUE)
pca_df <- data.frame(x=pca$x[,"PC1"], y=pca$x[,"PC13"], color=data_names)

# Plotando as direções de maior variância
ggplot(data = pca_df, aes(x,y, color=Rio) )+ 
  geom_point() + xlab("PC1") + ylab("PC2")

# Resultado do PCA
summary(pca)

# Plotando a variância acumulada
fviz_eig(pca,addlabels=TRUE)

# Contributions of variables to PC1
fviz_contrib(pca, choice = "var", axes = 1, top = 13)

# Contributions of variables to PC1 and PC2
fviz_contrib(pca, choice = "var", axes = 1:6, top = 25)

df_pca_6 <- pca$x[,1:6]

# Escolhendo o n?mero de clusters
fviz_nbclust(pca$x[,1:6], kmeans, method = "wss", k.max = 13)

# 1) Agrupando com K-Means
kClusters <- 4
km.res <- kmeans(pca$x[,1:6], 
                 centers = kClusters, 
                 iter.max = 100, 
                 nstart = 100)

ggplot() +
  geom_point(aes(x=df_pca_6[, 1], y=df_pca_6[, 2], color=factor(km.res$cluster))) + 
  #geom_text(aes(x=pca_df[, 1], y=pca_df[, 2], label=pca_df[,3])) +
  geom_point(aes(x=km.res$centers[, 1], y=km.res$centers[, 2]), color="black", size=5, shape=4, stroke=2) +
  scale_color_discrete(name = "Clusters")

fviz_cluster(km.res, 
             data = pca$x[,1:2],
             #palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07", "#CFE470"), 
             ellipse.type = "euclid", 
             star.plot = TRUE, 
             repel = FALSE, 
             ggtheme = theme_minimal()
)

# 2) Agrupando Hierarquico
res.dist <- dist(pca$x[,1:6], method = "euclidean")
res.hc <- hclust(d = res.dist, method = "ward.D2")

fviz_dend(res.hc, 
          k = 4,
          #h = 30,
          cex = 0.5, 
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"), 
          color_labels_by_k = TRUE,
          rect = TRUE)

fviz_nbclust(pca$x[,1:6], hcut, method = "wss", k.max = 13)

# 3) Analisando a silhueta
km.res <- eclust(pca$x[,1:6], "kmeans", k = kClusters, graph = FALSE, stand=FALSE, iter.max = 100, 
                 nstart = 100) # Visualize k-means clusters
fviz_silhouette(km.res, palette = "jco", ggtheme = theme_classic())

# Escolhendo o n?mero de clusters pela silhueta
fviz_nbclust(pca$x[,1:6], kmeans, method = "silhouette", k.max = 13)

# Comparação de diversos algoritmos
# Veja mais algoritmos em: https://cran.r-project.org/web/packages/clValid/clValid.pdf
clmethods <- c("hierarchical","kmeans","pam")
# PAM ? como o K-Means, mas utiliza med?ides e pode ser utilizado com outras m?tricas de dist?ncia.
intern <- clValid(pca$x[,1:6], 
                  nClust = 2:6,
                  clMethods = clmethods,
                  validation = "internal",
                  neighbSize = 10)
summary(intern)
]