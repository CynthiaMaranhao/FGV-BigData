#-------------------------------------------------------------------------------------#
# --------------------------- Importando um shapefile  -------------------------------#
#-------------------------------------------------------------------------------------#

#Carregando o pacote sf e tidyverse
library(sf)
library(tidyverse)

setwd("D:/Users/cynth/Desktop/Estatistica Espacial/Trabalho")

#Importando o shapefile de Houston
HOUShp <- read_sf("Houston_City_Limit.shp")

#Plotando o mapa de Houston e visualizando os eixos
ggplot(HOUShp) +
  geom_sf(fill = "White") +
  theme_light()

#Checando o CRS
st_crs(HOUShp)
#EPSG 4326 / WGS 84

## Carregando o pacote rmapshaper
library(rmapshaper)

## Simplificando o shape de HOU
HOUShp = ms_simplify(input = HOUShp,
                     keep = 0.02,
                     keep_shapes = TRUE)


#-------------------------------------------------------------------------------------#
# ------------------- Mapas com a localização exata de um evento  --------------------#
# ------------------- Mapas com a localização dos delitos em HOU  ---------------------#
#-------------------------------------------------------------------------------------#

#Importando o arquivo com as localizações dos delitos em HOU
delitos = read_csv2("Base Houston Final.csv")

# Visualizando a base NYPD
delitos

delitos_1t <- delitos %>% 
  filter (date >= "01/01/2018" & date<= "30/03/2018")

delitos_2t <- delitos %>% 
  filter (date >= "01/04/2018" & date<= "30/06/2018")

# Mapa 1 
#Plotando as localizacoes dos delitos identificando os tipos de delito 1o. Trimestre
ggplot(data = HOUShp) +
  geom_sf(fill = "White") +
  geom_point(data = delitos_1t,
             aes(x = lon,
                 y = lat),
             colour = 'Dark Red',
             size = 0.9) +
  facet_wrap(~offense)+
  ggtitle("Delitos 1o. Trimestre")+
  theme_light()

#Plotando as localizacoes dos delitos identificando os tipos de delito 2o. Trimestre
ggplot(data = HOUShp) +
  geom_sf(fill = "White") +
  geom_point(data = delitos_2t,
             aes(x = lon,
                 y = lat),
             colour = 'Dark Red',
             size = 0.9) +
  facet_wrap(~offense)+
  ggtitle("Delitos 2o. Trimestre")+
  theme_light()
## ggplot - funcao grafica
## geom_sf - geom do ggplot usado para mapas
## geom_point - geom do ggplot usado para plotar pontos
## theme_light - e um tema com algumas configuracoes ja definidas sobre o layout do grafico
#Argumentos:
#data - um objeto da classe sf ou tbl
#size - tamanho do ponto
#x - o que vai ser plotado no eixo x
#y - o que vai ser plotado no eixo y
#colour - cor dos pontos


# Mapa 2
# Plotando as localizacoes pelo tipo do delitos em mapas separados 1o. Trimestre
ggplot(data = HOUShp) +
  geom_sf(fill = "White") +
  geom_point(data = delitos_1t,
             aes(x = lon,
                 y = lat),
             colour = 'Dark Blue',
             size = 0.5) +
  theme_light() +
  ggtitle("Delitos 1o. Trimestre")+
  facet_wrap(~offense,
             labeller = labeller(offense = c("murder"="ASSASSINATO",
                                             "rape" = "ESTUPRO",
                                             "robbery" = "ROUBO",
                                             "aggravated assault" = "LESÃO CORPORAL QUALIFICADA",
                                             "auto theft" = "ROUBO DE CARRO",
                                             "burglary" = "INVASÃO A DOMICÍLIO")))

# Plotando as localizacoes pelo tipo do delitos em mapas separados 2o. Trimestre
ggplot(data = HOUShp) +
  geom_sf(fill = "White") +
  geom_point(data = delitos_2t,
             aes(x = lon,
                 y = lat),
             colour = 'Dark Blue',
             size = 0.5) +
  theme_light() +
  ggtitle("Delitos 2o. Trimestre")+
  facet_wrap(~offense,
             labeller = labeller(offense = c("murder"="ASSASSINATO",
                                             "rape" = "ESTUPRO",
                                             "robbery" = "ROUBO",
                                             "aggravated assault" = "LESÃO CORPORAL QUALIFICADA",
                                             "auto theft" = "ROUBO DE CARRO",
                                             "burglary" = "INVASÃO A DOMICÍLIO")))
## facet_wrap - particiona o grafico de acordo com a variavel especificada
#Argumentos:
#labeller - rotulos para cada categoria da variavel usada para particionar


#Carregando o ggmap
library(ggmap)

# Refazendo o Mapa 1 com o ggmap
qmplot(x = lon, 
       y = lat, 
       data = delitos_1t,
       colour = I('red'), 
       size = I(0.9), 
       darken = 0.3) +
  ggtitle("Delitos 1o. Trimestre")+
  facet_wrap(~offense,
             labeller = labeller(offense = c("murder"="ASSASSINATO",
                                             "rape" = "ESTUPRO",
                                             "robbery" = "ROUBO",
                                             "aggravated assault" = "LESÃO CORPORAL QUALIFICADA",
                                             "auto theft" = "ROUBO DE CARRO",
                                             "burglary" = "INVASÃO A DOMICÍLIO")))
qmplot(x = lon, 
       y = lat, 
       data = delitos_2t,
       colour = I('red'), 
       size = I(0.9), 
       darken = 0.3) +
  ggtitle("Delitos 2o. Trimestre")+
  facet_wrap(~offense,
             labeller = labeller(offense = c("murder"="ASSASSINATO",
                                             "rape" = "ESTUPRO",
                                             "robbery" = "ROUBO",
                                             "aggravated assault" = "LESÃO CORPORAL QUALIFICADA",
                                             "auto theft" = "ROUBO DE CARRO",
                                             "burglary" = "INVASÃO A DOMICÍLIO")))
##qmplot - plot de mapas rapidos (quick map plot)
#Argumentos:
#x - longitude
#y - latitude
#data - base de dados
#colour - cor
#size - tamanho do simbolo
#darken - quanto maior, mais escuro sera a cor\


#-------------------------------------------------------------------------------------#
# -------------- Estimando a intensidade de um padrao de pontos  ---------------------#
#-------------------------------------------------------------------------------------#

#Carregando o pacote spatstat
library(spatstat)

#Checando o CRS de HOUShp
st_crs(HOUShp)
#EPSG 4326 / WGS 84


#E preciso transformar as coordenadas para coordenadas projetadas (UTM, por exemplo)
#No Google earth e possivel checar qual a zona UTM a regiao pertence
#e usar a lista EPSG para encontrar o respectivo codigo
#e preciso pesquisar da seguinte forma UTM zone 18N
#Nesse caso e o EPSG e 32618
HOUShp_32618 = st_transform(HOUShp, 
                    crs = 32618)

#Criando um objeto owin (observation window)
HOUO <- as.owin(HOUShp_32618)

#Plotando o objeto owin
plot(HOUO, axes = TRUE)


#Criando o Padrao de Pontos no Plano (Planar Point Pattern)
HOppp = ppp(x = delitos$lon, 
            y = delitos$lat, 
            window = HOUO)

##ppp - criando um objeto da class ppp(Planar Point Pattern))
#Argumentos:
#x - longitude
#y - latitude
#window - janela de observacao (objeto do tipo owin)

#Warning message:
# 2248 points were rejected as lying outside the specified window 


#Como o shape esta com CRS EPSG:32618
#E os dados estao com CRS EPSG:4326 (WGS84)
#Sera preciso transformar os dados para o mesmo CRS do shapefile 
delitos_4326 = st_as_sf(x = delitos,
                   coords = c("lon","lat"),
                   crs = 4326)
##st_as_sf - converte um objeto qualquer em um sf (com um CRS
#Argumento:
#x - objeto a ser convertido
#coords - vetor com as coordenadas
#crs - sistema de referencia de coordenadas

#Transformando o objeto NYPD_4326 (que possui CRS WGS84)
#para um objeto com CRS EPSG:32618
delitos_32618 = st_transform(x = delitos_4326,
                       crs = 32618)

#Comparando os 3 objetos
delitos
delitos_4326
delitos_32618

#Extraindo as coordenadas do objeto NYPD_32618
coordenadas = st_coordinates(x = delitos_32618)

##st_coordinates - retira as coordenadas em forma de matriz
#Argumento:
#x - objeto da classe sf

#Visualiando o objeto coordenadas
head(coordenadas)

#Criando o padrao de pontos a ser plotado
HOUppp = ppp(x = coordenadas[,1], 
            y = coordenadas[,2], 
            window = HOUO)
#Warning messages:
#1: 46 points were rejected as lying outside the specified window 
#2: data contain duplicated points 
 
#-------------------------------------------------------------------------------------#
#Funcao que estima o raio por meio de validacao cruzada (custosa computacionalmente)
#-------------------------------------------------------------------------------------#

raio.est = bw.diggle(HOUppp)
raio.est

#Estimando o efeito de primeira ordem (intensidade) usando diferentes kernels
HOUkde.g = density.ppp(x = HOUppp, 
                      sigma = raio.est, 
                      kernel ="gaussian")

##density.ppp - calcula a funcao de intensidade de acordo com o kernel escolhido
#Argumentos:
#x - objeto da classe ppp
#sigma - Ã© o valor do raio (tau na expressao dos slides)
#sigma pode ser um valor numÃ©rico Ãºnico (especificando a largura de banda do kernel na mesma unidade que o padrao de pontos)
#kernel - o kernel que deseja-se usar

#VEJAM QUE A UNIDADE DE MEDIDA DO CRS E METRO

#Plotando os dados e as funcoes intensidades estimadas pelas diversas funcoes kernel
par(mfrow=c(1,2))
par(mar=c(1.1,1.1,1.1,1.3))
plot(HOUppp, 
     pch=21, 
     cex=0.5, 
     bg="blue", main="Ocorrencias", 
     cex.main=0.5)
plot(HOUkde.g, 
     main="Kernel Normal", 
     cex.main=0.5)
par(mfrow=c(1,1))

#Avaliando o impacto de diferentes raios (tau)

HOUkde.tau1 = density.ppp(x = HOUppp, 
                         sigma=555, 
                         kernel="gaussian")
HOUkde.tau2 = density.ppp(x = HOUppp, 
                         sigma=1100, 
                         kernel="gaussian")
HOUkde.tau3 = density.ppp(x = HOUppp, 
                         sigma=2200, 
                         kernel="gaussian")
HOUkde.tau4 = density.ppp(x = HOUppp, 
                         sigma=5550, 
                         kernel="gaussian")
HOUkde.tau5 = density.ppp(x = HOUppp, 
                         sigma=55500, 
                         kernel="gaussian")

#Plotando os dados e as funcoes intensidades estimadas pelos diversos valores de sigma
par(mfrow=c(3,2))
plot(HOUppp, 
     pch=21, 
     cex=0.9, 
     bg="blue", 
     main="Ocorrencias", 
     cex.main=0.5)
plot(HOUkde.tau1, 
     main="Sigma = 555m", 
     cex.main=0.5)
plot(HOUkde.tau2, 
     main="Sigma = 1100m", 
     cex.main=0.5)
plot(HOUkde.tau3, 
     main="Sigma = 2200m", 
     cex.main=0.5)
plot(HOUkde.tau4, 
     main="Sigma = 5550m", 
     cex.main=0.5)
plot(HOUkde.tau5, 
     main="Sigma = 55500m", 
     cex.main=0.5)
par(mfrow=c(1,1))


#-------------------------------------------------------------------------------------#
# Outra forma de estimar a densidade
#-------------------------------------------------------------------------------------#

# Com o ggplot
ggplot(data = HOUShp) +
  geom_sf(fill = "White") +
  geom_point(data = delitos,
             aes(x = lon, y = lat),
             colour = 'Dark Blue',
             size = 0.5) +
  theme_light() +
  stat_density2d(data = delitos,
                 aes(x = lon, y = lat, fill = ..level..),
                 alpha = 0.8,
                 h = 0.02,
                 n = 400,
                 geom = "polygon")

##stat_density2d - estima um kernel gaussiano
#Argumento:
#h - raio
#n - numero de pontos na grade
#alpha - opacidade

# se for grau, uma forma de ajudar a escolher o valor
#e usar a relacao que 1 grau de latitude - 111 Km
#0,02*111*1000 = 2200m

# Com o ggmap
density_ggmap = qmplot(x = lon, 
       y = lat, 
       data = delitos,
       colour = I('red'), 
       size = I(0.9), 
       darken = 0.3) +
  stat_density2d(data = delitos,
                 aes(x = lon, y = lat, fill = ..level..),
                 alpha = 0.8,
                 h = 0.02,
                 n = 400,
                 geom = "polygon") +
  scale_fill_gradient(low = "black", 
                      high= "red")

##scale_fill_gradient - cria uma escala de cores gradientes entre duas cores
#low - cor associada aos menores valores
#high - cor associada aos maiores valores

#Visualizando o objeto
density_ggmap

# Estimando a intensidade para cada tipo de delito, usando o grafico realizado para todos os delitos
density_ggmap + 
  scale_fill_gradient(low = "Light Yellow", 
                      high= "Dark Red") +
  facet_wrap(~offense)
  

#------------------------ Fim da Atividade -----------------

#Estimando a funcao G
HOU.G = Gest(HOUppp)

#Gest - estima a funcao de distribuicao G de um padrao de pontos
#Argumento
#X - um objeto da classe ppp

#Estimando a funcao L
HOU.L = Lest(HOUppp)

#Lest - estima a funcao K de Ripley centrada de um padrao de pontos
#Argumento
#X - um objeto da classe ppp

#Estimando a funcao F
HOU.F = Fest(HOUppp)

#Fest - estima a funcao F de um padrao de pontos
#Argumento
#X - um objeto da classe ppp

#Plotando a funcao G
plot(HOU.G, main="Funcao G")

#Plotando as funcoes G, K e F
par(mfrow = c(2,2))
par(mar=c(4,2.5,1.5,.5))
plot(HOU.G, 
     main="Funcao G", 
     xlab = "metro")
plot(HOU.L, 
     main="Funcao L", 
     xlab = "metro")
plot(HOU.F, 
     main="Funcao F", 
     xlab = "metro")
par(mfrow = c(1,1))

#-------------------------------------------------------------------------------------#
# ----------------------- Checando se e razoavel assumir CSR   -----------------------#
#-------------------------------------------------------------------------------------#

#Realizando o teste de Clark-Evans para verificar nao aleatoriedade espacial completa
clarkevans.test(X = HOUppp)

#Realizando o teste de Clark-Evans para verificar agregacao espacial
clarkevans.test(X = HOUppp, 
                alternative = "less")

#clarkevans.test - realiza o teste de Clark-Evans
#Argumento
#X - um objeto da classe ppp
#alternative - hipotese alternativa do teste (default - padrao nao aleatorio)

#Realizando o teste de Clark-Evans para verificar regularidade espacial
clarkevans.test(X = HOUppp, 
                alternative = "greater")


#Funcoes para estimar os envelopes das funcoes F, G e L
Env_Lest = envelope(Y = HOUppp, 
                    fun = Lest, 
                    nsim=10) #alto custo computacional
Env_Gest = envelope(Y = HOUppp, 
                    fun = Gest, 
                    nsim=10)
Env_Fest = envelope(Y = HOUppp, 
                    fun = Fest, 
                    nsim=10)

#envelope - Clacula as bandas de confianca das funcoes
#Argumento
#Y - um objeto da classe ppp
#fun - funcao que deseja computar as bandas de confianca
#nsim - numero de padroes de pontos simulados

#Plotando as funcoes e seus respectivos envelopes
par(mfrow=c(2,2))
plot(HOUppp, 
     pch=21, 
     cex=0.9, 
     bg="blue")
  plot(Env_Lest, 
       main = "Envelope L")
  plot(Env_Gest, 
       main = "Envelope G")
  plot(Env_Fest, 
       main = "Envelope F")
par(mfrow=c(1,1))

