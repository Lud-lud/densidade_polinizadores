setwd("C:/Users/Pc/Desktop/Ludmila/Teste_densidade")


#Esse script foi produzido para calcular a densidade média de polinizadores por
#pixel do cultivo de interesse em Itaberai ignorando o efeito de cobertura de
#habitat natural


TABELA_PIXEIS <- read.table(file.choose(), header=T) #tabela_de_distancias3.txt
adeq <- read.csv("Polin_laranja3.csv") #Polin_laranja3.csv
fordis <- read.csv("Tabela_dist_for_spp.csv") #Tabela_dist_for_spp.csv

#transformar a distância de km para metros
fordis$fordist_m_GrMfd <- fordis$fordist_km_GrMfd * 1000


unique(TABELA_PIXEIS$habitat)

unique(adeq$spp)

unique(adeq$classe_nome)

## 1.2 - juntar a informação de adequabilidade a tabela pixeis
adeqAmellifera=adeq[adeq$spp=="Apis mellifera",c(3,4)]
adeqCfuscata=adeq[adeq$spp=="Centris fuscata",c(3,4)]
adeqFdoederleini=adeq[adeq$spp=="Frieseomelita doederleini",c(3,4)]
adeqFlanguida=adeq[adeq$spp=="Frieseomelitta languida",c(3,4)]
adeqFvaria=adeq[adeq$spp=="Frieseomelitta varia",c(3,4)]
adeqOtaitara=adeq[adeq$spp=="Oxytrigona tataira",c(3,4)]
adeqPwittmanni=adeq[adeq$spp=="Plebeia wittmanni",c(3,4)]
adeqTangustula=adeq[adeq$spp=="Tetragonisca angustula",c(3,4)]
adeqTspinipes=adeq[adeq$spp=="Trigona spinipes",c(3,4)]
adeqXgrisescens=adeq[adeq$spp=="Xylocopa grisescens",c(3,4)]
adeqXsuspecta=adeq[adeq$spp=="Xylocopa suspecta",c(3,4)]
adeqCnitens=adeq[adeq$spp=="Centris nitens", c(3,4)]
adeqEanalis=adeq[adeq$spp=="Exomalopsis analis", c(3,4)]
adeqEauropilosa=adeq[adeq$spp=="Exomalopsis auropilosa", c(3,4)]
adeqGmombuca=adeq[adeq$spp=="Geotrigona mombuca", c(3,4)]
adeqMquadrifasciata=adeq[adeq$spp=="Melipona quadrifasciata", c(3,4)]
adeqPlienata=adeq[adeq$spp=="Paratrigona lineata", c(3,4)]
adeqPdroryana=adeq[adeq$spp=="Plebeia droryana", c(3,4)]
adeqTclavipes=adeq[adeq$spp=="Tetragona clavipes", c(3,4)]
adeqThyalinata=adeq[adeq$spp=="Trigona hyalinata", c(3,4)]
adeqTrecursa=adeq[adeq$spp=="Trigona recursa", c(3,4)]

#renomear a coluna adequab de cada subset de cada espécie para não dar problema no merge



adeq_spp <- list(adeqAmellifera, adeqCfuscata, adeqFdoederleini, adeqFlanguida,
              adeqFvaria, adeqOtaitara, adeqPwittmanni, adeqTangustula,
              adeqTspinipes, adeqXgrisescens, adeqXsuspecta, adeqCnitens,
              adeqEanalis, adeqEauropilosa, adeqGmombuca, adeqMquadrifasciata,
              adeqPlienata, adeqPdroryana, adeqTclavipes, adeqThyalinata,
              adeqTrecursa)

adeq_spp[[1]]

nome <- c("adeq.Amel", "adeq.Cfusc", "adeq.Fdoed", "adeq.Flang",
          "adeqFvar", "adeqOtait", "adeqPwitt", "adeqTang",
          "adeqTspin", "adeqXgris","adeqXsus", "adeqCnit",
          "adeqEan", "adeqEauro", "adeqGmom", "adeqMquad",
          "adeqPlin", "adeqPdro", "adeqTclav", "adeqThyali",
          "adeqTrecur")

length(adeq_spp)
length(nome)


for(i in 1:21) {
  colnames(adeq_spp[[i]])[2] <- nome[i]
}


adeq_spp[[1]]
View(adeq_spp)


teste=cbind(adeq_spp[[1]],adeq_spp[[2]][[2]],adeq_spp[[3]][[2]],adeq_spp[[4]][[2]],
            adeq_spp[[5]][[2]],adeq_spp[[6]][[2]],adeq_spp[[7]][[2]],adeq_spp[[8]][[2]],
            adeq_spp[[9]][[2]],adeq_spp[[10]][[2]],adeq_spp[[11]][[2]],adeq_spp[[12]][[2]],
            adeq_spp[[13]][[2]],adeq_spp[[14]][[2]],adeq_spp[[15]][[2]],adeq_spp[[16]][[2]],
            adeq_spp[[17]][[2]],adeq_spp[[18]][[2]],adeq_spp[[19]][[2]],adeq_spp[[10]][[2]],
            adeq_spp[[21]][[2]])

colnames(teste) = c("classe_num", nome)

Tabela <- merge(TABELA_PIXEIS, teste, by = intersect(names(TABELA_PIXEIS)), names(teste),
                    by.x = "habitat", by.y = "classe_num", all.x = T, all.y = F)




write.table(Tabela, file="Tabela1.txt")
read.table("Tabela1.txt")

#1.3 Para cada pixel da TABELA_PIXEIS, calcular a densidade de cada espécie de polinizador 'a'
#com a seguinte formula
#DR_pa1_a1  = (A_h1*  exp^(-Dist_h1 /Dist_max_a1))
#head(Tabela)

distancia_m <- fordis$fordist_m_GrMfd

Tabela$Dens_Amel=Tabela$adeq.Amel*exp(-(Tabela$min_distance+1)/distancia_m[1])

Tabela$Dens_Cfusc=Tabela$adeq.Cfusc*exp(-(Tabela$min_distance+1)/distancia_m[2])

Tabela$Dens_Fdoed=Tabela$adeq.Fdoed*exp(-(Tabela$min_distance+1)/distancia_m[3])

Tabela$Dens_Flang=Tabela$adeq.Flang*exp(-(Tabela$min_distance+1)/distancia_m[4])

Tabela$Dens_Fvar=Tabela$adeqFvar*exp(-(Tabela$min_distance+1)/distancia_m[5])

Tabela$Dens_Otait=Tabela$adeqOtait*exp(-(Tabela$min_distance+1)/distancia_m[6])

Tabela$Dens_Pwitt=Tabela$adeqPwitt*exp(-(Tabela$min_distance+1)/distancia_m[7])

Tabela$Dens_Tang=Tabela$adeqTang*exp(-(Tabela$min_distance+1)/distancia_m[8])

Tabela$Dens_Tspin=Tabela$adeqTspin*exp(-(Tabela$min_distance+1)/distancia_m[9])

Tabela$Dens_Xgris=Tabela$adeqXgris*exp(-(Tabela$min_distance+1)/distancia_m[10])

Tabela$Dens_Xsus=Tabela$adeqXsus*exp(-(Tabela$min_distance+1)/distancia_m[11])

Tabela$Dens_Cnit=Tabela$adeqCnit*exp(-(Tabela$min_distance+1)/distancia_m[12])

Tabela$Dens_Ean=Tabela$adeqEan*exp(-(Tabela$min_distance+1)/distancia_m[13])

Tabela$Dens_Eauro=Tabela$adeqEauro*exp(-(Tabela$min_distance+1)/distancia_m[14])

Tabela$Dens_Gmom=Tabela$adeqGmom*exp(-(Tabela$min_distance+1)/distancia_m[15])

Tabela$Dens_Mquad=Tabela$adeqMquad*exp(-(Tabela$min_distance+1)/distancia_m[16])

Tabela$Dens_Plin=Tabela$adeqPlin*exp(-(Tabela$min_distance+1)/distancia_m[17])

Tabela$Dens_Pdro=Tabela$adeqPdro*exp(-(Tabela$min_distance+1)/distancia_m[18])

Tabela$Dens_Tclav=Tabela$adeqTclav*exp(-(Tabela$min_distance+1)/distancia_m[19])

Tabela$Dens_Thyali=Tabela$adeqThyali*exp(-(Tabela$min_distance+1)/distancia_m[20])

Tabela$Dens_Trecur=Tabela$adeqTrecur*exp(-(Tabela$min_distance+1)/distancia_m[21])




#TABELA_PIXEIS_AD$Dens_Amel=TABELA_PIXEIS_AD$adeq.Amel*exp(-(TABELA_PIXEIS_AD$min_distance+1)/1152.5124)

#TABELA_PIXEIS_AD2$Dens_Tspi=TABELA_PIXEIS_AD2$adeq.Tspi*exp(-(TABELA_PIXEIS_AD2$min_distance+1)/238.7173)

#soma das densidades de todas as espécies de abelhas para cada habitat de cada pixel

#devido a limitação na memória RAM, eu dividi a soma em três etapas

Tabela$densT1 <- rowSums(Tabela[ ,c(24:34)])

Tabela$densT2 <- rowSums(Tabela[ ,35:45])

Tabela$densTotal <- rowSums(Tabela[ , c(46, 47)])

#TABELA_PIXEIS_AD2$Total=TABELA_PIXEIS_AD2$Dens_Amel+TABELA_PIXEIS_AD2$Dens_Tspi
#head(TABELA_PIXEIS_AD2)


#1.4 criar uma tabela com apenas 1 linha por pixel e com uma coluna com a soma de Dens_Amel e Dens_Tsi

Densidade_Total <- aggregate(densTotal ~ x + y, Tabela, sum)

#TABELA_PIXEIS_DensTotal=aggregate(Total ~ x + y, TABELA_PIXEIS_AD2, sum)


#hist(TABELA_PIXEIS_DensTotal$Total)

########################################################################################
#1.5 calcular a produtividade associada a cada pixel que não depende de polinizadores (V)
########################################################################################

#(assumindo que todos os pixeis agricolas são usados para a produção da cultura)
# ver slides com imagens esquematicas de campo agricola que enviei há uns tempos atrás

# value produced per pixel without pollinators (V) 
# D - crop pollinator dependency
# F - fração (0 a 1) do valor ótimo de densidade de polinizadores
# contribution  of pollinators in a given pixel (C) = V*D*F

# (temos de pensar qual será esse valor ótimo de polinizadores, 
# mas se for a soma das densidades máximas das espécies 
# e se uma dada espécies tiver 5 polinizadores, seria 1+1+1+1+1=5, ou seja riqueza_polinizadores
# mas quando temos vários habitats a contribuir o valor máximo por pixel pode ser maior
# por isso talvez seja melhor considerar como valor maximo de densidade o valor maximo
#caso todas as espécies de polinizadores nidificassem bem em todas as classes de paisagem: 
#riqueza_maxima*numero de classes de paisagem)




valor_maximo <- length(nome) # riqueza de espécies

for (j in 1:length(Densidade_Total$densTotal)) {
  if (Densidade_Total$densTotal[j] >= valor_maximo) {
    Densidade_Total$F[j] <- 1
  } else {
    Densidade_Total$F[j] <- Densidade_Total$densTotal[j] / valor_maximo
  }
}



#Densidade_Total$F=Densidade_Total$densTotal/valor_maximo


#TABELA_PIXEIS_DensTotal$F=TABELA_PIXEIS_DensTotal$Total/valor_maximo

#desta forma se a produção geral da crop no municipio for PC
# PC = somatorio_pixeis (V+ V*D*F)
# PC = num_pixeis*V+ V*D*somatorio(Fs)
# PC=nrow(TABELA_PIXEIS_DensTotal)*V+D*V*(sum(TABELA_PIXEIS_DensTotal$F))
# PC/V=nrow(TABELA_PIXEIS_DensTotal)+D*(sum(TABELA_PIXEIS_DensTotal$F))
# 1/V =(nrow(TABELA_PIXEIS_DensTotal)+D*(sum(TABELA_PIXEIS_DensTotal$F)))/PC
# V = PC/ (nrow(TABELA_PIXEIS_DensTotal)+D*(sum(TABELA_PIXEIS_DensTotal$F)))

#parametros
QPT=28570000 #quantidade produzida total (kg) em 2021

VPT= 18571000 #valor de produção total (reais) em 2021

#Taxas de dependência com base em Siopa 2023:

TDmin=0.06  
TDmax=0.31
TDmed=0.19



#Calculando para TDmed:
V_TDmed_QP = QPT/ (nrow(Densidade_Total)+(TDmed*(sum(Densidade_Total$F))))
V_TDmed_VP = VPT/ (nrow(Densidade_Total)+TDmed*(sum(Densidade_Total$F)))


########################################################################################
#1.6 calcular a produtividade do municipio caso todos os polinizadores sejam perdidos
########################################################################################
resultados <- data.frame(
  Parametros = numeric(3),
  Prod_total = numeric(3),
  TDmin_0.06 = numeric(3),
  TDmed_0.19 = numeric(3),
  TD_max_0.31 = numeric(3)
  )

resultados[c(1:3),1] <- c("Quant. prod. (kg)", "Valor de prod. (reais)",
                          "Contr. polin. (%)")


calcula_valor <- function (parametro, TD) {
  V <-  parametro/ (nrow(Densidade_Total)+TD*(sum(Densidade_Total$F)))
  Sem_polin <- V * (nrow(Densidade_Total))
  Contr_polin <- parametro - Sem_polin
  return(Contr_polin)
}

resultados[1,2] <- QPT
resultados[2,2] <- VPT
resultados[1,3] <- calcula_valor(QPT, TDmin)
resultados[1,4] <- calcula_valor(QPT, TDmed)
resultados[1,5] <- calcula_valor(QPT, TDmax)
resultados[2,3] <- calcula_valor(VPT, TDmin)
resultados[2,4] <- calcula_valor(VPT, TDmed)
resultados[2,5] <- calcula_valor(VPT, TDmax)

Contr_polin_QPT_perc <- function(parametro, contribuicao) {
  resu <- round((contribuicao*100/parametro),2)
  return(resu)
}

resultados[3,2] <- Contr_polin_QPT_perc(QPT, contribuicao=0)
resultados[3, c(3:5)] <- Contr_polin_QPT_perc(QPT, c(resultados[1,3], 
                          resultados[1,4], resultados[1,5]))


write.csv(resultados, file="contr_poli_laranja_2021.csv")

#mediana da distância de forrageamento dos polinizadores
median(fordis$fordist_m_GrMfd)
