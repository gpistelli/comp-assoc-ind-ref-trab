library(tidyverse)
library(igraph)

#### Composição do dataframe e dados iniciais ####

#Primeiro, vamos importar os dados
edges_fiep_ent <- read_csv("edges_fiep_ent.csv")
nodes_fiep_ent <- read_csv("nodes_fiep_ent.csv")
edges_sfiep_ent <- read_csv("edges_sfiep_ent.csv")
nodes_sfiep_ent <- read_csv("nodes_sfiep_ent.csv")

# Cálculos dos dados
length(nodes_fiep_ent$Ator) # Número de atores
length(edges_fiep_ent$Ator1) # Número de ligações
sps <- which(nodes_fiep_ent$Tipo == "Sindicato Patronal")
est <- which(nodes_fiep_ent$Abrangencia == "Estadual")
mun <- which(nodes_fiep_ent$Abrangencia == "Municipal")
nac <- which(nodes_fiep_ent$Abrangencia == "Nacional")
reg <- which(nodes_fiep_ent$Abrangencia == "Regional")

length(mun) # n municipais 
length(reg) # n regionais
length(est) # n estaduais
length(nac) # n nacional
length(sps) # n sindicatos

#### Construção das ARS ####

# E então começamos construindo os gráficos
SN_SFIEP_ENT <- graph_from_data_frame(d=edges_sfiep_ent, vertices=nodes_sfiep_ent, directed=F)
SN_FIEP_ENT <- graph_from_data_frame(d=edges_fiep_ent, vertices=nodes_fiep_ent, directed=F)

# E então passamos a customizar os gráficos
V(SN_FIEP_ENT)$color <- "blue"
V(SN_FIEP_ENT)$color <- ifelse(V(SN_FIEP_ENT)$Tipo == "Sindicato Patronal", "aquamarine", V(SN_FIEP_ENT)$color)
V(SN_FIEP_ENT)$color <- ifelse(V(SN_FIEP_ENT)$Tipo == "Associacao", "yellow", V(SN_FIEP_ENT)$color)
V(SN_FIEP_ENT)$color <- ifelse(V(SN_FIEP_ENT)$Tipo == "Federacao", "orange", V(SN_FIEP_ENT)$color)
V(SN_FIEP_ENT)$color <- ifelse(V(SN_FIEP_ENT)$Tipo == "Confederacao", "red", V(SN_FIEP_ENT)$color)
V(SN_FIEP_ENT)$color <- ifelse(V(SN_FIEP_ENT)$Tipo == "Outro", "grey", V(SN_FIEP_ENT)$color)

V(SN_FIEP_ENT)$size <- log(strength(SN_FIEP_ENT)) * 4 + 3

set.seed(123) # Sociograma 1
plot(SN_FIEP_ENT, label.color = "black", main = "SOCIOGRAMA 1: Sociograma das entidades nas quais os dirigentes dos sindicatos patronais
     da indústria de Curitiba (2014-2019) atuaram, com a FIEP",
     layout = layout_with_kk,
     sub = "FONTE: O autor. Legenda: Azul = Sindicatos; Amarelo = Associação; Laranja = Federação; Vermelho = Confederação; Outro = Cinza")

# Agora o mesmo processo é feito com o database sem a FIEP

V(SN_SFIEP_ENT)$color <- "blue"
V(SN_SFIEP_ENT)$color <- ifelse(V(SN_SFIEP_ENT)$Tipo == "Sindicato Patronal", "aquamarine", V(SN_SFIEP_ENT)$color)
V(SN_SFIEP_ENT)$color <- ifelse(V(SN_SFIEP_ENT)$Tipo == "Associacao", "yellow", V(SN_SFIEP_ENT)$color)
V(SN_SFIEP_ENT)$color <- ifelse(V(SN_SFIEP_ENT)$Tipo == "Federacao", "orange", V(SN_SFIEP_ENT)$color)
V(SN_SFIEP_ENT)$color <- ifelse(V(SN_SFIEP_ENT)$Tipo == "Confederacao", "red", V(SN_SFIEP_ENT)$color)
V(SN_SFIEP_ENT)$color <- ifelse(V(SN_SFIEP_ENT)$Tipo == "Outro", "grey", V(SN_SFIEP_ENT)$color)

V(SN_SFIEP_ENT)$size <- log(strength(SN_SFIEP_ENT)) * 4 + 3

set.seed(123) # SOCIOGRAMA 2
plot(SN_SFIEP_ENT, main = "SOCIOGRAMA 2: Sociograma das entidades nas quais os dirigentes dos sindicatos patronais
     da indústria de Curitiba (2014-2019) atuaram, sem a FIEP",
     layout = layout_with_fr,
     sub = "FONTE: O autor. Legenda: Azul = Sindicatos; Amarelo = Associação; Laranja = Federação; Vermelho = Confederação; Outro = Cinza",
     label.color = "black")

#### Cálculos da ARS ####

# Contagem de triângulos (não implementado na dissertação)
sum(count_triangles(SN_FIEP_ENT))
sum(count_triangles(SN_SFIEP_ENT))

# Agora, iremos fazer os cálculos de centralidade
eigen.sFIEP <- eigen_centrality(SN_SFIEP_ENT, directed = FALSE, scale = TRUE, weights = NULL, options = arpack_defaults)
eigen.cFIEP <- eigen_centrality(SN_FIEP_ENT, directed = FALSE, scale = TRUE, weights = NULL, options = arpack_defaults)
eig.cent.cFIEP <- eigen.cFIEP$vector
eig.cent.sFIEP <- eigen.sFIEP$vector
eig.cent.sFIEP <- sort(eig.cent.sFIEP, decreasing = TRUE)
eig.cent.cFIEP <- sort(eig.cent.cFIEP, decreasing = TRUE)

# Entidades centrais com e sem a FIEP
eig.cent.sFIEP[1:10]
eig.cent.cFIEP[1:10]

# Cálculo alternativo: betweenness
betw_sfiep <- sort(betweenness(SN_SFIEP_ENT, directed = FALSE, normalized = TRUE), decreasing = TRUE)
betw_fiep <- sort(betweenness(SN_FIEP_ENT, directed = FALSE, normalized = TRUE), decreasing = TRUE)
betw_fiep[1:10]
betw_sfiep[1:10]

# Cálculo da densidade
edge_density(SN_FIEP_ENT, loops = FALSE)
edge_density(SN_SFIEP_ENT, loops = FALSE)

#### Cálculo das ligações das entidades extracorporativas (TABELA X) ####
count.ties.socnet <- function(ent, df){
  vec2 <- grep(ent, df$Ator1)
  vec1 <- grep(ent, df$Ator2)
  nLig <- length(vec1)+length(vec2)
  return(nLig)
}

ACP <- count.ties.socnet("ACP", edges_fiep_ent)
CACB <- count.ties.socnet("CACB", edges_fiep_ent)
FECOMERCIO <- count.ties.socnet("FECOMERCIO-PR", edges_fiep_ent)
FACIAP <- count.ties.socnet("FACIAP", edges_fiep_ent)
CACIOPAR <- count.ties.socnet("CACIOPAR", edges_fiep_ent)
OCEPAR <- count.ties.socnet("Ocepar", edges_fiep_ent)
nLig.ExtCorp <- c(ACP, CACB, FECOMERCIO, FACIAP, CACIOPAR, OCEPAR)
pLig.ExtCorp <- nLig.ExtCorp/length(edges_sfiep_ent$Ator1)
Tab.ExtCorp <- cbind.data.frame(nLig.ExtCorp, pLig.ExtCorp)
row.names(Tab.ExtCorp) <- c("ACP", "CACB", "FECOMERCIO", "FACIAP", "CACIOPAR", "OCEPAR")
pos.eig.ACP <- which(names(eig.cent.sFIEP) == "ACP")
pos.eig.CACB <- which(names(eig.cent.sFIEP) == "CACB")
pos.eig.FACIAP <- which(names(eig.cent.sFIEP) == "FACIAP")
pos.eig.FECOMERCIO <- which(names(eig.cent.sFIEP) == "FECOMERCIO-PR")
pos.eig.CACIOPAR <- which(names(eig.cent.sFIEP) == "CACIOPAR")
pos.eig.OCEPAR <- which(names(eig.cent.sFIEP) == "Ocepar")
eig.ExtCorp <- eig.cent.sFIEP[c(pos.eig.ACP, pos.eig.CACB, pos.eig.FECOMERCIO, pos.eig.FACIAP, pos.eig.CACIOPAR, pos.eig.OCEPAR)]
Tab.ExtCorp <- cbind(Tab.ExtCorp, eig.ExtCorp)
lapply(Tab.ExtCorp, sum)
lapply(Tab.ExtCorp, mean)
