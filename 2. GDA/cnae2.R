library(rvest)
library(dplyr)
library(stringr)
library(factoextra)
library(FactoMineR)
library(GDAtools)
library(readr)
library(ggplot2)
library(gridExtra)

act.wd <- getwd()


#### Versão com os códigos da CONCLA diretamente do site, não implementado ####
# Vamos pegar os códigos
CNAE.get_code <- function(link){
  codigo <- read_html(link) %>% html_nodes(css = ".codigo", x = .) %>% html_text
  codigo <- gsub("\r|\n|\t", "", codigo)
  cod.n <- str_extract(codigo, "^(.+?) ") %>% gsub(" ", "", .)
  cod.a <- gsub("^(.+?) ", "", codigo)
  codigo <- cbind.data.frame(cod.n, cod.a)
  names(codigo) <- c("Cod", "Ativ")
  return(codigo)
}

cod.transf <- CNAE.get_code("https://concla.ibge.gov.br/busca-online-cnae.html?view=secao&tipo=cnae&versaosubclasse=10&versaoclasse=7&secao=C")
cod.transp <- CNAE.get_code("https://concla.ibge.gov.br/busca-online-cnae.html?view=secao&tipo=cnae&versaosubclasse=10&versaoclasse=7&secao=H")
cod.extrc <- CNAE.get_code("https://concla.ibge.gov.br/busca-online-cnae.html?view=secao&tipo=cnae&versaosubclasse=10&versaoclasse=7&secao=B")
cod.constr <- CNAE.get_code("https://concla.ibge.gov.br/busca-online-cnae.html?view=secao&tipo=cnae&versaosubclasse=10&versaoclasse=7&secao=F")
cod.comerc <- CNAE.get_code("https://concla.ibge.gov.br/busca-online-cnae.html?view=secao&tipo=cnae&versaosubclasse=10&versaoclasse=7&secao=G")

CNAE.conv2 <- function(vec){
  
  for (i in 1:length(cod.transf$Cod)){
    vec <- gsub(paste0("^", cod.transf$Cod[i], "(.+)"), cod.transf$Ativ[i], vec)
  }
  
  for (i in 1:length(cod.constr$Cod)){
    vec <- gsub(paste0("^", cod.constr$Cod[i], "(.+)"), cod.constr$Ativ[i], vec)
  }
  
  for (i in 1:length(cod.comerc$Cod)){
    vec <- gsub(paste0("^", cod.comerc$Cod[i], "(.+)"), cod.comerc$Ativ[i], vec)
  }  
  
  vec <- gsub("^[[:digit:]]+", "OUTRO", vec)
  return(vec)
}   


# Versão maior, com os códigos da CNAE coletados diretamente
base2 <- matrix(nrow = 0, ncol = 31)
colnames(base2) <- c(cod.transf$Ativ, cod.comerc$Ativ, cod.constr$Ativ, "OUTRO")
base2 <- as.data.frame(base2)
base2[, 1:31] <- lapply(base2[, 1:31], as.character)

#### Construção da ACM 2 ####

# Versão menor, com serviços
base2 <- matrix(nrow = 0, ncol = 7)
colnames(base2) <- c("Agric", "Extr", "Transf", "AgEletrEsg", "Constr", "Comerc", "Serv")
base2 <- as.data.frame(base2)
base2[, 1:7] <- lapply(base2[, 1:7], as.character)

an.cont_prep_mca <- function(vec){
  names.vec <- vec
  vec <- rep("y", length(vec))
  names(vec) <- names.vec
  return(vec)
}

# Versão menor, dos serviços
CNAE.conv2 <- function(vec){
  vec <- gsub("^01(.*)|^02(.*)|^03(.*)]", "Agric", vec)
  vec <- gsub("^05(.*)|^06(.*)|^07(.*)|^08(.*)|^09(.*)",  "Extr", vec)
  vec <- gsub("^10(.*)|^11(.*)|^12(.*)|^13(.*)|^14(.*)|^15(.*)|^16(.*)|^17(.*)|^18(.*)|^19(.*)|^20(.*)|^21(.*)|^22(.*)|^23(.*)|^24(.*)|^25(.*)|^26(.*)|^27(.*)|^28(.*)|^29(.*)|^30(.*)|^31(.*)|^32(.*)|^33(.*)", "Transf", vec)
  vec <- gsub("^35(.*)|^36(.*)|^37(.*)|^38(.*)|^39(.*)", "AgEletrEsg", vec)
  vec <- gsub("^41(.*)|^42(.*)|^43(.*)", "Constr", vec)
  vec <- gsub("^45(.*)|^46(.*)|^47(.*)", "Comerc", vec)
  vec <- gsub("^49(.*)|^50(.*)|^51(.*)|^52(.*)|^53(.*)|^55(.*)|^56(.*)|^58(.*)|^59(.*)|^60(.*)|^61(.*)|^62(.*)|^63(.*)|^64(.*)|^65(.*)|^66(.*)|^68(.*)|^69(.*)|^70(.*)|^71(.*)|^72(.*)|^73(.*)|^74(.*)|^75(.*)|^77(.*)|^78(.*)|^79(.*)|^80(.*)|^81(.*)|^82(.*)|^84(.*)|^85(.*)|^86(.*)|^87(.*)|^88(.*)|^90(.*)|^91(.*)|^92(.*)|^93(.*)|94(.*)|^95(.*)|^96(.*)|^97(.*)|^99(.*)", "Serv", vec)
  return(vec)
}

# Vamos para as funções

CNAE.get_from_direc2 <- function(sind){
  setwd(paste0(act.wd, "/Consultasocio/", sind))
  files <- list.files(pattern = "html$")
  df <- lapply(files, CNAE.prep_MCA2, sind)
  df <- do.call(rbind, df)
  setwd(act.wd)
  return(df)
}

CNAE.prep_MCA2 <- function(link, sind){
  vec <- CNAE.get_vec2(link)
  vec <- an.cont_prep_mca(vec)
  sind <- sind
  names(sind) <- "Sindicato"
  vec <- c(sind, vec)
  df <- bind_rows(base2, vec)
  df[is.na(df)] <- "n"
  row.names(df) <- link %>% gsub("[[:punct:]]html", "", x = .)
  return(df)
}

CNAE.get_vec2 <- function(link){
  page <- read_html(link) %>% html_nodes("#details div div div div div") %>% html_text() 
  vec <- page[grep("^Atividade econômica", page)]
  loc <- regexpr("[[:punct:]]([[:digit:]]+)[[:punct:]]", vec)
  vec <- regmatches(vec, loc)
  vec <- gsub("[[:punct:]]", "", vec)
  vec <- CNAE.conv2(vec)
  vec <- unique(vec)
}

CNAE.get_from_direc_mltpg2 <- function(drg, sind){
  setwd(paste0(act.wd, "/Consultasocio/", sind, "/", drg))
  vec <- list.files(pattern = "html$")
  vec <- lapply(vec, CNAE.get_vec2)
  vec <- unlist(vec)
  vec <- unique(vec)
  vec <- an.cont_prep_mca(vec)
  sind <- sind
  names(sind) <- "Sindicato"
  vec <- c(sind, vec)
  df <- bind_rows(base2, vec)
  df[is.na(df)] <- "n"
  row.names(df) <- drg
  setwd(act.wd)
  return(df)
}

CNAE.get_from_direc_mltpg_cpf2 <- function(drg, sind, cpf){
  setwd(paste0(act.wd, "/Consultasocio/", sind, "/CPF/", drg))
  vec <- list.files(pattern = "html$")
  vec <- lapply(vec, CNAE.get_vec_cpf2, cpf)
  vec <- unlist(vec)
  vec <- unique(vec)
  vec <- an.cont_prep_mca(vec)
  sind <- sind
  names(sind) <- "Sindicato"
  vec <- c(sind, vec)
  df <- bind_rows(base2, vec)
  df[is.na(df)] <- "n"
  row.names(df) <- drg
  setwd(act.wd)
  return(df)
}

# CPF

CNAE.get_from_direc_cpf2 <- function(sind, diretor, cpf){
  setwd(paste0(act.wd, "/Consultasocio/", sind, "/CPF/", diretor))
  page <- list.files(pattern = "html$")
  vec <- CNAE.get_vec_cpf2(page, cpf)
  vec <- an.cont_prep_mca(vec)
  sind <- sind
  names(sind) <- "Sindicato"
  vec <- c(sind, vec)
  df <- bind_rows(base2, vec)
  df[is.na(df)] <- "n"
  row.names(df) <- diretor
  setwd(act.wd)
  return(df)
}

CNAE.get_vec_cpf2 <- function(page, cpf){
  page <- read_html(page) %>% html_nodes("#details div div div div div") %>% html_text()
  vec <- page[grep(paste0("CPF(.+)", cpf, "(.+)Data de entrada(.+)Situação Cadastral"), page)]
  matches <- regexpr("Atividade econômica(.+?)[[:digit:]][[:punct:]]", vec)
  vec <- regmatches(vec, matches)
  vec <- gsub("Atividade econômica :", "", vec)
  loc <- regexpr("[[:punct:]]([[:digit:]]+)[[:punct:]]", vec)
  vec <- regmatches(vec, loc)
  vec <- gsub("[[:punct:]]", "", vec)
  vec <- CNAE.conv2(vec) %>% unique()
  return(vec)
}

CNAE.get_from_direc_mltpg_sind2 <- function(sind){
  drg <- list.dirs(path = paste0(act.wd, "/Consultasocio/", sind))
  drg <- gsub(paste0("C:(.+)", sind, "/"), "", drg)[2:length(drg)]
  df <- lapply(drg, CNAE.get_from_direc_mltpg2, sind)
  df <- do.call(rbind, df)
  return(df)
}


sindicatos <- list.dirs(paste0(act.wd, "/Consultasocio"))
sindicatos <- sindicatos[-grep("Consultasocio[[:punct:]].+[[:punct:]]", sindicatos)]
sindicatos <- gsub(paste0("C:/Users/Gabriel/Documents/Dissertação/Dados/R/ACM/Consultasocio/"), "", sindicatos)
sindicatos <- gsub("[[:punct:]]", "", sindicatos)
sindicatos <- sindicatos[-1]

CNAE_data2 <- lapply(sindicatos, CNAE.get_from_direc2)
CNAE_data2 <- do.call(rbind, CNAE_data2)
mltpg_sind <- c("SICEPOT", "SIITEP", "SINAEES", "SINDIOLEOS", "SINDIMETAL", "SINDEMCAP", "SINDICARNE", "SINDIADUBOS", "SINDICAF", "SINDILEITE", "SINDIVEST", "SINPACEL", "SINQFAR", "SIMADEIRA")
CNAE_data.mltpg2 <- lapply(X = mltpg_sind, FUN = CNAE.get_from_direc_mltpg_sind2)
CNAE_data.mltpg2 <- do.call(rbind, CNAE_data.mltpg2)
MGS <- c("n", "n", "n", "n", "n", "n", "n", "SINQFAR") # Tive que colocar manualmente por ser administrador
names(MGS) <- names(CNAE_data2)
CNAE_data2 <- rbind(CNAE_data2, CNAE_data.mltpg2, MGS)
row.names(CNAE_data2)[length(CNAE_data2$Agric)] <- "MGS"
CNAE_data2[grep("^VAM$", row.names(CNAE_data2)), 3]<- "y" # Tive que colocar manualmente por ser empresa privada no nome de sua mulher
CNAE_data2 <- rbind(CNAE_data2, CNAE.get_from_direc_mltpg_cpf2("DoMrt", "SINDIAVIPAR", 388509),
                    CNAE.get_from_direc_mltpg_cpf2("PCdA", "SINDITRIGO", 943797),
                    CNAE.get_from_direc_cpf2("SINDITEXTIL", "GeLuCa", 666489),
                    CNAE.get_from_direc_cpf2("SINDICAL", "MPoli", 673509),
                    CNAE.get_from_direc_cpf2("SINDUSCON", "MaAvzB", 040049),
                    CNAE.get_from_direc_cpf2("SINDIAVIPAR", "CdOJr", 114009),
                    CNAE.get_from_direc_cpf2("SINDIPINTURAS", "RMar", 210149),
                    CNAE.get_from_direc_cpf2("SINDILOUCAS", "ER", 600659),
                    CNAE.get_from_direc_cpf2("SIMADEIRA", "LC", 803219),
                    CNAE.get_from_direc_mltpg_cpf2("RicMue", "SIMADEIRA", 088769)
                    
)

CNAE_data2 <- CNAE_data2[-grep("^CPF", row.names(CNAE_data2)),]
CNAE_data2 <- CNAE_data2[-4]
CNAE_data2[,1:7] <- lapply(X = CNAE_data2[,1:7], FUN = as.factor)

##### Construção da ACM 2 ####

mca.cnae2 <- MCA(CNAE_data2, quali.sup = 7, graph = FALSE)

# ACM 2
fviz_mca_var(mca.cnae2, repel = TRUE, geom = c("point", "text"), col.var="contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), col.quali.sup = "darkgreen",
             title = "ACM 2: Atuação setorial dos dirigentes dos sindicatos da indústria de Curitiba",
             ggtheme = theme_gray ())

prop.table(table(CNAE_data2$Agric))*100
prop.table(table(CNAE_data2$Extr))*100
prop.table(table(CNAE_data2$Transf))*100
prop.table(table(CNAE_data2$Constr))*100
prop.table(table(CNAE_data2$Comerc))*100
prop.table(table(CNAE_data2$Serv))*100
prop.table(table(CNAE_data2$NivKSind))*100


#### Algumas pirações nada a ver ####

summary(CNAE_data2[which(CNAE_data2$Sindicato == "SINPACEL"), ])
summary(CNAE_data2[which(CNAE_data2$Sindicato == "SIMADEIRA"), ])
CNAE_data2[which(CNAE_data2$Sindicato == "SIAPAR"), ] %>% droplevels() %>% summary()
CNAE_data2[which(CNAE_data2$Sindicato == "SINDICER"), ] %>% droplevels() %>% summary()
CNAE_data2[which(CNAE_data2$Sindicato == "SINDUSCON"), ] %>% droplevels() %>% summary()
CNAE_data2[which(CNAE_data2$Sindicato == "SINCABIMA"), ] %>% droplevels() %>% summary()
CNAE_data2[which(CNAE_data2$Sindicato == "SINDIVEST"), ] %>% droplevels() %>% summary()
CNAE_data2[which(CNAE_data2$Sindicato == "SINPACEL"), ] %>% droplevels() %>% summary()
CNAE_data2[which(CNAE_data2$Sindicato == "SINDICOURO"), ] %>% droplevels() %>% summary()

CNAE_data2[which(CNAE_data2$Transf == "y"), ] %>% droplevels() %>% summary()

CNAE_data2 %>% filter(Constr == "y") %>% select(Serv) %>% table() %>% prop.table()*100
CNAE_data2 %>% filter(Transf == "y") %>% select(Serv) %>% table() %>% prop.table()*100

CNAE_data2 %>% filter(Constr == "y") %>% select(Agric) %>% table() %>% prop.table()*100
CNAE_data2 %>% filter(Transf == "y") %>% select(Agric) %>% table() %>% prop.table()*100

CNAE_data2 %>% filter(Constr == "y") %>% select(Agric) %>% table() %>% prop.table()*100
CNAE_data2 %>% filter(Transf == "y") %>% select(Agric) %>% table() %>% prop.table()*100

CNAE_data2 %>% filter(Constr == "y") %>% select(Transf) %>% table() %>% prop.table()*100
CNAE_data2 %>% filter(Transf == "y") %>% select(Constr) %>% table() %>% prop.table()*100

CNAE_data2 %>% filter(Constr == "y") %>% select(Comerc) %>% table() %>% prop.table()*100
CNAE_data2 %>% filter(Transf == "y") %>% select(Comerc) %>% table() %>% prop.table()*100

summary(CNAE_data2)
CNAE_data
# Renomear as colunas e arrumar os dados que estiverem distantes, mas que são próximos (Indústria de transformação, por exemplo)

CNAE_data_teste[,1:ncol(CNAE_data_teste)] <- lapply(CNAE_data_teste[,1:ncol(CNAE_data_teste)], as.factor)
CNAE_data_teste <- CNAE_data_teste[,-4]

mca.cnae2 <- MCA(CNAE_data2, quali.sup = 7, graph = FALSE)
fviz_mca_var(mca.cnae2, repel = TRUE, geom = c("point", "text"), col.var="contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), col.quali.sup = "darkgreen", ggtheme = theme_gray ())
fviz_contrib(mca.cnae2, choice = "var", axes = 1, top = 10)
fviz_contrib(mca.cnae2, choice = "var", axes = 2, top = 10)
fviz_contrib(mca.cnae2, choice = "var", axes = 3, top = 10)
fviz_screeplot(mca.cnae2, addlabels = TRUE)

## Média das posições dos membros dos sindicatos

facto_summarize(X = mca.cnae21, element = "mca.cor")
cnae.ind.pos <- facto_summarize(X = mca.cnae21, element = "ind")[, 2:3]
cnae.ind.pos <- cbind(cnae.ind.pos, CNAE_data2$Sindicato)
names(cnae.ind.pos) <- c("Dim.1", "Dim.2", "Sind")
cnae.ind.pos %>% filter(Sind == "SIAPAR") %>% droplevels() %>% summary()
cnae.ind.pos %>% filter(Sind == "SIMADEIRA") %>% droplevels() %>% summary()
cnae.ind.pos %>% filter(Sind == "SINDUSCON") %>% droplevels() %>% summary()

#### Análise das variações de setores por nível econômico do sindicato ####

CNAE_data2$NivKSind <- CNAE_data2$Sindicato %>% 
gsub(pattern = paste0("^", sind.a, "$", collapse = "|"), replacement = "A", x =  .) %>% 
  gsub(pattern = paste0("^", sind.b, "$", collapse = "|"), replacement = "B", x =  .) %>% 
  gsub(pattern = paste0("^", sind.c, "$", collapse = "|"), replacement = "C", x =  .) %>% 
  gsub(pattern = paste0("^", sind.d, "$", collapse = "|"), replacement = "D", x =  .) %>% as.factor()

sind.nivk.transf <- ggplot(data = CNAE_data2) +
  geom_bar(mapping = aes(x = NivKSind, fill = Transf), position = "fill")

sind.nivk.const <- ggplot(data = CNAE_data2) +
  geom_bar(mapping = aes(x = NivKSind, fill = Constr), position = "fill")

sind.nivk.comerc <- ggplot(data = CNAE_data2) +
  geom_bar(mapping = aes(x = NivKSind, fill = Comerc), position = "fill")

sind.nivk.serv <- ggplot(data = CNAE_data2) +
  geom_bar(mapping = aes(x = NivKSind, fill = Serv), position = "fill")

sind.nivk.agric <- ggplot(data = CNAE_data2) +
  geom_bar(mapping = aes(x = NivKSind, fill = Agric), position = "fill")

# GRÁFICO 9
grid.arrange(sind.nivk.transf, sind.nivk.const, sind.nivk.comerc, sind.nivk.serv, ncol = 2)
