##### ACM DA CNAE #####

#### Construção da função ####
library(rvest)
library(dplyr)
library(stringr)
library(factoextra)
library(FactoMineR)
library(GDAtools)

act.wd <- getwd()

# Vamos criar um df base para colocar os indivíduos já organizados no dataframe
base <- matrix(nrow = 0, ncol = 22)
colnames(base) <- c("Sindicato", "Agric", "Extr", "Transf", "EletrGas", "AgEsgoto", "Constr", "Comerc", "Transp", "Alim", "InfoCom", "Finan", "Imoveis", "AtivProf", "AtivAdm", "AdmPub", "Educ", "SaudeSS", "Entret", "OutrServ", "ServDom", "OrgInt")
base <- as.data.frame(base)
base[, 1:22] <- lapply(base[, 1:22], as.character)

# E então construímos as funções a partir das quais automatizaremos esse processo

an.cont_prep_mca <- function(vec){
  names.vec <- vec
  vec <- rep("y", length(vec))
  names(vec) <- names.vec
  return(vec)
}

# Escrever uma versão alternativa deste (FONTE: https://concla.ibge.gov.br/busca-online-cnae.html)
CNAE.conv <- function(vec){
  vec <- gsub("^01(.*)|^02(.*)|^03(.*)]", "Agric", vec)
  vec <- gsub("^05(.*)|^06(.*)|^07(.*)|^08(.*)|^09(.*)",  "Extr", vec)
  vec <- gsub("^10(.*)|^11(.*)|^12(.*)|^13(.*)|^14(.*)|^15(.*)|^16(.*)|^17(.*)|^18(.*)|^19(.*)|^20(.*)|^21(.*)|^22(.*)|^23(.*)|^24(.*)|^25(.*)|^26(.*)|^27(.*)|^28(.*)|^29(.*)|^30(.*)|^31(.*)|^32(.*)|^33(.*)", "Transf", vec)
  vec <- gsub("^35(.*)", "EletrGas", vec)
  vec <- gsub("^36(.*)|^37(.*)|^38(.*)|^39(.*)", "AgEsgoto", vec)
  vec <- gsub("^41(.*)|^42(.*)|^43(.*)", "Constr", vec)
  vec <- gsub("^45(.*)|^46(.*)|^47(.*)", "Comerc", vec)
  vec <- gsub("^49(.*)|^50(.*)|^51(.*)|^52(.*)|^53(.*)", "Transp", vec)
  vec <- gsub("^55(.*)|^56(.*)", "Alim", vec)
  vec <- gsub("^58(.*)|^59(.*)|^60(.*)|^61(.*)|^62(.*)|^63(.*)", "InfoCom", vec)
  vec <- gsub("^64(.*)|^65(.*)|^66(.*)", "Finan", vec)
  vec <- gsub("^68(.*)", "Imoveis", vec)
  vec <- gsub("^69(.*)|^70(.*)|^71(.*)|^72(.*)|^73(.*)|^74(.*)|^75(.*)", "AtivProf", vec)
  vec <- gsub("^77(.*)|^78(.*)|^79(.*)|^80(.*)|^81(.*)|^82(.*)", "AtivAdm", vec)
  vec <- gsub("^84(.*)", "AdmPub", vec)
  vec <- gsub("^85(.*)", "Educ", vec)
  vec <- gsub("^86(.*)|^87(.*)|^88(.*)", "SaudeSS", vec)
  vec <- gsub("^90(.*)|^91(.*)|^92(.*)|^93(.*)", "Entret", vec)
  vec <- gsub("94(.*)|^95(.*)|^96(.*)", "OutrServ", vec)
  vec <- gsub("^97(.*)", "ServDom", vec)
  vec <- gsub("^99(.*)", "OrgInt", vec)
  return(vec)
}

# Realizar a aplicação da função por pasta de destino divididas pelos sindicatos

CNAE.get_from_direc <- function(sind){
  setwd(paste0(act.wd, "/Consultasocio/", sind))
  files <- list.files(pattern = "html$")
  df <- lapply(files, CNAE.prep_MCA, sind)
  df <- do.call(rbind, df)
  setwd(act.wd)
  return(df)
}

CNAE.prep_MCA <- function(link, sind){
  vec <- CNAE.get_vec(link)
  vec <- an.cont_prep_mca(vec)
  sind <- sind
  names(sind) <- "Sindicato"
  vec <- c(sind, vec)
  df <- bind_rows(base, vec)
  df[is.na(df)] <- "n"
  row.names(df) <- link %>% gsub("[[:punct:]]html", "", x = .)
  return(df)
}

CNAE.get_vec <- function(link){
  page <- read_html(link) %>% html_nodes("#details div div div div div") %>% html_text() 
  vec <- page[grep("^Atividade econômica", page)]
  loc <- regexpr("[[:punct:]]([[:digit:]]+)[[:punct:]]", vec)
  vec <- regmatches(vec, loc)
  vec <- gsub("[[:punct:]]", "", vec)
  vec <- CNAE.conv(vec)
  vec <- unique(vec)
}

CNAE.get_from_direc_mltpg <- function(drg, sind){
  setwd(paste0(act.wd, "/Consultasocio/", sind, "/", drg))
  vec <- list.files(pattern = "html$")
  vec <- lapply(vec, CNAE.get_vec)
  vec <- unlist(vec)
  vec <- unique(vec)
  vec <- an.cont_prep_mca(vec)
  sind <- sind
  names(sind) <- "Sindicato"
  vec <- c(sind, vec)
  df <- bind_rows(base, vec)
  df[is.na(df)] <- "n"
  row.names(df) <- drg
  setwd(act.wd)
  return(df)
}

CNAE.get_from_direc_mltpg_cpf <- function(drg, sind, cpf){
  setwd(paste0(act.wd, "/Consultasocio/", sind, "/CPF/", drg))
  vec <- list.files(pattern = "html$")
  vec <- lapply(vec, CNAE.get_vec_cpf, cpf)
  vec <- unlist(vec)
  vec <- unique(vec)
  vec <- an.cont_prep_mca(vec)
  sind <- sind
  names(sind) <- "Sindicato"
  vec <- c(sind, vec)
  df <- bind_rows(base, vec)
  df[is.na(df)] <- "n"
  row.names(df) <- drg
  setwd(act.wd)
  return(df)
}

# Para fazer uma análise a partir do CPF

CNAE.get_from_direc_cpf <- function(sind, diretor, cpf){
setwd(paste0(act.wd, "/Consultasocio/", sind, "/CPF/", diretor))
page <- list.files(pattern = "html$")
vec <- CNAE.get_vec_cpf(page, cpf)
vec <- an.cont_prep_mca(vec)
sind <- sind
names(sind) <- "Sindicato"
vec <- c(sind, vec)
df <- bind_rows(base, vec)
df[is.na(df)] <- "n"
row.names(df) <- diretor
setwd(act.wd)
return(df)
}

CNAE.get_vec_cpf <- function(page, cpf){
page <- read_html(page) %>% html_nodes("#details div div div div div") %>% html_text()
vec <- page[grep(paste0("CPF(.+)", cpf, "(.+)Data de entrada(.+)Situação Cadastral"), page)]
matches <- regexpr("Atividade econômica(.+?)[[:digit:]][[:punct:]]", vec)
vec <- regmatches(vec, matches)
vec <- gsub("Atividade econômica :", "", vec)
loc <- regexpr("[[:punct:]]([[:digit:]]+)[[:punct:]]", vec)
vec <- regmatches(vec, loc)
vec <- gsub("[[:punct:]]", "", vec)
vec <- CNAE.conv(vec) %>% unique()
return(vec)
}

# Exemplo

CNAE_data_cpf <- rbind(CNAE.get_from_direc_mltpg_cpf("DoMrt", "SINDIAVIPAR", 388509),
                    CNAE.get_from_direc_mltpg_cpf("PCdA", "SINDITRIGO", 943797),
                    CNAE.get_from_direc_cpf("SINDITEXTIL", "GeLuCa", 666489),
                    CNAE.get_from_direc_cpf("SINDICAL", "MPoli", 673509),
                    CNAE.get_from_direc_cpf("SINDUSCON", "MaAvzB", 040049),
                    CNAE.get_from_direc_cpf("SINDIAVIPAR", "CdOJr", 114009),
                    CNAE.get_from_direc_mltpg_cpf("RicMue", "SIMADEIRA", 088769),
                    CNAE.get_from_direc_cpf("SINDIPINTURAS", "RMar", 210149),
                    CNAE.get_from_direc_cpf("SINDILOUCAS", "ER", 600659),
                    CNAE.get_from_direc_cpf("SIMADEIRA", "LC", 803219)
)

###

### Vamos pegar a lista dos sindicatos

sindicatos <- list.dirs(paste0(act.wd, "/Consultasocio"))
sindicatos <- sindicatos[-grep("Consultasocio[[:punct:]].+[[:punct:]]", sindicatos)]
sindicatos <- gsub(paste0(getwd(), "/Consultasocio"), "", sindicatos)
sindicatos <- gsub("[[:punct:]]", "", sindicatos)
sindicatos <- sindicatos[-1]

# Faz a coleta dos dados dos dirigentes com apenas uma página e sem conflito de CPF

CNAE_data <- lapply(sindicatos, CNAE.get_from_direc)
CNAE_data <- do.call(rbind, CNAE_data)

# Fazemos então uma coleta dos dirigentes com mais de uma página do Consultasocio

CNAE.get_from_direc_mltpg_sind <- function(sind){
drg <- list.dirs(path = paste0(act.wd, "/Consultasocio/", sind))
drg <- gsub(paste0("C:(.+)", sind, "/"), "", drg)[2:length(drg)]
df <- lapply(drg, CNAE.get_from_direc_mltpg, sind)
df <- do.call(rbind, df)
return(df)
}

# Coloquei manualmente os sindicatos, mas tem como fazer a pesquisa pelos diretórios
mltpg_sind <- c("SICEPOT", "SIITEP", "SIMADEIRA", "SINAEES", "SINDEMCAP", "SINDIADUBOS", "SINDICAF", "SINDILEITE", "SINDIVEST", "SINPACEL", "SINQFAR")
CNAE_data.mltpg <- lapply(mltpg_sind, CNAE.get_from_direc_mltpg_sind)
CNAE_data.mltpg <- do.call(rbind, CNAE_data.mltpg)
CNAE_data.mltpg <- CNAE_data.mltpg[-grep("^CPF", row.names(CNAE_data.mltpg)),]

CNAE_data <- rbind(CNAE_data, CNAE_data.mltpg, CNAE_data_cpf)
CNAE_data[, 1:22] <- lapply(CNAE_data[, 1:22], as.factor)
summary(CNAE_data)

#### Comparação da distribuição dos serviços por nível de capital ####

CNAE_data$NivKSind <- CNAE_data$Sindicato %>% gsub(pattern = paste0("^", sind.d, "$", collapse = "|"), replacement = "D", x = .) %>% 
  gsub(pattern = paste0("^", sind.c, "$", collapse = "|"), replacement = "C", x = .) %>%
  gsub(pattern = paste0("^", sind.b, "$", collapse = "|"), replacement = "B", x = .) %>% 
  gsub(pattern = paste0("^", sind.a, "$", collapse = "|"), replacement = "A", x = .)
View(CNAE_data)

serv.d <- CNAE_data %>% filter(NivKSind == "D") %>% .[,9:22]
tot.d <- lapply(serv.d[1:ncol(serv.d)], grep, pattern = "y") %>% unlist() %>% length()
tot.d/nrow(serv.d)

count.serv.d <- c(lapply(serv.d[1:ncol(serv.d)], grep, pattern = "y") %>% .[[1]] %>% length()/nrow(serv.d),
                  lapply(serv.d[1:ncol(serv.d)], grep, pattern = "y") %>% .[[2]] %>% length()/nrow(serv.d),
                  lapply(serv.d[1:ncol(serv.d)], grep, pattern = "y") %>% .[[3]] %>% length()/nrow(serv.d),
                  lapply(serv.d[1:ncol(serv.d)], grep, pattern = "y") %>% .[[4]] %>% length()/nrow(serv.d),
                  lapply(serv.d[1:ncol(serv.d)], grep, pattern = "y") %>% .[[5]] %>% length()/nrow(serv.d),
                  lapply(serv.d[1:ncol(serv.d)], grep, pattern = "y") %>% .[[6]] %>% length()/nrow(serv.d),
                  lapply(serv.d[1:ncol(serv.d)], grep, pattern = "y") %>% .[[7]] %>% length()/nrow(serv.d))

names(count.serv.d) <- c(names(serv.d)[1:7])

serv.a <- CNAE_data %>% filter(NivKSind == "A") %>% .[,9:22]
tot.a <- lapply(serv.a[1:ncol(serv.a)], grep, pattern = "y") %>% unlist() %>% length()
tot.a/nrow(serv.a)

count.serv.a <- c(lapply(serv.a[1:ncol(serv.a)], grep, pattern = "y") %>% .[[1]] %>% length()/tot.a,
                  lapply(serv.a[1:ncol(serv.a)], grep, pattern = "y") %>% .[[2]] %>% length()/tot.a,
                  lapply(serv.a[1:ncol(serv.a)], grep, pattern = "y") %>% .[[3]] %>% length()/tot.a,
                  lapply(serv.a[1:ncol(serv.a)], grep, pattern = "y") %>% .[[4]] %>% length()/tot.a,
                  lapply(serv.a[1:ncol(serv.a)], grep, pattern = "y") %>% .[[5]] %>% length()/tot.a,
                  lapply(serv.a[1:ncol(serv.a)], grep, pattern = "y") %>% .[[6]] %>% length()/tot.a,
                  lapply(serv.a[1:ncol(serv.a)], grep, pattern = "y") %>% .[[7]] %>% length()/tot.a)

count.serv.a <- c(lapply(serv.a[1:ncol(serv.a)], grep, pattern = "y") %>% .[[1]] %>% length()/nrow(serv.a),
                  lapply(serv.a[1:ncol(serv.a)], grep, pattern = "y") %>% .[[2]] %>% length()/nrow(serv.a),
                  lapply(serv.a[1:ncol(serv.a)], grep, pattern = "y") %>% .[[3]] %>% length()/nrow(serv.a),
                  lapply(serv.a[1:ncol(serv.a)], grep, pattern = "y") %>% .[[4]] %>% length()/nrow(serv.a),
                  lapply(serv.a[1:ncol(serv.a)], grep, pattern = "y") %>% .[[5]] %>% length()/nrow(serv.a),
                  lapply(serv.a[1:ncol(serv.a)], grep, pattern = "y") %>% .[[6]] %>% length()/nrow(serv.a),
                  lapply(serv.a[1:ncol(serv.a)], grep, pattern = "y") %>% .[[7]] %>% length()/nrow(serv.a))

names(count.serv.a) <- c(names(serv.a)[1:7])

comp.serv.nivk <- data.frame(c(rep("A", length(count.serv.a)), rep("D", length(count.serv.d))),
                             c(names(serv.d)[1:7], names(serv.d)[1:7]),
                             c(count.serv.a, count.serv.d))

names(comp.serv.nivk) <- c("NivK", "Serv", "Porc")
comp.serv.nivk

# GRÁFICO 10
ggplot(comp.serv.nivk, aes(fill=NivK, y=Porc, x=Serv)) + 
  geom_bar(position="dodge", stat="identity")


#### Construção da ACM com a CNAE_data (não impl.) #####
# Como podemos ver, esta análise teria vários problemas, pq teríamos mtas categorias para pouca amostra

prop.table(table(CNAE_data$Extr))*100

# Vamos retirar algumas colunas com pouquíssimos ou nenhum dirigente
CNAE_data <- CNAE_data[,-c(5, 16, 17, 21, 22)]

index <- getindexcat(CNAE_data)
grep("[[:punct:]]n$", index)
cat.n <- index[grep("[[:punct:]]n$", index)]
mca.cnae <- MCA(CNAE_data, quali.sup = 1, graph = FALSE)

eig_cnae <- get_eig(mca.cnae)
fviz_eig(X = mca.cnae, choice = "eigenvalue")

dist_cnae <- get_dist(x = CNAE_data, method = "euclidean")
fviz_dist(dist.obj = dist_cnae)

fviz_mca_var(mca.cnae, repel = TRUE, geom = c("point", "text"), col.var="contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), col.quali.sup = "black", ggtheme = theme_gray ())
fviz_mca_biplot(mca.cnae)

fviz_contrib(mca.cnae, choice = "var", axes = 1, top = 10)
fviz_contrib(mca.cnae, choice = "var", axes = 2, top = 10)

facto_summarize(mca.cnae, "var")