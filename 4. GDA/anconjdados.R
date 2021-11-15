# Análise conjunta de dados
library(tidyverse)
library(FactoMineR)
library(factoextra)

#### Funções auxiliares: IMPORTANTE ####
# É preciso rodar esta função prep_mca para o bind_row funcionar
an.cont_prep_mca <- function(vec){
  vec <- unique(vec)
  names.vec <- vec
  vec <- rep("y", length(vec))
  names(vec) <- names.vec
  return(vec)
}

#### Construção do dataframe das associações dos dirigentes ####

# Manuseio dos dados
perf_dir2 <- read.csv2("PerfDir2.csv", fileEncoding = "utf8")
perf_dir2$Assoc <- gsub("Itaqui Associacao", "Itaqui", perf_dir2$Assoc)
perf_dir2$Sind <- gsub("Ç", "C", perf_dir2$Sind) %>% gsub(pattern = "É", replacement =  "E", x = .)
assocs <- perf_dir2$Assoc %>% strsplit(x = ., split = ",| e ")
assocs <- lapply(X = assocs, FUN = gsub, pattern = " ", replacement = "")

# Importar nodes fiep
nodes_fiep_ent <- read_csv("~/Dissertação/Dados/R/Social Networks/Social Network - Employers Associations/nodes_fiep_ent.csv")
nodes_fiep_ent$Tipo[grep("^AC(.+)|IBRAFE", nodes_fiep_ent$Ator)]  <- "ACom"
nodes_fiep_ent$Tipo[grep("^ACSC$|^AECIC$|Itaqui", nodes_fiep_ent$Ator)] <- "AssocBairro"
nodes_fiep_ent$Ator <- gsub("Itaqui Associacao", "Itaqui", nodes_fiep_ent$Ator)
nodes_fiep_ent$Tipo[grep("^Associacao$", nodes_fiep_ent$Tipo)] <- "AsSet"
nodes_fiep_ent$Tipo[grep("CACIOPAR|FACIAP|CACB", nodes_fiep_ent$Ator)] <- "ACom"
nodes_fiep_ent$Tipo[grep("Conseleite", nodes_fiep_ent$Ator)] <- "AsSet"
nodes_fiep_ent$Tipo[grep("Sindicato", nodes_fiep_ent$Tipo)] <- "SindInd"
nodes_fiep_ent$Tipo[grep("FIEP", nodes_fiep_ent$Ator)] <- "FIEP"
nodes_fiep_ent$Tipo[grep("Sincopecas|SINCAM|Sindiarmazens|FECOMERCIO", nodes_fiep_ent$Ator)] <- "SindCom"
nodes_fiep_ent$Tipo[grep("Sincoopar|OCB|Ocepar", nodes_fiep_ent$Ator)] <- "SindCoop"

assocs <- lapply(X = assocs, FUN = gsub,
                pattern = paste0("^", nodes_fiep_ent$Ator[grep("ACom", nodes_fiep_ent$Tipo)], "$", collapse = "|"),
                ignore.case = T, replacement = "ACom")

assocs <- lapply(X = assocs, FUN = gsub,
                 pattern = paste0("^", nodes_fiep_ent$Ator[grep("AssocBairro", nodes_fiep_ent$Tipo)], "$", collapse = "|"),
                 ignore.case = T, replacement = "AssocBairro")

assocs <- lapply(X = assocs, FUN = gsub,
                 pattern = paste0("^", nodes_fiep_ent$Ator[grep("AsSet", nodes_fiep_ent$Tipo)], "$", collapse = "|"),
                 ignore.case = T, replacement = "AsSet")

assocs <- lapply(X = assocs, FUN = gsub,
                 pattern = paste0("^", nodes_fiep_ent$Ator[grep("SindInd", nodes_fiep_ent$Tipo)], "$", collapse = "|"),
                 ignore.case = T, replacement = "SindInd")

assocs <- lapply(X = assocs, FUN = gsub,
                 pattern = paste0("^", nodes_fiep_ent$Ator[grep("SindCoop", nodes_fiep_ent$Tipo)], "$", collapse = "|"),
                 ignore.case = T, replacement = "SindCoop")

assocs <- lapply(X = assocs, FUN = gsub,
                 pattern = paste0("^", nodes_fiep_ent$Ator[grep("SindCom", nodes_fiep_ent$Tipo)], "$", collapse = "|"),
                 ignore.case = T, replacement = "SindCom")

assocs <- lapply(X = assocs, FUN = gsub,
                 pattern = "SESI|SENAI",
                 ignore.case = T, replacement = "FIEP")

assocs <- lapply(X = assocs, FUN = gsub,
                 pattern = "SESC|SENAC",
                 ignore.case = T, replacement = "SindCom")

names.base.assocs <- assocs %>% unlist() %>% unique()
base.assocs <- matrix(nrow = 0, ncol = length(names.base.assocs))
colnames(base.assocs) <- names.base.assocs
base.assocs <- as.data.frame(base.assocs)
base.assocs[,1:ncol(base.assocs)] <- lapply(base.assocs[,1:ncol(base.assocs)], as.character)

assocs <- lapply(X = assocs, FUN = an.cont_prep_mca)

assocs <- bind_rows(base.assocs, assocs)
assocs <- assocs[,-c(4, 8)] %>% as.data.frame()
assocs[is.na(assocs)] <- "n"
assocs[,1:ncol(assocs)] <- lapply(assocs[,1:ncol(assocs)], as.factor)
row.names(assocs) <- perf_dir2$Dir
assocs$NvKSind <- perf_dir2$Sind

# Agora juntamos as associações com a atuação econômica destes dirigentes
assocs <- merge(assocs, CNAE_data_teste[,c(1:6)], by = 0)

row.names(assocs) <- assocs$Row.names
assocs <- assocs[-1]
assocs$NvKSind <- gsub(pattern = paste0("^", sind.d, "$", collapse = "|"), replacement = "D", x = assocs$NvKSind) %>% 
  gsub(pattern = paste0("^", sind.c, "$", collapse = "|"), replacement =  "C", x = .) %>% 
  gsub(pattern = paste0("^", sind.b, "$", collapse = "|"), replacement =  "B", x = .) %>% 
  gsub(pattern = paste0("^", sind.a, "$", collapse = "|"), replacement =  "A", x = .)
assocs[,1:ncol(assocs)] <- lapply(assocs[,1:ncol(assocs)], as.factor)

# Agora passamos para a construção do dataframe dos cursos destes dirigentes

# Ler o arquivo
perf_dir <- read.csv("PerfDir.csv", fileEncoding = "utf8")

# Arrumar as variáveis
perf_dir$Esc <- gsub("Pós-Graduação", "PGrad", perf_dir$Esc) %>% gsub("Graduação", "Grad", x = .) %>% 
  gsub("N/A", "SInf", x = .) %>% as.factor()

perf_dir$Func <- gsub("Sócio-proprietário", "SP", perf_dir$Func) %>% gsub("Administrador e SP", "Adm+SP", x = .) %>% 
  gsub("Administrador|Não-Proprietário|MEI", "MEI-NProp", x = .)

perf_dir$NivUn <- paste0("NivUn_", perf_dir$NivUn) %>% ifelse(. == "NivUn_", "NivUn_NA", .) %>% as.factor()

perf_dir$LigFIEP <- gsub("Não possui ligação", "LigFIEP_n", perf_dir$LigFIEP) %>% gsub("Possui Ligação", "LigFIEP_s", x = .) %>% 
  as.factor()

perf_dir$AmbMEnt <- abbreviate(perf_dir$AmbMEnt) %>% as.factor()

perf_dir$nLigFac <- cut(x = perf_dir$nLig, breaks = c(0, 1, 2, max(perf_dir$nLig)), labels = c("nLig1", "nLig2", "nLig3+"), ordered_result = T)

perf_dir$Sind <- gsub("Ç", "C", perf_dir$Sind)

row.names(perf_dir) <- perf_dir$Dir

cursos.grad <- perf_dir$CursG %>% strsplit(x = ., split = ",| e ") %>%  lapply(X = ., FUN = gsub, pattern = " ", replacement = "") %>% 
  lapply(X = ., FUN = gsub, pattern = "Mark|GestEmp", replacement = "Admin") %>% lapply(X = ., FUN = gsub, pattern = "Qu.+", replacement = "Quim") %>% 
  lapply(X = ., FUN = gsub, pattern = "N/A", replacement = "")
cursos.pgrad <- perf_dir$CursPG %>% strsplit(x = ., split = ",| e ") %>%  lapply(X = ., FUN = gsub, pattern = " ", replacement = "") %>%
  lapply(X = ., FUN = gsub, pattern = "Mark|GestEmp|GestEst|Agribus", replacement = "Admin") %>% lapply(X = ., FUN = gsub, pattern = "Qu.+", replacement = "Quim") %>% 
  lapply(X = ., FUN = gsub, pattern = "Finan", replacement = "Econ") %>% lapply(X = ., FUN = gsub, pattern = "N/A", replacement = "") %>% lapply(X = ., FUN = unique)

cursos.tot.lst <- list(cursos.grad, cursos.pgrad)
vec.cursos <- list()
for (i in 1:81){
  vec.cursos[[i]] <- c(cursos.tot.lst[[1]][[i]], cursos.tot.lst[[2]][[i]]) %>% unique()
}

vec.cursos
names.base.cursos <- vec.cursos %>% unlist() %>% unique()
base.cursos <- matrix(nrow = 0, ncol = length(names.base.cursos))
colnames(base.cursos) <- names.base.cursos
base.cursos <- as.data.frame(base.cursos)
base.cursos <- base.cursos[-2]
base.cursos[,1:ncol(base.cursos)] <- lapply(base.cursos[,1:ncol(base.cursos)], as.character)

vec.cursos <- lapply(X = vec.cursos, FUN = an.cont_prep_mca)

cursos.tot <- bind_rows(base.cursos, vec.cursos)
cursos.tot <- cursos.tot[,-ncol(cursos.tot)]
cursos.tot[is.na(cursos.tot)] <- "n"
cursos.tot[,1:ncol(cursos.tot)] <- lapply(cursos.tot[,1:ncol(cursos.tot)], as.factor)
row.names(cursos.tot) <- perf_dir$Dir

cursos.tot$Rural <- ifelse(cursos.tot$Agron == "y" | cursos.tot$Veter == "y",
                           "y", "n")

cursos.tot$Outros <- ifelse(cursos.tot$Fisica == "y" | cursos.tot$EngComp == "y" |
                              cursos.tot$Arquit == "y" | cursos.tot$Design == "y",
                            "y", "n")

cursos.tot$EngQuimQuim <- ifelse(cursos.tot$EngQuim == "y" | cursos.tot$Quim == "y",
                                 "y", "n")

cursos.tot$EngEletMec <- ifelse(cursos.tot$EngMec == "y" | cursos.tot$EngElet == "y",
                                "y", "n")

cursos.tot2 <- cursos.tot[ , c(1:4, 6, 16:ncol(cursos.tot))]
cursos.tot2[,1:ncol(cursos.tot2)] <- lapply(cursos.tot2[,1:ncol(cursos.tot2)], as.factor)

perf.assocs <- merge(perf_dir[,c(3, 7)], assocs, by = 0)
row.names(perf.assocs) <- perf.assocs$Row.names
perf.assocs <- perf.assocs[-1]

cursos_assocs <- merge(perf.assocs, cursos.tot2, by = 0)
row.names(cursos_assocs) <- cursos_assocs$Row.names
cursos_assocs <- cursos_assocs[-1]
# Nosso dataframe para a construção da ACMs será o cursos_assocs

#### Construção das ACMs 3 e 4 ####

mca.cursos.assocs <- MCA(cursos_assocs, quali.sup = 3:8, graph = F)
disj.table.cursos.assocs <- tab.disjonctif.prop(cursos_assocs) %>% as.data.frame()
var.sel.cursos_assocs <- names(disj.table.cursos.assocs)[grep("y$", names(disj.table.cursos.assocs))]

# ACM 4
fviz_mca_var(mca.cursos.assocs, repel = TRUE, axes = c(3,2), 
             geom.var = c("point", "text"), 
             alpha.var = 1, col.var="contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             col.quali.sup = "darkgreen", arrows = TRUE,
             select.var = list(name = c("Grad", "PGrad", "SInf", "Adm+SP", "SP", "MEI-NProp", "A", "B", "C", "D", "Agric_y", "Ext_y", "Transf_n",
                                        "Transf_y", "Constr_n", "Constr_y", "Serv_y", "Serv_n", "Comerc_y", "Comerc_n", "EngCiv_y",
                                        "Econ_y", "Dir_y", "Admin_y", "Contab_y", "Rural_y", "Outros_y", "EngQuimQuim_y", "EngEletMec_y",
                                        "AsSet_y", "FIEP_y", "AssocBairro_y", "ACom_y", "SindCoop_y", "SindCom_y")),
             title = "ACM 4: Espaço do complexo associativo da indústria de Curitiba por meio das características de seus dirigentes, dimensões 3 e 2",
             ggtheme = theme_gray ()) + geom_label(data = notas_3e2, aes(label = notas), colour = "black", size = 5)

# ACM 3
fviz_mca_var(mca.cursos.assocs, repel = TRUE, axes = c(1, 2),
             geom.var = c("point", "text"),
             alpha.var = 1, col.var="contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             col.quali.sup = "darkgreen",
             select.var = list(name = c("Grad", "PGrad", "SInf", "Adm+SP", "SP", "MEI-NProp", "A", "B", "C", "D", "Agric_y", "Ext_y", "Transf_n",
                                        "Transf_y", "Constr_n", "Constr_y", "Serv_y", "Serv_n", "Comerc_y", "Comerc_n", "EngCiv_y",
                                        "Econ_y", "Dir_y", "Admin_y", "Contab_y", "Rural_y", "Outros_y", "EngQuimQuim_y", "EngEletMec_y",
                                        "AsSet_y", "FIEP_y", "AssocBairro_y", "ACom_y", "SindCoop_y", "SindCom_y")),
             title = "ACM 3: Espaço do complexo associativo da indústria de Curitiba por meio das características de seus dirigentes, dimensões 1 e 2",
             ggtheme = theme_gray ()) + geom_label(data = notas_1e2, aes(label = notas), colour = "black", size = 5)

# GRÁFICO 11
fviz_contrib(mca.cursos.assocs, "var", 1, top = 10) # Oposição entre Construção e Administração

# GRÁFICO 12
fviz_contrib(mca.cursos.assocs, "var", 2, top = 10) # Diversificação setorial (nível de capital)

# GRÁFICO 13
fviz_contrib(mca.cursos.assocs, "var", 3, top = 10) # Oposição entre urbano especializado (no estrato C, com outras engenharias não-civis e outros cursos) e rural diversificado

# Representação das categorias
fviz_cos2(mca.cursos.assocs, choice = "var", axes = 1:2) # Deixa a desejar na representação dos setores rurais
fviz_cos2(mca.cursos.assocs, choice = "var", axes = 2:3) # Este deixa a desejar na representação da construção
fviz_cos2(mca.cursos.assocs, choice = "var", axes = 1:3) # O que mais dá conta do conjunto das variáveis, mas perde um pouco
fviz_mca_var(mca.cursos.assocs, "mca.cor")               ## na representação do nível econômico

# Divisões de ligações com a FIEP a partir dos cursos dos dirigentes
prop.table(table(cursos_assocs$FIEP, cursos_assocs$Admin), 2)
prop.table(table(cursos_assocs$FIEP, cursos_assocs$Dir), 2)
prop.table(table(cursos_assocs$FIEP, cursos_assocs$Econ), 2)
prop.table(table(cursos_assocs$FIEP, cursos_assocs$Contab), 2)
prop.table(table(cursos_assocs$FIEP, cursos_assocs$EngCiv), 2)
prop.table(table(cursos_assocs$FIEP, cursos_assocs$Rural), 2)
prop.table(table(cursos_assocs$FIEP, cursos_assocs$EngEletMec), 2)
prop.table(table(cursos_assocs$FIEP, cursos_assocs$EngQuimQuim), 2)
prop.table(table(cursos_assocs$FIEP, cursos_assocs$Outros), 2)

#### Cálculos das distâncias das associações com a CNAE ####
dist.fiep.x <- abs(mca.assocs$quali.sup$coord[3,1])+abs(mca.assocs$quali.sup$coord[4,1])*10
dist.fiep.y <- abs(mca.assocs$quali.sup$coord[3,2])+abs(mca.assocs$quali.sup$coord[4,2])*10
((dist.fiep.x^2+dist.fiep.y^2)^1/2)/10 # Essa distância é significativa, mas não tanto

dist.set.x <- abs(mca.assocs$quali.sup$coord[1,1])+abs(mca.assocs$quali.sup$coord[2,1])*10
dist.set.y <- abs(mca.assocs$quali.sup$coord[1,2])+abs(mca.assocs$quali.sup$coord[2,2])*10
((dist.set.x^2+dist.set.y^2)^1/2)/10 # Essa distância não é tão significativa

dist.acom.x <- abs(mca.assocs$quali.sup$coord[3,1])+abs(mca.assocs$quali.sup$coord[4,1])*10
dist.acom.y <- abs(mca.assocs$quali.sup$coord[3,2])+abs(mca.assocs$quali.sup$coord[4,2])*10
((dist.acom.x^2+dist.acom.y^2)^1/2)/10 # Essa distância não é tão significativa

dist.out.x <- abs(mca.assocs$quali.sup$coord[7,1])+abs(mca.assocs$quali.sup$coord[8,1])*10
dist.out.y <- abs(mca.assocs$quali.sup$coord[7,2])+abs(mca.assocs$quali.sup$coord[8,2])*10
((dist.out.x^2+dist.out.y^2)^1/2)/10 # Essa distância é bastante significativa

# Cálculos das distâncias das associações com a CNAE e formação
dist.fiep.x2 <- abs(mca.cursos.assocs$quali.sup$coord[3,1])+abs(mca.cursos.assocs$quali.sup$coord[4,1])*10
dist.fiep.y2 <- abs(mca.cursos.assocs$quali.sup$coord[3,2])+abs(mca.cursos.assocs$quali.sup$coord[4,2])*10
((dist.fiep.x2^2+dist.fiep.y2^2)^1/2)/10 # Essa distância não é significativa
