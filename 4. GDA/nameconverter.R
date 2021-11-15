# Vamos ter que mudar os nomes manualmente

CNAE_data_teste <- CNAE_data2

row.names(All_dir[which(All_dir$Sindicato == sindicatos[1]),])
row.names(CNAE_data_teste[which(CNAE_data_teste$Sindicato == sindicatos[1]),])
names1 <- cbind(row.names(CNAE_data_teste[which(CNAE_data_teste$Sindicato == sindicatos[1]),]), c("ARGJ", "JL", "RRMM", "SO"))

row.names(All_dir[which(All_dir$Sindicato == sindicatos[2]),])
row.names(CNAE_data_teste[which(CNAE_data_teste$Sindicato == sindicatos[2]),])
names2 <- cbind(row.names(CNAE_data_teste[which(CNAE_data_teste$Sindicato == sindicatos[2]),]), c("ECJ", "FCBG", "JJA", "JAPR",
                                                                                                        "SP", "JAGG"))
row.names(All_dir[which(All_dir$Sindicato == sindicatos[3]),])
row.names(CNAE_data_teste[which(CNAE_data_teste$Sindicato == sindicatos[3]),])
names3 <- cbind(row.names(CNAE_data_teste[which(CNAE_data_teste$Sindicato == sindicatos[3]),]), c("AOS", "CAL", "EB", "EFF", "LSS",
                                                                                                       "MDN", "RAF"))
row.names(All_dir[which(All_dir$Sindicato == sindicatos[4]),])
row.names(CNAE_data_teste[which(CNAE_data_teste$Sindicato == sindicatos[4]),])
names4 <- cbind(row.names(CNAE_data_teste[which(CNAE_data_teste$Sindicato == sindicatos[4]),]), c("BHAG", "JCMP", "JMPC", "RIFL",
                                                                                                  "HB"))
                       
row.names(All_dir[which(All_dir$Sindicato == sindicatos[5]),])
row.names(CNAE_data_teste[which(CNAE_data_teste$Sindicato == sindicatos[5]),])
names5 <- cbind(row.names(CNAE_data_teste[which(CNAE_data_teste$Sindicato == sindicatos[5]),]), c("AJDP", "JVC", "RRN", "DAGS",
                                                                                                  "GBe", "SCZ", "LC", "RM"))

row.names(All_dir[which(All_dir$Sindicato == sindicatos[6]),])
row.names(CNAE_data_teste[which(CNAE_data_teste$Sindicato == sindicatos[6]),])
names6 <- cbind(row.names(CNAE_data_teste[which(CNAE_data_teste$Sindicato == sindicatos[6]),]), c("ABR", "AV", "IDC", "JGGA", "RMJ"))

row.names(All_dir[which(All_dir$Sindicato == sindicatos[7]),])
row.names(CNAE_data_teste[which(CNAE_data_teste$Sindicato == sindicatos[7]),])
names7 <- cbind(row.names(CNAE_data_teste[which(CNAE_data_teste$Sindicato == sindicatos[7]),]), c("AVD", "ASA", "EAM", "HJD", "MAP",
                                                                                                  "MPS"))

row.names(All_dir[which(All_dir$Sindicato == sindicatos[8]),])
row.names(CNAE_data_teste[which(CNAE_data_teste$Sindicato == sindicatos[8]),])
names8 <- cbind(row.names(CNAE_data_teste[which(CNAE_data_teste$Sindicato == sindicatos[8]),]), c("DAG", "EAAZ", "JD", "MPre", "RRo",
                                                                                                  "THo"))

row.names(All_dir[which(All_dir$Sindicato == sindicatos[9]),])
row.names(CNAE_data_teste[which(CNAE_data_teste$Sindicato == sindicatos[9]),])
names9 <- cbind(row.names(CNAE_data_teste[which(CNAE_data_teste$Sindicato == sindicatos[9]),]), c("ADJ", "HU", "II", "VMF"))

row.names(All_dir[which(All_dir$Sindicato == sindicatos[10]),])
row.names(CNAE_data_teste[which(CNAE_data_teste$Sindicato == sindicatos[10]),])
names10 <- cbind(row.names(CNAE_data_teste[which(CNAE_data_teste$Sindicato == sindicatos[10]),]), c("BCM", "Rba", "RZ", "AlSa"))

row.names(All_dir[which(All_dir$Sindicato == sindicatos[11]),])
row.names(CNAE_data_teste[which(CNAE_data_teste$Sindicato == sindicatos[11]),])
names11 <- cbind(row.names(CNAE_data_teste[which(CNAE_data_teste$Sindicato == sindicatos[11]),]), c("APZ", "JLCJ", "VA"))

row.names(All_dir[which(All_dir$Sindicato == sindicatos[12]),])
row.names(CNAE_data_teste[which(CNAE_data_teste$Sindicato == sindicatos[12]),])
names12 <- cbind(row.names(CNAE_data_teste[which(CNAE_data_teste$Sindicato == sindicatos[12]),]), c("DMo", "JPC", "LHB", "CG"))

row.names(All_dir[which(All_dir$Sindicato == sindicatos[13]),])
row.names(CNAE_data_teste[which(CNAE_data_teste$Sindicato == sindicatos[13]),])
names13 <- cbind(row.names(CNAE_data_teste[which(CNAE_data_teste$Sindicato == sindicatos[13]),]), c("DF", "JJS", "SLAB"))

row.names(All_dir[which(All_dir$Sindicato == sindicatos[14]),])
row.names(CNAE_data_teste[which(CNAE_data_teste$Sindicato == sindicatos[14]),])
names14 <- cbind(row.names(CNAE_data_teste[which(CNAE_data_teste$Sindicato == sindicatos[14]),]), c("AST", "CRC", "JCG", "AEF",
                                                                                                    "EEP", "PAF", "RVG"))

row.names(All_dir[which(All_dir$Sindicato == sindicatos[15]),])
row.names(CNAE_data_teste[which(CNAE_data_teste$Sindicato == sindicatos[15]),])
names15 <- cbind(row.names(CNAE_data_teste[which(CNAE_data_teste$Sindicato == sindicatos[15]),]), "Rbi")

row.names(All_dir[which(All_dir$Sindicato == sindicatos[16]),])
row.names(CNAE_data_teste[which(CNAE_data_teste$Sindicato == sindicatos[16]),])
names16 <- cbind(row.names(CNAE_data_teste[which(CNAE_data_teste$Sindicato == sindicatos[16]),]), c("RP", "DM", "COJ"))

row.names(All_dir[which(All_dir$Sindicato == sindicatos[17]),])
row.names(CNAE_data_teste[which(CNAE_data_teste$Sindicato == sindicatos[17]),])
names17 <- cbind(row.names(CNAE_data_teste[which(CNAE_data_teste$Sindicato == sindicatos[17]),]), c("FTV", "GZ", "RMS"))

row.names(All_dir[which(All_dir$Sindicato == sindicatos[18]),])
row.names(CNAE_data_teste[which(CNAE_data_teste$Sindicato == sindicatos[18]),])
names18 <- cbind(row.names(CNAE_data_teste[which(CNAE_data_teste$Sindicato == sindicatos[18]),]), c("ELJF", "GFP", "EJV"))

row.names(All_dir[which(All_dir$Sindicato == sindicatos[19]),])
row.names(CNAE_data_teste[which(CNAE_data_teste$Sindicato == sindicatos[19]),])
names19 <- cbind(row.names(CNAE_data_teste[which(CNAE_data_teste$Sindicato == sindicatos[19]),]), c("GC", "MMB", "MP"))

row.names(All_dir[which(All_dir$Sindicato == sindicatos[20]),])
row.names(CNAE_data_teste[which(CNAE_data_teste$Sindicato == sindicatos[20]),])
names20 <- cbind(row.names(CNAE_data_teste[which(CNAE_data_teste$Sindicato == sindicatos[20]),]), c("ASN", "JT", "PS", "UCB",
                                                                                                    "EJZ"))

row.names(All_dir[which(All_dir$Sindicato == sindicatos[21]),])
row.names(CNAE_data_teste[which(CNAE_data_teste$Sindicato == sindicatos[21]),])
names21 <- cbind(row.names(CNAE_data_teste[which(CNAE_data_teste$Sindicato == sindicatos[21]),]), c("AFO", "DW", "GRak", "JDNi",
                                                                                                    "LAR"))

row.names(All_dir[which(All_dir$Sindicato == sindicatos[22]),])
row.names(CNAE_data_teste[which(CNAE_data_teste$Sindicato == sindicatos[22]),])
names22 <- cbind(row.names(CNAE_data_teste[which(CNAE_data_teste$Sindicato == sindicatos[22]),]), c("ABS", "AFr", "INC", "JCOJ",
                                                                                                    "JPD"))

row.names(All_dir[which(All_dir$Sindicato == sindicatos[23]),])
row.names(CNAE_data_teste[which(CNAE_data_teste$Sindicato == sindicatos[23]),])
names23 <- cbind(row.names(CNAE_data_teste[which(CNAE_data_teste$Sindicato == sindicatos[23]),]), c("JPZ", "LCZ", "NM", "WWL"))

row.names(All_dir[which(All_dir$Sindicato == sindicatos[24]),])
row.names(CNAE_data_teste[which(CNAE_data_teste$Sindicato == sindicatos[24]),])
names24 <- cbind(row.names(CNAE_data_teste[which(CNAE_data_teste$Sindicato == sindicatos[24]),]), c("FB", "Rbe", "WT", "MAGS", "VV"))

row.names(All_dir[which(All_dir$Sindicato == sindicatos[25]),])
row.names(CNAE_data_teste[which(CNAE_data_teste$Sindicato == sindicatos[25]),])
names25 <- cbind(row.names(CNAE_data_teste[which(CNAE_data_teste$Sindicato == sindicatos[25]),]), c("AJG", "DCAB", "EATB", "ICN", "JC", "ER"))

row.names(All_dir[which(All_dir$Sindicato == sindicatos[26]),])
row.names(CNAE_data_teste[which(CNAE_data_teste$Sindicato == sindicatos[26]),])
names26 <- cbind(row.names(CNAE_data_teste[which(CNAE_data_teste$Sindicato == sindicatos[26]),]), c("AAT", "ABC", "CBe", "DJB",
                                                                                                    "GL", "JLR", "NRH"))

row.names(All_dir[which(All_dir$Sindicato == sindicatos[27]),])
row.names(CNAE_data_teste[which(CNAE_data_teste$Sindicato == sindicatos[27]),])
names27 <- cbind(row.names(CNAE_data_teste[which(CNAE_data_teste$Sindicato == sindicatos[27]),]), c("FBJ", "LCF"))

row.names(All_dir[which(All_dir$Sindicato == sindicatos[28]),])
row.names(CNAE_data_teste[which(CNAE_data_teste$Sindicato == sindicatos[28]),])
names28 <- cbind(row.names(CNAE_data_teste[which(CNAE_data_teste$Sindicato == sindicatos[28]),]), c("CEF", "EJJ", "RMar"))
                 
row.names(All_dir[which(All_dir$Sindicato == sindicatos[29]),])
row.names(CNAE_data_teste[which(CNAE_data_teste$Sindicato == sindicatos[29]),])
names29 <- cbind(row.names(CNAE_data_teste[which(CNAE_data_teste$Sindicato == sindicatos[29]),]), c("EK", "RCJ", "SC", "SLM", "WB"))

row.names(All_dir[which(All_dir$Sindicato == sindicatos[29]),])
row.names(CNAE_data_teste[which(CNAE_data_teste$Sindicato == sindicatos[29]),])
names30 <- cbind(row.names(CNAE_data_teste[which(CNAE_data_teste$Sindicato == sindicatos[30]),]), c("BMM", "MS", "VAM", "GLC"))

row.names(All_dir[which(All_dir$Sindicato == sindicatos[30]),])
row.names(CNAE_data_teste[which(CNAE_data_teste$Sindicato == sindicatos[30]),])
names31 <- cbind(row.names(CNAE_data_teste[which(CNAE_data_teste$Sindicato == sindicatos[31]),]), c("DK", "PVC", "PCAlb"))

row.names(All_dir[which(All_dir$Sindicato == sindicatos[31]),])
row.names(CNAE_data_teste[which(CNAE_data_teste$Sindicato == sindicatos[31]),])
names32 <- cbind(row.names(CNAE_data_teste[which(CNAE_data_teste$Sindicato == sindicatos[32]),]), c("DFG", "LBF", "RCG", "ANA"))

row.names(All_dir[which(All_dir$Sindicato == sindicatos[32]),])
row.names(CNAE_data_teste[which(CNAE_data_teste$Sindicato == sindicatos[32]),])
names33 <- cbind(row.names(CNAE_data_teste[which(CNAE_data_teste$Sindicato == sindicatos[33]),]), c("AlvFC", "CAEC", "JCP", "MADM",
                                                                                                    "NBRN", "SLCr", "TCG", "MAVzB"))

row.names(All_dir[which(All_dir$Sindicato == sindicatos[33]),])
row.names(CNAE_data_teste[which(CNAE_data_teste$Sindicato == sindicatos[33]),])
names34 <- cbind(row.names(CNAE_data_teste[which(CNAE_data_teste$Sindicato == sindicatos[34]),]), c("ARF", "ARFo", "ENH", "PABJ", "RLB"))

row.names(All_dir[which(All_dir$Sindicato == sindicatos[34]),])
row.names(CNAE_data_teste[which(CNAE_data_teste$Sindicato == sindicatos[34]),])
names35 <- cbind(row.names(CNAE_data_teste[which(CNAE_data_teste$Sindicato == sindicatos[35]),]), c("CR", "DL", "FWS", "JEN", "RGB", "ACan"))

row.names(All_dir[which(All_dir$Sindicato == sindicatos[35]),])
row.names(CNAE_data_teste[which(CNAE_data_teste$Sindicato == sindicatos[35]),])
names36 <- cbind(row.names(CNAE_data_teste[which(CNAE_data_teste$Sindicato == sindicatos[36]),]), c("JJGsp", "MIM", "SMBC" , "ERPR",
                                                                                                    "MGS"))
row.names(All_dir[which(All_dir$Sindicato == sindicatos[36]),])
row.names(CNAE_data_teste[which(CNAE_data_teste$Sindicato == sindicatos[36]),])
names37 <- cbind(row.names(CNAE_data_teste[which(CNAE_data_teste$Sindicato == sindicatos[37]),]), c("DANF", "RFGK", "SRLS", "VFB"))

names.dict <- ls()[grep("^names[[:digit:]]+", ls())]
names.dict.list <- list()

for (i in 1:length(names.dict)){
names.dict.list[[i]] <-  mget(x = names.dict[i])
names.dict.list[[i]] <- names.dict.list[[i]][[1]]
}

names.dict <- do.call(rbind.data.frame, names.dict.list)
names(names.dict) <- c("CNAE", "Alldir")


# Vamos criar uma função para modificar os nomes de vetores

vec <- row.names(CNAE_data_teste)
vec2 <- vector(length = length(vec), mode = "character")
for (i in 1:length(vec)){
vec2[i]  <- names.dict$Alldir[grep(paste0("^", vec[i], "$"), names.dict$CNAE)]
}

row.names(CNAE_data_teste) <- vec2
