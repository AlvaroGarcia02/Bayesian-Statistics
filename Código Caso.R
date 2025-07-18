#==============================================================================
#===                 CÓDIGO R PARA EL CASO DE ESTUDIO                       ===
#==============================================================================
#=== Estadística Bayesiana - D. Santiago Bautista S. y Álvaro A. García G.  ===
#===                              2025-1                                    ===
#==============================================================================

rm(list = ls())

#==============================================================================
# Creación de la base de datos
#==============================================================================

library(readxl)
library(dplyr)
# (Todos los archivos, excepto el Panel de Educación y el Panel de Conflicto y
# Violencia, se cargan tal cual aparecen en el Dropbox del Caso de estudio.
# Debe especificarse la ruta desde la cual se cargan los archivos. La ruta que
# aparece en las siguientes líneas de código corresponde a la del computador en
# el cual fue creado este archivo de código).

Examen <- read.table("Examen_Saber_11_2022_2_Entrenamiento.txt",
                     header = TRUE, sep = ";", dec = ".")

Base <- Examen[ , c("cole_cod_depto_ubicacion", "cole_cod_mcpio_ubicacion",
                    "estu_depto_reside", "estu_mcpio_reside",
                    "estu_cod_reside_depto", "estu_cod_reside_mcpio",
                    "estu_nacionalidad", "estu_pais_reside",
                    "estu_tieneetnia", "fami_educacionmadre",
                    "fami_educacionpadre", "fami_estratovivienda",
                    "fami_numlibros", "fami_tienecomputador",
                    "fami_tieneinternet", "punt_global")]
rm(Examen)

Base <- Base %>%
  filter(
    estu_nacionalidad == "COLOMBIA",
    estu_pais_reside == "COLOMBIA",   
    cole_cod_depto_ubicacion != 88, 
    !if_any(everything(), is.na) 
    )
Base <- Base[ , c("cole_cod_depto_ubicacion", "cole_cod_mcpio_ubicacion",
                  "estu_depto_reside", "estu_mcpio_reside",
                    "estu_cod_reside_depto", "estu_cod_reside_mcpio",
                    "estu_tieneetnia", "fami_educacionmadre",
                    "fami_educacionpadre", "fami_estratovivienda",
                    "fami_numlibros", "fami_tienecomputador",
                    "fami_tieneinternet", "punt_global")]

fami_educacionmadre_bin <- ifelse(Base$fami_educacionmadre ==
                                    "Educación profesional completa" |
                                    Base$fami_educacionmadre == "Postgrado", 1, 0)
Base$fami_educacionmadre <- fami_educacionmadre_bin

fami_educacionpadre_bin <- ifelse(Base$fami_educacionpadre ==
                                    "Educación profesional completa" |
                                    Base$fami_educacionpadre == "Postgrado", 1, 0)
Base$fami_educacionpadre <- fami_educacionpadre_bin

Panel.Carac.Gen <- read_excel("C:/Users/USER/OneDrive/Documentos/LaTeX/b34 Caso de estudio - EB - 2025-1/Insumos para la base de datos/PANEL_CARACTERISTICAS_GENERALES(2022).xlsx")
Panel.Carac.Gen.nbi <- Panel.Carac.Gen[!is.na(Panel.Carac.Gen$nbi),
                                       c("codmpio", "ano", "nbi")]
rm(Panel.Carac.Gen)
Panel.Carac.Gen.nbi <- Panel.Carac.Gen.nbi %>% 
  group_by(codmpio) %>%
  slice(which.max(ano))
Panel.Carac.Gen.nbi <- Panel.Carac.Gen.nbi[ , c("codmpio", "nbi")]
colnames(Panel.Carac.Gen.nbi) <- c("estu_cod_reside_mcpio", "nbi")
Base <- merge(Base, Panel.Carac.Gen.nbi, by = "estu_cod_reside_mcpio")
rm(Panel.Carac.Gen.nbi)
dim(Base)

# (Se recomienda crear un nuevo archivo de Excel para el Panel de Educación
# que contenga únicamente las columnas codmpio, ano, docen_total y alumn_total,
# con el fin de no forzar la memoria del computador al ser el archivo original
# un documento de Excel con demasiadas columnas, las cuales no serán consultadas
# para el presente trabajo. De todas formas, se puede correr el archivo original
# en caso de trabajar con un ordenador con buena memoria RAM).
Panel.Educ <- read_excel("C:/Users/USER/OneDrive/Documentos/LaTeX/b34 Caso de estudio - EB - 2025-1/Insumos para la base de datos/PANEL_DE_EDUCACION(2022)_Modificado.xlsx")
Panel.Educ.doc.alu <- Panel.Educ[!is.na(Panel.Educ$docen_total), ]
rm(Panel.Educ)
Panel.Educ.doc.alu <- Panel.Educ.doc.alu %>% 
  group_by(codmpio) %>%
  slice(which.max(ano))
Panel.Educ.doc.alu$doc_por_est <- Panel.Educ.doc.alu$docen_total/Panel.Educ.doc.alu$alumn_total
Panel.Educ.doc.alu <- Panel.Educ.doc.alu[c("codmpio", "doc_por_est")]
colnames(Panel.Educ.doc.alu) <- c("estu_cod_reside_mcpio", "doc_por_est")
Base <- merge(Base, Panel.Educ.doc.alu, by = "estu_cod_reside_mcpio")
rm(Panel.Educ.doc.alu)
dim(Base)

LAFT.FT <- read_excel("C:/Users/USER/OneDrive/Documentos/LaTeX/b34 Caso de estudio - EB - 2025-1/Insumos para la base de datos/LAFT.xlsx",
                      sheet = "FT")
estu_cod_reside_mcpio <- c(05001, 05002, 05004, 05021, 05030, 05031, 05034,
                           05036, 05038, 05040, 05042, 05044, 05045, 05051, 
                           05055, 05059, 05079, 05086, 05088, 05091, 05093,
                           05101, 05107, 05113, 05120, 05125, 05129, 05134,
                           05138, 05142, 05145, 05147, 05148, 05150, 05154,
                           05172, 05190, 05197, 05206, 05209, 05212, 05234,
                           05237, 05240, 05250, 05264, 05266, 05282, 05284,
                           05306, 05308, 05310, 05313, 05315, 05318, 05321,
                           05347, 05353, 05360, 05361, 05364, 05368, 05376,
                           05380, 05390, 05400, 05411, 05425, 05440, 05467,
                           05475, 05480, 05483, 05490, 05495, 05501, 05541,
                           05543, 05576, 05579, 05585, 05591, 05604, 05607,
                           05615, 05628, 05631, 05642, 05647, 05649, 05652,
                           05656, 05658, 05659, 05660, 05664, 05665, 05667,
                           05670, 05674, 05679, 05686, 05690, 05697, 05736,
                           05756, 05761, 05789, 05790, 05792, 05809, 05819,
                           05837, 05842, 05847, 05854, 05856, 05858, 05861,
                           05873, 05885, 05887, 05890, 05893, 05895, 08001,
                           08078, 08137, 08141, 08296, 08372, 08421, 08433,
                           08436, 08520, 08549, 08558, 08560, 08573, 08606,
                           08634, 08638, 08675, 08685, 08758, 08770, 08832,
                           08849, 11001, 13001, 13006, 13030, 13042, 13052,
                           13062, 13074, 13140, 13160, 13188, 13212, 13222,
                           13244, 13248, 13268, 13300, 13430, 13433, 13440,
                           13442, 13458, 13468, 13473, 13490, 13549, 13580,
                           13600, 13620, 13647, 13650, 13654, 13655, 13657,
                           13667, 13670, 13673, 13683, 13688, 13744, 13760,
                           13780, 13810, 13836, 13838, 13873, 13894, 15001,
                           15022, 15047, 15051, 15087, 15090, 15092, 15097,
                           15104, 15106, 15109, 15114, 15131, 15135, 15162,
                           15172, 15176, 15180, 15183, 15185, 15187, 15189,
                           15204, 15212, 15215, 15218, 15223, 15224, 15226,
                           15232, 15236, 15238, 15244, 15248, 15272, 15276,
                           15293, 15296, 15299, 15317, 15322, 15325, 15332,
                           15362, 15367, 15368, 15377, 15380, 15401, 15403,
                           15407, 15425, 15442, 15455, 15464, 15466, 15469,
                           15476, 15480, 15491, 15494, 15500, 15507, 15511,
                           15514, 15516, 15518, 15522, 15531, 15533, 15537,
                           15542, 15550, 15572, 15580, 15599, 15600, 15621,
                           15632, 15638, 15646, 15660, 15664, 15667, 15673,
                           15676, 15681, 15686, 15690, 15693, 15696, 15720,
                           15723, 15740, 15753, 15755, 15757, 15759, 15761,
                           15762, 15763, 15764, 15774, 15776, 15778, 15790,
                           15798, 15804, 15806, 15808, 15810, 15814, 15816,
                           15820, 15822, 15832, 15835, 15837, 15839, 15842,
                           15861, 15879, 15897, 17001, 17013, 17042, 17050,
                           17088, 17174, 17272, 17380, 17388, 17433, 17442,
                           17444, 17446, 17486, 17495, 17513, 17524, 17541,
                           17614, 17616, 17653, 17662, 17665, 17777, 17867,
                           17873, 17877, 18001, 18029, 18094, 18150, 18205,
                           18247, 18256, 18410, 18460, 18479, 18592, 18610,
                           18753, 18756, 18785, 18860, 19001, 19022, 19050,
                           19075, 19100, 19110, 19130, 19137, 19142, 19212,
                           19256, 19290, 19300, 19318, 19355, 19364, 19392,
                           19397, 19418, 19450, 19455, 19473, 19513, 19517,
                           19532, 19533, 19548, 19573, 19585, 19622, 19693,
                           19698, 19701, 19743, 19760, 19780, 19785, 19807,
                           19809, 19821, 19824, 19845, 20001, 20011, 20013,
                           20032, 20045, 20060, 20175, 20178, 20228, 20238,
                           20250, 20295, 20310, 20383, 20400, 20443, 20517,
                           20550, 20570, 20614, 20621, 20710, 20750, 20770,
                           20787, 23001, 23068, 23079, 23090, 23162, 23168,
                           23182, 23189, 23300, 23350, 23417, 23419, 23464,
                           23466, 23500, 23555, 23570, 23574, 23580, 23586,
                           23660, 23670, 23672, 23675, 23678, 23682, 23686,
                           23807, 23856, 23855, 25001, 25019, 25035, 25040,
                           25053, 25086, 25095, 25099, 25120, 25123, 25126,
                           25148, 25151, 25154, 25168, 25175, 25178, 25181,
                           25183, 25200, 25214, 25224, 25245, 25258, 25260,
                           25269, 25279, 25281, 25286, 25288, 25290, 25293,
                           25295, 25297, 25299, 25307, 25312, 25317, 25320,
                           25322, 25324, 25326, 25328, 25335, 25339, 25368,
                           25372, 25377, 25386, 25394, 25398, 25402, 25407,
                           25426, 25430, 25436, 25438, 25473, 25483, 25486,
                           25488, 25489, 25491, 25506, 25513, 25518, 25524,
                           25530, 25535, 25572, 25580, 25592, 25594, 25596,
                           25599, 25612, 25645, 25649, 25653, 25658, 25662,
                           25718, 25736, 25740, 25743, 25745, 25754, 25758,
                           25769, 25772, 25777, 25779, 25781, 25785, 25793,
                           25797, 25799, 25805, 25807, 25815, 25817, 25823,
                           25839, 25841, 25843, 25845, 25851, 25862, 25867,
                           25871, 25873, 25875, 25878, 25885, 25898, 25899,
                           27001, 27006, 27025, 27050, 27073, 27075, 27077,
                           27099, 27135, 27150, 27160, 27205, 27245, 27250,
                           27361, 27372, 27413, 27425, 27430, 27450, 27491,
                           27150, 27495, 27580, 27600, 27615, 27660, 27745,
                           27787, 27800, 27810, 41001, 41006, 41013, 41016,
                           41020, 41026, 41078, 41132, 41206, 41244, 41298,
                           41306, 41319, 41349, 41357, 41359, 41378, 41396,
                           41483, 41503, 41518, 41524, 41530, 41548, 41551,
                           41615, 41660, 41668, 41676, 41770, 41791, 41797,
                           41799, 41801, 41807, 41872, 41885, 44001, 44035,
                           44078, 44090, 44098, 44110, 44279, 44378, 44420,
                           44430, 44560, 44650, 44847, 44855, 44874, 47001,
                           47030, 47053, 47058, 47161, 47170, 47189, 47205,
                           47245, 47258, 47268, 47288, 47318, 47460, 47541,
                           47545, 47551, 47555, 47570, 47605, 47660, 47675,
                           47692, 47703, 47707, 47720, 47745, 47798, 47960,
                           47980, 50001, 50006, 50110, 50124, 50150, 50223,
                           50226, 50245, 50251, 50270, 50287, 50313, 50318,
                           50325, 50330, 50350, 50370, 50400, 50450, 50568,
                           50573, 50577, 50590, 50606, 50680, 50683, 50686,
                           50689, 50711, 52001, 52019, 52022, 52036, 52051,
                           52079, 52083, 52110, 52203, 52207, 52210, 52215,
                           52224, 52227, 52233, 52240, 52250, 52254, 52256,
                           52258, 52260, 52287, 52317, 52320, 52323, 52352,
                           52354, 52356, 52378, 52381, 52385, 52390, 52399,
                           52405, 52411, 52418, 52427, 52435, 52473, 52480,
                           52490, 52506, 52520, 52540, 52560, 52565, 52573,
                           52585, 52612, 52621, 52678, 52683, 52685, 52687,
                           52693, 52694, 52696, 52699, 52720, 52786, 52788,
                           52835, 52838, 52885, 54001, 54003, 54051, 54099,
                           54109, 54125, 54128, 54172, 54174, 54206, 54223,
                           54239, 54245, 54250, 54261, 54313, 54344, 54347,
                           54377, 54385, 54398, 54405, 54418, 54480, 54498,
                           54518, 54520, 54553, 54599, 54660, 54670, 54673,
                           54680, 54720, 54743, 54800, 54810, 54820, 54871,
                           54874, 63001, 63111, 63130, 63190, 63212, 63272,
                           63302, 63401, 63470, 63548, 63594, 63690, 66001,
                           66045, 66075, 66088, 66170, 66318, 66383, 66400,
                           66440, 66456, 66572, 66594, 66682, 66687, 68001,
                           68013, 68020, 68051, 68077, 68079, 68081, 68092,
                           68101, 68121, 68132, 68147, 68152, 68160, 68162,
                           68167, 68169, 68176, 68179, 68190, 68207, 68209,
                           68211, 68217, 68229, 68235, 68245, 68250, 68255,
                           68264, 68266, 68271, 68276, 68296, 68298, 68307,
                           68318, 68320, 68322, 68324, 68327, 68344, 68368,
                           68370, 68377, 68385, 68397, 68406, 68418, 68425,
                           68432, 68444, 68464, 68468, 68498, 68500, 68502,
                           68522, 68524, 68533, 68547, 68549, 68572, 68573,
                           68575, 68615, 68655, 68669, 68673, 68679, 68682,
                           68684, 68686, 68689, 68705, 68720, 68745, 68755,
                           68770, 68773, 68780, 68820, 68855, 68861, 68867,
                           68872, 68895, 70001, 70110, 70124, 70204, 70215,
                           70221, 70230, 70233, 70235, 70265, 70400, 70418,
                           70429, 70473, 70508, 70523, 70670, 70678, 70702,
                           70708, 70713, 70717, 70742, 70771, 70820, 70823,
                           73001, 73024, 73026, 73030, 73043, 73055, 73067,
                           73124, 73148, 73152, 73168, 73200, 73217, 73226,
                           73236, 73268, 73270, 73275, 73283, 73319, 73347,
                           73349, 73352, 73408, 73411, 73443, 73449, 73461,
                           73483, 73504, 73520, 73547, 73555, 73563, 73585,
                           73616, 73622, 73624, 73671, 73675, 73678, 73686,
                           73770, 73854, 73861, 73870, 73873, 76001, 76020,
                           76036, 76041, 76054, 76100, 76109, 76111, 76113,
                           76122, 76126, 76130, 76147, 76233, 76243, 76246,
                           76248, 76250, 76275, 76306, 76318, 76364, 76377,
                           76400, 76403, 76497, 76520, 76563, 76606, 76616,
                           76622, 76670, 76736, 76823, 76828, 76834, 76845,
                           76863, 76869, 76890, 76892, 76895, 81001, 81065,
                           81220, 81300, 81591, 81736, 81794, 85001, 85010,
                           85015, 85125, 85136, 85139, 85162, 85225, 85230,
                           85250, 85263, 85279, 85300, 85315, 85325, 85400,
                           85410, 85430, 85440, 86001, 86219, 86320, 86568,
                           86569, 86571, 86573, 86749, 86755, 86757, 86760,
                           86865, 86885, 88001, 88564, 91001, 91263, 91405,
                           91407, 91430, 91460, 91530, 91536, 91540, 91669,
                           91798, 94001, 94343, 94883, 94884, 94885, 94886,
                           94887, 94888, 95001, 95015, 95025, 95200, 97001,
                           97161, 97511, 97666, 97777, 97889, 99001, 99524,
                           99624, 99773)
LAFT.FT.RISK <- cbind(LAFT.FT, estu_cod_reside_mcpio)
rm(LAFT.FT)
LAFT.FT.RISK <- LAFT.FT.RISK[ , c("RISK_VICTIM_2022", "estu_cod_reside_mcpio")]
Base <- merge(Base, LAFT.FT.RISK, by = "estu_cod_reside_mcpio")
rm(LAFT.FT.RISK)
dim(Base)

DANE <- read_excel("C:/Users/USER/OneDrive/Documentos/LaTeX/b34 Caso de estudio - EB - 2025-1/Insumos para la base de datos/DANE - PIB.xlsx",
                   sheet = "Cuadro 3")
DANE.PIB <- DANE[-(1:7), ]
rm(DANE)
names(DANE.PIB) <- DANE.PIB[1, ]
DANE.PIB <- DANE.PIB[-c(1:2, 36:40), c("Código Departamento (DIVIPOLA)", "2022")]
DANE.PIB$`Código Departamento (DIVIPOLA)` <- as.numeric(DANE.PIB$`Código Departamento (DIVIPOLA)`)
DANE.PIB$PIB <- DANE.PIB$`2022`/1000000
DANE.PIB <- DANE.PIB[ , c("Código Departamento (DIVIPOLA)", "PIB")]
colnames(DANE.PIB) <- c("estu_cod_reside_depto", "PIB")
Base <- merge(Base, DANE.PIB, by = "estu_cod_reside_depto")
rm(DANE.PIB)
dim(Base)

Panel.Carac.Gen <- read_excel("C:/Users/USER/OneDrive/Documentos/LaTeX/b34 Caso de estudio - EB - 2025-1/Insumos para la base de datos/PANEL_CARACTERISTICAS_GENERALES(2022).xlsx")
Panel.Carac.Gen.Rur <- Panel.Carac.Gen[!is.na(Panel.Carac.Gen$pobl_rur),
                                       c("codmpio", "ano", "pobl_rur", "pobl_tot")]
rm(Panel.Carac.Gen)
Panel.Carac.Gen.Rur <- Panel.Carac.Gen.Rur %>% 
  group_by(codmpio) %>%
  slice(which.max(ano))
Panel.Carac.Gen.Rur$pobl_rur_percent <- (Panel.Carac.Gen.Rur$pobl_rur/Panel.Carac.Gen.Rur$pobl_tot)*100
Panel.Carac.Gen.Rur <- Panel.Carac.Gen.Rur[ , c("codmpio", "pobl_rur_percent")]
colnames(Panel.Carac.Gen.Rur) <- c("estu_cod_reside_mcpio", "pobl_rur_percent")
Base <- merge(Base, Panel.Carac.Gen.Rur, by = "estu_cod_reside_mcpio")
rm(Panel.Carac.Gen.Rur)
dim(Base)

# (Se recomienda crear un nuevo archivo de Excel para el Panel de Conflicto y
# Violencia que contenga únicamente las columnas codmpio, ano, homicidios y
# desplazados_expulsion, con el fin de no forzar la memoria del computador al ser 
# el archivo original un documento de Excel con demasiadas columnas, las cuales 
# no serán consultadas para el presente trabajo. De todas formas, se puede correr 
# el archivo original en caso de trabajar con un ordenador con buena memoria RAM).
Panel.Conf.Viol <- read_excel("C:/Users/USER/OneDrive/Documentos/LaTeX/b34 Caso de estudio - EB - 2025-1/Insumos para la base de datos/PANEL_CONFLICTO_Y_VIOLENCIA(2022)_Modificado.xlsx")
Panel.Conf.Viol.Desp <- Panel.Conf.Viol[!is.na(Panel.Conf.Viol$desplazados_expulsion),
                                        c("codmpio", "ano", "desplazados_expulsion")]
rm(Panel.Conf.Viol)
Panel.Conf.Viol.Desp <- Panel.Conf.Viol.Desp %>% 
  group_by(codmpio) %>%
  slice(which.max(ano))
Panel.Carac.Gen <- read_excel("C:/Users/USER/OneDrive/Documentos/LaTeX/b34 Caso de estudio - EB - 2025-1/Insumos para la base de datos/PANEL_CARACTERISTICAS_GENERALES(2022).xlsx")
Pobl_tot <- Panel.Carac.Gen[!is.na(Panel.Carac.Gen$pobl_tot),
                            c("codmpio", "ano", "pobl_tot")]
rm(Panel.Carac.Gen)
Pobl_tot <- Pobl_tot %>% 
  group_by(codmpio) %>%
  slice(which.max(ano))
desp_forz <- merge(Panel.Conf.Viol.Desp, Pobl_tot, by = "codmpio")
rm(Panel.Conf.Viol.Desp)
rm(Pobl_tot)
estu_cod_reside_depto <- c(rep(5, 125), rep(8, 23), 11, rep(13, 46), rep(15, 122),
                           rep(17, 27), rep(18, 16), rep(19, 42), rep(20, 25),
                           rep(23, 30), rep(25, 115), rep(27, 30), rep(41, 37),
                           rep(44, 15), rep(47, 30), rep(50, 29), rep(52, 64),
                           rep(54, 40), rep(63, 12), rep(66, 14), rep(68, 87),
                           rep(70, 26), rep(73, 47), rep(76, 42), rep(81, 7),
                           rep(85, 19), rep(86, 13), rep(88, 2), rep(91, 11),
                           rep(94, 9), rep(95, 4), rep(97, 6), rep(99, 4))
desp_forz <- cbind(desp_forz, estu_cod_reside_depto)
desp_depto <- aggregate(desp_forz$desplazados_expulsion,
          by = list(estu_cod_reside_depto = desp_forz$estu_cod_reside_depto),
          FUN = sum)
pobl_depto <- aggregate(desp_forz$pobl_tot,
                        by = list(estu_cod_reside_depto = desp_forz$estu_cod_reside_depto),
                        FUN = sum)
desp_forz <- merge(desp_depto, pobl_depto, by = "estu_cod_reside_depto")
rm(desp_depto)
rm(pobl_depto)
desp_forz$tasa_desp_forz <- (desp_forz$x.x/desp_forz$x.y)*100000
desp_forz <- desp_forz[ , c("estu_cod_reside_depto", "tasa_desp_forz")]
Base <- merge(Base, desp_forz, by = "estu_cod_reside_depto")
rm(desp_forz)
dim(Base)

# (Se incluyen datos de MOE (2022) copiados directamente del PDF. No se carga
# ninguna base de datos).
estu_cod_reside_depto <- c(5, 19, 27, 54, 52, 81, 13, 86, 23, 76, 18, 50, 68,
                           20, 8, 95, 70, 73, 44, 47, 85, 66, 41, 94, 99, 11,
                           17, 63, 15, 25, 91, 88, 97)
risk_percent <- c(48.00, 76.19, 90.00, 47.50, 46.88, 100.00, 45.65, 76.92,
                  53.33, 47.62, 93.75, 62.07, 6.90, 64.00, 17.39, 75.00,
                  69.23, 19.15, 60.00, 20.00, 21.05, 28.57, 18.92, 55.56,
                  50.00, 100.00, 3.70, 8.33, 1.63, 1.72, 0.00, 0.00, 0.00)
MOE <- cbind(estu_cod_reside_depto, risk_percent)
Base <- merge(Base, MOE, by = "estu_cod_reside_depto")
rm(MOE)
dim(Base)

# (Se recomienda crear un nuevo archivo de Excel para el Panel de Conflicto y
# Violencia que contenga únicamente las columnas codmpio, ano, homicidios y
# desplazados_expulsion, con el fin de no forzar la memoria del computador al ser 
# el archivo original un documento de Excel con demasiadas columnas, las cuales 
# no serán consultadas para el presente trabajo. De todas formas, se puede correr 
# el archivo original en caso de trabajar con un ordenador con buena memoria RAM).
Panel.Conf.Viol <- read_excel("C:/Users/USER/OneDrive/Documentos/LaTeX/b34 Caso de estudio - EB - 2025-1/Insumos para la base de datos/PANEL_CONFLICTO_Y_VIOLENCIA(2022)_Modificado.xlsx")
Homic.Depto <- Panel.Conf.Viol[-(1:75), c("codmpio", "ano", "homicidios")]
rm(Panel.Conf.Viol)
Homic.Depto <- Homic.Depto[!is.na(Homic.Depto$homicidios), ]
Homic.Depto <- Homic.Depto %>% 
  group_by(codmpio) %>%
  slice(which.max(ano))
Panel.Carac.Gen <- read_excel("C:/Users/USER/OneDrive/Documentos/LaTeX/b34 Caso de estudio - EB - 2025-1/Insumos para la base de datos/PANEL_CARACTERISTICAS_GENERALES(2022).xlsx")
Pobl_tot <- Panel.Carac.Gen[!is.na(Panel.Carac.Gen$pobl_tot), 
                            c("codmpio", "ano", "pobl_tot")]
rm(Panel.Carac.Gen)
Pobl_tot <- Pobl_tot %>% 
  group_by(codmpio) %>%
  slice(which.max(ano))
Pobl_tot <- Pobl_tot[ , c("codmpio", "pobl_tot")]
Homic.Depto <- merge(Homic.Depto, Pobl_tot, by = "codmpio")
rm(Pobl_tot)
estu_cod_reside_depto <- c(rep(5, 125), rep(8, 23), 11, rep(13, 46), rep(15, 123),
                           rep(17, 27), rep(18, 16), rep(19, 42), rep(20, 25),
                           rep(23, 30), rep(25, 116), rep(27, 30), rep(41, 37),
                           rep(44, 15), rep(47, 30), rep(50, 29), rep(52, 64),
                           rep(54, 40), rep(63, 12), rep(66, 14), rep(68, 87),
                           rep(70, 26), rep(73, 47), rep(76, 42), rep(81, 7),
                           rep(85, 19), rep(86, 13), rep(88, 2), rep(91, 11),
                           rep(94, 9), rep(95, 4), rep(97, 6), rep(99, 4))
Homic.Depto$estu_cod_reside_depto <- estu_cod_reside_depto
pobl_tot_by_depto <- aggregate(Homic.Depto$pobl_tot,
                               by = list(estu_cod_reside_depto = Homic.Depto$estu_cod_reside_depto),
                                             FUN = sum)
Homic.Depto <- merge(Homic.Depto, pobl_tot_by_depto, by = "estu_cod_reside_depto")
Homic.Depto$homic_times_pobl <- Homic.Depto$homicidios*Homic.Depto$pobl_tot
prom_dep_homic_sum <- aggregate(Homic.Depto$homic_times_pobl,
                            by = list(estu_cod_reside_depto = Homic.Depto$estu_cod_reside_depto),
                            FUN = sum)
Homic.Depto <- merge(prom_dep_homic_sum, pobl_tot_by_depto, by = "estu_cod_reside_depto")
rm(pobl_tot_by_depto)
prom_dep_homic <- Homic.Depto$x.x/Homic.Depto$x.y
Homic.Depto <- cbind(prom_dep_homic_sum$estu_cod_reside_depto, prom_dep_homic)
rm(prom_dep_homic_sum)
colnames(Homic.Depto) <- c("estu_cod_reside_depto", "prom_dep_homic")
Base <- merge(Base, Homic.Depto, by = "estu_cod_reside_depto")
rm(Homic.Depto)
dim(Base)

length(unique(Base$estu_cod_reside_depto))
length(unique(Base$estu_cod_reside_mcpio))

# (Opcional: Almacenar la base de datos en el computador).
write.table(Base,
            "C:/Users/USER/OneDrive/Documentos/LaTeX/b34 Caso de estudio - EB - 2025-1/Insumos para la base de datos/Base.txt",
            sep = ";", dec = ".", row.names = TRUE)

rm(list = ls())

#==============================================================================
# Puntaje global de la Prueba vs. incidencia de la pobreza monetaria en 2018 y
# vs. cobertura neta en educación secundaria en 2022
#==============================================================================

library(readxl)
library(sf)
library(readr)
library(dplyr)
library(ggplot2)
library(rnaturalearth)
library(stringi)
library(viridis)
library(patchwork)

# Se utiliza la base de datos creada en la sección anterior. Se puede cargar
# desde el computador en caso que ya esté almacenada:
Base <- read.table("C:/Users/USER/OneDrive/Documentos/LaTeX/b34 Caso de estudio - EB - 2025-1/Insumos para la base de datos/Base.txt",
                   header = TRUE, sep = ";", dec = ".")
summary(Base$punt_global)

# Cargar el documento de GeoJSON adjunto para la creación de los mapas por mcpio:
municipios_geo <- st_read("C:/Users/USER/OneDrive/Documentos/LaTeX/b34 Caso de estudio - EB - 2025-1/Insumos para la base de datos/colombia-municipios.json",
                          quiet = TRUE)
municipios_geo$id <- as.numeric(municipios_geo$id)

# El mapa por departamentos es creado mediante la función rnaturalearth:
departamentos_geo <- ne_states(country = "Colombia", returnclass = "sf")
cod_depto <- c(52, 86, 27, 94, 97, 91, 44, 20, 54, 81, 15, 99, 19, 76, 5, 23,
               70, 13, 8, 47, 88, NA, 18, 41, 95, 17, 85, 50, 11, 68, 73, 63, 
               25, 66)
departamentos_geo$cod_depto <- cod_depto

# Medias aritméticas muestrales del puntaje global por departamento y municipio:
punt_global_media_depto <- aggregate(Base$punt_global,
                                     by = list(estu_cod_reside_depto = Base$estu_cod_reside_depto),
                                     FUN = mean)
punt_global_media_mcpio <- aggregate(Base$punt_global,
                                     by = list(estu_cod_reside_mcpio = Base$estu_cod_reside_mcpio),
                                     FUN = mean)
rm(Base)
summary(punt_global_media_depto$x)
punt_global_media_depto[which.min(punt_global_media_depto$x), ]
punt_global_media_depto[which.max(punt_global_media_depto$x), ]
summary(punt_global_media_mcpio$x)
punt_global_media_mcpio[which.min(punt_global_media_mcpio$x), ]
punt_global_media_mcpio[which.max(punt_global_media_mcpio$x), ]

# Código de los mapas de puntaje global:
mapa_punt_global_media_depto <- departamentos_geo %>%
  left_join(punt_global_media_depto, by = c("cod_depto" = "estu_cod_reside_depto"))
rm(punt_global_media_depto)
mapa_punt_global_media_mcpio <- municipios_geo %>%
  left_join(punt_global_media_mcpio, by = c("id" = "estu_cod_reside_mcpio"))
rm(punt_global_media_mcpio)
mcpios_titulos <- tools::toTitleCase(tolower(mapa_punt_global_media_mcpio$name))
mapa_punt_global_media_mcpio$name <- mcpios_titulos
rm(mcpios_titulos)
centroides <- st_centroid(mapa_punt_global_media_mcpio)
coords <- st_coordinates(centroides)
mapa_punt_global_media_mcpio <- mapa_punt_global_media_mcpio %>%
  mutate(X = coords[, 1], Y = coords[, 2])
rm(centroides, coords)

# Generación del mapa de puntajes por departamento:
p1 <- ggplot(mapa_punt_global_media_depto) +
  geom_sf(aes(fill = x), color = "gray60", size = 0.1) +
  xlim(-80, -66) +
  geom_label(
    data = mapa_punt_global_media_depto[-c(11, 14, 19, 21, 22, 26, 29, 32, 34), ],
    mapping = aes(x = longitude, y = latitude, label = woe_name),
    color = "black",
    size = 3,
    label.size = 0.1,
    label.r = grid::unit(0, "lines")
  ) +
  scale_fill_viridis_c(option = "C", na.value = "gray60", direction = -1) +
  theme_minimal() +
  labs(
    title = "",
    x = "Longitud",
    y = "Latitud",
    fill = "Media"
  ); p1
rm(mapa_punt_global_media_depto)

# Generación del mapa de puntajes por municipio:
p2 <- ggplot(mapa_punt_global_media_mcpio) +
  geom_sf(aes(fill = x), color = NA, size = 0.001) +
  xlim(-69, -53) +
  geom_label(
    data = mapa_punt_global_media_mcpio[c(460, 1109), ],
    mapping = aes(x = X, y = Y, label = name),
    color = "black",
    size = 3,
    label.size = 0.1,
    label.r = grid::unit(0, "lines")
  ) +
  scale_fill_viridis_c(option = "C", na.value = "gray60",
                       limits = c(160, 300),
                       breaks = c(170, 190, 210, 230, 250, 270, 290),
                       direction = -1) +
  theme_minimal() +
  labs(
    title = "",
    x = "Longitud",
    y = "Latitud",
    fill = "Media"
  ); p2
rm(mapa_punt_global_media_mcpio)

# Se carga la base de datos anexo_pobreza_monetaria_18_departamento.xls: 
Incid.Pobr.Mon <- read_excel("C:/Users/USER/OneDrive/Documentos/LaTeX/b34 Caso de estudio - EB - 2025-1/Insumos para la base de datos/anexo_pobreza_monetaria_18_departamento.xls",
                             sheet = "Pobreza Monetaria (%)")
Incid.Pobr.Mon.2018 <- Incid.Pobr.Mon[-(1:9), ]
rm(Incid.Pobr.Mon)
colnames(Incid.Pobr.Mon.2018) <- c("Departamento", "2002", "2003", "2004",
                                   "2005", "2008", "2009", "2010", "2011",
                                   "2012", "2013", "2014", "2015", "2016",
                                   "2017", "2018")
Incid.Pobr.Mon.2018 <- Incid.Pobr.Mon.2018[-c(1, 26:34), c("Departamento", "2018")]
Incid.Pobr.Mon.2018$cod_depto <- c(5, 8, 11, 13, 15, 17, 18, 19, 20, 27, 23, 25,
                                   41, 44, 47, 50, 52, 54, 63, 66, 68, 70, 73,
                                   76)
summary(Incid.Pobr.Mon.2018$`2018`)
Incid.Pobr.Mon.2018[which.max(Incid.Pobr.Mon.2018$`2018`), ]
Incid.Pobr.Mon.2018[which.min(Incid.Pobr.Mon.2018$`2018`), ]

# Código del mapa de incidencia de la pobreza monetaria por departamento:
mapa_incid_pobr_mon_depto <- departamentos_geo %>%
  left_join(Incid.Pobr.Mon.2018, by = c("cod_depto" = "cod_depto"))
rm(departamentos_geo, Incid.Pobr.Mon.2018)

# Generación del mapa:
p3 <- ggplot(mapa_incid_pobr_mon_depto) +
  geom_sf(aes(fill = `2018`), color = "gray60", size = 0.1) +
  xlim(-80, -66) +
  geom_label(
    data = mapa_incid_pobr_mon_depto[-c(11, 14, 19, 21, 22, 26, 29, 32, 34), ],
    mapping = aes(x = longitude, y = latitude, label = woe_name),
    color = "black",
    size = 3,
    label.size = 0.1,
    label.r = grid::unit(0, "lines")
  ) +
  scale_fill_viridis_c(option = "C", na.value = "gray60", direction = 1) +
  theme_minimal() +
  labs(
    title = "",
    x = "Longitud",
    y = "Latitud",
    fill = "Índice"
  ); p3
rm(mapa_incid_pobr_mon_depto)

# Unión en una sola imagen con dos paneles:
p1+p3

# Se carga la base de datos MEN_ESTADISTICAS_EN_EDUCACION_EN_PREESCOLAR__B_SICA_
# Y_MEDIA_POR_MUNICIPIO_20250701.csv: 
MEN <- read_csv("C:/Users/USER/OneDrive/Documentos/LaTeX/b34 Caso de estudio - EB - 2025-1/Insumos para la base de datos/MEN_ESTADISTICAS_EN_EDUCACION_EN_PREESCOLAR__B_SICA_Y_MEDIA_POR_MUNICIPIO_20250701.csv")
MEN.2022 <- MEN %>% 
  group_by(CÓDIGO_MUNICIPIO) %>%
  slice(which(AÑO == 2022))
rm(MEN)
MEN.2022 <- MEN.2022[ , c("CÓDIGO_MUNICIPIO", "COBERTURA_NETA_SECUNDARIA")]
MEN.2022$CÓDIGO_MUNICIPIO <- as.numeric(MEN.2022$CÓDIGO_MUNICIPIO)
summary(MEN.2022$COBERTURA_NETA_SECUNDARIA)
MEN.2022[which.max(MEN.2022$COBERTURA_NETA_SECUNDARIA), ]
MEN.2022[which(MEN.2022$COBERTURA_NETA_SECUNDARIA == 0), ]

# Código del mapa de cobertura neta en educación secundaria por municipio:
mapa_cober_educ_sec_mcpio <- municipios_geo %>%
  left_join(MEN.2022, by = c("id" = "CÓDIGO_MUNICIPIO"))
rm(municipios_geo, MEN.2022)
mcpios_titulos <- tools::toTitleCase(tolower(mapa_cober_educ_sec_mcpio$name))
mapa_cober_educ_sec_mcpio$name <- mcpios_titulos
rm(mcpios_titulos)
centroides <- st_centroid(mapa_cober_educ_sec_mcpio)
coords <- st_coordinates(centroides)
mapa_cober_educ_sec_mcpio <- mapa_cober_educ_sec_mcpio %>%
  mutate(X = coords[, 1], Y = coords[, 2])
rm(centroides, coords)

# Generación del mapa:
p4 <- ggplot(mapa_cober_educ_sec_mcpio) +
  geom_sf(aes(fill = COBERTURA_NETA_SECUNDARIA), color = NA, size = 0.001) +
  xlim(-69, -53) +
  geom_label(
    data = mapa_cober_educ_sec_mcpio[c(720, 979, 1001, 1006, 1076, 1097), ],
    mapping = aes(x = X, y = Y, label = name),
    color = "black",
    size = 3,
    label.size = 0.1,
    label.r = grid::unit(0, "lines")
  ) +
  scale_fill_viridis_c(option = "C", na.value = "gray60", direction = -1) +
  theme_minimal() +
  labs(
    title = "",
    x = "Longitud",
    y = "Latitud",
    fill = "Cobertura"
  ); p4
rm(mapa_cober_educ_sec_mcpio)

# Unión en una sola imagen con dos paneles:
p2+p4

rm(list = ls())

#==============================================================================
# Análisis exploratorio descriptivo de las covariables
#==============================================================================

library(readxl)
library(sf)
library(readr)
library(dplyr)
library(ggplot2)
library(rnaturalearth)
library(stringi)
library(viridis)
library(patchwork)

# Se utiliza la base de datos creada en la primera sección anterior. Se puede
# cargar desde el computador en caso que ya esté almacenada:
Base <- read.table("C:/Users/USER/OneDrive/Documentos/LaTeX/b34 Caso de estudio - EB - 2025-1/Insumos para la base de datos/Base.txt",
                   header = TRUE, sep = ";", dec = ".")

# Análisis de variables cualitativas:
table(Base$fami_educacionmadre)
table(Base$fami_educacionmadre)/nrow(Base)

table(Base$fami_educacionpadre)
table(Base$fami_educacionpadre)/nrow(Base)

table(Base$estu_tieneetnia)
table(Base$estu_tieneetnia)/nrow(Base)

table(Base$fami_numlibros)
table(Base$fami_numlibros)/nrow(Base)

table(Base$fami_estratovivienda)
table(Base$fami_estratovivienda)/nrow(Base)

table(Base$fami_tienecomputador)
table(Base$fami_tienecomputador)/nrow(Base)

table(Base$fami_tieneinternet)
table(Base$fami_tieneinternet)/nrow(Base)

# Mapas:
municipios_geo <- st_read("C:/Users/USER/OneDrive/Documentos/LaTeX/b34 Caso de estudio - EB - 2025-1/Insumos para la base de datos/colombia-municipios.json",
                          quiet = TRUE)
municipios_geo$id <- as.numeric(municipios_geo$id)

educ_1_madre_percent <- aggregate(Base$fami_educacionmadre,
                                     by = list(estu_cod_reside_mcpio = Base$estu_cod_reside_mcpio),
                                     FUN = mean)
educ_0_madre_percent <- as.data.frame(rep(1, 1113)-educ_1_madre_percent$x)
educ_0_madre_percent$estu_cod_reside_mcpio <- educ_1_madre_percent$estu_cod_reside_mcpio
rm(educ_1_madre_percent)
mapa_educ_0_madre_percent <- municipios_geo %>%
  left_join(educ_0_madre_percent, by = c("id" = "estu_cod_reside_mcpio"))
rm(educ_0_madre_percent)
names(mapa_educ_0_madre_percent) <- c("id", "dpt", "name", "x", "geometry")
p1 <- ggplot(mapa_educ_0_madre_percent) +
  geom_sf(aes(fill = x), color = NA, size = 0.001) +
  xlim(-69, -53) +
  scale_fill_viridis_c(option = "C", na.value = "gray60",
                       labels = function(x) paste0(round(x*100), "%"),
                       direction = 1) +
  theme_minimal() +
  labs(
    title = "Madres sin educación superior",
    x = "Longitud",
    y = "Latitud",
    fill = "Porcentaje"
  ); p1
rm(mapa_educ_0_madre_percent)
educ_1_padre_percent <- aggregate(Base$fami_educacionpadre,
                                  by = list(estu_cod_reside_mcpio = Base$estu_cod_reside_mcpio),
                                  FUN = mean)
educ_0_padre_percent <- as.data.frame(rep(1, 1113)-educ_1_padre_percent$x)
educ_0_padre_percent$estu_cod_reside_mcpio <- educ_1_padre_percent$estu_cod_reside_mcpio
rm(educ_1_padre_percent)
mapa_educ_0_padre_percent <- municipios_geo %>%
  left_join(educ_0_padre_percent, by = c("id" = "estu_cod_reside_mcpio"))
rm(educ_0_padre_percent)
names(mapa_educ_0_padre_percent) <- c("id", "dpt", "name", "x", "geometry")
p2 <- ggplot(mapa_educ_0_padre_percent) +
  geom_sf(aes(fill = x), color = NA, size = 0.001) +
  xlim(-69, -53) +
  scale_fill_viridis_c(option = "C", na.value = "gray60",
                       labels = function(x) paste0(round(x*100), "%"),
                       direction = 1) +
  theme_minimal() +
  labs(
    title = "Padres sin educación superior",
    x = "Longitud",
    y = "Latitud",
    fill = "Porcentaje"
  ); p2
rm(mapa_educ_0_padre_percent)
p1+p2
rm(p1, p2)
Base[which(Base$estu_cod_reside_mcpio == 97666), ]
Base[which(Base$estu_cod_reside_mcpio == 94883), ]

etnia_si <- Base %>%
  group_by(estu_cod_reside_mcpio) %>%
  summarise(prop_etnia_si = sum(estu_tieneetnia == "Si")/n())
mapa_etnia_si <- municipios_geo %>%
  left_join(etnia_si, by = c("id" = "estu_cod_reside_mcpio"))
rm(etnia_si)
names(mapa_etnia_si) <- c("id", "dpt", "name", "x", "geometry")
p2.1 <- ggplot(mapa_etnia_si) +
  geom_sf(aes(fill = x), color = NA, size = 0.001) +
  xlim(-69, -53) +
  scale_fill_viridis_c(option = "C", na.value = "gray60",
                       labels = function(x) paste0(round(x*100), "%"),
                       direction = 1) +
  theme_minimal() +
  labs(
    title = "Estudiantes con autorreconocimiento étnico",
    x = "Longitud",
    y = "Latitud",
    fill = "Porcentaje"
  ); p2.1
rm(mapa_etnia_si)

compu_no <- Base %>%
  group_by(estu_cod_reside_mcpio) %>%
  summarise(prop_compu_no = sum(fami_tienecomputador == "No")/n())
mapa_compu_no <- municipios_geo %>%
  left_join(compu_no, by = c("id" = "estu_cod_reside_mcpio"))
rm(compu_no)
names(mapa_compu_no) <- c("id", "dpt", "name", "x", "geometry")
p2.2 <- ggplot(mapa_compu_no) +
  geom_sf(aes(fill = x), color = NA, size = 0.001) +
  xlim(-69, -53) +
  scale_fill_viridis_c(option = "C", na.value = "gray60",
                       labels = function(x) paste0(round(x*100), "%"),
                       direction = 1) +
  theme_minimal() +
  labs(
    title = "Estudiantes sin acceso a computador en casa",
    x = "Longitud",
    y = "Latitud",
    fill = "Porcentaje"
  ); p2.2
rm(mapa_compu_no)

internet_no <- Base %>%
  group_by(estu_cod_reside_mcpio) %>%
  summarise(prop_internet_no = sum(fami_tieneinternet == "No")/n())
mapa_internet_no <- municipios_geo %>%
  left_join(internet_no, by = c("id" = "estu_cod_reside_mcpio"))
rm(internet_no)
names(mapa_internet_no) <- c("id", "dpt", "name", "x", "geometry")
p2.3 <- ggplot(mapa_internet_no) +
  geom_sf(aes(fill = x), color = NA, size = 0.001) +
  xlim(-69, -53) +
  scale_fill_viridis_c(option = "C", na.value = "gray60",
                       labels = function(x) paste0(round(x*100), "%"),
                       direction = 1) +
  theme_minimal() +
  labs(
    title = "Estudiantes sin acceso a internet en casa",
    x = "Longitud",
    y = "Latitud",
    fill = "Porcentaje"
  ); p2.3
rm(mapa_internet_no)

p2.1+p2.2+p2.3
rm(p2.1, p2.2, p2.3)

# Análisis de variables cuantitativas
summary(Base$doc_por_est)
summary(Base$RISK_VICTIM_2022)
summary(Base$risk_percent)
summary(Base$prom_dep_homic)

# Boxplots:
par(mfrow = c(2, 2))

boxplot(Base$doc_por_est,
        main = "Docentes por estudiante",
        ylab = "Razón de docentes/estudiantes",
        col = "lightblue",
        border = "dodgerblue")
text(x = 1, y = max(Base$doc_por_est), labels = paste("Chima"), pos = 1,
     col = "dodgerblue")
Base[which(Base$doc_por_est > 0.7), "estu_cod_reside_mcpio"]

boxplot(Base$RISK_VICTIM_2022,
        main = "Riesgo por hechos victimizantes",
        ylab = "Índice",
        col = "palegreen",
        border = "green")

boxplot(Base$risk_percent,
        main = "Municipios en riesgo de violencia",
        ylab = "Porcentaje",
        col = "lemonchiffon",
        border = "gold")

boxplot(Base$prom_dep_homic,
        main = "Promedio departamental de homicidios",
        ylab = "Promedio ponderado",
        col = "lightsalmon",
        border = "darkorange")
text(x = 1, y = 1137, labels = paste("Bogotá"), pos = 1,
     col = "darkorange")
text(x = 1, y = 657, labels = paste("Valle del Cauca"), pos = 1,
     col = "darkorange")
Base %>%
  group_by(estu_cod_reside_depto) %>%
  summarise(valor_unico = unique(prom_dep_homic)) %>%   
  filter(valor_unico > 600)

par(mfrow = c(1, 1))

# Mapas:
doc_per_est <- Base %>%
  group_by(estu_cod_reside_mcpio) %>%
  summarise(doc_per_est = unique(doc_por_est))
mapa_doc_per_est <- municipios_geo %>%
  left_join(doc_per_est, by = c("id" = "estu_cod_reside_mcpio"))
mapa_doc_per_est <- mapa_doc_per_est[-361, ]
p3 <- ggplot(mapa_doc_per_est) +
  geom_sf(aes(fill = doc_per_est), color = NA, size = 0.001) +
  xlim(-69, -53) +
  scale_fill_viridis_c(option = "C", na.value = "gray60",
                       direction = -1) +
  theme_minimal() +
  labs(
    title = "Docentes por estudiante",
    x = "Longitud",
    y = "Latitud",
    fill = "Razón"
  ); p3
rm(doc_per_est, mapa_doc_per_est)

RISK_VICTIM_2022 <- Base %>%
  group_by(estu_cod_reside_mcpio) %>%
  summarise(RISK_VICTIM_2022 = unique(RISK_VICTIM_2022))
mapa_RISK_VICTIM_2022 <- municipios_geo %>%
  left_join(RISK_VICTIM_2022, by = c("id" = "estu_cod_reside_mcpio"))
rm(municipios_geo)
mcpios_titulos <- tools::toTitleCase(tolower(mapa_RISK_VICTIM_2022$name))
mapa_RISK_VICTIM_2022$name <- mcpios_titulos
rm(mcpios_titulos)
centroides <- st_centroid(mapa_RISK_VICTIM_2022)
coords <- st_coordinates(centroides)
mapa_RISK_VICTIM_2022 <- mapa_RISK_VICTIM_2022 %>%
  mutate(X = coords[, 1], Y = coords[, 2])
rm(centroides, coords)
p4 <- ggplot(mapa_RISK_VICTIM_2022) +
  geom_sf(aes(fill = RISK_VICTIM_2022), color = NA, size = 0.001) +
  xlim(-69, -53) +
  geom_label(
    data = mapa_RISK_VICTIM_2022[c(129, 220), ],
    mapping = aes(x = X, y = Y+1, label = name),
    color = "black",
    size = 3,
    label.size = 0.1,
    label.r = grid::unit(0, "lines")
  ) +
  scale_fill_viridis_c(option = "C", na.value = "gray60",
                       direction = 1) +
  theme_minimal() +
  labs(
    title = "Riesgo por hechos victimizantes",
    x = "Longitud",
    y = "Latitud",
    fill = "Índice"
  ); p4
Base %>%
  group_by(estu_cod_reside_mcpio) %>%
  summarise(valor_unico = unique(RISK_VICTIM_2022)) %>%   
  filter(valor_unico > .95)
rm(RISK_VICTIM_2022, mapa_RISK_VICTIM_2022)

departamentos_geo <- ne_states(country = "Colombia", returnclass = "sf")
cod_depto <- c(52, 86, 27, 94, 97, 91, 44, 20, 54, 81, 15, 99, 19, 76, 5, 23,
               70, 13, 8, 47, 88, NA, 18, 41, 95, 17, 85, 50, 11, 68, 73, 63, 
               25, 66)
departamentos_geo$cod_depto <- cod_depto

risk_percent <- Base %>%
  group_by(estu_cod_reside_depto) %>%
  summarise(risk_percent = unique(risk_percent))
mapa_risk_percent <- departamentos_geo %>%
  left_join(risk_percent, by = c("cod_depto" = "estu_cod_reside_depto"))
p5 <- ggplot(mapa_risk_percent) +
  geom_sf(aes(fill = risk_percent), color = "gray60", size = 0.1) +
  xlim(-80, -66) +
  geom_label(
    data = mapa_risk_percent[c(3, 5, 6, 10, 23, 29), ],
    mapping = aes(x = longitude, y = latitude+1, label = woe_name),
    color = "black",
    size = 3,
    label.size = 0.1,
    label.r = grid::unit(0, "lines")
  ) +
  scale_fill_viridis_c(option = "C", na.value = "gray60", direction = 1) +
  theme_minimal() +
  labs(
    title = "Municipios en riesgo de violencia",
    x = "Longitud",
    y = "Latitud",
    fill = "Porcentaje"
  ); p5
rm(risk_percent, mapa_risk_percent)

prom_dep_homic <- Base %>%
  group_by(estu_cod_reside_depto) %>%
  summarise(prom_dep_homic = unique(prom_dep_homic))
mapa_prom_dep_homic <- departamentos_geo %>%
  left_join(prom_dep_homic, by = c("cod_depto" = "estu_cod_reside_depto"))
rm(departamentos_geo)
p6 <- ggplot(mapa_prom_dep_homic) +
  geom_sf(aes(fill = prom_dep_homic), color = "gray60", size = 0.1) +
  xlim(-80, -66) +
  geom_label(
    data = mapa_prom_dep_homic[c(14, 29), ],
    mapping = aes(x = longitude, y = latitude-1, label = woe_name),
    color = "black",
    size = 3,
    label.size = 0.1,
    label.r = grid::unit(0, "lines")
  ) +
  scale_fill_viridis_c(option = "C", na.value = "gray60", direction = 1) +
  theme_minimal() +
  labs(
    title = "Promedio departamental de homicidios",
    x = "Longitud",
    y = "Latitud",
    fill = "Promedio"
  ); p6
rm(prom_dep_homic, mapa_prom_dep_homic)

p3+p4+p5+p6

rm(list = ls())
