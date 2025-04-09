library(readxl)
setwd("C:\\Users\\jgorzo\\OneDrive - New Jersey Office of Information Technology\\Documents\\data\\")
#lines below won't work if file is open
ny_comm <- read_excel("2025SA_NY_Tautog Data 2021-2023_corrected.xlsx", sheet="CommBioSamples", range = cell_rows(6:901))
ny_comm$state <- "NY"
ny_comm$mode <- "comm"
ny_rec <- read_excel("2025SA_NY_Tautog Data 2021-2023_corrected.xlsx", sheet="RecBioSamples", range = cell_rows(6:110))
ny_rec$state <- "NY"
ny_rec$mode <- "rec"
nj_comm <- read_excel("Tautog Data Template 2025_NJDEP.xlsx", sheet="CommBioSamples", range = cell_rows(6:682))
nj_comm$Year <- format(nj_comm$Year, '%Y')
nj_comm$state <- "NJ"
nj_comm$mode <- "comm"
njny <- rbind(ny_comm, nj_comm[,names(ny_comm)], ny_rec)
njny$structure <- njny$`Ageing Structure`
njny$structure[njny$structure %in% c("opercular", "opec", "Operculum")] <- "operc"
njny$structure[njny$structure == "Otolith"] <- "oto"
#write.csv(njny, "njny.csv")
