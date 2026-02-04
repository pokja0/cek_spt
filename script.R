
library(fst)
library(googlesheets4)

# Baca dengan spesifikasi tipe kolom
lapor_spt25 <- read_sheet(
  "https://docs.google.com/spreadsheets/d/1CI6PxR_Su_CdTqnESs5AxzhqNXmOJoe-AOwyvGECvKs/edit?gid=759413326#gid=759413326",
  col_types = "c"  # 'c' berarti character untuk semua kolom, atau sesuaikan
)

lapor_spt25$NIP <- as.character(lapor_spt25$NIP)

lapor_spt25 <- lapor_spt25 |>
  distinct(NIP, .keep_all = TRUE)

asn_penyuluh <- fst::read.fst("data/asn_pkb.fst")

asn_penyuluh <- left_join(asn_penyuluh, lapor_spt25, by = c("NIP Baru" = "NIP")) |>
  mutate(
    `Status Lapor` = ifelse(is.na(Timestamp), "Belum Melapor", "Sudah Melapor")
  )# 

asn_perwakilan <- fst::read.fst("data/asn_perwakilan.fst")

asn_perwakilan <- left_join(asn_perwakilan, lapor_spt25, by = c("NIP Baru" = "NIP")) |>
  mutate(
    `Status Lapor` = ifelse(is.na(Timestamp), "Belum Melapor", "Sudah Melapor")
  )# 
# wilker_pkb <- readr::read_csv2("data/nama pkb.csv")
# 
# asn_pkb <- read_xlsx("data/DaftarPegawai_30012026.xlsx", 1)
# 
# 
# asn_pkb <- asn_pkb |>
#   inner_join(wilker_pkb |>
#                select(KABUPATEN, `NAMA PKB`) |>
#                distinct(),
#              by = c("Nama Lengkap" = "NAMA PKB"))
# 
# 
# fst::write.fst(asn_pkb, "data/asn_perwakilan.fst")
# 
# 
# 
# 
