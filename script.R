wilker_pkb <- readr::read_csv2("data/nama pkb.csv")

asn_pkb <- read_xlsx("data/DaftarPegawai_30012026.xlsx", 1)


asn_pkb <- asn_pkb |>
  inner_join(wilker_pkb |>
               select(KABUPATEN, `NAMA PKB`) |>
               distinct(),
             by = c("Nama Lengkap" = "NAMA PKB"))


fst::write.fst(asn_pkb, "data/asn_perwakilan.fst")

asn_penyuluh <- fst::read.fst("data/asn_pkb.fst")

asn_perwakilan <- fst::read.fst("data/asn_perwakilan.fst")

lapor_spt <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/1CI6PxR_Su_CdTqnESs5AxzhqNXmOJoe-AOwyvGECvKs/edit?gid=759413326#gid=759413326")

as.character(lapor_spt$NIP)

library(googlesheets4)

# Baca dengan spesifikasi tipe kolom
lapor_spt <- read_sheet(
  "https://docs.google.com/spreadsheets/d/1CI6PxR_Su_CdTqnESs5AxzhqNXmOJoe-AOwyvGECvKs/edit?gid=759413326#gid=759413326",
  col_types = "c"  # 'c' berarti character untuk semua kolom, atau sesuaikan
)


