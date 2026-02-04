library(bs4Dash)
library(dplyr)
library(fst)
library(reactable)
library(googlesheets4)
library(waiter)
library(bslib)
library(bsicons)

gs4_deauth()
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
  )

ui <- dashboardPage(
  preloader = list(html = tagList(spin_1(), "Loading ..."), color = "#343a40"),
  dashboardHeader(title = "Cek Lapor SPT25"),
  dashboardSidebar(
    sidebarMenu(
      id = "sidebarMenu",
      menuItem(
        text = "ASN Perwakilan",
        tabName = "tab1"
      ),
      menuItem(
        text = "PKB/PLKB",
        tabName = "tab2"
      )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "tab1",
        # Boxes need to be put in a row (or column)
        fluidRow(
          column(
            4,
            value_box( 
              title = "Jumlah ASN Perwakilan", 
              "68",
              showcase = bs_icon("music-note-beamed"),
              theme = "bg-gradient-indigo-purple" 
            )
          ),
          column(
            4,
            value_box( 
              title = "Telah Melapor", 
              "$1 Billion Dollars",  
              showcase = bs_icon("music-note-beamed"),
              theme = "bg-gradient-indigo-purple" 
            )
          ),
          column(
            4,
            value_box( 
              title = "Belum Melapor", 
              "$1 Billion Dollars", 
              showcase = bs_icon("music-note-beamed"),
              theme = "bg-gradient-indigo-purple" 
            )
          )
        ),
        fluidRow(
          column(
            3,
            value_box(
              title = "Sisa Hari Hingga Batas Waktu",
              value = textOutput("sisa_hari"),
              showcase = bs_icon("music-note-beamed"),
              theme = "primary",
              p("Batas akhir: 28 Februari 2026"),
              p("Nota Dinas Kepala Perwakilan")
            )
          )
        )
      ),
      tabItem(
        tabName = "tab2",
        "Welcome TAB2!"
      )
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$reload, {
    session$reload()
  })
  
  output$sisa_hari <- renderText({
    batas_waktu <- as.Date("2026-02-28")
    hari_ini <- Sys.Date()
    sisa <- as.numeric(batas_waktu - hari_ini)
    
    if(sisa > 0) {
      paste0(sisa, " hari lagi")
    } else if(sisa == 0) {
      "HARI INI BATAS WAKTU!"
    } else {
      paste0("Terlambat ", abs(sisa), " hari!")
    }
  })
  
}

shinyApp(ui, server)