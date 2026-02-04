library(bs4Dash)
library(dplyr)
library(fst)
library(reactable)
library(googlesheets4)
library(waiter)
library(bslib)
library(bsicons)
library(writexl) 

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
    `Status Lapor` = ifelse(is.na(Timestamp), "Belum Melapor", "Sudah Melapor"),
    No = as.character(1:nrow(asn_penyuluh))
  ) |>
  select(No, KABUPATEN, `NIP Baru`, `Nama Lengkap`, `Jenis Pegawai`, `Status Lapor`)

asn_perwakilan <- fst::read.fst("data/asn_perwakilan.fst")

asn_perwakilan <- left_join(asn_perwakilan, lapor_spt25, by = c("NIP Baru" = "NIP")) |>
  mutate(
    `Status Lapor` = ifelse(is.na(Timestamp), "Belum Melapor", "Sudah Melapor"),
    No = as.character(1:nrow(asn_perwakilan))
  ) |>
  select(No, `NIP Baru`, `Nama Lengkap`, `Jenis Pegawai`, `Status Lapor`)

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
            3,
            value_box( 
              title = "Jumlah ASN Perwakilan", 
              "68",
              showcase = bs_icon("people-fill"),
              theme = "bg-gradient-indigo-purple",
              p("Sumber: SIMSDM 30 Januari 2026")
            )
          ),
          column(
            3,
            value_box( 
              title = "Telah Melapor", 
              textOutput("perwakilan_sudah"),  
              showcase = bs_icon("emoji-heart-eyes-fill"),
              theme = "bg-gradient-indigo-purple" 
            )
          ),
          column(
            3,
            value_box( 
              title = "Belum Melapor", 
              textOutput("perwakilan_belum"),  
              showcase = bs_icon("emoji-tear-fill"),
              theme = "bg-gradient-indigo-purple" 
            )
          ),
          column(
            3,
            value_box(
              title = "Sisa Hari Hingga Batas Waktu",
              value = textOutput("sisa_hari"),
              showcase = bs_icon("calendar-week-fill"),
              theme = "primary",
              p("Batas akhir: 28 Februari 2026"),
              p("Nota Dinas Kepala Perwakilan")
            )
          )
        ),
        downloadButton("download_excel", "Download Excel"),
        card(
          reactableOutput("tabel_perwakilan")
        )
      ),
      tabItem(
        tabName = "tab2",
        fluidRow(
          column(
            3,
            value_box( 
              title = "Jumlah ASN PKB/PLKB", 
              "413",
              showcase = bs_icon("people"),
              theme = "bg-gradient-indigo-purple",
              p("Sumber: SIMSDM 30 Januari 2026")
            )
          ),
          column(
            3,
            value_box( 
              title = "Telah Melapor", 
              textOutput("pkb_sudah"),  
              showcase = bs_icon("emoji-heart-eyes"),
              theme = "bg-gradient-indigo-purple" ,
              
            )
          ),
          column(
            3,
            value_box( 
              title = "Belum Melapor", 
              textOutput("pkb_belum"),  
              showcase = bs_icon("emoji-tear"),
              theme = "bg-gradient-indigo-purple" 
            )
          ),
          column(
            3,
            value_box(
              title = "Sisa Hari Hingga Batas Waktu",
              value = textOutput("sisa_hari_pkb"),
              showcase = bs_icon("calendar-week"),
              theme = "primary",
              p("Batas akhir: 28 Februari 2026"),
              p("Nota Dinas Kepala Perwakilan")
            )
          )
        ),
        downloadButton("download_excel_penyuluh", "Download Excel"),
        card(
          reactableOutput("tabel_penyuluh")
        )
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
  
  status_lapor_perwakilan <- asn_perwakilan |>
    count(`Status Lapor`)
 
  
  output$perwakilan_sudah <- renderText({
    status_lapor_perwakilan$n[status_lapor_perwakilan$`Status Lapor` == "Sudah Melapor"]
  })
  
  output$perwakilan_belum <- renderText({
    status_lapor_perwakilan$n[status_lapor_perwakilan$`Status Lapor` == "Belum Melapor"]
  })
  
  output$download_excel <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      write_xlsx(asn_perwakilan, file)
    }
  )
  
  output$tabel_perwakilan <- renderReactable({
    reactable(asn_perwakilan, filterable = TRUE, 
              pagination = FALSE,
             # virtual = TRUE,
              height = 500,
              showPagination = T)
  })
  
  #PKB
  
  output$sisa_hari_pkb <- renderText({
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
  
  status_lapor_penyuluh <- asn_penyuluh |>
    count(`Status Lapor`)
  
  
  output$pkb_sudah <- renderText({
    status_lapor_penyuluh$n[status_lapor_penyuluh$`Status Lapor` == "Sudah Melapor"]
  })
  
  output$pkb_belum <- renderText({
    status_lapor_penyuluh$n[status_lapor_penyuluh$`Status Lapor` == "Belum Melapor"]
  })
  
  output$download_excel_penyuluh <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      write_xlsx(asn_penyuluh, file)
    }
  )
  
  output$tabel_penyuluh <- renderReactable({
    reactable(asn_penyuluh, filterable = TRUE, 
              pagination = F,
              # virtual = TRUE,
              height = 500,
              showPagination = TRUE)
  })
  
}

shinyApp(ui, server)