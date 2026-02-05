library(bs4Dash)
library(dplyr)
library(fst)
library(reactable)
library(googlesheets4)
library(waiter)
library(bslib)
library(bsicons)
library(writexl) 

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
        h2("Status Pengiriman Bukti Dukung SPT25 - ASN Perwakilan", style="text-align: center;"),
        br(),
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
              title = "Sudah Mengirim", 
              textOutput("perwakilan_sudah"),  
              showcase = bs_icon("emoji-heart-eyes-fill"),
              theme = "bg-gradient-indigo-purple" 
            )
          ),
          column(
            3,
            value_box( 
              title = "Belum Mengirim", 
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
        h2("Status Pengiriman Bukti Dukung SPT25 - PKB/PLKB", style="text-align: center;"),
        br(),
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
              title = "Sudah Mengirim", 
              textOutput("pkb_sudah"),  
              showcase = bs_icon("emoji-heart-eyes"),
              theme = "bg-gradient-indigo-purple" ,
              
            )
          ),
          column(
            3,
            value_box( 
              title = "Belum Mengirim", 
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
  
  lapor_spt25 <- reactive({
    gs4_deauth()
    # Baca dengan spesifikasi tipe kolom
    lapor_spt <- read_sheet(
      "https://docs.google.com/spreadsheets/d/1CI6PxR_Su_CdTqnESs5AxzhqNXmOJoe-AOwyvGECvKs/edit?gid=759413326#gid=759413326",
      col_types = "c"  # 'c' berarti character untuk semua kolom, atau sesuaikan
    ) 
    
    lapor_spt$NIP <- as.character(lapor_spt$NIP)
    
    lapor_spt <- lapor_spt |>
      distinct(NIP, .keep_all = TRUE)
    
    return(lapor_spt)
  })
  
  asn_penyuluh <- reactive({
    asn_penyuluh <- fst::read.fst("data/asn_pkb.fst")
    
    asn_penyuluh <- left_join(asn_penyuluh, lapor_spt25(), by = c("NIP Baru" = "NIP")) |>
      mutate(
        `Bukti Dukung` = ifelse(is.na(Timestamp), "Belum Mengirim", "Sudah Mengirim"),
        No = as.character(1:nrow(asn_penyuluh))
      ) |>
      select(No, KABUPATEN, `NIP Baru`, `Nama Lengkap`, `Jenis Pegawai`, `Bukti Dukung`)
  })
  
  asn_perwakilan <- reactive({
    asn_perwakilan <- fst::read.fst("data/asn_perwakilan.fst")
    
    asn_perwakilan <- left_join(asn_perwakilan, lapor_spt25(), by = c("NIP Baru" = "NIP")) |>
      mutate(
        `Bukti Dukung` = ifelse(is.na(Timestamp), "Belum Mengirim", "Sudah Mengirim"),
        No = as.character(1:nrow(asn_perwakilan))
      ) |>
      select(No, `NIP Baru`, `Nama Lengkap`, `Jenis Pegawai`, `Bukti Dukung`)
  })
  
  
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
  
  status_lapor_perwakilan <- reactive({
    status_lapor_perwakilan <- asn_perwakilan() |>
      count(`Bukti Dukung`)
  })
  
 
  
  output$perwakilan_sudah <- renderText({
    status_lapor_perwakilan = status_lapor_perwakilan()
    status_lapor_perwakilan$n[status_lapor_perwakilan$`Bukti Dukung` == "Sudah Mengirim"]
  })
  
  output$perwakilan_belum <- renderText({
    status_lapor_perwakilan = status_lapor_perwakilan()
    status_lapor_perwakilan$n[status_lapor_perwakilan$`Bukti Dukung` == "Belum Mengirim"]
  })
  
  output$download_excel <- downloadHandler(
    filename = function() {
      paste("data-lapor-spt-perwakilan-", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      write_xlsx(asn_perwakilan(), file)
    }
  )
  
  output$tabel_perwakilan <- renderReactable({
    asn_perwakilan = asn_perwakilan()
    reactable(asn_perwakilan(), filterable = TRUE, 
              pagination = FALSE,
             # virtual = TRUE,
              height = 500,
              showPagination = T,
             rowStyle = function(index) {
               if (asn_perwakilan[index, "Bukti Dukung"] == "Belum Mengirim") list(background = "rgb(255, 153, 153)")
             })
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
  
  status_lapor_penyuluh <- reactive({
    status_lapor_penyuluh <- asn_penyuluh() |>
      count(`Bukti Dukung`)
  })

  
  
  output$pkb_sudah <- renderText({
    status_lapor_penyuluh = status_lapor_penyuluh()
    status_lapor_penyuluh$n[status_lapor_penyuluh$`Bukti Dukung` == "Sudah Mengirim"]
  })
  
  output$pkb_belum <- renderText({
    status_lapor_penyuluh = status_lapor_penyuluh()
    status_lapor_penyuluh$n[status_lapor_penyuluh$`Bukti Dukung` == "Belum Mengirim"]
  })
  
  output$download_excel_penyuluh <- downloadHandler(
    filename = function() {
      paste("data-lapor-spt-penyuluh-", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      write_xlsx(asn_penyuluh(), file)
    }
  )
  
  output$tabel_penyuluh <- renderReactable({
    asn_penyuluh = asn_penyuluh()
    asn_penyuluh <- asn_penyuluh |>
      arrange(KABUPATEN)
    reactable(asn_penyuluh, filterable = TRUE, 
              pagination = F,
              # virtual = TRUE,
              height = 500,
              showPagination = TRUE,
              rowStyle = function(index) {
                 if (asn_penyuluh[index, "Bukti Dukung"] == "Belum Mengirim") list(background = "rgb(255, 153, 153)")
              })
  })
  
}

shinyApp(ui, server)