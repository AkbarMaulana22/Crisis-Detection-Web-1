library(shiny)
library(shinythemes)

ui =fluidPage(
  navbarPage(
    theme = shinythemes::shinytheme("united"),  # <--- To use a theme, uncomment this
    "Crisis Web Detection",
    tabPanel("Deteksi Krisis Keuangan",
             headerPanel("Pendeteksian Dini Krisis Keuangan Di Indonesia"),
             sidebarPanel(width = 6,
                          wellPanel(
                            dateInput("date",label="Date",format="yyyy-MM",startview = "year"),
                            br(),
                            numericInput("Impor",label="Impor",value = ""),
                            numericInput("Ekspor",label="Ekspor",value =""),
                            numericInput("Cadangan.Devisa",label="Cadangan Devisa",value = ""),
                            numericInput("IHSG",label="Indeks Harga Saham Gabungan",value = ""),
                            numericInput("SPinSim",label="Rasio Suku Bunga Pinjaman dan Simpanan",value = ""),
                            numericInput("SBSR",label="Suku Bunga Simpanan Riil",value = ""),
                          )
             ),
             sidebarPanel(width = 6,
                          wellPanel(
                            numericInput("SBIrFer",label="Selisih BI Rate Riil dan Fed Rate Riil",value = ""),
                            numericInput("Simpanan.bank.",label="Simpanan Bank",value = ""),
                            numericInput("NTRiil",label="Nilai Tukar Riil",value = ""),
                            numericInput("NTPdag.",label="Nilai Tukar Perdagangan",value = ""),
                            numericInput("M1",label="M1",value = ""),
                            numericInput("M2.CD",label="M2 per Cadangan Devisa",value = ""),
                            numericInput("M2M",label="M2 Multiplier",value = ""),
                            actionButton("hasil","Prediksi"),
                            br(),
                            h3("Hasil Prediksi"),
                            br(),
                            textOutput(outputId="prediksi"),
                          )
             )
    ),
    tabPanel("13 Indikator", 
             headerPanel("13 Indikator Krisis Keuangan"),
             sidebarPanel(
               wellPanel(
                 selectInput("indikator",label="Pilih Indikator",choices = c("Impor"="Impor2","Ekspor"="Ekspor2","Cadangan Devisa"="Cadangan.Devisa2",
                                                                             "Indeks Harga Saham Gabungan"="IHSG2","Rasio Suku Bunga Pinjaman dan Simpanan"="SPinSim2",
                                                                             "Suku Bunga Simpanan Riil"="SBSR2","Selisih BI Rate Riil dan Fed Rate Riil"="SBIrFer2",
                                                                             "Simpanan Bank"="Simpanan.bank.2","Nilai Tukar Riil"="NTRiil2","Nilai Tukar Perdagangan"="NTPdag.2",
                                                                             "M1"="M12","M2 per Cadangan Devisa"="M2.CD2","M2 Multiplier"="M2M2"),selected = NULL),
                 actionButton("hasil2","Pilih"),
               )
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Plot",plotOutput("plot",click="plot_click")),
                 tabPanel("Data",verbatimTextOutput("table"))
               )
             )
    )
  )
)
server = function(input, output) {
  library(mxnet)
  library(caret)
  library(ggfortify)
  data_plot=read.csv("E:/Data Skripsi Akbar/Data_Shiny.csv",sep=";")
  data_plot=ts(data_plot,start = c(1990,1),frequency = 12)
  ide=eventReactive(input$hasil,{
    model=readRDS("E:/Data Skripsi Akbar/Adam.rds")
    data=read.csv("E:/Data Skripsi Akbar/Data_Smote_Baru.csv")
    data2=data[,-c(1,15)]
    Impor=input$Impor
    Ekspor=input$Ekspor
    Cadangan.Devisa=input$Cadangan.Devisa
    IHSG=input$IHSG
    SPinSim=input$SPinSim
    SBSR=input$SBSR
    SBIrFer=input$SBIrFer
    Simpanan.bank.=input$Simpanan.bank.
    NTRiil=input$NTRiil
    NTPdag.=input$NTPdag.
    M1=input$M1
    M2.CD=input$M2.CD
    M2M=input$M2M
    data_prediksi=cbind.data.frame(Impor,Ekspor,Cadangan.Devisa,IHSG,SPinSim,SBSR,SBIrFer,Simpanan.bank.,NTRiil,NTPdag.,M1,M2.CD,M2M)
    names(data_prediksi)=c("Impor","Ekspor","Cadangan.Devisa","IHSG","SPinSim","SBSR","SBIrFer","Simpanan.bank.","NTRiil","NTPdag.","M1","M2.CD","M2M")
    data3=rbind.data.frame(data2,data_prediksi)
    data4=scale(data3)
    data5=as.data.frame(data4)
    data_prediksi2=data5[-c(1:550),]
    prediksi1=predict(model,data_prediksi2)
    prediksi2=ifelse(prediksi1==1,print("Terdeteksi Sinyal Terjadinya Krisis Keuangan Satu Tahun Kedepan"),print("Tidak Terdeteksi Sinyal Terjadinya Krisis Keuangan Satu Tahun Kedepan"))
    paste(prediksi2)})  
  ide2=eventReactive(input$hasil2,{
    autoplot(data_plot[,input$indikator],xlab="Time",ylab="Value")})
  ide3=eventReactive(input$hasil2,{
    data_plot[,input$indikator]})
  output$prediksi=renderText({
    ide()
  })
  output$plot <- renderPlot({
    ide2()
  })
  output$table <- renderPrint({
    ide3()
  })
}
shinyApp(ui=ui,server=server)