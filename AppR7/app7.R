#App7


library(ggplot2)
library(shiny)
library(plotly)
library(DT)
library(visNetwork)
library(reshape2)
library(shinythemes)

SM_MOA <- read.csv(file="data/L1000_SM_MOA2.csv", header=TRUE)

      
ui <- fluidPage(theme = shinytheme("flatly"),
                navbarPage(
                  title = div(
                    
                      img(src = "Logos_3.png",height="30",align="left",position="fixed",hspace="10"),
                    "SynergySeq"
                  ),

                          ####  Introduction  ####
                          tabPanel("Introduction",
                                    mainPanel(
                                      
                                      tags$head(includeHTML(("google-analytics.html"))),
                                      
                                              h3(strong("SynergySeq:")),
                                              h4("Identifying drug combinations that transcriptionally reverse a disease gene expression signature."),
                                              br(),
                                              h4(strong(tags$u("Workflow"))),
                                              br(),
                                              img(src="Figure1_Nov2018_v2.png",height="300", width="522.34"),
                                              br(),
                                              br(),
                                              h4(strong(tags$u("Tools"))),
                                              tags$ul(
                                                tags$li(
                                                  p(strong("Synergy Plot"),br(),"Ranking of the LINCS L1000 compounds based on their transcriptional similarity to a reference compound and their reversal of a disease signature"))
                                               
                                                 #tags$li(
                                                 #        p(strong("Synergy Plot using Reference Drug"),br(),"Identify drugs that would best combine with a reference drug and reverse a given Disease Gene Expression Signature"))
                                                
                                                
                                                
                                                )
                                              
                                             )),
                                  

                          tabPanel("Tools", tabsetPanel( 
                            ####  Synergy Plot  ####
                            tabPanel("Synergy Plot",fluid=TRUE,
                                     sidebarLayout(sidebarPanel(
                                             h3("Step 1:"),
                                             selectInput(inputId = "L1000_Dataset",
                                                         label = "Choose an L1000 Dataset:",
                                                         choices = c("LINCS L1000 Dec 2015", "LINCS L1000 March 2017"),selected = "LINCS L1000 March 2017"),
                                                         #choices = c("LINCS L1000 Dec 2015", "LINCS L1000 March 2017","LINCS Median Scores"),selected = "LINCS L1000 Dec 2015"),
                                             hr(),
                                             h3("Step 2:"),
                                             uiOutput("ui"),
                                             sliderInput(inputId = "bins",
                                                             label = "",
                                                             min = 1,
                                                             max = 100,
                                                             value = 33),
                                             helpText("Filter out the lowest 'n' percentile of the gene consensus scores from the Reference Drug Signature"),
                                             hr(),
                                             h3("Step 3:"),
                                             selectInput(inputId = "disease",
                                                             label = "Choose a Disease Signature:",
                                                             choices = c("Glioblastoma TCGA (GBM)","Colon TCGA (CRC)", "Breast TCGA (BRCA)",
                                                                         "PDX GBM Group 1", "PDX GBM Group 2", "PDX GBM Group 3", "PDX GBM Group 4")),
                                             helpText("OR"),
                                             fileInput("disease2", "Upload a Disease Signature",
                                                           multiple = FALSE,
                                                           accept = c(".txt")),
                                            # downloadButton("downloadData", "Download_Template", class="butt1",  tags$head(tags$style(".butt1{background-color:white;} .butt1{color: black;}")))
                                             
                                             downloadButton(outputId="downloadExample", "Disease Signature Example", class="btn-sm")
                                               ),
                                     mainPanel(
                                       p("More details coming soon."),
                                       tags$ul(
                                       tags$li(textOutput("selected_var")),
                                       tags$li(textOutput("selected_var2")),
                                       tags$li(textOutput("selected_var3"))
                                             ),
                                       plotlyOutput("plot1"),
                                       br(),
                                       br(),
                                       br(),
                                       br(),
                                       downloadButton("downloadData", "Download Synergy Plot Data"),
                                       downloadButton("downloadData_Drug_Sig", "Download Drug Signatures"),
                                       DT::dataTableOutput("view1")
                                       #tableOutput("table1")
                                     ))
                                     
                            ))),
                  ### ABOUT UI ####
                  tabPanel("About",
                           mainPanel(
                             h4(strong(tags$u("Citation: "))),
                             p("Stathias, V., Jermakowicz, A.M., Maloof, M.E., Forlin, M., Walters, W., Suter, R.K., Durante, M.A., Williams, S.L., Harbour, J.W., Volmar, C.H., et al. (2018). Drug and disease signature integration identifies synergistic combinations in glioblastoma.", a("Nature Communications 9, 5315",target="_blank",href="https://doi.org/10.1038/s41467-018-07659-z")),
                             br(),
                             br(),
                             h4(strong(tags$u("Acknowledgements: "))),
                             p("This research was supported by grant U54HL127624 awarded by the National Heart, Lung, and Blood Institute through funds provided by the trans-NIH Library of Integrated Network-based Cellular Signatures (LINCS) Program (http://www.lincsproject.org/) and the trans-NIH Big Data to Knowledge (BD2K) initiative (https://datascience.nih.gov/)."),
                             br(),
                             br(),
                             h4(strong(tags$u("Code Availability: "))),
                             a("https://github.com/schurerlab/SynergySeq",target="_blank",href="https://github.com/schurerlab/SynergySeq")
                             ))
                  
              
                ))
                                     
               
server <- function(input, output) {
  
#### Server TAB1 #### 
datasetInput_Dataset <- reactive({
                                  if (is.null(input$L1000_Dataset))
                                  {return()}
    
                                   switch(input$L1000_Dataset,
                                          "LINCS L1000 Dec 2015" = "data/OUT3_noDMSO_noUntreated_Regina_removed.txt",
                                          "LINCS L1000 March 2017" = "data/matPH3_2_1_0.2_0.3_L1000_Batch2017_Regina_removed.txt"
                                          # "LINCS Median Scores" = "data/TCS_L1000_Ph2_Lvl5_MedianB_01_15_2019.txt"
                                          
                                         )
                                })
  

Drugs_SigsR <- reactive({
                         Drugs_Sigs <- read.table(file=datasetInput_Dataset(),sep ="\t", header=TRUE)
                         Drugs_Sigs <- na.omit(Drugs_Sigs)
                         row.names(Drugs_Sigs) <-  as.character(Drugs_Sigs$Genes)
                         #Drugs <- as.character(Drugs_Sigs$Genes)
                         Drugs_Sigs <- Drugs_Sigs[,-1]
                         Drugs_Sigs
                       })


  
Drugs <- reactive({ row.names(Drugs_SigsR())})
  
  
  
output$ui <- renderUI({ selectInput("signature", "Choose a Reference Drug",choices = Drugs(),selected = "GBM_JQ1")})



values <- reactiveValues()



output$selected_var <- renderText({
                                   value2 <- as.numeric(100 - as.numeric(input$bins))
                                   paste("You have selected to use a ", value2,"% threshold for the gene consensus score. Genes with the lowest ",input$bins,"% scores will be filtered out",sep = "")
                                   })

  
  
  
datasetInput_Dis <- reactive({
                               if(is.null(input$disease2))
                                  {
                                   switch(input$disease,
                                          "Glioblastoma TCGA (GBM)" = "data/TCGA_GBM_Signature.txt",
      "Colon TCGA (CRC)" = "data/TCGA_CRC_Signature.txt",
      "Breast TCGA (BRCA)" = "data/TCGA_BRCA_Signature.txt",
      "PDX GBM Group 1" = "data/Table_G1_1_PDX_Group1_L1000_only.txt",
      "PDX GBM Group 2" = "data/Table_G2_1_PDX_Group2_L1000_only.txt",
      "PDX GBM Group 3" = "data/Table_G3_1_PDX_Group3_L1000_only.txt" ,
      "PDX GBM Group 4" = "data/Table_G4_1_PDX_Group4_L1000_only.txt") 
    }
    else
    {
      input$disease2$datapath
    }
  })
 # JQ1_Sig <- reactive({read.table(file=datasetInput_Sig(),sep ="\t", header=TRUE)})
  JQ1_Sig <- reactive({
                      T <-t(Drugs_SigsR()[as.character(input$signature),])
    
                      })
  
  TCGA_Sig <- reactive({ read.table(file=datasetInput_Dis(),header = TRUE,sep = "\t")})

  
  output$plot1 <- renderPlotly({
    JQ1_Sig_v <- JQ1_Sig()
    #JQ1_Sig_v <- t(Drugs_Sigs["JQ1",])
    JQ1_Sig_v<- data.frame(JQ1_Sig_v,Genes=row.names(JQ1_Sig_v))
    colnames(JQ1_Sig_v) <- c("JQ1","Genes")
    TCGA_Sig_v <- TCGA_Sig()
    #TCGA_Sig_v <- read.table(file="data/TCGA_GBM_Signature.txt",header = TRUE,sep = "\t")
    V <- unique(as.numeric(as.character(JQ1_Sig_v$JQ1)))
    V2 <- max(abs(V))
    V4 <- as.numeric(input$bins)/100
    #V4 <- as.numeric(30/100)
    V5 <- V2*V4
    V6 <- round(V5)
    JQ1_Sig_v$JQ1 <- as.numeric(as.character(JQ1_Sig_v$JQ1))
    JQ1_Sig_v2 <- JQ1_Sig_v[which(JQ1_Sig_v$JQ1 > V6 | JQ1_Sig_v$JQ1 < -V6),]
    JQ1_Sig_v2 <- JQ1_Sig_v2[which(JQ1_Sig_v2$JQ1 > 0 | JQ1_Sig_v2$JQ1 < 0),]
    jq1_genes <- as.character(JQ1_Sig_v2$Genes)
    Drugs_Sigs2 <- Drugs_SigsR()[,jq1_genes]
    tt <-  apply(Drugs_Sigs2,1,function(x){x*JQ1_Sig_v2$JQ1})

    #this is to calculate how similar are the drug signatures to jq1 (ratio of # of genes that have same direction with the JQ1 signature devided with # of genes that are disocrdant to JQ1)
    tt2 <-  apply(tt,2,function(x) { a <- sum(x>0)
    b <- sum(x<0)
    b[b==0] <- 1 # replace b with 1 so we can devide by that number. Another way would be to add one to both a and b
    c <- a/b
    c
    })
    tt3 <- as.data.frame(tt2)
    tmax <- max(tt3)
    tt4 <- tt3/tmax
    ### ### ### ### ### ### ### ### ### ### ### ###
    #this is to calculate the ratio of the genes that are discordant to the Disease Signature (and are not affected by JQ1) devided by the # of genes that are concordant to the Disease Signature (and non-JQ1)
   
    jq1_genes2 <- as.character(JQ1_Sig_v[which(JQ1_Sig_v$JQ1==0),2])
    #01/23/2019 jq1_genes2 <- as.character(JQ1_Sig_v[which(JQ1_Sig_v$JQ1 < V6 & JQ1_Sig_v$JQ1 > -V6),2])
    
    genes <- colnames(Drugs_SigsR())
    TCGA_Sig_v$log2FoldChange <- as.numeric(TCGA_Sig_v$log2FoldChange)
    TCGA_Sig_v2 <- TCGA_Sig_v[,-1,drop=FALSE]
    TCGA_Sig_v3 <- aggregate(TCGA_Sig_v2, by = list(TCGA_Sig_v$Genes),FUN=mean) # this is in case we have duplicate gene symbols in the signature
    non_jq1_genes <- intersect(as.character(TCGA_Sig_v3$Group.1),jq1_genes2)
    Drugs_Sigs3 <- Drugs_SigsR()[,non_jq1_genes]
    row.names(TCGA_Sig_v3) <- as.character(TCGA_Sig_v3$Group.1)
    TCGA_Sig_v4 <- TCGA_Sig_v3[non_jq1_genes,]
    
    pp <-  apply(Drugs_Sigs3,1,function(x){x*TCGA_Sig_v4$log2FoldChange})
    pp2 <-  apply(pp,2,function(x) { 
                                    aa <- sum(x>0)
                                    bb <- sum(x<0)
                                    aa[aa==0] <- 1 # replace bb with 1 so we can devide by that number. Another way would be to add one to both aa and bb
                                    cc <- bb/aa
                                    
                                    dd<- c(aa,bb,cc)
                                   })
    pp3 <- as.data.frame(t(pp2))
    pmax <- max(pp3[,3])
    pp4 <- pp3/pmax
    Final <- merge(pp4,tt4,by="row.names")
    #text2 <- paste(input$signature,"_Orthogonality",sep="")
    colnames(Final) <- c("Drug","Disease_Same","Disease_Opp","Disease_Discordance","Reference_Drug_Orthogonality")
    Final
    #values$Final2 <- data.frame(Final)
   Final[,2] <- round(Final[,2],digits=3)
   Final[,3] <- round(Final[,3],digits=3)
   
   values$Final2 <- merge(Final,SM_MOA,by.x="Drug",by.y="Drugs",all.x = TRUE)
    plot_ly(Final, x = ~Reference_Drug_Orthogonality, y = ~Disease_Discordance,type = 'scatter',alpha=0.7,marker = list(size = 14),
            mode = 'markers',hoverinfo= 'text',text=~paste(Drug,'<br>',"Ratio:",Disease_Discordance,Reference_Drug_Orthogonality)) %>% layout(dragmode = "select")
    
    #library(ggplot2)
    #plotly_IMAGE(p, format = "pdf", out_file = "output.pdf")
    #ggplot(Final, aes(x=JQ1_Orthogonality, y=Disease_Discordance)) + 
      #geom_point(size=4,alpha =0.7,color="#0771E4",shape=16)+
     # labs(
     #      x="JQ1_Orthogonality", y = "Disease_Discordance")+
     # theme_minimal()  
    
    
    })
  
  # output$table1 <- renderTable({
  #                               
  #                               s <- event_data("plotly_selected")
  #                               values$Final2$Reference_Drug_Orthogonality <- as.character(values$Final2$Reference_Drug_Orthogonality)
  #                               values$Final2$Disease_Discordance <- as.character(values$Final2$Disease_Discordance)
  #                               values$Final2[which(values$Final2$Reference_Drug_Orthogonality %in% s$x & values$Final2$Disease_Discordance %in% s$y)  ,]
  #                             })
  
  output$view1 <- DT::renderDataTable(escape=FALSE,{ 
    s <- event_data("plotly_selected")
   values$Final2
   values$Final2[which(values$Final2$JQ1_Orthogonality == s$x & values$Final2$Disease_Discordance == s$y)  ,]
    values$Final2[which(values$Final2$JQ1_Orthogonality %in% s$x & values$Final2$Disease_Discordance %in% s$y)  ,]
    
    values$Final2[,c(1,7,4,5,8,9)]
    
  })
  
  
  
  output$selected_var2 <- renderText({
    cols <- as.character(colnames(Drugs_SigsR()))
    TCGA_Sig2 <- TCGA_Sig()
    length2 <- intersect(cols,as.character(TCGA_Sig2$Genes))
    paste("The Disease Signature has ",length(length2)," genes that are measured by the L1000 Platform")
  })
  
  
  output$selected_var3 <- renderText({
    JQ1_Sig3 <- JQ1_Sig()
    JQ1_Sig3<-  data.frame(JQ1_Sig3,Genes=row.names(JQ1_Sig3))
    colnames(JQ1_Sig3) <- c("JQ1","Genes")
    T <- unique(as.numeric(as.character(JQ1_Sig3$JQ1)))
    T2 <- max(abs(T))
    T4 <- as.numeric(input$bins)/100
    T5 <- T2*T4
    T6 <- round(T5)
    JQ1_Sig3$JQ1 <- as.numeric(as.character(JQ1_Sig3$JQ1))
    JQ1_Sig3_2 <- JQ1_Sig3[which(JQ1_Sig3$JQ1 > T6 | JQ1_Sig3$JQ1 < -T6),]
    JQ1_Sig3_2 <- JQ1_Sig3_2[which(JQ1_Sig3_2$JQ1 > 0 | JQ1_Sig3_2$JQ1 < 0),]
    paste("The",input$signature," signature has",length(JQ1_Sig3_2$Genes),"genes that are measured by the L1000 Platform")
  })
  
 
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("output", ".txt", sep = "")
    },
    
    content = function(file) {
      write.table(values$Final2 , file, row.names = TRUE,sep="\t")
      
    },
    contentType="text/plain"
  )
  
  
  
  
  output$downloadData_Drug_Sig <- downloadHandler(
    filename = function() {
      paste("Drug_Signature_Table", ".txt", sep = "")
    },
    content = function(file) {
      write.table(Drugs_SigsR() , file, row.names = TRUE,sep="\t")
      
    },
    contentType="text/plain"
  ) 
  
  
  output$downloadExample <- downloadHandler(
    filename = function() {
      paste("Drug_Signature_Example", ".txt", sep = "")
    },
    content = function(file) {
      write.table(TCGA_Sig() , file, row.names = FALSE,sep="\t")
      
    },
    contentType="text/plain"
  ) 
  
  
  
#### Server TAB2 ####
  
  datasetInput_Dataset_N <- reactive({
                                       if (is.null(input$L1000_Dataset_N))
                                       return()
                                       switch(input$L1000_Dataset_N,
                                              "LINCS L1000 Dec 2015" = "data/OUT3_noDMSO_noUntreated_Regina_removed.txt",
                                              "LINCS L1000 March 2017" = "data/matPH3_2_1_0.2_0.3_L1000_Batch2017_Regina_removed.txt"
                                             )
                                    })
  
  
  Drugs_SigsR_N <- reactive({
                              Drugs_Sigs_N <- read.table(file=datasetInput_Dataset_N(),sep ="\t", header=TRUE)
                              Drugs_Sigs_N <- na.omit(Drugs_Sigs_N)
                              row.names(Drugs_Sigs_N) <-  as.character(Drugs_Sigs_N$Genes)
                              Drugs_Sigs_N <- Drugs_Sigs_N[,-1]
                              Drugs_Sigs_N 
                           }) 
  
  
  Drugs_Ν <- reactive({
                     as.character(row.names(Drugs_SigsR_N()))
                    })
   
  
  output$ui_N <- renderUI({ 
                            selectInput(inputId="signature_N",label="",choices = Drugs_Ν(),selected = "JQ1")
                         })
  
  
  values_N <- reactiveValues()
  #values_N$OUT2 <- apply(Drugs_SigsR_N(),2,as.numeric) 
  #row.names(values_N$OUT2) <- row.names(Drugs_SigsR_N())
  #valuesN$Cor1 <- cor(t(values_N$OUT2),t(values_N$OUT2), method="spearman")
  #valuesN$Cor1 <- na.omit(valuesN$Cor1)

  Correlation_N <- reactive({ 
    Ta1 <- apply(Drugs_SigsR_N(),2,as.numeric) 
    row.names(Ta1) <- row.names(Drugs_SigsR_N())
    Cor1 <- cor(t(Ta1),t(Ta1), method="spearman")
    Cor1                              
                                 })
  #nodes <- data.frame(id = row.names(Correlation_N()))
  #edges <- data.frame(from = c(1,2), to = c(2,3))
  #visNetwork(nodes, edges, width = "100%")
  
  output$network <- renderVisNetwork({
                                       nodes <- data.frame(id = row.names(Correlation_N()),label=row.names(Correlation_N()))
                                       V1 <- upper.tri(Correlation_N(),diag = TRUE)
                                       CD1 <- Correlation_N()
                                       CD1[V1] <- NA
                                       edges <- melt(CD1, na.rm = TRUE)
                                       
                                       edges <- edges[which(edges$value > as.numeric(input$bins_N) ),1:2]
                                       colnames(edges) <- c("from","to")
                                       
                                      visNetwork(nodes, edges, width = "100%")})
  

}

shinyApp(ui, server)