tabPanel("Introduction",
         #mainPanel(
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
           # )
         ))