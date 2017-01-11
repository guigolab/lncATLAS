library(shiny)
source("mysqlHelper.R")


#cn <- lncatlasConnect()

#gene.info <- getTable(cn,"genes", whole_table = FALSE,
                      #column = c("gene_name","ensembl_gene_id"),
                      #condition = "coding_type = 'nc'")
#dbDisconnect(cn)

#gene.info <- gene.info <- readRDS("gene.info.Rds")
#gene.name <- gene.info$gene_name
#gene.id <- gene.info$ebsembl_gene_id


cn <- lncatlasConnect()

cl.name <- getTable(cn,"expression_sites", whole_table = FALSE,
                      column = "name")$name

dbDisconnect(cn)






### SAFE BOX


# ratios.bp.df <- readRDS("../data/ratios_lnc.Rds")
# names(ratios.bp.df)


#gene.name.safe <- as.character(genes.df$Associated.Gene.Name)

verarrow <- HTML("<div class = 'row'>
                    <div class = 'container-fluid'>
                     <p class = 'arrows'>&#8593;</p> <h3> Cytoplasmic</h3>
                    </div></div>
                    <div class = 'row' style = 'height:200px'>
                    </div>
                    <div class = 'row'>
                    <div class = 'container-fluid'>
                    <h3> Nuclear</h3> <p class = 'arrows'>&#8595;</p>
                    </div></div>
                    ")



# Define the overall UI
shinyUI(

  # first the page
  navbarPage("lncATLAS", inverse = TRUE,
    header = tags$head(
      tags$link(rel = "stylesheet", type="text/css", href = "https://fonts.googleapis.com/css?family=Secular+One"),
      tags$style(HTML(
        "
        ul.nav-pills {
        top: 25px;
        position: fixed;
        }
        .jumbotron p {
        font-size: 14px;
        }
        h1 {
        font-family: 'Secular One', sans-serif;
        font-size: 80px;
        text-align: center;
        }
        body {
        position: relative;
        }
        h2{
        font-family: 'Secular One', sans-serif;
        font-size: 35px;
        }
        h3{
        font-family: 'Secular One', sans-serif;
        font-size: 30px;
        }
        h4{
        text-align:center;
        }
        #retrieve{
            text-align:center;
        }
        #svgMain {
            margin-left:auto;
            margin-right:auto;
            display:block;}
        .s1{
            color: chocolate;
        }
        .s3{
            color: olive;
        }
        .s2{
            color: navy;
        }
        .arrows{
          font-size: 100px;
        }
        .shiny-progress .progress {
        position: absolute;
        width: 100%;
        top: 0px;
        height: 15px;
        margin: 0px;
        }
        .shiny-progress .bar {
        opacity: 0.7;
        transition-duration: 500ms;
        }
        .shiny-progress .progress-text {
        position: absolute;
        right: 10px;
        height: 50px;
        width: 240px;
        background-color: #eef8ff;
        margin: 0px;
        padding: 2px 3px;
        opacity: 0.85;
        }
        .shiny-progress .progress-text .progress-message {
        padding: 0px 3px;
        font-weight: bold;
        font-size: 90%;
        }
        .shiny-progress .progress-text .progress-detail {
        padding: 0px 3px;
        font-size: 80%;
        }
        .top-buffer { margin-top:250px; }
        .glyphicon-remove { color: #e05959;}
        .glyphicon-ok { color: #acf274;}
        .shiny-output-error-validation {color: #ed8b36;font-size: 25px;}
        "))
  ),
    tabPanel("lncATLAS",
    # then 3 rows
    fluidRow(
      column(2, offset = 1,hr(),
             HTML('<img src="lncatlas.logo2.svg" class="img-fluid center-block" alt="Logo HERE" width = 250px>')),
              # img(src = "", class="center-block;")
      column(6,
             h1("lncATLAS")
      ),
      column(2, hr(),
             HTML('<img src="CRG_logo_colour.svg" class="img-fluid" alt="Logo HERE" width = 250px>')
              # img(src = "", class="center-block;")

      )

    ),fluidRow(
    fluidRow( id="section1",
      hr(),
#       column(2,
#              HTML('
#                   <ul class="nav nav-pills nav-stacked" data-spy="affix" data-offset-top="205">
#                   <li class="active"><a href="#section1">Search Box</a></li>
#                   <li><a href="#section2">Distributions</a></li>
#                   <li><a href="#section3"></a></li>
#                   </ul>
#                   ')
#              ),
      column(4,offset = 2,
             tags$div(
             h2("Search box:"),
             h3("Quick start:"),
             p("Find the subcellular localisation of your long non-conding RNA of interest:"),
             p("1) Enter the official GENCODE gene name or ENSEMBL gene id below (max. 3 genes)"),
             p("2) Check for the ID to appear below and press Go to generate the plots"),
             h3("Search by gene name:"),
             textInput('e1', "Enter your lncRNA of interest (e.g., MALAT1)"),
             #selectizeInput('e1', 'Enter a Gene Name:',
              #              choices = gene.name,
              #              options = list(maxOptions = 5,maxItems = 3),
              #              multiple = TRUE,
              #              selected = "MALAT1"),

              #
              conditionalPanel(
                condition = "input.e1 == '' & (input.refnucl == '' & input.refcyto == '' & input.refdual == '')",
                textInput("geneIdtmp", label = h3("Enter a Ensembl ID:"),
                          value = "")
                ),
             conditionalPanel(
               condition = "input.e1 != '' | input.refnucl != '' | input.refcyto != '' | input.refdual != ''",
                uiOutput("ID")
               ),
             actionButton("go", "GO",icon("send",lib="glyphicon")),
             h3("Add reference genes:"),
             fluidRow(
             column(4,offset = 0,
                    checkboxGroupInput("refnucl", label="Nuclear Genes",
                                       choices =
                                         c("NEAT1",
                                           "MALAT1"),
                                       inline = FALSE)),
             column(4,offset = 0,
                    checkboxGroupInput("refcyto", label="Cytoplasmic Genes",
                                       choices =
                                         c("DANCR","H19"),
                                       inline = FALSE)
             ),
             column(4,offset = 0,
                    checkboxGroupInput("refdual", label="Other Genes",
                                       choices =
                                         c("HOTAIR","TUG1"),
                                       inline = FALSE)
             )
             )
      ), class="jumbotron") ,
        column(4, offset =0 ,tags$div(
          h2("Help Box:"),
          p("Search for your lncRNAs of interest and press GO to obatain the plots."),
          h3("What is being displayed?"),
          p('LncATLAS displays the subcellular localisation for user-selected lncRNAs. Only GENCODE-annotated lncRNA genes are present, and may be accessed using their identifier (ENSG...) or official Gene Name. The localisation of your selected gene, or genes, will be displayed below for available cell types and cellular compartments. Please note that some plots display data for individual cell types, that may be selected using "Select a Cell Line" button.'),
          p('This localisation is expressed in units of Relative Concentration Index (RCI) - a comparison of the concentration of a gene, per unit mass of RNA, between two cellular compartments. For more information about how this data was analysed, please consult the "About LncATLAS" tab above.'),
          p('Raw data for individual genes, or all genes, may be accessed using the "Download Raw Data" button below, or from the "Get Raw Data" tab above, respectively. All plots may be downloaded using "Download Plot" buttons.'),
          downloadButton('downloadData', 'Download raw data')
          ,class="jumbotron"))
      )
    ),
    fluidRow(column(8, offset=2,align="center", h3("S1 - Inspect the cytoplasmic-nuclear localisation of your gene of interest (GOI)"),hr())),
    fluidRow(column(8, offset=2,align="center",h2("Plot 1 - Cytoplasmic/Nuclear Localisation: Real values (all cell types)", style="text-align: center;",
       class = "s1"))),
    fluidRow(column(2, align = "center",verarrow),
      column(8, align="center",
             plotOutput("ratioPlot",width = "100%", height = "650px")
    )),
    fluidRow(column(8, offset=2,align="center",downloadButton('downloadPlotR1', 'Download plot'))),
    fluidRow(class="top-buffer"),
    fluidRow(column(8, offset=2,align="center", h3("S2 - Inspect the cytoplasmic-nuclear localisation of your GOI within the distribution of all genes"),hr())),
    fluidRow(
      column(8, offset=2, align="center",h2("Plot 2 - Cytoplasmic/Nuclear Localisation: Distribution (all cell types)", style="text-align: center;",
         class = "s2")
      )),
    fluidRow(column(8,offset=2,align="center", HTML("<p><b> Note: </b> In the next plot <code>n</code> indicates the total number of genes in each group and <code>m</code> the median RCI value per group. The group percentile corresponding to each gene is also displayed next to the gene point.</p>"))),
    fluidRow(id = "section2",
      column(2,align = "center",verarrow),
      column(10, align="center",
            plotOutput("distributionAlt",width = "100%", height = "700px"),
            downloadButton('downloadPlotR2', 'Download plot')
      )),
      fluidRow(class="top-buffer"),
      fluidRow(
        column(6, offset = 3, align="center",
          h2("Plot 3 - Cytoplasmic/Nuclear Localisation: Distribution (individual cell type)", style="text-align: center;",
            class = "s2")
        )),
      fluidRow(
        column(2, offset =1, h3("Select a Cell Line:"),
                  selectInput("cellLine","Choose a Cell Line to get a zoom in into the distribution.",cl.name),
                  class="jumbotron"),
        column(7, align="center",
               plotOutput("distribution",width = "100%", height = "575px")
               )),
      fluidRow(column(3,offset=3,align="center",h3("Nuclear"),
               HTML("<p class = 'arrows'> <span>&#8592;</span> </p>")),
               column(3,align="center", h3("Cytoplasmic"),
               HTML("<p class = 'arrows'> <span>&#8594;</span> </p>"))
               ),
        fluidRow(column(2,offset=5,align="center",downloadButton('downloadPlotD1', 'Download plot'))),
        fluidRow(class="top-buffer")

      ,
    fluidRow(
      column(6,align="center",offset=3,
        h2("Plot 4 - Cytoplasmic/Nuclear Localisation: Comparison with expression (individual cell type)",style="text-align: center;",
          class = "s2")
      )),
    fluidRow( column(2,align = "center",verarrow),
      column(6,offset=1,align="center",
            plotOutput("distribution2D",width = "100%", height = "575px")
      )),
    fluidRow(column(2,offset=5,
    downloadButton('downloadPlotD2', 'Download plot'))),
    fluidRow(class="top-buffer"),
    fluidRow(column(8, offset=2,align="center", h3("S3 - Inspect the localisation of your GOI at sub-compartment level"),hr())),
    fluidRow( id = "section3",
      column(8, offset = 2, align="center",
             h2("Plot 5 - Subcytoplasmic, Subnuclear Localisation: K562 cells", class = "text-align s3"),
             plotOutput("distroK",width = "90%", height = "450px"))
    ),
fluidRow(id = "p",
         column(4, offset = 4, align="center",

         HTML("<p><b>Note</b> that data for <b>chromatin, nucleoplasm and nucleolus </b> in this plot were obtained from a <b>total RNA</b> extraction contrary to the other figures that were obtained with a enriched polyA sample. <br> <b> Note 2: </b>  In the plot <code>n</code> indicates the total number of genes in each group. The group percentile corresponding to each gene is also displayed next to the gene point.<br></p>"),
         downloadButton('downloadPlotK', 'Download plot')))
  ),
  tabPanel("Get Raw Data",
        h2("Retrieve raw data from ENSEMBL IDs"),
        p("Enter in the box a list of ENSMBL IDs, conding or non-coding to retrieve the raw values from our database."),
        radioButtons("downltype", label = h3("Choose the data to retrieve"),
    choices = list("RCI - Localisation values" = 1, "All - Localisation and expression" = 2, "FPKM - Expression values" = 3),
    selected = 1),
        hr(),
        fluidRow(
          column(width=4,offset=4,
              HTML('<textarea id="listretrive" rows="10" cols="40">ENSG00000251562\n...</textarea>'))
        ),
        fluidRow(column(width=2,offset=5,
          downloadButton('downloadData2', 'Download Selected raw data'),
          class="center-block;",style="margin-top:15px"
        )),
         fluidRow(column(width=2,offset=5,
          downloadButton('retrieveall', 'Download All raw data'),
          class="center-block;",style="margin-top:15px"
        )),
        fluidRow(column(width=8,offset=2,
                        tableOutput("retrieve"),style="margin-top:15px"
        ))

  ),
  tabPanel("About",
         includeHTML("info_page.html")
  )
  )
)
