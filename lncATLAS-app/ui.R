################################################################################
################################################################################
#                             lncATLAS -- User Interface
#
#         author: David Mas-Ponte @CRG
#         R Shiny app - UI script
#
################################################################################
################################################################################


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


# verarrow is a html object to place vertical arrows in the plots.
# it is useful to hint the user the cyto and nuclear direction in the
# plots
verarrow <- HTML("<div class = 'row'>
                    <div class = 'container-fluid'>
                     <p class = 'arrows'>&#8593;</p> <h3> Cytoplasmic</h3>
                    </div></div>
                    <div class = 'row' style = 'height:150px'>
                    </div>
                    <div class = 'row'>
                    <div class = 'container-fluid'>
                    <h3> Nuclear</h3> <p class = 'arrows'>&#8595;</p>
                    </div></div>
                    ")
# same as before but here with different heigh
verarrow2 <- HTML("<div class = 'row'>
                    <div class = 'container-fluid'>
                     <p class = 'arrows'>&#8593;</p> <h3> Cytoplasmic</h3>
                    </div></div>
                    <div class = 'row' style = 'height:240px'>
                    </div>
                    <div class = 'row'>
                    <div class = 'container-fluid'>
                    <h3> Nuclear</h3> <p class = 'arrows'>&#8595;</p>
                    </div></div>
                    ")

# Define the overall UI -  general function
shinyUI(

  # first the page
  navbarPage("lncATLAS", inverse = TRUE,
    header = tags$head(
      tags$link(rel = "stylesheet", type="text/css", href = "https://fonts.googleapis.com/css?family=Secular+One"), # adding fonts
      tags$link(rel = "stylesheet", type = "text/css", href = "main.css")
  ),
    tabPanel("lncATLAS",
    # then 3 rows
    fluidRow(class="center-block", # row 1 - title
      column(2,offset=1,
             HTML('<img src="lncatlas.logo2.svg" class="img-fluid center-block" alt="Logo HERE" width = 250px>')),
              # img(src = "", class="center-block;")
      column(4,offset=1,
             h1("lncATLAS"),class="center-text"
      ),
      column(2,
             HTML('<img src="CRG_logo_colour.svg" class="img-fluid center-block" alt="Logo HERE" width = 150px>')
      )

    ),
    conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                 tags$div("Shiny is busy...",id="loadmessage")
),
    fluidRow(
    fluidRow( id="section1", # row 2 the search row
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
    column(10,offset=1,

    tags$div(class = "row equal",
      column(5,offset =0,
             h2(align = "center","Search box:"),
             h3(align="center","Quick start:"),
             HTML("<p>Find the subcellular localisation of your long non-coding RNA of interest:</p>
	       <ol>
	       	<li>Enter the official GENCODE gene name (e.g. Malat1) or ENSEMBL gene ID (e.g. ENSG...) below (max. 3 genes)</li>
	       	<li>Check for the ID to appear below and press Go to generate the plots</li>
	       </ol>"),
             column(12,align = "center",

	     h3("Search by GENCODE gene name:"),

             textInput('e1', "Enter your lncRNA of interest (e.g., MALAT1)")
	     ),
             #selectizeInput('e1', 'Enter a Gene Name:',
              #              choices = gene.name,
              #              options = list(maxOptions = 5,maxItems = 3),
              #              multiple = TRUE,
              #              selected = "MALAT1"),

              #
	    column(12,align="center",
              conditionalPanel(
                condition = "input.e1 == '' & (input.refnucl == '' & input.refcyto == '' & input.refdual == '')",
                textInput("geneIdtmp", label = h3("Search by ENSEMBL gene ID:"),
                          value = "")
                ),
             conditionalPanel(
               condition = "input.e1 != '' | input.refnucl != '' | input.refcyto != '' | input.refdual != ''",
                uiOutput("ID")
               ),
               conditionalPanel( # this is the good gene
                 condition = "((input.e1 == '' & (input.refnucl == '' & input.refcyto == '' & input.refdual == '')) & input.geneIdtmp != '') | ((input.e1 != '' | input.refnucl != '' | input.refcyto != '' | input.refdual != '') & input.geneId != '' )",
                 actionButton("go",HTML(' GO <span class="glyphicon glyphicon-send" style="color:#229305"></span>'))
                 ),
                conditionalPanel(
                  condition = "((input.e1 == '' & (input.refnucl == '' & input.refcyto == '' & input.refdual == '')) & input.geneIdtmp == '') | ((input.e1 != '' | input.refnucl != '' | input.refcyto != '' | input.refdual != '') & input.geneId == '' )",
                  actionButton("nogo",HTML(' GO <span class="glyphicon glyphicon-remove" style="color:#93052b"></span>'))
                  )
              ),# here ends the column
	     HTML("<br>"),
             h3(align="center","Add reference genes:"),
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
      , class="jumbotron setpad center-block") ,
        column(5, offset =1 ,
          h2(align = "center","Help box:"),
          p("Search for your lncRNAs of interest and press GO to obtain the plots."),
          p("You can select at least 3 genes in both the search and reference inputs. If 3 are selected a high resolution screen is recommended."),
          h3(align = "center","What is being displayed?"),
          p(class="text-justify",'LncATLAS displays the subcellular localisation for user-selected lncRNAs. Only GENCODE-annotated lncRNA genes are present, and may be accessed using their identifier (ENSG...) or official Gene Name. The localisation of your selected gene, or genes, will be displayed below for available cell types and cellular compartments. Please note that some plots display data for individual cell types, that may be selected using "Select a Cell Line" button.'),
          p(class="text-justify",'This localisation is expressed in units of Relative Concentration Index (RCI) - a comparison of the concentration of a gene, per unit mass of RNA, between two cellular compartments. For more information about how this data was analysed, please consult the "About LncATLAS" tab above.'),
          HTML("<br>"),
          p(class="text-justify note",'Raw data for individual genes, or all genes, may be accessed from the "Get Raw Data" tab above.'),
          p(class="text-justify note",'All plots may be downloaded using "Download Plot" buttons.'),
          #column(12,align="center",downloadButton('downloadData', 'Download raw data')),
          class="jumbotron setpad center-block")) # here
      ))
    ),
    fluidRow(column(8, offset=2,align="center", h3("S1 - Inspect the cytoplasmic-nuclear localisation of your gene of interest (GOI)"),hr())),
    fluidRow(column(8, offset=2,align="center",h2("Plot 1 - Cytoplasmic/Nuclear Localisation: RCI and expression values (all cell types)", style="text-align: center;", # here the plots start
       class = "s1"))),
    fluidRow(column(2, align = "center",verarrow),
      column(9, align="center",
             plotOutput("ratioPlot",width = "100%", height = "700px")
    )),
    fluidRow(column(8, offset=2,align="center",downloadButton('downloadPlotR1', 'Download plot'))),
    fluidRow(class="top-buffer"),
    fluidRow(column(8, offset=2,align="center", h3("S2 - Inspect the cytoplasmic-nuclear localisation of your GOI within the distribution of all genes"),hr())),
    fluidRow(
      column(8, offset=2, align="center",h2("Plot 2 - Cytoplasmic/Nuclear Localisation: RCI distribution (all cell types)", style="text-align: center;",
         class = "s2")
      )),
    fluidRow(column(8,offset=2,align="center", HTML("<p><b> Note: </b> In the next plot <code>n</code> indicates the total number of genes in each group and <code>m</code> the median RCI value per group. The group percentile corresponding to each gene is also displayed next to the gene point.</p>"))),
    fluidRow(id = "section2",
      column(2,align = "center",verarrow),
      column(9, align="center",
            plotOutput("distributionAlt",width = "100%", height = "700px"),
            downloadButton('downloadPlotR2', 'Download plot')
      )),
      fluidRow(class="top-buffer"),
      fluidRow(
        column(6, offset = 3, align="center",
          h2("Plot 3 - Cytoplasmic/Nuclear Localisation: RCI distribution (individual cell type)", style="text-align: center;",
            class = "s2")
        )),
      fluidRow(
        column(2, offset =1, h3("Select a Cell Line:"),
                  selectInput("cellLine","Choose a cell line to zoom in on the distribution.",cl.name),
                  class="jumbotron", id="lowpad"),
        column(8, align="center",
               plotOutput("distribution",width = "100%", height = "575px")
               )),
      fluidRow(column(8,offset=3,
                fluidRow(style="padding-left: 65px;",
                    column(2,HTML("<p class='horarrow'>&#8592;</p>")),
                    column(3,
                      HTML("<p class='horarrowtext'>Nuclear</p>")),
                    column(3,offset = 2,
                      HTML("<p class='horarrowtext'>Cytoplasmic</p>")),
                    column(2,HTML("<p class='horarrow'>&#8594;</p>"))
                  ))),
        fluidRow(column(6,offset=4,align="center", HTML("<p><b> Note: </b> The blue colouring of the inside area corresponds to the genes with more extreme values than the gene selected. This follows a visualisation purpose to help users see the position of their gene in the distribution.</p>"))),
        fluidRow(column(8,offset=3,align="center",downloadButton('downloadPlotD1', 'Download plot'))),
        fluidRow(class="top-buffer")
      ,
    fluidRow(
      column(6,align="center",offset=3,
        h2("Plot 4 - Cytoplasmic/Nuclear Localisation: Comparison with expression (individual cell type)",style="text-align: center;",
          class = "s2")
      )),
    fluidRow( column(2,align = "center",verarrow2),
      column(8,offset=0,align="center",
            plotOutput("distribution2D",width = "100%", height = "700px")
      )),
    fluidRow(column(6,offset=3,
      HTML("<p><b>Note:</b> When 2 genes present similar values in both axis, one of
      them might get hidden to avoid problems in readability due to
      the overlap.</p>"))),
    fluidRow(column(2,offset=5,
    downloadButton('downloadPlotD2', 'Download plot'))),
    fluidRow(class="top-buffer"),
    fluidRow(column(8, offset=2,align="center", h3("S3 - Inspect the localisation of your GOI at sub-compartment level"),hr())),
    fluidRow( id = "section3",
      column(8,offset=2,
        h2("Plot 5 - Subcytoplasmic, Subnuclear Localisation: K562 cells", class = "text-align s3"))),
    fluidRow(
      column(2,align="center",style="padding-top: 20px;",
        HTML('<img src="a.svg" class="img-fluid" alt="enrich" heigth = 300px>')),
      column(8, align="center",
             plotOutput("distroK",width = "100%", height = "500px"))
    ),
fluidRow(id = "p",
         column(6, offset = 3, align="center",

         HTML("<p><b>Note:</b> Data for <b>chromatin, nucleoplasm and nucleolus </b> in this plot were obtained from a <b>total RNA</b> extraction contrary to the other figures that were obtained with a enriched polyA sample. <br> <b> Note 2: </b>  In the plot <code>n</code> indicates the total number of genes in each group. The group percentile corresponding to each gene is also displayed next to the gene point.<br></p>"),
         downloadButton('downloadPlotK', 'Download plot')))
  ),
  tabPanel("Get Raw Data",
	   fluidRow(column(12,align="center",
        	h2("Retrieve raw data from ENSEMBL IDs"),
        	p("Enter in the box a list of ENSEMBL IDs, coding or non-coding to retrieve the raw values from our database."))),
        fluidRow(column(4,offset=2,align="center",
	    	radioButtons("downltype", label = h3("Choose the data to retrieve"),
    choices = list("RCI - Localisation values" = 1, "All - Localisation and expression" = 2, "FPKM - Expression values" = 3),
    selected = 1)),
		 column(4,align="center",
			h3(align="center","Insert a ENSEMBL gene ID list:"),
			HTML('<textarea id="listretrive" rows="8" cols="40">ENSG00000251562\n...</textarea>')
        )),
        fluidRow(
         column(width=2,offset=4,
          downloadButton('retrieveall', 'Download All raw data'),
          class="center-block;",style="margin-top:15px"
        ),
        column(width=2,offset=0,
          downloadButton('downloadData2', 'Download raw data from the list'),
          class="center-block;",style="margin-top:15px"
        )),
        HTML('<br><p class="text-center"><b>Note</b> that you should wait until a table below the line is generated to download the raw data.</p>'),
        hr(),
        fluidRow(column(width=6,offset=3,
                        tableOutput("retrieve"),style="margin-top:15px"
        ))

  ),
  tabPanel("About",
         includeHTML("info_page.html")
  )
  )
)
