#
# In this app, you could input design parameters, constraints thresholds, and financial settings, 
# then an optimized trial design, in terms of budget friendly, as well as total cost/revenue, will show as outcome.
#
require(shiny)
require(shinyWidgets)
require(plotly)
require(shinyhelper)
require(DT)
require(OptTrialDesign)
source('str2vec.R')

ui <- navbarPage(title = "Optimal Design Strategy with Time-To-Event Endpoint",
                 tabPanel("Design",
                          fluidRow(
                            column(3, 
                                   h4("DESIGN PARAMETERS"),
                                   numericInput(inputId = 'alpha', label = "Alpha (one-sided)", min = 0, max = 1, value = 0.025, step = 0.001), 
                                   numericInput(inputId = 'power', label = "Power", min = 0, max = 1, value = 0.90, step = 0.01),
                                   helper(numericInput(inputId = 'alloc', label = "Percentage of Treatment Arm Patients", min = 0, max = 1, value = 0.5, step = 0.01),
                                          colour = "blue", type = "inline", content = "Allocation between two arms, i.e. treatment/(treatment + control), a numeric value between (0,1)"),
                                   style = "background-color: white; border-width: 0px"
                            ),
                            column(3, offset = 1,
                                   h4("TREATMENT ARM"),
                                   helper(
                                     numericInput(inputId = 'lambda1', label = "Median Survival Time", value = 10.5, min = 1, step = 0.1),
                                     colour = "blue", type = "inline", content = "Under the exponential dsitribution assumption, the median survial time (or loss to follow-up time in below) is simply log(2)/lambda, where lambda is the rate parameter of an exponential distribution"
                                   ),
                                   radioButtons(inputId = 'Censor1', label = 'Loss to Follow-up:', choices = c('Median (unit time)', 'Percentage')),
                                   conditionalPanel(condition = "input.Censor1 == 'Median (unit time)'",
                                                    numericInput('m.C1', 'Value', min = 0, value = 0),
                                                    helpText('Notice: Set 0 if no loss to follow-up',style = "font-size:12px")
                                   ),
                                   conditionalPanel(condition = "input.Censor1 == 'Percentage'",
                                                    numericInput('pct.C1', 'Probability (during a given time period)', min = 0, max = 1, value = NULL),
                                                    numericInput('time.C1', 'Time Period (unit time)', min = 0, value = NULL)
                                   ),
                                   style = "background-color: white; border-width: 0px"
                            ),
                            column(3, offset = 1,
                                   h4("CONTROL ARM"),
                                   numericInput(inputId = 'lambda2', label = "Median Survival Time", value = 7.5, min = 1, step = 0.1),
                                   radioButtons(inputId = 'Censor2', label = 'Loss to Follow-up:', choices = c('Median (unit time)', 'Percentage')),
                                   conditionalPanel(condition = "input.Censor2 == 'Median (unit time)'",
                                                    numericInput('m.C2', 'Value', min = 0, value = 0),
                                                    helpText('Notice: Set 0 if no loss to follow-up',style = "font-size:12px")
                                   ),
                                   conditionalPanel(condition = "input.Censor2 == 'Percentage'",
                                                    numericInput('pct.C2', 'Probability (during a given time period)', min = 0, max = 1, value = NULL),
                                                    numericInput('time.C2', 'Time Period (unit time)', min = 0, value = NULL)
                                   ),
                                   style = "background-color: white; border-width: 0px"
                            )
                          ),
                          tags$hr(),
                          #########################################################
                          fluidRow(
                            column(3, 
                                   helper(h4("FINANCIAL SETTINGS"),
                                          colour = "blue", type = "inline", content = "<p>Multiple inputs are allowed for c1, c2, and b. All combinations will be considered to find the optimal design.</p>
                                          <p>c0 is a constant value which is optional. It will not influence the optimal design but change the final net revenue or cost</p>"
                                   ),
                                   numericInput(inputId = 'c0', label = "Fixed Cost (c0, Unit/$)", value = 0, min = 0),
                                   textInput(inputId = 'c1', label = "*Additional Cost/Patient (c1, Unit/$)", value = "", placeholder = 'e.g. 10,12,16-20'),
                                   textInput(inputId = 'c2', label = "*Additional Cost/unit time (c2, Unit/$)", value = "", placeholder = 'e.g. 5,8,12-15'),
                                   textInput(inputId = 'b', label = "Sales Income/unit time (b, Unit/$)", value = "", placeholder = 'e.g. 50,60,70-75'),
                                   style = "background-color: white; border-width: 0px"
                            ),
                            column(3, offset = 1,
                                   helper(h4("TIMELINE SETTIGNS"),
                                          colour = "blue", type = "inline", 
                                          content = "Please keep time and expense unit the same across all inputs"),
                                   numericInput(inputId = 'l', label = "Time b/w Trial Start and LOE (loss of exclusivity) (l)", value = 180, min = 0),
                                   numericInput(inputId = 'l0', label = "Time b/w final analysis & market access (l0)", value = 10, min = 0),
                                   style = "background-color: white; border-width: 0px"
                            )
                          ),
                          tags$hr(),
                          #########################################################
                          fluidRow(
                            column(3, 
                                   helper(h4("DATA MATURITY REQUIREMENTS"),
                                          colour = "blue", type = "inline", content = "<p>C1-4 are the prespecified data maturity constraints.</p>
                                          <p>C1 puts constraint on the minimal follow-up time;</p>
                                          <p>C2 puts constraint on the ratio of events and samples;</p>
                                          <p>C3 puts constraint on the minimal probability of observing the median of KM curves; it is measured via Monte Carlo simulations.</p>
                                          <p>C4 puts constraint on the median of follow-up time.</p>
                                          <p>By filling in the values to active the contraints. For more details, please see the reference.</p>"
                                   ),
                                   numericInput(inputId = 't0', label = "Constraint #1 (C1)", value = 5, min = 0, step = 0.1),
                                   numericInput(inputId = 'e0', label = "Constraint #2 (C2)", value = 0.5, min = 0, max = 1, step = 0.01),
                                   textInput(inputId = 'p0', label = "Constraint #3 (C3)", value = ""),
                                   conditionalPanel(
                                     "input.p0 != ''",
                                     numericInput(inputId = 'nsim', label = "Number of simulations", value = 1000, min = 10, step = 1)
                                   ),
                                   numericInput(inputId = 'm0', label = "Constraint #4 (C4)", value = 5, min = 1, step = 0.1),
                                   style = "background-color: white; border-width: 0px"
                            ),
                            column(3, offset = 1,
                                   h4("CLINICAL MEANINGFUL TREATMENT EFFECT"),
                                   helper(radioButtons(inputId = "Aj", label = "Select One Criterion",
                                                c("Median Difference", "Median Ratio", "None"), inline = F, selected = "None"),
                                          colour = "blue", type = "inline", content = "<p>Statistical significance does not translate directly to the clinical meaningful results. 
                                          Here we provide two options to specify the difference between treatment and control arms in terms of clinical importance.</p>
                                          <p>Median Difference: MST(treatment) - MST(control) > Threshold value</p>
                                          <p>Median Ratio: MST(treatment) / MST(control) > Threshold value</p>
                                          <p>MST: Median Survival Time.</p>"
                                   ),
                                   uiOutput("mThreshold"),
                                   style = "background-color: white; border-width: 0px"
                            ),
                            column(3, offset = 1,
                                   h4("INPUT & GO"),
                                   helper(
                                     radioButtons(inputId = "r_Sa", label = "Select Input Variable",
                                                  c("Accrual Rate (patients/unit time)", "Accrual Period (unit time)"), inline = F),
                                     colour = "blue", type = "inline", content = "<p>Accrual Rate supports three types of input:</p>
                                     <p>1. Single value, indicating a uniform accrual rate;</p>
                                     <p>2. A vector, e.g., 10,20,25,28,30, indicating a piecewise accrual rate;</p>
                                     <p>3. A vector of length 3, e.g., 10,5,40, indicating a linearly increasing accrual rate with initial rate 10, insease by 5 each unit time until reaches the maximal rate 40.</p>
                                     <p>If Accrual Period is chosen, it directly assumes a uniform accrual rate.</p>"
                                   ),
                                   uiOutput("rSa"),
                                   
                                   #####################
                                   tags$hr(),
                                   ## action buttons
                                   actionButton(inputId = "GoButton", label = 'Go Design', icon = icon("flask"), style = "display: inline-block !important;"),
                                   actionButton(inputId = "reset", label = "Reset", icon = icon("redo"), style = "display:inline-block !important;"),
                                   style = "background-color: white; border-width: 0px"
                            )
                          ),
                          #########################################################
                          uiOutput("ResultsPanel"),
                          tags$footer(tags$hr(),
                                      p("Have a question? Spot an error? Send an email", 
                                        tags$a(href = "mailto:junyzhou@iu.edu", 
                                               tags$i(class = 'fa fa-envelope', style = 'color:#111111'), 
                                               target = '_blank'), style = "font-size: 80%"), 
                                      p("App created by Junyi Zhou (2019 Summer)", style = "font-size: 80%"),
                                      p("Last updated: April 2021", style = "font-size: 65%"),
                                      style = "
                                      bottom:0;
                                      width:100%;
                                      height:90px;   /* Height of the footer */
                                      color: black;
                                      padding: 10px;
                                      background-color: white;
                                      z-index: 1000;")
                          
                 ),
                 tabPanel("Reference", 
                          uiOutput("pdfview")
                 ),
                 tabPanel("About",
                          tags$div(
                            tags$h4("Background"),
                            "This project is developed by Junyi Zhou as a summer intern at",
                            tags$a(href="https://www.nektar.com/", "Nektar Therapeutics"), 
                            "2019, mentored by Dr. Yi Liu.",
                            
                            tags$br(),tags$br(),tags$h4("Code"), 
                            "The source code of this Shiny App is available on", 
                            tags$a(href="https://github.com/junyzhou10/OptTrialDesign_ShinyApp", "Github."),
                            "To run the app on local device, R package", 
                            tags$a(href="https://github.com/junyzhou10/OptTrialDesign", "OptTrialDesign"),
                            "is required. To install the package, please use devtools::install_github('junyzhou10/OptTrialDesign').",
                            
                            tags$br(),tags$br(),tags$h4("Author & Developer"),
                            p("Junyi Zhou, Department of Biostatistics, School of Medicine, Indiana University")
                          )
                 ),
                 position = "static-top"
)





server <- function(input, output, session) {
  observe_helpers()
  Dat = reactiveValues(results = NULL)
  
  output$rSa <- renderUI(
    if (input$r_Sa == 'Accrual Rate (patients/unit time)'){
      textInput(inputId = 'val', label = "", value = "", placeholder = 'e.g. 25; or 10,20,25,28,30...')
    } else {
      textInput(inputId = 'val', label = "", value = "", placeholder = 'e.g. 36')
    }
  )
  
  output$mThreshold <- renderUI(
    if (input$Aj == 'Median Difference'){
      textInput(inputId = 'd0_r0', value = "", label = 'Threshold Value', placeholder = 'e.g. 3')
    } else if (input$Aj == 'Median Ratio') {
      textInput(inputId = 'd0_r0', value = "", label = 'Threshold Value', placeholder = 'e.g. 1.2')
    }
  )
  
  output$ResultsPanel <- renderUI(
    if (!is.null(Dat$results)) { 
      fluidRow(
        tags$hr(),
        column(width = 12,
               wellPanel(
                 h4("Optimal Designs Corresponding to Various Settings"),
                 div(DT::DTOutput("table1"), style = "font-size: 80%;"),
                 fluidRow(
                   column(2, offset = 10,
                          tags$br(),
                          downloadButton(outputId = 'downTable', label = 'Download Table')
                   )
                 )
               )
        ),
        column(width = 5,
               wellPanel(
                 h4("Maturity Requirement Conditions"),
                 div(DT::DTOutput("table2"), style = "font-size: 80%;")
               )
               
        ),
        column(width = 7,
               wellPanel(
                 h4("Optimal Design Frontier"),
                 plotlyOutput("plotOpt"),
                 helpText('Note: only plot the optimal one if multiple scenarios are given'),
                 div(
                   style = "position: absolute; right: 1.3em; bottom: 1.8em;",
                   dropdown(
                     downloadButton(outputId = "downPNG", label = "PNG", style = "width:70px;font-size:11px"),
                     downloadButton(outputId = "downJPG", label = "JPG", style = "width:70px;font-size:11px"),
                     downloadButton(outputId = "downPDF", label = "PDF", style = "width:70px;font-size:11px"),
                     size = "s",
                     icon = icon("download", class = "opt"), 
                     up = TRUE,
                     right = TRUE
                   )
                 )
                 # downloadButton(outputId = 'downPlot', label = 'Download Plot'),
                 # radioButtons(inputId = 'format', label = 'Choose output format:',
                 #              choices = c('pdf', 'png', 'jpeg'), selected = 'pdf', inline = T)
               )
        )
      )
      #####################
    }
  )
  
  ## render pdf review
  output$pdfview <- renderUI({
    tags$iframe(style="height:1000px; width:100%; scrolling=yes",
                src="Dgn_stgy_Junyi_Nektar.pdf")
  })
  
  
  
  ## Triggered actions by 'Go Design' button
  observeEvent(input$GoButton, {
    alpha = input$alpha                                        #1
    power  = input$power                                       #2
    alloc = input$alloc                                        #3
    lambda1 = log(2)/input$lambda1                             #4
    lambda2 = log(2)/input$lambda2                             #5
    if (input$Censor1 == 'Median (unit time)') {
      eta1 = ifelse(input$m.C1 == 0, 0, log(2)/input$m.C1)     #6
    } else if (input$Censor1 == 'Percentage') {
      eta1 = -log(1 - input$pct.C1) / input$time.C1
    }
    if (input$Censor2 == 'Median (unit time)') {
      eta2 = ifelse(input$m.C2 == 0, 0, log(2)/input$m.C2)     #7
    } else if (input$Censor2 == 'Percentage') {
      eta2 = -log(1 - input$pct.C2) / input$time.C2
    }
    
    c0 = input$c0                                              #8
    c1 = str2vec(input$c1)                                     #9
    c2 = str2vec(input$c2)                                     #10
    b  = str2vec(input$b)                                      #11
    l  = input$l                                               #12
    l0 = input$l0                                              #13
    d0_r0 = ifelse(is.null(input$d0_r0), NA,
                   as.numeric(input$d0_r0))                    #14
    nsim = input$nsim
    
    if (input$r_Sa == 'Accrual Rate (patients/unit time)') {
      ra0 = str2vec(input$val)                                 #15
      Sa0 = NULL                                               #16 #notice here, only one of ra/Sa will appear
      In = data.frame(expand.grid(alpha = alpha, power = power, alloc = alloc, lambda1 = lambda1, eta1 = eta1, 
                                  lambda2 = lambda2, eta2 = eta2, c0 = c0, c1 = c1, c2 = c2, b = b, l = l, l0 = l0, d0_r0 = d0_r0, ra0 = ifelse(length(ra0)>1, NA, ra0)))
      In.name = c('One-sided Type I Error', 'Power', 'Percentage of Treatment Arm Patients', 'Median Survival Time (treatment)', 'Median Loss to Follow-up (treatment)',
                  'Median Survival Time (control)', 'Median Loss to Follow-up (control)', 'Fixed Cost (Unit/$)', 'Additional Cost/Patient (Unit/$)',
                  'Additional Cost/unit time (Unit/$)', 'Sales Income/unit time (Unit/$)', 'Time b/w Trial Start and LOE', 'Time b/w final analysis & market access',
                  'Meaningful Trt Eff', 'Accrual Rate (given)', 'Target Event')
    } else if (input$r_Sa == 'Accrual Period (unit time)') {
      ra0 = NULL
      Sa0 = str2vec(input$val)
      In = data.frame(expand.grid(alpha = alpha, power = power, alloc = alloc, lambda1 = lambda1, lambda2 = lambda2,
                                  eta1 = eta1, eta2 = eta2, c0 = c0, c1 = c1, c2 = c2, b = b, l = l, l0 = l0, d0_r0 = d0_r0, Sa0 = Sa0))
      In.name = c('One-sided Type I Error', 'Power', 'Percentage of Treatment Arm Patients', 'Median Survival Time (treatment)', 'Median Loss to Follow-up (treatment)',
                  'Median Survival Time (control)', 'Median Loss to Follow-up (control)', 'Fixed Cost (Unit/$)', 'Additional Cost/Patient (Unit/$)',
                  'Additional Cost/unit time (Unit/$)', 'Sales Income/unit time (Unit/$)', 'Time b/w Trial Start and LOE', 'Time b/w final analysis & market access',
                  'Meaningful Trt Eff', 'Accrual Period (given)', 'Target Event')
    }
    
    obs = seq(1, dim(In)[1])
    C.Name = paste('s',obs,sep = '')
    # 16
    In$EA = sapply(obs, function(x) E_a(HR = In[x,'lambda1']/In[x,'lambda2'], alpha = In[x,'alpha'], power =  In[x,'power'], alloc = In[x,'alloc']))
    # 17 (do not include)
    In$dL = sapply(obs, function(x) In[x,'l'] - In[x,'l0'] )
    
    
    ###########################################################            CORE PART          ######################################################################
    Opt = NULL; All.dat = list(); Valid.set = list()
    for (x in obs) {
      obj = findOpt(alpha = In[x,'alpha'], POWER = In[x,'power'], ra = ra0, Sa = In[x,'Sa0'], Ea = In[x,'EA'], lambda = c(In[x,'lambda1'], In[x,'lambda2']), eta = c(In[x,'eta1'], In[x,'eta2']), 
                    alloc = In[x,'alloc'], b = In[x,'b'], c0 = In[x,'c0'], c1 = In[x,'c1'], c2 = In[x,'c2'], dL = In[x,'dL'], d0_r0 = In[x,'d0_r0'], Aj.Ind = input$Aj, t0 = input$t0, e0 = input$e0, p0 = as.numeric(input$p0), m0 = input$m0, nsim = nsim)
      if (length(obj$OptRes$ra)>1) {obj$OptRes$ra=NA}
      Opt = cbind(Opt, unlist(obj$OptRes))
      All.dat = c(All.dat, list(obj$Designs))
      Valid.set = c(Valid.set, list(obj$Valid.set))
    }
    ################################################################################################################################################################
    # for further analysis
    sale = Opt[10,]
    cost = Opt[11,]
    rev  = Opt[12,]
    Mat  = cbind(In, t(Opt))
    PA   = Opt[13,]
    if (input$b == '') {
      sel.id = which.min(cost)
    } else {
      sel.id = which.max(rev)
    }
    Dat$results = Mat[sel.id,]
    Dat$Ns = NULL
    All.dat   = All.dat[[sel.id]]
    colnames(All.dat) <- c("N", "S", "Sa", "ra", "PA", "Rev", "Cost", "Sale"); All.dat = as.data.frame(All.dat)
    Dat$All.dat   = All.dat
    Dat$Valid.set = Valid.set[[sel.id]]
    
    # output tables:
    Opt = t(t(Opt[1:9,]))
    rownames(Opt) = c("Actual Power", "Sample Size", "Study Duration", "Accrual Period", "Accrual Rate",
                      paste("Constraint #1: Study Duration - Accrual Period >=", input$t0),
                      paste("Constraint #2: Event Number/Total Patients >=", input$e0),
                      paste("Constraint #3: Prob. of observing two medians >", as.numeric(input$p0)),
                      paste("Constraint #4: Median follow up time >", input$m0)
    )
    
    colnames(Opt) = C.Name
    Cond  = t(Opt[6:9,])
    Table1 = In[, !colnames(In) %in% c('dL')]
    Table1$lambda1 = round(log(2)/Table1$lambda1, 2); Table1$lambda2 = round(log(2)/Table1$lambda2, 2)
    Table1$eta1 = round(log(2)/Table1$eta1); Table1$eta2 = round(log(2)/Table1$eta2)
    Opt[1, ] = round(Opt[1, ], 4)
    Opt[c(3,4,5), ] = round(Opt[c(3,4,5), ], 2)
    
    colnames(Table1) = In.name; rownames(Table1) = C.Name
    Table1 = cbind(Table1, t(Opt[1:5,]), PA = round(PA, 4), Cost = round(cost,1), Sales = round(sale,1), Rev = round(rev,1))
    colnames(Table1) = c(colnames(Table1)[1:(dim(Table1)[2]-4)], 'Pr(meaningful results)','Trial Cost', 'Sales from Market Access to LOE', 'Total Revenue from Market Access to LOE (Revenue - Cost)')
    Dat$Table1 = Table1
    Cond[which(Cond==1, arr.ind = T)]='Activated';Cond[which(Cond==0, arr.ind = T)]='Inactivated'
    Dat$Table2 = Cond
    
    ## print settings, mainly from inputs
    output$table1 <- renderDT({Dat$Table1}, options = list(scrollX = TRUE, scrollY = TRUE, scrollCollapse = TRUE))
    #output$table1 <- renderTable(Dat$Table1, rownames = T, spacing = 's', width = 'auto', striped =T)
    output$table2 <- renderDT({Dat$Table2}, options = list(scrollX = TRUE, scrollY = TRUE))
    
    
    
    ## Render plotly:
    output$plotOpt <- renderPlotly({
      # first determine the range of N 
      N.lim <- Dat$results$N.min
      N.max <- Dat$results$N.max
      All.dat <- Dat$All.dat
      All.dat$Valid.set = ifelse(Dat$Valid.set, "Feasible_Sets", "Immature_Sets")
      ignore = ifelse(is.na(All.dat$ra[1]), rr <- rep(paste0(input$val, collapse = ","), dim(All.dat)[1]), rr <- All.dat$ra)
      
      p = ggplot(data = All.dat, aes(x = N, y = S, linetype = Valid.set)) + ylim(quantile(All.dat$S, c(0,0.98), names = F)) + 
        geom_line(aes(color="Feasible Sets"), size=1, color = "grey30") +
        geom_point(aes(x = Dat$results$N, y = Dat$results$S, color = "Optimal Set"), size = 3,color= "red") +
        xlab('Sample Size (N)') +
        ylab('Study Duration (S)') + theme_bw()
      
      
      pp = plotly_build(p) %>% layout(font = list(family="Times New Roman"), legend = list(x = 0.6, y = 0.99, tracegroupgap = 1, font = list(size = 12)))
      pp$x$data[[1]]$text = paste("Sample Size:", All.dat$N, "<br>", 
                                  "Study Duration:", round(All.dat$S,3), "<br>",
                                  "Accrual Rate:", rr, "<br>",
                                  "Accrual Period:", round(All.dat$Sa,3), "<br>",
                                  "Trial Cost:", round(All.dat$Cost,2), "<br>",
                                  "Total Sales:", round(All.dat$Sale,2), "<br>",
                                  "Revenue:", round(All.dat$Rev,2), "<br>",
                                  "Feasibility:", "Feasible","<br>"
      )
      
      pp$x$data[[2]]$text = paste("Sample Size:", All.dat$N[All.dat$Valid.set=="Immature_Sets"], "<br>", 
                                  "Study Duration:", round(All.dat$S,3)[All.dat$Valid.set=="Immature_Sets"], "<br>",
                                  "Accrual Rate:", rr[All.dat$Valid.set=="Immature_Sets"], "<br>",
                                  "Accrual Period:", round(All.dat$Sa,3)[All.dat$Valid.set=="Immature_Sets"], "<br>",
                                  "Trial Cost:", round(All.dat$Cost,2)[All.dat$Valid.set=="Immature_Sets"], "<br>",
                                  "Total Sales:", round(All.dat$Sale,2)[All.dat$Valid.set=="Immature_Sets"], "<br>",
                                  "Revenue:", round(All.dat$Rev,2)[All.dat$Valid.set=="Immature_Sets"], "<br>",
                                  "Feasibility:", "Immature","<br>"
      )
      
      # pp$x$data[[3]]$text = NULL
      pp$x$data[[4]]$text = paste("Optimal Set:", "<br>", 
                                  "Sample Size:", Dat$results$N, "<br>", 
                                  "Study Duration:", round(Dat$results$S,3), "<br>",
                                  "Accrual Rate:", rr, "<br>",
                                  "Accrual Period:", round(Dat$results$Sa,3), "<br>",
                                  "Trial Cost:", round(Dat$results$Cost,2), "<br>",
                                  "Total Sales:", round(Dat$results$Sale,2), "<br>",
                                  "Revenue:", round(Dat$results$Rev,2), "<br>"
      )
      pp
    })
  })
  
  
  ## print settings, mainly from inputs
  output$table1 <- renderDT({Dat$Table1}, options = list(scrollX = TRUE, scrollY = TRUE, scrollCollapse = TRUE))
  #output$table1 <- renderTable(Dat$Table1, rownames = T, spacing = 's', width = 'auto', striped =T)
  output$table2 <- renderDT({Dat$Table2}, options = list(scrollX = TRUE, scrollY = TRUE))
  
  
  
  ## Render plotly:
  output$plotOpt <- renderPlotly({
    # first determine the range of N 
    N.lim <- Dat$results$N.min
    N.max <- Dat$results$N.max
    All.dat <- Dat$All.dat
    All.dat$Valid.set = ifelse(Dat$Valid.set, "Feasible_Sets", "Immature_Sets")
    ignore = ifelse(is.na(All.dat$ra[1]), rr <- rep(paste0(input$val, collapse = ","), dim(All.dat)[1]), rr <- All.dat$ra)
    
    p = ggplot(data = All.dat, aes(x = N, y = S, linetype = Valid.set)) + ylim(quantile(All.dat$S, c(0,0.98), names = F)) + 
      geom_line(aes(color="Feasible Sets"), size=1, color = "grey30") +
      geom_point(aes(x = Dat$results$N, y = Dat$results$S, color = "Optimal Set"), size = 3,color= "red") +
      xlab('Sample Size (N)') +
      ylab('Study Duration (S)') + theme_bw()
    
    
    pp = plotly_build(p) %>% layout(font = list(family="Times New Roman"), legend = list(x = 0.6, y = 0.99, tracegroupgap = 1, font = list(size = 12)))
    pp$x$data[[1]]$text = paste("Sample Size:", All.dat$N, "<br>", 
                                "Study Duration:", round(All.dat$S,3), "<br>",
                                "Accrual Rate:", rr, "<br>",
                                "Accrual Period:", round(All.dat$Sa,3), "<br>",
                                "Trial Cost:", round(All.dat$Cost,2), "<br>",
                                "Total Sales:", round(All.dat$Sale,2), "<br>",
                                "Revenue:", round(All.dat$Rev,2), "<br>",
                                "Feasibility:", "Feasible","<br>"
    )
    
    pp$x$data[[2]]$text = paste("Sample Size:", All.dat$N[All.dat$Valid.set=="Immature_Sets"], "<br>", 
                                "Study Duration:", round(All.dat$S,3)[All.dat$Valid.set=="Immature_Sets"], "<br>",
                                "Accrual Rate:", rr[All.dat$Valid.set=="Immature_Sets"], "<br>",
                                "Accrual Period:", round(All.dat$Sa,3)[All.dat$Valid.set=="Immature_Sets"], "<br>",
                                "Trial Cost:", round(All.dat$Cost,2)[All.dat$Valid.set=="Immature_Sets"], "<br>",
                                "Total Sales:", round(All.dat$Sale,2)[All.dat$Valid.set=="Immature_Sets"], "<br>",
                                "Revenue:", round(All.dat$Rev,2)[All.dat$Valid.set=="Immature_Sets"], "<br>",
                                "Feasibility:", "Immature","<br>"
    )
    
    # pp$x$data[[3]]$text = NULL
    pp$x$data[[4]]$text = paste("Optimal Set:", "<br>", 
                                "Sample Size:", Dat$results$N, "<br>", 
                                "Study Duration:", round(Dat$results$S,3), "<br>",
                                "Accrual Rate:", rr, "<br>",
                                "Accrual Period:", round(Dat$results$Sa,3), "<br>",
                                "Trial Cost:", round(Dat$results$Cost,2), "<br>",
                                "Total Sales:", round(Dat$results$Sale,2), "<br>",
                                "Revenue:", round(Dat$results$Rev,2), "<br>"
    )
    pp
  })
  
  
  ##### Reset button #####
  observeEvent(input$reset, {
    updateNumericInput(session, inputId = 'alpha', value = 0.025)
    updateNumericInput(session, inputId = 'power', value = 0.90)
    updateNumericInput(session, inputId = 'alloc', value = 0.5)
    updateNumericInput(session, inputId = 'lambda1', value = 10.5)
    updateRadioButtons(session, inputId = 'Censor1', selected = 'Median (unit time)')
    updateNumericInput(session, inputId = 'lambda2', value = 7.5)
    updateRadioButtons(session, inputId = 'Censor2', selected = 'Median (unit time)')
    updateNumericInput(session, inputId = 'm.C1', value = 0)
    updateNumericInput(session, inputId = 'm.C2', value = 0)
    updateNumericInput(session, inputId = 'c0', value = 0)
    updateTextInput(session, inputId = 'c1', value = "", placeholder = 'e.g. 10,12,16-20')
    updateTextInput(session, inputId = 'c2', value = "", placeholder = 'e.g. 5,8,12-15')
    updateTextInput(session, inputId = 'b', value = "", placeholder = 'e.g. 50,60,70-75')
    updateNumericInput(session, inputId = 'l', value = 180)
    updateNumericInput(session, inputId = 'l0', value = 10)
    updateNumericInput(session, inputId = 't0', value = 5)
    updateNumericInput(session, inputId = 'e0', value = 0.5)
    updateTextInput(session, inputId = 'p0', value = "")
    updateNumericInput(session, inputId = 'nsim', value = 1000)
    updateNumericInput(session, inputId = 'm0', value = 5)
    updateRadioButtons(session, inputId = "Aj", selected = "None")
    updateRadioButtons(session, inputId = "r_Sa", selected = "Accrual Rate (patients/unit time)")
    updateTextInput(session, inputId = 'val', value = "", placeholder = 'e.g. 25; or 10,20,25,28,30...')
    output$table1 <- NULL
    output$table2 <- NULL
    output$plotOpt <- NULL
  })
  
  
  ## download table
  output$downTable <- downloadHandler(
    filename = function() {
      'Output_Results.csv'
    },
    content = function(file) {
      write.csv(Dat$Table1, file, row.names = T)
    }
  )
  
  ## Download the PNG
  output$downPNG <- downloadHandler(
    filename = function() {
      paste('C1_',input$c1, 'C2_', input$c2, ifelse(input$r_Sa == 'Accrual Rate (patients/unit time)', 'ra_','Sa_'), input$val, '.png', sep='')
    },
    content = function(file) {
      png(file)
      ## here is exactly the same as in renderPlot
      plot(Dat$All.dat$N[Dat$Valid.set], Dat$All.dat$S[Dat$Valid.set], type = 'l', lwd = 2, col = 'grey30', xlab = 'Sample Size (N)', ylab = 'Study Duration (S)', ylim = quantile(Dat$All.dat$S, c(0,0.98), names = F), xlim = range(Dat$All.dat$N))
      lines(Dat$All.dat$N[!Dat$Valid.set], Dat$All.dat$S[!Dat$Valid.set], type = 'l', lty = 3, lwd = 2, col = 'grey30')
      points(Dat$results$N, Dat$results$S, col = 'red', pch = 19, cex = 1.2)
      text(quantile(Dat$All.dat$N, 0.1), quantile(Dat$All.dat$S, 0.05), paste('Ea = ', Dat$results$EA, sep = ''), col = 'darkblue')
      ignore = ifelse(is.na(Dat$results$ra), rr <- paste0(input$val,collapse = ","), rr <- round(Dat$results$ra,2))
      text(Dat$results$N + min(5, 1.05*Dat$results$N), Dat$results$S + min(6, 1.3*Dat$resultss$S), paste("Acc. Rate:", rr,
                                                                                                         "\nSample Size:", Dat$results$N, 
                                                                                                         "\nStudy Dura.:", round(Dat$results$S,2), 
                                                                                                         "\nAcc. Dura.:", round(Dat$results$Sa,2),
                                                                                                         "\nCost:", round(Dat$results$Cost,1),
                                                                                                         "\nENR:", round(Dat$results$Rev,1)), col = 'black', pos = 4)
      legend("topright", legend = c('Feasible Sets', 'Immature Sets', 'Optimal Design'), col = c('grey30','grey30','red'), lwd = c(2,2,2), lty = c(1,3,0), pch = c(NA,NA,19), cex = 0.8)
      
      dev.off()  
    }
  )
  
  ## Download the PDF
  output$downPDF <- downloadHandler(
    filename = function() {
      paste('C1_',input$c1, 'C2_', input$c2, ifelse(input$r_Sa == 'Accrual Rate (patients/unit time)', 'ra_','Sa_'), input$val, '.pdf', sep='')
    },
    content = function(file) {
      pdf(file)
      ## here is exactly the same as in renderPlot
      plot(Dat$All.dat$N[Dat$Valid.set], Dat$All.dat$S[Dat$Valid.set], type = 'l', lwd = 2, col = 'grey30', xlab = 'Sample Size (N)', ylab = 'Study Duration (S)', ylim = quantile(Dat$All.dat$S, c(0,0.98), names = F), xlim = range(Dat$All.dat$N))
      lines(Dat$All.dat$N[!Dat$Valid.set], Dat$All.dat$S[!Dat$Valid.set], type = 'l', lty = 3, lwd = 2, col = 'grey30')
      points(Dat$results$N, Dat$results$S, col = 'red', pch = 19, cex = 1.2)
      text(quantile(Dat$All.dat$N, 0.1), quantile(Dat$All.dat$S, 0.05), paste('Ea = ', Dat$results$EA, sep = ''), col = 'darkblue')
      ignore = ifelse(is.na(Dat$results$ra), rr <- paste0(input$val,collapse = ","), rr <- round(Dat$results$ra,2))
      text(Dat$results$N + min(5, 1.05*Dat$results$N), Dat$results$S + min(6, 1.3*Dat$resultss$S), paste("Acc. Rate:", rr,
                                                                                                         "\nSample Size:", Dat$results$N, 
                                                                                                         "\nStudy Dura.:", round(Dat$results$S,2), 
                                                                                                         "\nAcc. Dura.:", round(Dat$results$Sa,2),
                                                                                                         "\nCost:", round(Dat$results$Cost,1),
                                                                                                         "\nENR:", round(Dat$results$Rev,1)), col = 'black', pos = 4)
      legend("topright", legend = c('Feasible Sets', 'Immature Sets', 'Optimal Design'), col = c('grey30','grey30','red'), lwd = c(2,2,2), lty = c(1,3,0), pch = c(NA,NA,19), cex = 0.8)
      
      dev.off()  
    }
  )
  
  ## Download the JPEG
  output$downJPG <- downloadHandler(
    filename = function() {
      paste('C1_',input$c1, 'C2_', input$c2, ifelse(input$r_Sa == 'Accrual Rate (patients/unit time)', 'ra_','Sa_'), input$val, '.jpeg', sep='')
    },
    content = function(file) {
      jpeg(file)
      ## here is exactly the same as in renderPlot
      plot(Dat$All.dat$N[Dat$Valid.set], Dat$All.dat$S[Dat$Valid.set], type = 'l', lwd = 2, col = 'grey30', xlab = 'Sample Size (N)', ylab = 'Study Duration (S)', ylim = quantile(Dat$All.dat$S, c(0,0.98), names = F), xlim = range(Dat$All.dat$N))
      lines(Dat$All.dat$N[!Dat$Valid.set], Dat$All.dat$S[!Dat$Valid.set], type = 'l', lty = 3, lwd = 2, col = 'grey30')
      points(Dat$results$N, Dat$results$S, col = 'red', pch = 19, cex = 1.2)
      text(quantile(Dat$All.dat$N, 0.1), quantile(Dat$All.dat$S, 0.05), paste('Ea = ', Dat$results$EA, sep = ''), col = 'darkblue')
      ignore = ifelse(is.na(Dat$results$ra), rr <- paste0(input$val,collapse = ","), rr <- round(Dat$results$ra,2))
      text(Dat$results$N + min(5, 1.05*Dat$results$N), Dat$results$S + min(6, 1.3*Dat$resultss$S), paste("Acc. Rate:", rr,
                                                                                                         "\nSample Size:", Dat$results$N, 
                                                                                                         "\nStudy Dura.:", round(Dat$results$S,2), 
                                                                                                         "\nAcc. Dura.:", round(Dat$results$Sa,2),
                                                                                                         "\nCost:", round(Dat$results$Cost,1),
                                                                                                         "\nENR:", round(Dat$results$Rev,1)), col = 'black', pos = 4)
      legend("topright", legend = c('Feasible Sets', 'Immature Sets', 'Optimal Design'), col = c('grey30','grey30','red'), lwd = c(2,2,2), lty = c(1,3,0), pch = c(NA,NA,19), cex = 0.8)
      
      dev.off()  
    }
  )
  
}                 



# Run the application 
shinyApp(ui = ui, server = server)