
# header with title
header <- dashboardHeader(title = "Data harmonisation",
                          tags$li(class = "dropdown",
                                  dropdownMenu(type = "messages",
                                               # from for first line, message 2nd line smaller font
                                               messageItem(
                                                 from = "Project in Github",
                                                 message = "Documentation, Source, Citation",
                                                 icon = icon("github"),
                                                 href = "https://github.com/VincyaneBadouard/TreeData"),
                                               messageItem(
                                                 from = "Issues",
                                                 message = "Report Issues",
                                                 icon = icon("exclamation-circle"),
                                                 href = "https://github.com/VincyaneBadouard/TreeData/issues"),
                                               badgeStatus = NULL,
                                               icon = icon("info-circle"),
                                               # icon = fontawesome::fa("info-circle"),
                                               headerText = "App Information"
                                  )
                          #         ,
                          #
                          #         tags$li(class = "dropdown", actionButton("browser", "browser", icon  =  icon("r-project")))
                          )
                          # tags$li(class = "dropdown",
                          #
                          #         pickerInput ("languages", NULL, width = "auto",
                          #                     choices = languages,
                          #
                          #                     choicesOpt = list(content =
                          #                                         mapply(languages, flags, FUN = function(country, flagUrl) {
                          #                                           HTML(paste(
                          #                                             tags$img(src=flagUrl, width=20, height=15),
                          #                                             country
                          #                                           ))
                          #                                         }, SIMPLIFY = FALSE, USE.NAMES = FALSE)
                          #
                          #                     ))
                          # )
                          )

# sidebar contains menu items
sidebar <- dashboardSidebar(
  useShinyjs(),
  sidebarMenu(id = "tabs", # see here for icons https://fontawesome.com/v5/search
              menuItem("Upload your file(s)", tabName = "Upload", icon = icon("upload")),
              menuItem("Stack tables", tabName = "Stacking", icon = icon("layer-group")),
              menuItem("Merge tables", tabName = "Merging", icon = icon("key")),
              menuItem("Tidy table", tabName = "Tidying", icon = icon("check")),
              menuItem("Headers and Units", tabName = "Headers", icon = icon("arrows-alt")),
              menuItem("Codes", tabName = "Codes", icon = icon("table", verify_fa = F)),
              menuItem("Corrections", tabName = "Correct", icon = icon("check-circle")),
              menuItem("Output format", tabName = "OutputFormat", icon = icon("sign-out", verify_fa = FALSE)),
              # menuItem("Visualise results", tabName="Visualise", icon = icon("eye")),
              menuItem("Download", tabName="Save", icon = icon("save")),
              menuItem("Help", tabName = "Help", icon = icon("book"))
  )
)

body <- dashboardBody(
  tags$head(

    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/select2/4.0.13/css/select2.min.css"),# this is to edit Codes table
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/select2/4.0.13/js/select2.min.js"), # this is to edit Codes table
    # tags$script(src = "https://code.jquery.com/jquery-3.5.1.js"), # this is to allow grouping of rows in Code translation table
    # tags$script(src = "https://cdn.datatables.net/1.12.1/js/jquery.dataTables.min.js"), # this is to allow grouping of rows in Code translation table
    tags$style(
      HTML(".shiny-notification {
             position:fixed;
             top: calc(10%);
             left: calc(25%);
             @import url(https://use.fontawesome.com/releases/v5.7.2/css/all.css);
      }

             .dropdown-menu{z-index:10000 !important;}
             .sw-dropdown-content {z-index: 3000005 !important;}
             .sw-dropdown-in {z-index: 3000006 !important;}
             .vscomp-search-container {z-index: 3000005 !important;}
             .vscomp-dropbox-container {z-index: 3000005 !important;}
             tr.dtrg-group {cursor: pointer;}

           "

      )
    ) # to make notification show up at top of page
  ),
  tabItems(

    tabItem(tabName = "Upload",

            fluidRow(

              actionBttn(
                inputId = makeUniqueID("inactive"),
                label = div(
                  strong("If your connexion is slow and/or your data is very large, you may want to run this app locally. For that, open R Studio and type:"),
                  br()),
                style = "stretch",
                color = "success"),
              box(width = 12,
                  # helpText("Some text and then ", code("some code"), "."),
                  helpText(code('shiny::runGitHub( "VincyaneBadouard/TreeData", subdir = "inst/app")'),
                           br(),
                           br(),
                           '# If you have run this app in the past and you think/know the TreeData package has been updated since, you may need to restart you R session and re-install TreeData package (using code below) before running the app again',
                           br(),
                           code('devtools::install_github("VincyaneBadouard/TreeData", build_vignettes = TRUE)')


                           ),
                  # textOutput("CodeRunApp"),
                  tags$head(tags$style("#CodeRunApp{
                  color: red;
                  font-family: courier;
                  font-size: 100%;
                                 }"
                  ))),
              br(),
              br(),
              box(title = "checklist",
                  width = 12,

                 # dropdownButton(width = NULL,
                 prettyCheckbox(
                   inputId = makeUniqueID("ChckLst"),
                   label = "Inputs are prepared as CSV files.",
                   # value = TRUE,
                   status = "warning"
                 ),
                   prettyCheckbox(
                     inputId = makeUniqueID("ChckLst"),
                     label = "Tables that will need to be stacked have the exact same columns, in same order and with same names.",
                     # value = TRUE,
                     status = "warning"
                   ),
                   prettyCheckbox(
                     inputId = makeUniqueID("ChckLst"),
                     label = "The key columns of tables that will be merged have information that is correctly spelled and capitalized.",
                     # value = TRUE,
                     status = "warning"
                   ),
                   prettyCheckbox(
                     inputId = makeUniqueID("ChckLst"),
                     label = "...",
                     # value = TRUE,
                     status = "warning"
                   ),

                circle = TRUE, status = "danger",
                label  = tags$h2("Checklist before you upload"),
                icon = icon("cog"),
                inline =T,
                tooltip = tooltipOptions(title = "Click to see checklist !")
                # )
                # ,
                # span("Checklist before you upload")
                ),
              br(),
              br(),
              column(width = 6,
                     actionBttn(
                       inputId =  makeUniqueID("inactive"),
                       label = "1",
                       style = "pill",
                       color = "warning"),
                     strong("How many tables do you wish to upload?"),
                     numericInput(inputId = "nTable",
                                  label = "",
                                  value = 1,
                                  min = 1,
                                  max = NA
                     )
              ),
              column(width = 6,
                     actionBttn(
                       inputId =  makeUniqueID("inactive"),
                       label = "2",
                       style = "pill",
                       color = "warning"),
                     strong("What is your deepest level of measurements?"),
                     radioButtons(
                       inputId = "MeasLevel",
                       label = "",
                       choices = c("Plot", "Species", "Tree", "Stem"),
                       selected  = character(0)
                     )
              )),

            fluidRow(
              column(width = 6,
                     actionBttn(
                       inputId =  makeUniqueID("inactive"),
                       label = "3",
                       style = "pill",
                       color = "warning"),
                     strong("Upload your tables"),

                     uiOutput("uiUploadTables"),

                     actionBttn(
                       inputId =  makeUniqueID("inactive"),
                       label = "4",
                       style = "pill",
                       color = "warning"),
                     actionBttn(
                       inputId = "submitTables",
                       label = "submit",
                       style = "material-flat",
                       color = "success"
                     )),
              column(6,
                     uiOutput("uiViewTables"))

            )

    ),  ## end of "upload" panel


      tabItem(tabName = "Stacking",

              fluidRow(
                # column(width = 12,
                       actionBttn(
                         inputId =  makeUniqueID("inactive"),
                         label = " ! ",
                         style = "pill",
                         color = "danger"),
                       strong("make sure you clicked on 'Submit' in Upload tab")
                       # )
              ),
                fluidRow(
                column(width = 12,

                       h1("Stacking tables"),
                       h3("Select the tables that have the same set of columns and can be stacked on top of each other (e.g. one table per census, or one table per plot etc...)"),
                      code("If you have no tables to stack, skip this step."),
                      checkboxGroupButtons("TablesToStack", choices = ""),
                      actionBttn(
                        inputId = "Stack",
                        label = "Stack tables",
                        style = "material-flat",
                        color = "success"
                      ),
                      actionBttn(
                        inputId = "SkipStack",
                        label = "Skip this step",
                        style = "material-flat",
                        color = "warning"
                      ),
                      # insertUI("#Stack", "afterEnd",
                     hidden( actionBttn(
                        inputId = "GoToMerge",
                        label = "Go To Merge",
                        style = "material-flat",
                        color = "success"
                      ),
                      actionBttn(
                        inputId = "SkipMerge",
                        label = "Skip Merging since all your data is now stacked",
                        style = "material-flat",
                        color = "warning"
                      ))
                      #)
                )
                ),
              fluidRow(

                column(width = 12,
                       h4("View of your stacked tables:"),
                       DTOutput(outputId = "StackedTables"),
                       h4("summary of your stacked tables:"),
                       verbatimTextOutput("StackedTablesSummary")
                )
                # ,
                # actionButton("UpdateTable", label = "Update table!", style = "color: #fff; background-color: #009e60; border-color: #317256;   position: fixed")
              )


      ),  ## end of "Stacking" panel
    tabItem(tabName = "Merging",

            fluidRow(
              # column(width = 12,
              actionBttn(
                inputId =  makeUniqueID("inactive"),
                label = " ! ",
                style = "pill",
                color = "danger"),
              strong("make sure you clicked on 'Sumbit' in Upload tab (and `Stack tables` in Stack tab, if used) ")
              # )
            ),
            fluidRow(
              column(width = 12,

                     h1("Merging tables"),
                     h4("Select the tables that need to be merged and the key to merge them."),
                     h4("The first table should be the most exhaustive table (the one you want to keep all the rows from.)"),
                     p(strong("Tip: You can name your tables in the 'upload' tab so you know which tbale is which in the dropdown menus here")),
                     # actionButton("addMerge", "Add a Merging relationship"),
                     # uiOutput("MergeTablesUI"),
                     # textOutput("test"),
                     # verbatimTextOutput("test2"),
                     # checkboxGroupButtons("TablesToMerge", choices = ""),

                     box(width = 12,

                         fluidRow(column(3, pickerInput("leftTable", "Merge this table", choices = "")),
                                  column(1, br(),actionBttn("selectLeft", "", icon = icon("arrow-right"), size = "sm")),
                                  column(8,  shinyWidgets::virtualSelectInput("leftKey", div("Using this/these KEY column(s)", br(), em("if you need multiple columns for the merge, the order you select them matters")), choices = "", multiple = T, search = T, optionsCount = 6))),

                         fluidRow(column(3, pickerInput("rightTable", "And this table", choices = "")),
                                  column(1, br(),actionBttn("selectRight", "", icon = icon("arrow-right"), size = "sm")),
                                  column(8,  shinyWidgets::virtualSelectInput("rightKey", div("Using this/these KEY column(s)", br(), em("if you need multiple columns for the merge, the order you select them matters")), choices = "", multiple = T, search = T, optionsCount = 6))),


                     #   hidden(div(id = "SelectColumns",
                     #       box(width = 12,
                     #           # fluidRow(
                     #
                     #             pickerInput("SelectedMergedColumns", div("Select only the columns you want to keep moving forward", br(), em("By default (recommended), columns that are repeats in your second table are unselected.")), choices = "", multiple = T)
                     #       ))
                     #
                     # ),

                       actionBttn(
                       inputId = "Merge",
                       label = "Merge tables",
                       style = "material-flat",
                       color = "success")
                     ),
                     fluidRow(
                       hidden(actionBttn(inputId = "addMerge",  label =  span(icon("plus"), em("Add a Merging relationship", strong("(You need to end up with only one table)"))),
                                     style = "material-flat",
                                     color = "danger")),
                     ),
                     hidden(div(id ="Merge2Div", box(width = 12,

                         fluidRow(column(3, pickerInput("leftTable2", "Merge this table", choices = "")),
                                  column(1, br(),actionBttn("selectLeft2", "", icon = icon("arrow-right"), size = "sm")),
                                  column(8,  shinyWidgets::virtualSelectInput("leftKey2", div("Using this/these KEY column(s)", br(), em("if you need multiple columns for the merge, the order you select them matters")), choices = "", multiple = T, search = T, optionsCount = 6))),

                         fluidRow(column(3, pickerInput("rightTable2", "And this table", choices = "")),
                                  column(1, br(),actionBttn("selectRight2", "", icon = icon("arrow-right"), size = "sm")),
                                  column(8,  shinyWidgets::virtualSelectInput("rightKey2", div("Using this/these KEY column(s)", br(), em("if you need multiple columns for the merge, the order you select them matters")), choices = "", multiple = T, search = T, optionsCount = 6))),
                         actionBttn(
                           inputId = "Merge2",
                           label = "Merge tables",
                           style = "material-flat",
                           color = "success"
                         )
                     ))),

                     hidden( actionBttn(
                       inputId = "GoToTidy",
                       label = "Go To Tidy",
                       style = "material-flat",
                       color = "success"
                     ))
              )
            ),
            fluidRow(

              column(width = 12,
                     h4("View of your stacked tables:"),
                     DTOutput(outputId = "mergedTables"),
                     h4("summary of your merged tables:"),
                     verbatimTextOutput("mergedTablesSummary")
              )
            #   # ,
            #   # actionButton("UpdateTable", label = "Update table!", style = "color: #fff; background-color: #009e60; border-color: #317256;   position: fixed")
            )


    ),  ## end of "Merging" panel

    tabItem(tabName = "Tidying",
            h3("This is where we want to make your data 'tidy'"),
            p("This means that we want one row per observation. An observation is one measurement (of one stem, at one census, and one height)."),
            p("If you have stored several measurements on a same row (for example, you have several DBH columns, one for each census), we need to tidy your data..."),
            p("This is called wide-to-long reshaping. If you already have one observation per row, you can skip this step"),
            actionBttn(
              inputId = "SkipTidy",
              label = "Skip this step",
              style = "material-flat",
              color = "warning"
            ),
            box(width = 12,
                radioButtons(
              "VariableName",
              "Why do you have repeated column?",
              choices = c("One column per census" = "CensusID", "One column per height of measurement, measurement method, ..." = "MeasureID", "One column per stem" = "StemID", "One column per year" = "Year"),
              selected = "",
              inline = FALSE
            ),
            actionButton("ClearValueName","Clear")),
            br()
,            h3("Tick the grouping(s) that should be applied and fix the prefilled information if necessary."),

            uiOutput("uiMelt"),

            # box(
            # textInput("ValueName", "What type of measurement is repeated horizontally? (Give a column name without space)", value = "DBH"),
            # radioButtons(
            #   "VariableName",
            #   "What is the meaning of the repeated column?",
            #   choices = c("CensusID", "Year", "POM", "StemID"),
            #   selected = "",
            #   inline = FALSE
            # ),
            # actionButton("ClearValueName","Clear"),
            # pickerInput("Variablecolumns", label = "Select the columns that are repeats of measurements", choices = "", multiple = T, options = list(size = 10)),
            # ),
            actionBttn(
              inputId = "Tidy",
              label = "Tidy",
              style = "material-flat",
              color = "success"
            ),
            hidden( actionBttn(
              inputId = "GoToHeaders",
              label = "Go To Headers",
              style = "material-flat",
              color = "success"
            )),



            fluidRow(

              column(width = 12,
                     h4("View of your tidy table:"),
                     DTOutput(outputId = "TidyTable"),
                     h4("summary of your tidy table:"),
                     verbatimTextOutput("TidyTableSummary")
              ))


            ), ## end of "Tidy" panel
    tabItem(tabName = "Headers",
            fluidRow(
              # inform if profile already exists
              box(width = 12,
                  radioButtons(inputId = "predefinedProfile",
                               label = div("Use a predifined format?", br(), em("(if your data follows one of the following network template)")),
                               choices = list("No thanks!" = "No",
                                              # "ATDN: The Amazon Tree Diversity Network" = "ATDN",
                                              "ForestGEO: The Smithsonian Forest Global Earth Observatory" = "ForestGEO",
                                              "App's profile (if the data you upload was downloaded from this app, using this app's standards)" = "App"#,
                                              # "RBA: Red de Bosques Andinos" = "RBA"
                               ),
                               selected = "No"),

                  # load a profile it one already exists
                  fileInput(inputId = "profile", div("You may also load your own profile", br(), em("(if you already used this app and saved your profile (.rds))")), accept = ".rds"),
                  span(textOutput("RDSWarning"), style="color:red"),
                  br(),
                  hidden(actionBttn(
                    inputId = "UseProfile",
                    label = "Click Twice here to use Profile",
                    style = "pill",
                    color = "success")
                  )),
              hidden(div( id = "AttentionDates",
                          box(width = 12,
                              actionBttn(
                                inputId =  makeUniqueID("inactive"),
                                label = "!",
                                style = "pill",
                                color = "danger"),
                              strong("pay attention to your Date format and double check it in step 2, even if you imported a profile."),
                              p("A sample or your dates look like this:"),
                              textOutput("sampleDates")))),

              column(width = 6,
                     actionBttn(
                       inputId = makeUniqueID("inactive"),
                       label = "1",
                       style = "pill",
                       color = "warning"),
                     strong("  Match your columns to ours (when you can)"),
                     br(),
                     br(),
                     box(
                       # title = "Match your columns to ours (if you can)",
                         width = NULL,
                         # status = "primary",
                         # solidHeader = TRUE,
                         # uiOutput("ui1"),
                         div(id="mainWrapper",

                        lapply(unique(x1$Group), function(g) {div(h3(g),
                          dropdown(
                            h3(g),
                            do.call(div, lapply(which(x1$Group %in% g), function(i) {

                              eval(parse(text = paste0(x1$ItemType[i], "(inputId = x1$ItemID[i], label = ifelse(x1$helpText[i] %in% '', x1$Label[i], paste0(x1$Label[i], ' (', x1$helpText[i], ')')),", x1$Argument[i]," ='",  x1$Default[i],"'", ifelse(x1$Multiple[i] %in% TRUE, ", multiple = TRUE)", ")"))))

                            })),
                            label = g,
                            icon = icon("sliders", verify_fa = FALSE),
                            size = "lg",
                            circle = FALSE,
                            tooltip = tooltipOptions(title = "Click to see inputs !")

                         )
                        )
                         })
                         )

                           # lapply(1:nrow(x1), function(i) {
                           #
                           #   eval(parse(text = paste0(x1$ItemType[i], "(inputId = x1$ItemID[i], label = ifelse(x1$helpText[i] %in% '', x1$Label[i], paste0(x1$Label[i], ' (', x1$helpText[i], ')')),", x1$Argument[i]," ='",  x1$Default[i],"'", ifelse(x1$Multiple[i] %in% TRUE, ", multiple = TRUE)", ")"))))
                           #
                           # })

                         # actionBttn("Header1Next", "next", style = "fill", color = "primary")
                         )
              ),
              column(width = 6,

                     div(
                       actionBttn(
                         inputId = makeUniqueID("inactive"),
                         label = "2",
                         style = "pill",
                         color = "warning")
                       ,   strong("  Fill in information that is not in your columns"),
                       p("ATTENTION: do this after completing step 1 otherwise it will turn blank again."),
                       lapply(which(x$ItemID %in% unlist(lapply(list(x2, x3, x4, x5, x6), "[[", "ItemID"))), function(i) {

                         eval(parse(text = paste0(x$ItemType[i], "(inputId = x$ItemID[i], label = ifelse(x$helpText[i] %in% '', x$Label[i], paste0(x$Label[i], ' (', x$helpText[i], ')')),", x$Argument[i], "='", x$Default[i], "'", ifelse(x$Options[i] != FALSE, paste0(", options = ", x$Options[i]), ""), ifelse(x$Multiple[i] %in% TRUE, paste0(", multiple = TRUE, selected = '", x$Default[i], "')"), ")"))))

                       }),
                       actionBttn("LaunchFormating", label = "Apply changes!", style = "material-flat", color = "success") #style = "color: #fff; background-color: #009e60; border-color: #317256")
                     ),
                     box(title = "Save your profile",
                         width = NULL,
                         status = "primary",
                         solidHeader = TRUE,
                         downloadButton(outputId = "dbProfile", label = "Save profile")),
                     hidden( actionBttn(
                       inputId = "GoToCodes",
                       label = "Next",
                       style = "material-flat",
                       color = "success"
                     ))),

                     box(width = 12,

                       # column(width = 12,
                              dropdownButton( title = h1("Our standard units"), icon = icon("info-circle"), size  ="sm", width = "500px",
                                              datatable(x[!x$Unit %in% c("", "-"), c("ItemID", "Unit")],
                                                        rownames = F,
                                                        width = 300)
                                            ),
                              h4("View of your formatted table:"),
                              DTOutput(outputId = "FormatedTable"),
                              h4("summary of your formatted table:"),
                              verbatimTextOutput("FormatedTableSummary")
                       )
              # )


    )),
tabItem("Codes",
        h3("This is where we are going to try to understand the tree codes you have..."),
        # strong(style = "color:red", "This is not functional yet, you can skip this step for now... (click on 'Apply Corrections' on the left pannel)"),

        h4("Please, fill-out the", code("Definition"), "by selecting a pre-written denfinition or manually writting yours."),
        hidden(actionBttn(inputId = "UseProfileCodes" , label = "Use your profile")),
        actionBttn(
          inputId = "GoToCorrect",
          label = "Go To Correct",
          style = "material-flat",
          color = "success"
        ),
        hidden(downloadButton(outputId = "dbProfile1", label = "Save profile again")),

        # uiOutput("uiCodes"),
        br(),
        box(width = NULL,
            DTOutput("CodeTable", height =  "600px"))
        # tags$hr(),
        # h2("Edited table:"),
        # tableOutput("NewCodeTable")

        # tableOutput("NewCodeTable")
        ),

    tabItem(tabName = "Correct",

            div(includeMarkdown("www/Corrections.md"),
                actionBttn(
                  inputId = "SkipCorrections",
                  label = "Skip Corrections",
                  style = "material-flat",
                  color = "warning"
                )
            ),

            lapply(unique(xCorr$Function), function(f) {
              box(
                title = f,
                radioButtons(inputId = f, label = paste("Apply", f, "?"), choices = list("Yes" = "Yes", "No" = "No"), selected = "No"),
                hidden(div(id = paste0(f, "Yes"),

                lapply(which(xCorr$Function %in% f), function(i) {
                  # eval(parse(text = paste0(xCorr$ItemType[i], "(inputId = xCorr$ItemID[i], label = div(HTML(xCorr$Label[i])),", xCorr$Argument[i], " = eval(parse(text = '", xCorr$Default[i], "'))", ifelse(xCorr$Argument2[i] != FALSE, paste0(", ", xCorr$Argument2[i], " = eval(parse(text = '",xCorr$Default[i], "'))"), ""), ifelse(xCorr$Options[i] != FALSE, paste0(", options = ", xCorr$Options[i]), ""), ifelse(xCorr$Multiple[i] %in% TRUE, ", multiple = TRUE)", ")"))))
                  eval(parse(text = paste0(xCorr$ItemType[i], "(inputId = xCorr$ItemID[i], label = div(HTML(xCorr$Label[i])),", xCorr$Argument[i], ifelse(grepl("input", xCorr$Default[i]), " = 'pending'", paste0(" = eval(parse(text = '", xCorr$Default[i], "'))")), ifelse(xCorr$Argument2[i] != FALSE, paste0(", ", xCorr$Argument2[i], ifelse(grepl("input", xCorr$Default[i]), " = 'pending'", paste0(" = eval(parse(text = '", xCorr$Default[i], "'))"))), ""), ifelse(xCorr$Options[i] != FALSE, paste0(", options = ", xCorr$Options[i]), ""), ifelse(xCorr$Multiple[i] %in% TRUE, ", multiple = TRUE)", ")"))))
                })
              )
              ))
            }),
            hidden(actionBttn(
              inputId = "ApplyCorrections",
              label = "Apply Corrections",
              style = "material-flat",
              color = "success"
            )),
            hidden(actionBttn(
              inputId = "GoToOutput",
              label = "Go To Output format",
              style = "material-flat",
              color = "success"
            )),
            fluidRow(

              column(width = 12,
                     h4("View of your corrected table:"),
                     withSpinner(DTOutput(outputId = "CorrectedTable"), color="#0dc5c1", id = "spinner"),
                     h4("summary of your corrected table:"),
                     withSpinner(verbatimTextOutput("CorrectedTableSummary"), color="#0dc5c1", id = "spinner")
              ))
            ),

    tabItem(tabName = "OutputFormat",

            fluidRow(box(width = 12,
                         radioButtons(inputId = "predefinedProfileOutput",
                                      label = div("Use a predifined format for your output?"),
                                      choices = list("No thanks! I'll upload a profile I have handy." = "No",
                                                     "This App's standard" = "App",
                                                     # "ATDN: The Amazon Tree Diversity Network" = "ATDN",
                                                     "ForestGEO: The Smithsonian Forest Global Earth Observatory" = "ForestGEO"#,
                                                     # "RBA: Red de Bosques Andinos" = "RBA"
                                      ),
                                      selected = "No"),

                         # load a profile it one already exists
                         div(id = "profileOutputfileInput",
                           fileInput(inputId = "profileOutput", div("Load a profile you have on your machine", br(), em("(if you or a colleague already used this app and saved a profile (.rds))")), accept = ".rds")),
                         span(textOutput("RDSOutputWarning"), style="color:red"),
                         br(),
                        actionBttn(
                           inputId = "UseProfileOutput",
                           label = "Apply Profile",
                           style = "pill",
                           color = "success"),
                        hidden(actionBttn(
                           inputId = "DontUseProfileOutput",
                           label = "Don't use profile",
                           style = "pill",
                           color = "warning")),
                         hidden(actionBttn(
                           inputId = "GoToDownload",
                           label = "Go To Download",
                           style = "material-flat",
                           color = "success"
                         ))

            )
            ),
            fluidRow(hidden(div(id = "CodeTranslationsDiv", box(width = 12,
              h4("The output profile you selected has a table of codes that you may want to match with your codes. We tried to help you out by already matching the codes with the same definitions, but you need to double check and fill out the rest of the codes"),
              h3("Help"),
              p(strong("Row names:"), "These are the your codes. They may come from multiple columns, indicated in the grey rows"),
              p(strong("Column names:"), "These are the codes in the output profile you selected. They may come from multiple columns, given at the top of the table."),
              p("What to do:"),
              # tags$li("Hover over the codes in the column names to see the defintions of the profile you selected."),
              tags$li("For each of your codes, select the radio button in the column that corresponds most to your defintion. We already selected the buttons for codes that match defintions perfectly."),
              tags$li("If there is no match, leave blank."),
              tags$li("When you are done, double check your selection in the next table. When you are satisfied, click on the 'Apply Code Translation' button."),
              br(),
              fileInput("UserCodeTranslationTable", "If you have already been throught this and have a .csv file of your code translation table, you can upload it here to fill the table automatically."),
              hidden(actionBttn("updateCT", label = "Update")),
              p(strong("Tip: You can collapse the rows by clicking on the gray ones")),
              p(strong("Tip: Hover over the codes in the column names to see the definitions.")),
              # strong(style = "color:red", "This is not implemented yet, so ignore for now, thanks!"),

              uiOutput("uiCodeTranslations"))))),
            fluidRow(

              column(width = 12,
                     h4("View of your final table:"),
                     withSpinner(DTOutput(outputId = "DataOutput"),color="#0dc5c1", id = "spinner"),
                     h4("summary of your final table:"),
                     withSpinner(verbatimTextOutput("DataOutputSummary"),color="#0dc5c1", id = "spinner")
              ))
    ), # end of "OutputFormat" panel
tabItem(tabName = "Save",
        fluidRow(
          column(width = 5,
                 box(title = "Save all outputs as zipfile",
                     width  = NULL,
                     status = "primary",
                     solidHeader = T,
                     downloadButton(outputId = "dbZIP", label = "Save all"))
                 # box(title =  "Save files independantly",
                 #     width = NULL,
                 #     solidHeader = T,
                 #     box(title = "Save file",
                 #         width = NULL,
                 #         # status = "primary",
                 #         solidHeader = F,
                 #         downloadButton(outputId = "dbFile", label = "Save file")),
                 #     box(title = "Save your profile",
                 #         width = NULL,
                 #         # status = "primary",
                 #         solidHeader = F,
                 #         downloadButton(outputId = "dbProfile2", label = "Save profile"),
                 #         p("(You can overwrite what you saved before)")),
                 #     # box(title = "Save R code",
                 #     #     width = NULL,
                 #     #     # status = "primary",
                 #     #     solidHeader = F,
                 #     #     downloadButton(outputId = "dbCode", label = "Save R code")),
                 #
                 #     # p("ATTENTION:, LifeStatus and CommercialSp were not converted to your desired output profile because we cannot interprete TRU/FALSE to your desired profile's code system!"),
                 #     box(title = "Save metadata",
                 #         width = NULL,
                 #         # status = "primary",
                 #         solidHeader = F,
                 #         downloadButton(outputId = "dbMetadata", label = "Save metadata")
                 #     )
                 # )
          )
        )), # end of "save" panel
    tabItem(tabName = "Help",
            tabsetPanel(
              tabPanel(title = "General",
                       imageOutput("AppGeneralWorkflow")
                     ),

              tabPanel_helper("Upload"),
              tabPanel_helper("Stack"),
              tabPanel_helper("Merge"),
              tabPanel_helper("Tidy"),
              tabPanel_helper("Headers"),
              tabPanel_helper("Codes"),
              tabPanel_helper("Corrections"),
              tabPanel(title = "Output",
                       tabsetPanel(
                         tabPanel_helper("Selecting_an_output_profile"),
                         tabPanel_helper("Code_translation")
                       )),
              tabPanel_helper("Download")
            )

    ) # end of "Help" panel
  )
)


ui <- dashboardPage(header, sidebar, body, skin = "black")





