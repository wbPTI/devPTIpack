# rethinking the weights page logic
library(tidyverse)
# library(devPTIpack)
library(profvis)
library(DT)
library(shiny)


shp_dta <- "../other_countries/south_sudan/South_Sudan.rds" %>% read_rds() #devPTIpack::ukr_shp
imp_dta <- "../other_countries/south_sudan/South_Sudan--metadata-2021-11-29_v2.1.xlsx" %>%
  devPTIpack::fct_template_reader()

# Parameters of the inputs UI module
# id = "wt_inputs"
# input_dta <- ukr_mtdt_full


# nss <- function(x) x

# Step 1. Convert input data into the table-ready style ========================
# ind_list <- imp_dta %>% get_indicators_list()

# # Profiling. ~80ms , Ok
# profvis::profvis({ukr_mtdt_full %>% get_indicators_list()})

# Step 2. Generate inputs UI and render it. ========================
# devtools::load_all()
# make_input_DT(ind_list)

# # # Profiling. ~160ms , Ok
# profvis::profvis({ make_input_DT(ind_list, scrollY = "550px" )})


# # Step 3. Render the table and debugging data ======================
# ui <- fluidPage(
#   column(4, mod_DT_inputs_ui("input_tbl_1", height = NULL)),
#   column(8, leaflet::leaflet() %>%
#            leaflet::addTiles() %>%
#            setView(-93.65, 42.0285, zoom = 12)),
#     absolutePanel(
#       fluidRow(actionButton("aa", "aaaa")),
#       fluidRow(
#         mod_DT_inputs_ui("input_tbl_2", height = "250px") %>%
#           div(style = "zoom:0.75;")
#       ),
#       top = 10, right = 75, width = 250, height = 450,
#                   style = "max-height: 300px !important; z-index: 1000;")
#   )
# 
# server <- function(input, output, session) {
#   mod_DT_inputs_server("input_tbl_1", input_dta = reactive(imp_dta))
#   mod_DT_inputs_server("input_tbl_2", input_dta = reactive(imp_dta))
# }
# 
# devtools::load_all()
# shinyApp(ui, server)


# # Adding tooltip to the DT content =====================================
# 
# # This is a little problematic.
# nested_dta <- prep_input_data(ind_list, ns = function(x)x)
# targets_dta <- make_vis_targets_for_dt(nested_dta)
# 
# 
# # Tailoring the WT page layout ===========================================
# options(golem.app.prod = TRUE)
# devtools::load_all()
# ui <- fluidPage(
#   shinyjs::useShinyjs(),
#   fluidRow(
#     column(5,
#            mod_wt_inp_ui("input_tbl_1", dt_style = "max-height: calc(70vh);"),
#            style = "padding-right: 0px; padding-left: 5px;"),
#     column(7,
#            leaflet::leaflet() %>%
#              leaflet::addTiles() %>%
#              setView(-93.65, 42.0285, zoom = 12) ,
#            absolutePanel(
#                mod_wt_inp_ui("input_tbl_2", dt_style = "max-height: 300px;") %>%
#                  div(style = "zoom:0.8;"),
#                top = 10, right = 75, width = 350, height = 550,
#                style = "!important; z-index: 1000;")
#            )
#   )
# )
# 
# server <- function(input, output, session) {
#   mod_wt_inp_server("input_tbl_1", input_dta = reactive(imp_dta))
#   mod_wt_inp_server("input_tbl_2", input_dta = reactive(imp_dta))
# }
# 
# # devtools::load_all()
# shinyApp(ui, server)


# Checking the inputs UI layout -------------------------------------------


devtools::load_all()
ui <- mod_ptipage_twocol_ui("pagepti")

server <- function(input, output, session) {
  mod_ptipage_newsrv("pagepti",
                     imp_dta = reactive(ukr_mtdt_full), #ukr_mtdt_full), #imp_dta),
                     shp_dta = reactive(ukr_shp), #ukr_shp),  #shp_dta))
                     show_adm_levels =  NULL #c("admin1")
  )
}


devtools::load_all()
shinyApp(ui, server)


# # Trying ScrollResize Plugin =================================================
# 
# 
# nested_dta %>% 
#   datatable( 
#     # width = width,
#     # height = height,
#     escape = FALSE, 
#     selection = 'none',
#     fillContainer = F,
#     rownames = NULL,
#     colnames = NULL,
#     plugins = c('scrollResize'),
#     options = list(
#       dom = 'ft',
#       bPaginate = FALSE,
#       columnDefs = targets_dta$columnDefs,
#       ordering = FALSE,
#       autoWidth = F,
#       
#       # scrollResize potions
#       paging = FALSE,
#       scrollResize = TRUE, 
#       scrollY =  100,
#       scrollCollapse = TRUE,
#       
#       headerCallback = JS(
#         "function(thead, data, start, end, display){
#           $('th', thead).css('display', 'none');
#           }"
#       )
#       #   paging = TRUE,
#       #   
#       #   columnDefs = targets_dta$columnDefs,
#       #   # deferRender = TRUE,
#       #   scrollY = scrollY,
#       #   # scrollX = FALSE,
#       #   scroller = TRUE,
#       #   # scrollCollapse = TRUE
#     ),
#     callback = JS("table.rows().every(function(i, tab, row) {
#         var $this = $(this.node());
#         $this.attr('id', this.data()[0]);
#         $this.addClass('shiny-input-container');
#       });
#       Shiny.unbindAll(table.table().node());
#       Shiny.bindAll(table.table().node());")
#   ) %>% 
#   formatStyle(
#     'type',
#     target = 'row',
#     backgroundColor = styleEqual("pillar", c('lightgray')),
#     fontWeight = styleEqual("pillar", c('bold')),
#   ) 


# 
# 
# 
# library(shiny)
# library(DT)
# library(tidyverse)
# 
# module_ui = function(id, label) {
#   
#   ns = NS(id)
#   
#   tagList(
#     DT::dataTableOutput(ns('foo')),
#     verbatimTextOutput(ns('sel'))
#   )
#   
# }
# 
# 
# 
# 
# nn <- 26
# 
# fake_dta <-
#   tibble(
#     Pillar = c("Pillar 1", "Pillar 2") %>% rep(nn/2),
#     name = stringi::stri_rand_strings(nn, length = 35),
#     id = stringi::stri_rand_strings(nn, length = 3),
#     descr = stringi::stri_rand_strings(nn, length = 25)
#   ) %>% 
#   arrange(Pillar, name)
# 
# module_server = function(input, output, session){
#   
#   ns = session$ns
#   
#   
#   to_render_dta <- 
#     reactive({
#       fake_dta %>% 
#         mutate(
#           input = map_chr(id, ~{
#             .x %>% 
#               ns() %>% 
#               numericInput(label = NULL, step = 1, value = 0, width = "100%") %>% 
#               as.character()
#           } )
#         )
#     })
#   
#   dt_columnDefs <-
#     reactive({
#       to_render <- to_render_dta()
#       
#       # Getting columns that are visible and invisible
#       visible_vars <-
#         names(to_render) %>%
#         set_names(seq_along(.)-1, .) %>%
#         `[`(names(.) %in% c("name", "input"))
#       
#       invisible_vars <-
#         names(to_render) %>%
#         set_names(seq_along(.)-1, .) %>%
#         `[`(!names(.) %in% c("name", "input"))
#       
#       # browser()
#       visible_targets <-
#         visible_vars %>%
#         unname() %>%
#         map2(c("75%", rep("25%", length(.)-1)),
#              ~{list(targets=c(.x), visible=TRUE, width=.y)})
#       
#       visible_targets[[length(visible_targets)]]$className <- c("dtcustom dtcenter")
#       visible_targets[[1]]$className <- c("dtcustom")
#       
#       invisible_targets <-
#         invisible_vars%>%
#         unname() %>%
#         c() 
#       
#       invisible_targets <- 
#         list(targets=c(invisible_targets), visible=FALSE, width="0px")
#       
#       append(list(invisible_targets), visible_targets)
#     })
#   
#   
#   premade_dta <- reactive({
#     # browser()
#     to_render_dta() %>% 
#       datatable( 
#         escape = FALSE, selection = 'none', 
#         fillContainer = T,
#         rownames = FALSE,
#         extensions = c('Scroller', "RowGroup"),
#         options = list(
#           dom = 'Bfrt',
#           rowGroup = list(dataSrc=c(0)),
#           autoWidth = FALSE,
#           paging = TRUE,
#           ordering = FALSE,
#           columnDefs = dt_columnDefs(),
#           deferRender = TRUE,
#           scrollY = 400,
#           scroller = TRUE,
#           searching = TRUE),
#         callback = JS("table.rows().every(function(i, tab, row) {
#         var $this = $(this.node());
#         $this.attr('id', this.data()[0]);
#         $this.addClass('shiny-input-container');
#       });
#       Shiny.unbindAll(table.table().node());
#       Shiny.bindAll(table.table().node());")
#       )
#   })
#   
#   output$foo = DT::renderDataTable(
#     premade_dta(), server = FALSE
#   )
#   
#   output$sel = renderPrint({
#     str(sapply(fake_dta$id, function(i) input[[i]]))
#   })
# }
# 
# ui <- fluidPage(
#   title = 'Selectinput column in a table',
#   h3("Source:", tags$a("Yihui Xie", href = "https://yihui.shinyapps.io/DT-radio/")),
#   module_ui("tabl")
# )
# 
# server <- function(input, output, session) {
#   callModule(module_server, "tabl")
# }
# 
# shinyApp(ui, server)



# 
# 
# 
# 
# library(shiny)
# library(DT)
# library(tidyverse)
# library(devPTIpack)
# 
# library(shiny)
# library(DT)
# 
# 
# nn <- 26
# 
# fake_dta <-
#   tibble(
#     Pillar = c("Pillar 1", "Pillar 2") %>% rep(nn/2),
#     name = stringi::stri_rand_strings(nn, length = 35),
#     id = stringi::stri_rand_strings(nn, length = 3),
#     descr = stringi::stri_rand_strings(nn, length = 25)
#   ) %>% 
#   arrange(Pillar, name)
# 
# fake_dta
# 
# 
# ns <- function(x) x
# 
# 
# to_render <-
#   fake_dta %>% 
#   mutate(
#     input = map_chr(id, ~{
#       .x %>% 
#         ns() %>% 
#         numericInput(label = NULL, step = 1, value = 0, width = "100%") %>% 
#         as.character()
#     } )
#   )
# 
# # Getting columns that are visible and invisible
# visible_vars <- 
#   names(to_render) %>% 
#   set_names(seq_along(.)-1, .) %>% 
#   `[`(names(.) %in% c("name", "input"))
# 
# invisible_vars <- 
#   names(to_render) %>% 
#   set_names(seq_along(.)-1, .) %>% 
#   `[`(!names(.) %in% c("name", "input"))
# 
# visible_targets <- 
#   visible_vars %>% 
#   unname() %>% 
#   map2(c("75%", rep("25%", length(.)-1)), 
#          ~{list(targets=.x, visible=TRUE, width=.y)}) 
# 
# visible_targets[[length(visible_targets)]]$className <- 'dt-center'
# 
# invisible_targets <- 
#   invisible_vars%>% 
#   unname() %>% 
#   map(~{list(targets=.x, visible=FALSE, width="0px")})
# 
# dt_render <-
#   to_render %>% 
#   DT::datatable(
#     escape = FALSE, 
#     rownames = FALSE,
#     selection = 'none',
#     fillContainer = T,
#     # height = "550px",
#     # extensions = c('Buttons', 'Scroller'),
#     # extensions = c('Scroller', "RowGroup"),
#     extensions = c('Scroller'),
#     options = list(
#       dom = 'Bfrt',
#       rowGroup = list(dataSrc=c(0)),
#       autoWidth = FALSE,
#       columnDefs = visible_targets %>% append(invisible_targets),
#       # buttons =
#       #   list(
#       #     list(extend = 'copy'),
#       #     list(extend = 'excel',
#       #          filename = file_name,
#       #          text = "Download in Excel")
#       #   ),
#       deferRender = TRUE,
#       scrollY = 400,
#       scroller = TRUE,
#       searching = TRUE
#     )
#   ) 
# 
# 
# 
# 
# 
# 
# 
# module_server = function(input, output, session){
# 
#   nss = session$ns
# 
#   
#   to_render_dta <- 
#     reactive({
#         fake_dta %>% 
#         mutate(
#           input = map_chr(id, ~{
#             .x %>% 
#               nss() %>% 
#               numericInput(label = NULL, step = 1, value = 0, width = "100%") %>% 
#               as.character()
#           } )
#         )
#     })
#   
#   dt_columnDefs <- 
#     reactive({
#       to_render <- to_render_dta()
#       
#       # Getting columns that are visible and invisible
#       visible_vars <- 
#         names(to_render) %>% 
#         set_names(seq_along(.)-1, .) %>% 
#         `[`(names(.) %in% c("name", "input"))
#       
#       invisible_vars <- 
#         names(to_render) %>% 
#         set_names(seq_along(.)-1, .) %>% 
#         `[`(!names(.) %in% c("name", "input"))
#       
#       visible_targets <- 
#         visible_vars %>% 
#         unname() %>% 
#         map2(c("75%", rep("25%", length(.)-1)), 
#              ~{list(targets=.x, visible=TRUE, width=.y)}) 
#       
#       visible_targets[[length(visible_targets)]]$className <- 'dt-center'
#       
#       invisible_targets <- 
#         invisible_vars%>% 
#         unname() %>% 
#         map(~{list(targets=.x, visible=FALSE, width="0px")})
#       
#       visible_targets %>% append(invisible_targets)
#     })
#       
#   
#   output$wghts_dt = DT::renderDataTable(
#     {
#       to_render_dta() %>% 
#         DT::datatable(
#           escape = FALSE, 
#           rownames = FALSE,
#           selection = 'none',
#           fillContainer = T,
#           # height = "550px",
#           # extensions = c('Buttons', 'Scroller'),
#           # extensions = c('Scroller', "RowGroup"),
#           extensions = c('Scroller'),
#           options = list(
#             dom = 'Bfrt',
#             # rowGroup = list(dataSrc=c(0)),
#             autoWidth = FALSE,
#             paging = TRUE, 
#             ordering = FALSE,
#             columnDefs = ,
#             # buttons =
#             #   list(
#             #     list(extend = 'copy'),
#             #     list(extend = 'excel',
#             #          filename = file_name,
#             #          text = "Download in Excel")
#             #   ),
#             deferRender = TRUE,
#             scrollY = 400,
#             scroller = TRUE,
#             searching = TRUE
#           ),
#           callback = JS("table.rows().every(function(i, tab, row) {
#           var $this = $(this.node());
#           $this.attr('id', this.data()[0]);
#           $this.addClass('shiny-input-container');
#           });
#           Shiny.unbindAll(table.table().node());
#                         Shiny.bindAll(table.table().node());")
#         ) 
#       
#     })
#       
#     }
#     # data, escape = FALSE, selection = 'none', server = FALSE,
#     dt_out(),
#     # options = list(dom = 'tf', paging = FALSE, ordering = FALSE)
#   )
#   
#   observe({
#     to_render_dta()$id %>%
#       nss() %>% 
#       map(~input[[.x]])
#     
#     browser()
#   })
# 
#   output$sel = renderPrint({
#     # 
#     # browser()
#     to_render_dta()$id %>%
#       map(~input[[.x]])
#   
#     # str(sapply(1:nrow(data), function(i) input[[paste0("sel", i)]]))
#   })
# }
# 
# ui <- 
#   # fluidPage(
#   # title = 'Selectinput column in a table',
#   # # h3("Source:", tags$a("Yihui Xie", href = "https://yihui.shinyapps.io/DT-radio/")),
#   mod_dtNumInputs_ui("tabl")
# # )
# 
# server <- function(input, output, session) {
#   callModule(module_server, "tabl")
# }
# 
# shinyApp(ui, server)
# 
# 
# 
# 
# 
# dddt <- datatable(iris,
#                   editable =  
#                     list(target = "cell", numeric = c(2), 
#                     # list(target = 'column', 
#                     #                numeric = c(2,3)
#                                    # # event = "click",
#                                    # type = "number",
#                                    # tooltip = "Click to edit: number",
#                                    # placeholder = "0",
#                                    # min = -100,
#                                    # max = 100,
#                                    # step = 1,
#                                    disable = list(columns = c(1,3))
#                                    ))
# dddt
# str(dddt)
# 
# 
# dt_output = function(title, id) {
#   fluidRow(column(
#     12, h1(paste0('Table ', sub('.*?([0-9]+)$', '\\1', id), ': ', title)),
#     hr(), DTOutput(id)
#   ))
# }
# render_dt = function(data, editable = 'cell', server = TRUE, ...) {
#   renderDT(data, selection = 'none', server = server, editable = editable, ...)
# }
# 
# shinyApp(
#   ui = fluidPage(
#     title = 'Double-click to edit table cells',
#     
#     dt_output('client-side processing (editable = "column")', 'x3'),
#     dt_output('server-side processing (editable = "column")', 'x7'),
#     dt_output('edit rows but disable certain columns (editable = list(target = "row", disable = list(columns = c(2, 4, 5))))', 'x10')
#   ),
#   
#   server = function(input, output, session) {
#     d1 = iris
#     d1$Date = Sys.time() + seq_len(nrow(d1))
#     d10 = d9 = d8 = d7 = d6 = d5 = d4 = d3 = d2 = d1
#     
#     options(DT.options = list(pageLength = 5))
#     
#     # client-side processing
#     output$x3 = render_dt(d3, 'column', FALSE)
#     output$x4 = render_dt(d4, 'all', FALSE)
#     
#     observe(str(input$x1_cell_edit))
#     observe(str(input$x2_cell_edit))
#     observe(str(input$x3_cell_edit))
#     observe(str(input$x4_cell_edit))
#     
#     # server-side processing
#     output$x5 = render_dt(d5, 'cell')
#     output$x6 = render_dt(d6, 'row')
#     output$x7 = render_dt(d7, 'column')
#     output$x8 = render_dt(d8, 'all')
#     
#     output$x9 = render_dt(d9, 'cell', rownames = FALSE)
#     output$x10 = render_dt(d10, list(target = 'row', disable = list(columns = c(2, 4, 5))))
#     
#     # edit a single cell
#     proxy5 = dataTableProxy('x5')
#     observeEvent(input$x5_cell_edit, {
#       info = input$x5_cell_edit
#       str(info)  # check what info looks like (a data frame of 3 columns)
#       d5 <<- editData(d5, info)
#       replaceData(proxy5, d5, resetPaging = FALSE)  # important
#       # the above steps can be merged into a single editData() call; see examples below
#     })
#     
#     # edit a row
#     observeEvent(input$x6_cell_edit, {
#       d6 <<- editData(d6, input$x6_cell_edit, 'x6')
#     })
#     
#     # edit a column
#     observeEvent(input$x7_cell_edit, {
#       d7 <<- editData(d7, input$x7_cell_edit, 'x7')
#     })
#     
#     # edit all cells
#     observeEvent(input$x8_cell_edit, {
#       d8 <<- editData(d8, input$x8_cell_edit, 'x8')
#     })
#     
#     # when the table doesn't contain row names
#     observeEvent(input$x9_cell_edit, {
#       d9 <<- editData(d9, input$x9_cell_edit, 'x9', rownames = FALSE)
#     })
#     
#     # edit rows but disable columns 2, 4, 5
#     observeEvent(input$x10_cell_edit, {
#       d10 <<- editData(d10, input$x10_cell_edit, 'x10')
#     })
#     
#   }
# )