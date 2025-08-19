library(shiny)
library(DiagrammeR)

options(shiny.maxRequestSize = 100 * 1024^2)

# Function to generate a dynamic graph and return the GDL string
generate_dynamic_graph <- function(config_file) {
  # Read the configuration file
  config <- read.delim(config_file, stringsAsFactors = FALSE, row.names = NULL)

  # Extract the graph name from the config file
  graph_name_row <- config[config$type == "meta" & config$node_id == "graph_name", ]
  if (nrow(graph_name_row) > 0) {
    graph_name <- graph_name_row$label
  } else {
    graph_name <- "dynamic_graph"
  }

  # Initialize the GDL string
  gdl <- "digraph Dynamic_Graph {\n  graph [layout = dot, rankdir = TB, nodesep = 0.5, ranksep = 0.8]\n"

  # Process nodes
  nodes <- config[config$type == "node", ]
  for (i in 1:nrow(nodes)) {
    node <- nodes[i, ]
    gdl <- paste0(
      gdl,
      sprintf("  %s [label = '%s', shape = %s, style = %s, fillcolor = %s]\n",
              node$node_id, node$label, node$shape, node$style, node$fillcolor)
    )
  }

  # Process edges
  edges <- config[!is.na(config$source) & !is.na(config$target), ]
  for (i in 1:nrow(edges)) {
    edge <- edges[i, ]
    if (edge$source == "" || edge$target == "") {
      next
    }
    edge_label <- ifelse(!is.na(edge$edge_label), edge$edge_label, "")
    gdl <- paste0(
      gdl,
      sprintf("  %s -> %s [label = '%s']\n", edge$source, edge$target, edge_label)
    )
  }

  # Process ranks
  ranks <- config[!is.na(config$rank), ]
  if (nrow(ranks) > 0) {
    for (i in 1:nrow(ranks)) {
      rank_nodes <- unlist(strsplit(ranks$rank[i], ";"))
      rank_nodes <- trimws(rank_nodes)
      if (length(rank_nodes) > 0) {
        gdl <- paste0(gdl, "  { rank = same; ", paste(rank_nodes, collapse = "; "), " }\n")
      }
    }
  }

  # Add legend
  legend_items <- config[config$type == "legend", ]
  if (nrow(legend_items) > 0) {
    gdl <- paste0(gdl, "  subgraph cluster_legend {\n    label = 'Legend';\n    labelloc = left;\n    fontsize = 12;\n    color = black;\n")
    for (i in 1:nrow(legend_items)) {
      legend <- legend_items[i, ]
      gdl <- paste0(
        gdl,
        sprintf("    %s [label = '%s', shape = %s, style = %s, fillcolor = %s]\n",
                legend$node_id, legend$label, legend$shape, legend$style, legend$fillcolor)
      )
    }
    gdl <- paste0(gdl, "  }\n")
  }

  # Close the GDL string
  gdl <- paste0(gdl, "}\n")

  return(gdl)
}

# Define the Shiny UI
ui <- fluidPage(
  titlePanel("Dynamic Graph Generator"),
  sidebarLayout(
    sidebarPanel(
      fileInput("config_file", "Upload Configuration File (TSV)",
                accept = c(".tsv")),
      downloadButton("download_gv", "Download .gv File")
    ),
    mainPanel(
      grVizOutput("graph_output")
    )
  )
)

# Define the Shiny server
server <- function(input, output, session) {
  # Reactive to store the GDL code
  gdl_code <- reactive({
    req(input$config_file)
    config_path <- input$config_file$datapath
    generate_dynamic_graph(config_path)
  })

  # Render the graph in the main panel
  output$graph_output <- renderGrViz({
    req(gdl_code())
    grViz(gdl_code())
  })

  # Allow downloading the .gv file
  output$download_gv <- downloadHandler(
    filename = function() {
      paste0("dynamic_graph_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".gv")
    },
    content = function(file) {
      writeLines(gdl_code(), file)
    }
  )
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
