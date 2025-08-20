info_content <- list(
  representation = list(
    title = "Representation Type Information",
    body = tagList(
      "This section allows you to choose how the protein structure is visually represented. Each representation type highlights different aspects of the protein:",
      tags$ul(
        tags$li("Cartoon: A simplified ribbon view, often used to showcase the overall structure and folding of the protein."),
        tags$li("Ball+Stick: Displays individual atoms and the bonds between them, offering a detailed look at the molecular interactions."),
        tags$li("Spacefill: Atoms are represented as spheres, providing an idea of how the protein might look in space."),
        tags$li("Licorice: A detailed view of chemical bonds, useful for analyzing interactions at the atomic level."),
        tags$li("Backbone: Shows only the protein's backbone, useful for observing its primary structure.")
      )
    )
  ),
  pdb = list(
    title = "PDB File Information",
    body = tagList(
      "Proteins are often studied using models stored in the Protein Data Bank (PDB), a global archive of structural data for biological molecules. Here, you can enter the PDB ID (e.g., '1crn') of a protein to load its 3D structure.",
      tags$a(href = "https://www.rcsb.org", "Learn more about PDB files", target = "_blank")
    )
  ),
  protein_structure = list(
    title = "3D Protein Structure Viewer",
    body = tagList(
      "This section lets you interact with a 3D model of a protein, providing a virtual view of its shape and structure. Here’s what you can do:",
      tags$ul(
        tags$li("Rotate and zoom the model using your mouse to explore the protein from different angles."),
        tags$li("Select different representation types to highlight specific features, such as bonds, atoms, or the overall shape."),
        tags$li("Load protein structures directly from the Protein Data Bank using their unique IDs.")
      ),
      "By understanding the 3D structure, researchers can gain insights into how proteins function and interact with other molecules, which is crucial for drug and vaccine development."
    )
  ),
  domain = list(
    title = "Functional Domains of the Spike Protein",
    body = tagList(
      "Each domain of the spike protein plays a critical role in the functionality of the coronavirus. Understanding these domains is essential for research and therapeutic development.",
      tags$ul(
        tags$li("These data provide researchers with a quick overview of the protein's general functionality and structural features."),
        tags$li("Facilitate the inspection of domains and mutations on the viral surface."),
        tags$li("Enable analysis of envelope-related characteristics across different viral strains.")
      )
    )
  ),
  envelope_view = list(
    title = "3D Envelope and Spike Proteins Viewer",
    body = tagList(
      "This section provides a 3D view of the coronavirus envelope and its spike proteins. The model illustrates the surface structure of the virus and highlights the crucial spike proteins that enable it to infect host cells.",
      tags$ul(
        tags$li("The envelope, shown in blue, represents the outer lipid layer of the virus. It protects the genetic material and structural components inside."),
        tags$li("The spike proteins, shown in red and orange, are the protrusions on the virus's surface. These proteins act as grappling hooks, allowing the virus to attach to and penetrate host cells."),
        tags$li("You can rotate and zoom the model using your mouse to explore the spatial arrangement of the envelope and spike proteins."),
        tags$li("By analyzing this 3D structure, researchers can study the interactions between the virus and host cells, aiding in the design of antiviral drugs and vaccines.")
      )
    )
  )#domin end
)


infoButtonModuleUI <- function(id) {
  ns <- NS(id)
  actionButton(ns("info_btn"), "", icon = icon("info-circle"), class = "btn-info btn-sm")
}

infoButtonModule <- function(id, info_content_key) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$info_btn, {
      info <- info_content[[info_content_key]]
      showModal(
        modalDialog(
          title = info$title,
          info$body,
          easyClose = TRUE,
          footer = NULL
        )
      )
    })
  })
}