# Download/export handlers for DREAMR Shiny app

setup_download_handlers <- function(output, rv) {
  # Download multiple files as a single zip
  output$download_raw_table <- downloadHandler(
    filename = function() {
      paste0("openalex_results_", Sys.Date(), ".zip")
    },
    content = function(file) {
      req(rv$oa_data)

      # Create a temporary directory
      tmpdir <- tempdir()

      # Write component CSVs
      write.csv(rv$oa_data$pub_metadata,
                file.path(tmpdir, "pub_metadata.csv"), row.names = FALSE)
      write.csv(rv$oa_data$institutions,
                file.path(tmpdir, "institutions.csv"), row.names = FALSE)
      write.csv(rv$oa_data$authors,
                file.path(tmpdir, "authors.csv"), row.names = FALSE)

      # Generate and write summary table CSV
      summary_df <- tryCatch({
        generate_summary_table(rv$oa_data)
      }, error = function(e) {
        data.frame(Characteristic = character(0), Summary = character(0))
      })
      write.csv(summary_df, file.path(tmpdir, "summary_table.csv"), row.names = FALSE)

      # Create zip archive
      zip::zipr(
        zipfile = file,
        files = c(
          file.path(tmpdir, "pub_metadata.csv"),
          file.path(tmpdir, "institutions.csv"),
          file.path(tmpdir, "authors.csv"),
          file.path(tmpdir, "summary_table.csv")
        ),
        root = tmpdir
      )
    },
    contentType = "application/zip"
  )
}
