renderStyledTable <- function(table_name, rownames = TRUE, download_version = c()) {
  renderDT({
    datatable(
      table_name,
      extensions = c('Buttons', 'Scroller'),
      options = list(
        dom = 'Bfrtip',
        buttons = download_version,
        pageLength = 10, # Changed this from 10 to 5.
        autoWidth = TRUE,
        scrollX = TRUE, # Already present and set to TRUE.
        scroller = TRUE,
        deferRender = TRUE,
        scrollY = '400px',  # adjust this value to change visible height of table.
        scrollCollapse = TRUE
      ),
      rownames = rownames,
      class = 'compact stripe hover row-border order-column'
    ) %>%
      formatStyle(
        columns = names(table_name),
        backgroundColor = styleEqual(c(NA, 1), c("white", "#f7f9f9")),
        color = 'black',
        fontSize = '14px',
        fontWeight = styleEqual(c(NA, 1), c("normal", "bold")),
        lineHeight = '16px',
        textAlign = 'center'
      ) %>%
      formatStyle(
        columns = names(table_name),
        borderTop = '1px solid #dee2e6',
        borderBottom = '1px solid #dee2e6',
        textAlign = 'center'
      )
  }, server = FALSE)
}