renderStyledTable <- function(table_name, rownames = TRUE) {
  return(
    renderDT({
      datatable(table_name,
                extensions = c('Buttons', 'FixedHeader', 'ColReorder'),
                options = list(
                  dom = 'Bfrtip',
                  buttons = c('csv', 'excel', 'pdf'),
                  pageLength = 10,
                  autoWidth = FALSE,
                  scrollX = TRUE,
                  columnDefs = list(
                    list(width = 'auto', targets = "_all")
                  ),
                  fixedHeader = TRUE,
                  colReorder = TRUE,
                  drawCallback = JS(
                    "function(settings) {
                      $(this.api().table().header()).css({
                        'background-color': '#004466', 
                        'color': '#ffffff', 
                        'font-weight': 'bold'
                      });
                      $(this.api().table().body()).find('tr').hover(function() {
                        $(this).css({
                          'background-color': '#f0f0f0'
                        });
                      }, function() {
                        $(this).css({
                          'background-color': ''
                        });
                      });
                      this.api().columns.adjust().draw();
                    }"
                  )
                ),
                rownames = rownames, 
                class = 'compact stripe hover row-border order-column'
      ) %>%
        formatStyle(names(table_name),
                    backgroundColor = 'white', color = 'black', fontWeight = 'bold')
      
    })
  )
}