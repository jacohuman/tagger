#' generate_html
#' @description
#' Function to generate an HTML representation of an entity.
#'
#' @param entity A list representing an entity, typically containing metadata such as `rdfs:label`, `dc:title`, or `sur:hasTitle`.
#'
#' @returns A string containing the HTML representation of the entity.
#' @export
#'
#' @examples
generate_html <- function(entity) {
  label <- entity[["rdfs:label"]] %||%
    entity[["dc:title"]] %||%
    entity[["sur:hasTitle"]] %||%
    sanitize_id(entity[["@id"]])

  json_pretty <- toJSON(entity, pretty = TRUE, auto_unbox = TRUE)

  paste0(
    "<!DOCTYPE html><html><head><meta charset='UTF-8'>",
    "<title>", label, "</title>",
    "<style>",
    "body { font-family: Arial, sans-serif; padding: 1em; max-width: 800px; margin: auto; }",
    "pre { background: #f4f4f4; padding: 1em; border-radius: 8px; overflow-x: auto; }",
    "h1 { color: #333; }",
    "</style></head><body>",
    "<h1>", label, "</h1>",
    "<p><strong>ID:</strong> ", entity[["@id"]], "</p>",
    "<pre><code>", json_pretty, "</code></pre>",
    "</body></html>"
  )
}
