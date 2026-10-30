library(xml2)

svg_files <- list.files(
  "www/img",
  pattern = "\\.svg$",
  full.names = TRUE,
  recursive = TRUE
)

check_svg <- function(file) {
  tryCatch(
    {
      if (file.info(file)$size == 0) {
        return("empty")
      }

      doc <- read_xml(file)

      if (xml_name(xml_root(doc)) != "svg") {
        return("not_svg")
      }

      "ok"
    },
    error = function(e) {
      paste0("invalid_xml: ", conditionMessage(e))
    }
  )
}

results <- data.frame(
  file = svg_files,
  status = vapply(svg_files, check_svg, character(1)),
  stringsAsFactors = FALSE
)

results[results$status != "ok", ]
