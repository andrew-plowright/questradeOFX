#' Write OFX
#'
#' Write formated Questrade statement to an OFX file. Input should come from the `format_statement` function.
#'
#' @param statement data.frame. Questrade statement formatted using the `format_statement` function
#' @param outfile character. Output file path. Should have '.ofx' extension.
#' @param date POSIXct. Statement date and time.
#' @param funds data.frame. A table containing the name and symbol of all the funds on the statement
#' @param header character. XML header. Default header used for Microsoft Money.
#'
#' @export

write_OFX <- function(statement, outfile, date, funds, header = '<?xml version="1.0" encoding="UTF-8"?>\n<?OFX OFXHEADER="200" VERSION="200" SECURITY="NONE" OLDFILEUID="NONE" NEWFILEUID="NONE"?>\n'){

  doc <- XML::newXMLDoc()

  OFX <- XML::newXMLNode(

    "OFX",
    doc = doc,

    .children = list(

      # SIGNONMSGSRSV1
      SignOn(),

      # INVSTMTMSGSRSV1
      InvStateResponseSet(statement, date),

      # SECLISTMSGSRSV1
      SecurityListSet(funds)
      )
    )

  XML::saveXML(doc, file = outfile, prefix = header)

}





