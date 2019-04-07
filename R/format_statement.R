#' Format Questrade statement
#'
#' Read an Excel statement exported from the Questrade and format it prior to writing as an OFX file.
#'
#' This function will:
#'
#' 1. Reformat date/times
#' 2. Identify the type of action performed
#' 3. Create unique transfer IDs
#'
#' Currently supported actions are: Dividend, BuyMF, SellMF, Withdraw, Deposit, TransferIn and TransferOut.
#'
#' @param in_table data.frame. Questrade statement read from an Excel file
#' @return data.frame
#'
#' @export

format_statement <- function(in_table){

  ### PREP DATA ----

    # Check input log
    expectedHeaders <- c("Settlement.Date", "Action", "Symbol", "Description", "Quantity", "Price", "Commission", "Currency", "Account..", "Activity.Type")
    if(any(!expectedHeaders %in% names(in_table))) stop("Invalid headers for input Questrade logs")


  ### CREATE OUTPUT LOG ----

    # Create output frame
    out_table <- data.frame(

      date        = as.POSIXct(in_table$Settlement.Date, format="%d/%m/%Y %H:%M:%S %p"),
      symbol      = in_table$Symbol,
      account     = in_table$Account..,
      description = in_table$Description,
      quantity    = as.numeric(in_table$Quantity),
      price       = as.numeric(in_table$Price),
      commission  = as.numeric(in_table$Commission),
      amount      = as.numeric(in_table$Net.Amount),
      currency    = in_table$Currency,
      stringsAsFactors = FALSE
    )

    # Determine action type
    out_table[["action"]] <- factor(apply(in_table, 1, function(x){

      ac <- x["Action"]
      at <- x["Activity.Type"]
      dc <- x["Description"]

      if(at == "Dividends")                                         "Dividend"
      else if(at %in% c("Deposits", "Fees and rebates"))            "Deposit"
      else if(at == "Withdrawals")                                  "Withdraw"
      else if(at == "Trades" & ac == "Buy")                         "BuyMF"
      else if(at == "Trades" & ac == "Sell")                        "SellMF"
      else if(at == "Other" & ac == "BRW" & grepl("FROM ACCT", dc)) "TransferIn"
      else if(at == "Other" & ac == "BRW" & grepl("TO ACCT", dc))   "TransferOut"
      else "ERROR"
    }),
    levels = c("Dividend", "BuyMF", "SellMF", "Withdraw", "Deposit",  "TransferIn", "TransferOut"))

    # Name
    out_table[["name"]] <- "Questrade"

    # Create transaction IDs
    out_table[["tranID"]] <- openssl::md5(apply( out_table, 1 , paste , collapse = ""))
    if(any(duplicated(out_table[["tranID"]]))) stop("Unexpected duplication of transaction IDs")

  ### ADDITIONAL CHECKS ----

    actionError <- out_table$action == "ERROR"
    if(any(actionError)) stop("Could not determine action type for following entries:\n  ", paste(which(actionError), collapse = "\n  "))

    # Check that quantity * units + commission are correct
    qtCheck <- with(out_table, round(-(quantity * price) + commission, 2) == round(amount, 2))
    if(any(!qtCheck[out_table$action %in% c("SellMF", "BuyMF")])) stop("Quantity * units + commission do not add up")

  return(out_table)

}
