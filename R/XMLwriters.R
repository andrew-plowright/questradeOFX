
### SIGN ON ----
SignOn <- function(signOnTime = Sys.time()){

  SIGNONMSGSRSV1 = XML::newXMLNode("SIGNONMSGSRSV1")
  SONRS = XML::newXMLNode("SONRS", parent = SIGNONMSGSRSV1)

  STATUS = XML::newXMLNode("STATUS", parent = SONRS)
  XML::newXMLNode("CODE", 0, parent = STATUS)
  XML::newXMLNode("SEVERITY", "INFO", parent = STATUS)
  XML::newXMLNode("MESSAGE", "Successful Sign On", parent = STATUS)

  XML::newXMLNode("DTSERVER", format(signOnTime, "%Y%m%d%H%M%S"), parent = SONRS)
  XML::newXMLNode("LANGUAGE", "ENG", parent = SONRS)

  return(SIGNONMSGSRSV1)

}


### SECURITY LIST ----
SecurityListSet <- function(stocks){

  secList <- apply(stocks, 1, function(sec){
    Security(sec["symbol"], sec["name"])
  })

  SECLISTMSGSRSV1 = XML::newXMLNode("SECLISTMSGSRSV1")
  SECLIST = XML::newXMLNode("SECLIST", .children = secList, parent = SECLISTMSGSRSV1)

  return(SECLISTMSGSRSV1)

}

Security <- function(ticker, name){

  MFINFO  = XML::newXMLNode("MFINFO")

  SECINFO = XML::newXMLNode("SECINFO", parent = MFINFO)

  SECID = XML::newXMLNode("SECID", parent = SECINFO)

  XML::newXMLNode("UNIQUEID", ticker, parent = SECID)
  XML::newXMLNode("UNIQUEIDTYPE", "TICKER", parent = SECID)

  XML::newXMLNode("SECNAME", name, parent = SECINFO)
  XML::newXMLNode("TICKER", ticker, parent = SECINFO)

  return(MFINFO)

}

### INVESTMENT STATEMENT ----

# Make a INVSTMTMSGSRSV1 from an investment log
InvStateResponseSet <- function(invLog, statementDate){

  # Spit log into account and transfer categories
  invLog[["acc_tran"]] <- paste(invLog$account, invLog$action %in% c("TransferIn", "TransferOut"))

  XML::newXMLNode(

    "INVSTMTMSGSRSV1",

    .children = lapply(unique(invLog$acc_tran), function(accT){

      # Get subset of log for this account
      accLog <- subset(invLog, acc_tran == accT)
      accID <- accLog$account[1]

      # Unique ID from concatenation of date and account number
      statementID <- openssl::md5(paste0(accID, as.character(statementDate)))

      # Get currency
      accCur <- unique(accLog$currency)
      if(length(accCur) > 1) stop("Cannot have multiple currencies within a single acocunt")

      # Get investment transactions
      accTrans <- TransactionList(accLog)

      InvStateResponse(
        ID            = statementID,
        accountID     = accID,
        statementDate = statementDate,
        startDate     = min(accLog$date),
        endDate       = max(accLog$date),
        tranList      = accTrans,
        currency      = accCur)

    })
  )
}

# Make a INVSTMTTRNRS for a given account from an investment log
InvStateResponse <- function(ID, accountID, statementDate, startDate, endDate, tranList, currency = "CAD"){

  INVSTMTTRNRS = XML::newXMLNode("INVSTMTTRNRS")
  XML::newXMLNode("TRNUID", ID, parent = INVSTMTTRNRS)

  STATUS = XML::newXMLNode("STATUS", parent = INVSTMTTRNRS)
  XML::newXMLNode("CODE", 0, parent = STATUS)
  XML::newXMLNode("SEVERITY", "INFO", parent = STATUS)

  INVSTMTRS = XML::newXMLNode("INVSTMTRS", parent = INVSTMTTRNRS)
  XML::newXMLNode("DTASOF", format(statementDate,"%Y%m%d%H%M%S"), parent = INVSTMTRS)
  XML::newXMLNode("CURDEF", currency, parent = INVSTMTRS)

  INVACCTFROM = XML::newXMLNode("INVACCTFROM", parent = INVSTMTRS)
  XML::newXMLNode("BROKERID", "questrade.com", parent = INVACCTFROM)
  XML::newXMLNode("ACCTID", accountID, parent = INVACCTFROM)

  INVTRANLIST = XML::newXMLNode("INVTRANLIST", parent = INVSTMTRS)
  XML::newXMLNode("DTSTART", format(startDate,"%Y%m%d"), parent = INVTRANLIST)
  XML::newXMLNode("DTEND",   format(endDate,"%Y%m%d"),   parent = INVTRANLIST)

  XML::addChildren(INVTRANLIST, kids = tranList)

  return(INVSTMTTRNRS)
}

# Make a INVTRANLIST for a given account from an investment log
TransactionList <- function(invLog){

  invLog <- invLog[order(invLog$action),]

  lapply(1:nrow(invLog), function(i){

    with(invLog[i,],{

      if(action == "Dividend")            Dividend(tranID, date, symbol, amount)
      else if(action == "TransferIn")   TransferIn(tranID, date, symbol, quantity)
      else if(action == "TransferOut") TransferOut(tranID, date, symbol, quantity)
      else if(action == "Deposit")         Deposit(tranID, date, name, amount, description)
      else if(action == "Withdraw")       Withdraw(tranID, date, name, amount, description)
      else if(action == "BuyMF")             BuyMF(tranID, date, symbol, quantity, price, commission)
      else if(action == "SellMF")            SellMF(tranID, date, symbol, quantity, price, commission)
      else stop("Unrecognized action type:", action)
    })
  })
}

### INVESTMENT TRANSACTIONS ----

BuyMF <- function(ID, date, ticker, units, price, commission){

  BUYMF  = XML::newXMLNode("BUYMF")

  INVBUY = XML::newXMLNode("INVBUY", parent = BUYMF)

  INVTRAN = XML::newXMLNode("INVTRAN", parent = INVBUY)
  XML::newXMLNode("FITID", ID, parent = INVTRAN)
  XML::newXMLNode("DTTRADE", format(date,"%Y%m%d%H%M%S"), parent = INVTRAN)

  SECID = XML::newXMLNode("SECID", parent = INVBUY)
  XML::newXMLNode("UNIQUEID",     ticker,   parent = SECID)
  XML::newXMLNode("UNIQUEIDTYPE", "TICKER", parent = SECID)

  XML::newXMLNode("UNITS",       units,      parent = INVBUY)
  XML::newXMLNode("UNITPRICE",   price,      parent = INVBUY)
  XML::newXMLNode("COMMISSION",  abs(commission), parent = INVBUY)
  XML::newXMLNode("TOTAL",       -(units * price) - abs(commission), parent = INVBUY)
  XML::newXMLNode("SUBACCTSEC",  "CASH",     parent = INVBUY)
  XML::newXMLNode("SUBACCTFUND", "CASH",     parent = INVBUY)

  XML::newXMLNode("BUYTYPE", "BUY", parent = BUYMF)

  return(BUYMF)
}

SellMF <- function(ID, date, ticker, units, price, commission){

  SELLMF  = XML::newXMLNode("SELLMF")

  INVSELL = XML::newXMLNode("INVSELL", parent = SELLMF)

  INVTRAN = XML::newXMLNode("INVTRAN", parent = INVSELL)
  XML::newXMLNode("FITID", ID, parent = INVTRAN)
  XML::newXMLNode("DTTRADE", format(date,"%Y%m%d%H%M%S"), parent = INVTRAN)

  SECID = XML::newXMLNode("SECID", parent = INVSELL)
  XML::newXMLNode("UNIQUEID",     ticker,   parent = SECID)
  XML::newXMLNode("UNIQUEIDTYPE", "TICKER", parent = SECID)

  XML::newXMLNode("UNITS",       units,      parent = INVSELL)
  XML::newXMLNode("UNITPRICE",   price,      parent = INVSELL)
  XML::newXMLNode("COMMISSION",  abs(commission), parent = INVSELL)
  XML::newXMLNode("TOTAL",       -(units * price) - abs(commission), parent = INVSELL)
  XML::newXMLNode("SUBACCTSEC",  "CASH",     parent = INVSELL)
  XML::newXMLNode("SUBACCTFUND", "CASH",     parent = INVSELL)

  XML::newXMLNode("SELLTYPE", "SELL", parent = SELLMF)

  return(SELLMF)
}

Dividend <- function(ID, date, ticker, amount){

  INCOME  = XML::newXMLNode("INCOME")

  INVTRAN = XML::newXMLNode("INVTRAN", parent = INCOME)
  XML::newXMLNode("FITID", ID, parent = INVTRAN)
  XML::newXMLNode("DTTRADE", format(date,"%Y%m%d%H%M%S"), parent = INVTRAN)

  SECID = XML::newXMLNode("SECID", parent = INCOME)
  XML::newXMLNode("UNIQUEID", ticker,       parent = SECID)
  XML::newXMLNode("UNIQUEIDTYPE", "TICKER", parent = SECID)

  XML::newXMLNode("INCOMETYPE",  "DIV",   parent = INCOME)
  XML::newXMLNode("TOTAL",       amount,  parent = INCOME)
  XML::newXMLNode("SUBACCTSEC",  "OTHER", parent = INCOME)
  XML::newXMLNode("SUBACCTFUND", "OTHER", parent = INCOME)

  return(INCOME)
}

TransferIn <- function(ID, date, ticker, units){

  .transfer(ID, date, ticker, units, direction = "IN")
}

TransferOut <- function(ID, date, ticker, units){

  .transfer(ID, date, ticker, units, direction = "OUT")
}

.transfer <- function(ID, date, ticker, units, direction){

  TRANSFER  = XML::newXMLNode("TRANSFER")

  INVTRAN = XML::newXMLNode("INVTRAN", parent = TRANSFER)
  XML::newXMLNode("FITID", ID, parent = INVTRAN)
  XML::newXMLNode("DTTRADE", format(date,"%Y%m%d%H%M%S"), parent = INVTRAN)

  SECID = XML::newXMLNode("SECID", parent = TRANSFER)
  XML::newXMLNode("UNIQUEID",     ticker,   parent = SECID)
  XML::newXMLNode("UNIQUEIDTYPE", "TICKER", parent = SECID)

  XML::newXMLNode("SUBACCTSEC",  "CASH",    parent = TRANSFER)
  XML::newXMLNode("UNITS",       units,     parent = TRANSFER)
  XML::newXMLNode("TFERACTION",  direction, parent = TRANSFER)
  XML::newXMLNode("POSTYPE",     "LONG",    parent = TRANSFER)

  return(TRANSFER)
}

Deposit <- function(ID, date, name, amount, memo = NULL){

  .transaction(ID, date, name, amount, type = "CREDIT", memo)
}

Withdraw <- function(ID, date, name, amount, memo = NULL){

  .transaction(ID, date, name, amount, type = "DEBIT", memo)
}

.transaction <- function(ID, date, name, amount, type, memo = NULL){

  INVBANKTRAN  = XML::newXMLNode("INVBANKTRAN")

  STMTTRN = XML::newXMLNode("STMTTRN", parent = INVBANKTRAN)

  XML::newXMLNode("TRNTYPE", type, parent = STMTTRN)
  XML::newXMLNode("DTPOSTED", format(date,"%Y%m%d%H%M%S"), parent = STMTTRN)
  XML::newXMLNode("TRNAMT", amount, parent = STMTTRN)
  XML::newXMLNode("FITID", ID, parent = STMTTRN)
  XML::newXMLNode("NAME", name, parent = STMTTRN)
  if(!is.null(memo)) XML::newXMLNode("MEMO", memo, parent = STMTTRN)


  XML::newXMLNode("SUBACCTFUND", "CASH", parent = INVBANKTRAN)

  return(INVBANKTRAN)
}
