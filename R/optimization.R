#' Conditional Value at Risk portfolio optimization
#'
#' Portfolio optimization using CVaR risk constraints. Linear programming a la Uryasev
#' @export
#' @param assetrets numobs x numassets numeric matrix of asset returns
#' @param exprets vector of expected returns. Default - calculated automatically from
#'  \code{assetrets}
#' @param confidence numeric CVaR confidence level. Default = .95
#' @param opttype character optimization type. Default = "tgtret". Valid options include
#'  "minrisk", "tgtret", "tgtrisk", "maxret"
#' @param target numeric target value, depending on optimization type. Default = 0
#' @param name character name of optimized portfolio
#' @inheritParams opt_mv
#' @return Optimization results list (one portfolio) including $name, $solved, $wts, $exprets, $ret
#'  $vol, $var, $var.model, $cvar, $cvar.model, $summary, $model, objfn
#' @seealso
#' \code{\link{opt_mv}}: mean-variance portfolio optimization \cr
#' \code{\link{frontier_cvar}}: CVaR efficient frontier \cr
#' @examples
#' opt_cvar(oi::simrets, opttype="tgtrisk", target=0)
#' opt_cvar(oi::simrets, opttype="tgtret", target=.03, bounds=oi::samplebounds$bounds[[2]])
opt_cvar <- function(assetrets,
                     exprets=apply(assetrets, 2, mean),
                     confidence=.95,
                     opttype="tgtret",  #valid: minrisk, tgtret, tgtrisk, maxret
                     target=0,
                     name="Portfolio",
                     lb=rep(0, length(exprets)),
                     ub=rep(1, length(exprets)),
                     bounds=NA,
                     portwt=1) {

  assetnames <- colnames(assetrets)
  numassets <- ncol(assetrets)
  numobs <- nrow(assetrets)

  # set up group bounds
  ifelse(!is.na(bounds),usegbounds<-T,usegbounds<-F)
  if(usegbounds) {
    numgroups <- nrow(bounds$groups)
    lb <- bounds$lb
    ub <- bounds$ub
  } else {
    numgroups <- 0
  }

  numconstraints <- 1 + 1 + numobs + numgroups  # portfolio weight + [exprets or cvar] + numobs + numgroups
  numvars <- numassets + 1 + numobs  # numassets + VaR + numobs
  cvarcoef <- 1 / (numobs * (1 - confidence))  # CVaR coefficient for auxiliary variables

  # create lp model
  lp <- lpSolveAPI::make.lp(numconstraints, numvars)

  expretrow <- c(exprets, 0, rep(0, numobs))  # expected returns, 0 VaR, 0 CVaR
  cvarrow <- c(rep(0, numassets), 1, rep(cvarcoef, numobs))  # 0 returns, 1 VaR, cvarcoef CVaR

  # set objective function
  if (opttype %in% c("tgtret", "minrisk")) {
    objfn <- cvarrow  # minimize CVaR
  } else {  # tgtrisk, maxret
    objfn <- -expretrow  # maximize return
  }
  lpSolveAPI::set.objfn(lp, objfn)  # optimizer minimizes; set exprets negative to maximize

  # set constraints
  # portfolio weight constraint
  rownum <- 1
  lpSolveAPI::set.row(lp, rownum, rep(1, numassets), 1:numassets)  # only needed for asset columns
  lpSolveAPI::set.constr.type(lp, "=", rownum)
  lpSolveAPI::set.rhs(lp, portwt, rownum)

  # expected return or cvar constraint
  rownum <- 2
  if (opttype == "minrisk") {
    lpSolveAPI::set.row(lp, rownum, expretrow)
    lpSolveAPI::set.constr.type(lp, ">=", rownum)
    lpSolveAPI::set.rhs(lp, -Inf, rownum)  # any return number is fine, not a constraint here
  } else if (opttype == "tgtrisk") {
    lpSolveAPI::set.row(lp, rownum, cvarrow)
    lpSolveAPI::set.constr.type(lp, "<=", rownum)
    lpSolveAPI::set.rhs(lp, -target, rownum)  # cvar sign is flipped in this methodology (positive number = loss)
  } else if (opttype == "tgtret") {
    lpSolveAPI::set.row(lp, rownum, expretrow)
    lpSolveAPI::set.constr.type(lp, ">=", rownum)
    lpSolveAPI::set.rhs(lp, target, rownum)
  } else if (opttype == "maxret") {
    lpSolveAPI::set.row(lp, rownum, cvarrow)
    lpSolveAPI::set.constr.type(lp, "<=", rownum)
    lpSolveAPI::set.rhs(lp, Inf, rownum)  # any risk number is fine, not a constraint here
  }

  # cvar constraints
  varcolix <- numassets + 1
  auxstartix <- varcolix + 1
  for (o in 1:numobs) {
    rownum <- o + 2
    # each row = asset returns for one obs, 1 for VaR, then 1 for aux in appropriate column
    lpSolveAPI::set.row(lp, rownum, c(assetrets[o, ], 1, 1), c(1:numassets, varcolix, auxstartix + o - 1))
    lpSolveAPI::set.constr.type(lp, ">=", rownum)
    lpSolveAPI::set.rhs(lp, 0, rownum)
  }

  # group constraints
  if (usegbounds) {
    for (g in 1:numgroups) {
      rownum <- 1 + 1 + numobs + g  # portwt + [expret or cvar] + numobs
      lpSolveAPI::set.row(lp, rownum, bounds$groups[g, ], 1:numassets)
      lpSolveAPI::set.constr.type(lp, "<=", rownum)
      lpSolveAPI::set.constr.value(lp, lhs=bounds$glb[g], rhs=bounds$gub[g], constraints=rownum)  # both lhs and rhs
    }
  }

  # set upper and lower bounds
  lpSolveAPI::set.bounds(lp,
                         c(lb, -Inf, rep(0, numobs)),
                         c(ub, Inf, rep(Inf, numobs)))

  # solve
  optstatus <- solve(lp)

  # set up results
  res <- list(name=name,
              solved=F,
              wts=NA,
              exprets=NA,
              ret=NA,
              vol=NA,
              var=NA,
              var.model=NA,
              cvar=NA,
              cvar.model=NA,
              summary=NA,
              model=lp,
              objfn=objfn
  )
  if (optstatus == 0) { # solved successfully
    res$solved=T
    res$wts <- lpSolveAPI::get.variables(lp)[1:numassets]
    names(res$wts) <- assetnames
    res$exprets <- exprets
    names(exprets) <- assetnames
    res$ret <- t(res$wts) %*% exprets
    portrets <- res$wts %*% t(assetrets)
    res$vol <- sd(portrets)
    res$var <- caim::var_emp(portrets)
    res$var.model <- ifelse(opttype == "maxret", # risk is unconstrained in maxret, need to calc separately
                            res$var,
                            -lpSolveAPI::get.variables(lp)[numassets + 1])
    res$cvar <- caim::cvar_emp(portrets)
    res$cvar.model <- ifelse(opttype == "maxret",
                             res$cvar,
                             -t(lpSolveAPI::get.variables(lp)) %*% cvarrow)
    res$summary <- matrix(c(res$ret, res$vol, res$var, res$var.model, res$cvar, res$cvar.model),1,6)
    colnames(res$summary) <- c("Mean", "Vol", "VaR", "VaR.Mdl", "CVaR", "CVaR.Mdl")
    row.names(res$summary) <- name
    print(paste("solution found", round(res$ret, 4), round(res$cvar, 4)))
  } else {
    print("solution NOT found")
  }
  return(res)
}
