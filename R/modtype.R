modtype <- function(model, measure, call.fn)
{
  if (inherits(model, what = 'list') && length(model) > 1L)
    stop("passing multiple objects at a time is not allowed", call. = FALSE)
  mtype <- NULL
  if (inherits(model, what = "glm")){
    if (call.fn == "hosmerlem" && !(model$family$family=="binomial"))
      stop("the intended test is only available for the binomial family",
           call. = FALSE)
    mtype <- "glm"
  } else if (inherits(model, what = "vglm")){
    fname <- VGAM::familyname(model)
    cm <- fname == "cumulative"
    ac <- fname == "acat"
    cr <- fname == "cratio"
    mn <- fname == "multinomial"
    if (call.fn == "Rsquared"){
      if (!(cm || mn))
        stop("model family should be any of cumulative, propodds or " ,
             "multinomial.", call. = FALSE)
    }
    if (call.fn =="brant"|| call.fn =="LRT"){
      if (!fname == "cumulative")
        stop("model family should be either cumulative or propodds.",
             call. = FALSE)
      if (model@misc$parallel != TRUE)
        stop("the non-parallel model is not supported, consider adding ",
             "parallel=TRUE in vglm call.", call. = FALSE)
    }
    if (call.fn == "hosmerlem"||call.fn =="lipsitz"||call.fn =="pulkroben"){
      if (call.fn == "hosmerlem"){
        if (!any(cm, ac, cr, mn)){
          stop("model family should be any of cumulative, propodds, acat, cratio or ",
               "multinomial.", call. = FALSE)
        }
      }
      if (call.fn =="lipsitz"||call.fn =="pulkroben")
        if (!any(cm, ac, cr)) stop("model family should be either cumulative, propodds.",
                                  "acat or cratio", call. = FALSE)
      nL <- ncol(model@y)
      md <- names(model@coefficients)[-c(1:(nL-1))]
      mf <- colnames(model@x)[-1L]
      uncon <- c("test for the unconstrained model is not yet available, consider ",
                 "using parallel=TRUE in the model specification.")
      if (cm && model@misc$parallel == FALSE) stop(uncon, call. = FALSE)
      if (mn && model@misc$parallel == FALSE) stop(uncon, call. = FALSE)
      if (ac && length(md)!=length(mf)) stop(uncon, call. = FALSE)
      if (cr && length(md)!=length(mf)) stop(uncon, call. = FALSE)
    }
    mtype <- "vglm"
  } else if (inherits(model, what = "serp")){
    funs <- c("brant", "LRT", "hosmerlem", "lipsitz", "pulkroben")
    if (any(call.fn == funs)){
      if (model$slope != "parallel")
        stop("the non-parallel model is not supported, consider adding ",
             "slope='parallel' in serp call.", call. = FALSE)
    }
    mtype <- "serp"
  } else if (inherits(model, what = "multinom")){
    mtype <- "multinom"
  } else if (inherits(model, what = "clm") || inherits(model, what = "clm2")){
    mtype <- "clm"
  } else if (inherits(model, what = "polr")){
    mtype <- "polr"
  } else if (inherits(model, what = "mlogit")){
    mtype <- "mlogit"
  } else return(NA)
  if (call.fn == "lipsitz" || call.fn == "pulkroben") call.fn <- "lp_pk"
  grpmod1 <- c("glm", "multinom", "mlogit")
  grpmod2 <- c("glm", "vglm","serp", "polr", "clm", "mlogit", "multinom")
  grpmod3 <- c("serp","vglm")
  grpmod4 <- c("serp", "polr", "clm", "vglm")
  grpmod5 <- c("glm", "vglm")
  if (call.fn == "Rsquared"){
  mx <- c("mckelvey", "efron", "tjur")
  if (measure %in% mx && !mtype %in% grpmod5)
    stop("requested measure is not available for the supplied model",
         call. = FALSE)}
  if (call.fn == "brant" && mtype %in% grpmod1) return(NA)
  if (call.fn == "erroR" && !mtype %in% grpmod2) return(NA)
  if (call.fn == "LRT"   && !mtype %in% grpmod3) return(NA)
  if (call.fn == "hosmerlem" && !mtype %in% grpmod2) return(NA)
  if (call.fn == "lp_pk" && !mtype %in% grpmod4) return(NA)
  mtype
}
