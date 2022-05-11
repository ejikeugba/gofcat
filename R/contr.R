contr.fn <- function(model, group, customFreq, measure,
                     thresh, call.fn)
{
  modeltype <- modtype(model, measure, call.fn)
  main.df <- fit <- model
  if (!is.na(modeltype)){
    if (isS4(fit) && nrow(fit@model) == 0L) fit <- update(fit, model = TRUE)

    uu <- if (isS4(fit)) fit@model else fit$model
    if (!modeltype=="multinom"){
      dmr <- is.null(dim(model.response(uu)))
      if(!dmr){
        new.dt <- longDt(fit, uu)

        if (isS4(fit)) fit@model <- new.dt
        else fit$model <- new.dt
        fit <- stats::update(fit, resp ~ ., data=new.dt)
      }
    }
    if (modeltype=="vglm" &&
        length(unique(VGAM::model.frame(fit)[,1L]))==2L)
      stop("binary models via vglm function are not supported! consider using glm function.")
    if (modeltype=="clm"){
      y <- fit$model[,1L]
      var <- ncleaner(fit$model[, -1L, drop = FALSE])
      ypred <- try(data.frame(predict(fit, newdata = var, type = "prob")$fit),
                   silent = TRUE)
      if (inherits(ypred, "try-error"))
        stop("in-formular factoring of categorical response is not allowed ",
             "for this particular model.",
             call. = FALSE)
    } else if (modeltype=="vglm"){
      if (call.fn=="lipsitz" || call.fn=="pulkroben" ||
          call.fn=="hosmerlem" )
        y <- ordered(fit@model[,1L])
      if (call.fn=="hosmerlem" || call.fn=="erroR"){
        fname <- VGAM::familyname(fit)
        cm <- fname == "cumulative"
        ac <- fname == "acat"
        cr <- fname == "cratio"
        mn <- fname == "multinomial"
        if (mn) {
          zz <- apply(VGAM::depvar(fit), 1L, function(rr) which(rr==TRUE))
          y <- factor(data.frame(zz)[,1L])
        }
        if (cm || ac || cr) {
          zz <- apply(VGAM::depvar(fit), 1L, function(rr) which(rr==TRUE))
          y <- ordered(data.frame(zz)[,1L])
        }
      }
      if (call.fn=="Rsquared") y <- fit@model[,1L]
      ypred <- VGAM::fitted(fit)
    } else if (modeltype=="mlogit"){
      ypred <- fitted(fit, outcome = FALSE)
      mtr <- matrix(fit$model[,1L], dim(ypred), byrow = TRUE)
      y <- factor(apply(mtr, 1L, function(rr) which(rr==TRUE)))
    } else if (modeltype=="multinom"){
      ypred <- fit$fitted.values
      y <- factor(model.frame(fit)[,1L])
      if (is.ordered(y)) y <- factor(y, ordered = FALSE)
    } else {
      y <- factor(fit$model[,1L])
      ypred <- fitted(fit)
    }
   main.df <- data.frame(y, ypred)
    colnames(main.df) <- c("y", colnames(ypred))
  } else {
    if (call.fn=="lipsitz")
      stop("lipsitz test is unavailable for this class of model.",
           call. = FALSE)
    if (call.fn=="pulkroben")
      stop("Pulkstenis-Robinson test is unavailable for this class of ",
           "model.", call. = FALSE)
  }
  if (!is.data.frame(main.df))
    stop("object should be of class data.frame.", call. = FALSE)
  if (is.na(modeltype)){
    hh <- 0
    for(r in seq_len(ncol(main.df))){
      hh[r] <- class(main.df[,r])[1L]
    }
    on <- c("ordered", "numeric")
    fn <- c("factor", "numeric")
    fc <- all(fn %in% unique(hh))
    od <- all(on %in% unique(hh))
    if (fc || od){
      if (fc){
        fa <- which(hh=="factor")
        if (length(fa)==1L)
          y <- main.df[,fa]
        else
          stop("multiple columns of factored or ordered categories found in the ",
               " given data.frame, whereas, it should be one.", call. = FALSE)
        ypred <- main.df[,-fa]
      }
      if (od){
        or <- which(hh=="ordered")
        if (length(or)==1L)
          y <- main.df[,or]
        else
          stop("multiple columns of factored or ordered categories found in the ",
               "given data.frame, whereas it should be one.", call. = FALSE)
        ypred <- main.df[,-or]
      }
    } else stop("provide a model object or a data.frame with a column of ",
                "factored/ordered observations and numeric column of fitted values.", call. = FALSE)
  }
  y <- droplevels(y)
  nL <- nlevels(y)
  n <- length(y)
  ord <- FALSE
  NAs <- 0
  if (nL < 2)
    stop("observed values should have two or more levels", call. = FALSE)
  classx <- if (nL > 2) "multiclass" else "binary"
  if (classx=="multiclass"){
    if (nrow(data.frame(ypred)) != n || max(na.omit(unclass(y))) != ncol(data.frame(ypred))
        || min(na.omit(unclass(y))) < 0)
      stop("levels of observed values do not equal the number ",
           "of columns of predicted values", call. = FALSE)
    if (is.ordered(y)) ord <- TRUE
  }else{
    ypred <- data.frame(ypred)
    if (ncol(ypred) > 1L)
      stop("more than one column of fitted values for a binary model ",
           "is not allowed", call. = FALSE)
  }
  orig.cn <- colnames(ypred)
  dframe <- cbind(y, ypred)
  if (any(is.na(dframe))){
    dframe <- na.omit(dframe)
    y <- dframe[,1L]
    ypred <- dframe[,-1L]
    nx <- if (classx=="multiclass") nrow(ypred) else length(ypred)
    NAs <- n - nx
  }
  if (!is.data.frame(ypred)) ypred <- data.frame(ypred)
  colnames(ypred) <- orig.cn
  cf <- customFreq
  freq <- NULL
  if (!is.null(cf)){
    if (!is.vector(cf) || !is.numeric(cf) || any(is.na(cf)))
      stop("custom frequencies should be numeric vector with no NAs",
           call. = FALSE)
    if (!sum(cf) == nrow(ypred))
      stop("custom frequencies do not sum up to the total number of ",
           "observations", call. = FALSE)
    group <- length(cf)
    freq <- cf
  }
  if(call.fn=="erroR"){
    eps <- .Machine$double.eps
    if (any(ypred < 0) || any(ypred > 1)) {
      warning(paste0("estimated values below 0 were all replaced with a ",
                     "near-zero constant."), call. = FALSE)
      ypred[ypred < 0] <- eps
      ypred[ypred > 1] <- 1-eps
      ypred <- if (classx=="multiclass") ypred/rowSums(ypred) else ypred
    }
  }
  if (call.fn=="erroR" || call.fn=="Rsquared" || call.fn=="pulkroben") group <- 1e1
  if (call.fn=="hosmerlem" || call.fn=="lipsitz" || call.fn=="pulkroben" ||
      call.fn=="Rsquared")
    thresh <- 1e-1
  nonLik.measures <- c("mckelvey", "efron", "tjur")
  if (call.fn=="Rsquared"){
    if (any(measure == nonLik.measures) && classx == "multiclass")
      stop("measure not available for multi-categorical response models",
           call. = FALSE)
  }
  if (group < 2L) stop("it's not possible to run this test on one group",
                      call. = FALSE)
  if(!is.numeric(thresh) || thresh < 0L || thresh > 1L)
    stop("thresh should be numeric and lie between 0 and 1 inclusive",
         call. = FALSE)
  if (NAs > 1L)
    warning(NAs," ", "observations were deleted ","due to missingness",
            call. = FALSE)
  if (NAs > 0L && NAs <= 1L)
    warning(NAs," ", "observation was deleted ","due to missingness",
            call. = FALSE)
  catpred <- c("at least one categorical predictor is required to run this test. ",
               "Factor and ordinal variables, including binary outcomes, should be ",
               "declared in dataset or model.")
  fail <- c("unsuccessful! the original model could not be updated.")
  badvariable <- c("'pseudo_variable_' is a reserved variable name! ",
                   "consider renaming the variable in the dataset and in the model")
  list(y=y, ypred=ypred, freq=freq, ord=ord, catg=classx, obs=n, group=group,
       mt=modeltype, nL=nL, thresh=thresh, catpred=catpred, fail=fail, model=fit,
       badvariable=badvariable)
}
