#' Infix length 0
#' @rdname infix-operators
#' @export
`%l0%` <- function(x, y) if (length(x) == 0) y else x

#' Infix length is.null
#' @rdname infix-operators
#' @export
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Infix attr
#' @rdname infix-operators
#' @export
`%@%` <- function(x, name) attr(x, name, exact = TRUE)

#' Infix not-in
#' @rdname infix-operators
#' @export
`%nin%` <- function(x, table) match(x, table, nomatch = 0) == 0

#' Infix is even
#' @export
is_even <- function(x) x %% 2 == 0

#' Infix is odd
#' @export
is_odd <- function(x) x %% 2 != 0

#' Infix is positive
#' @export
is_positive <- function(x) x>0

#' Infix is negative
#' @export
is_negative <- function(x) x < 0

#' Infix is zero
#' @export
is_zero  <- function(x) x == 0

#' Infix is empty
#' @export
is_empty <- function(x) length(x) == 0

#' Infix is TRUE
#' @export
is_true <- function(x) identical(x, TRUE)

#' Infix is false
#' @export
is_false <- function(x) identical(x, FALSE)

#' Keep elements based on condition
#' @export
keep <- function(.x, .p, ...) .x[map_lgl(.x, .p, ...)]

#' Discard elements based on condition
#' @export
discard <- function(.x, .p, ...) .x[!map_lgl(.x, .p, ...)]

#' Discard empty elements
#' @export
compact <- function(.x, .p = identity) discard(.x, function(x) is_empty(.p(x)))

#' Set object names
#' @export
set_names <- function(object = nm, nm) {
  names(object) <- nm
  object
}

#' More convenient Map
#' @export
map <- function(.x, .f, ..., .default) {
  default_exists <- !missing(.default)

  nm <- names(.x)

  if (inherits(.f, "function")) {
    lapply(.x, function(x) {
      res <- .f(x, ...)
      if ((length(res) == 0) & default_exists) res <- .default
      res
    }) -> out
  } else if (is.numeric(.f) | is.character(.f)) {
    lapply(.x, function(x) {
      res <- try(x[[.f]], silent = TRUE)
      if (inherits(res, "try-error")) res <- NULL
      if ((length(res) == 0) & default_exists) res <- .default
      res
    }) -> out
  }

  if (length(nm) > 0) out <- set_names(out, nm)

  out
}

#' More convenient mapply
#' @export
map2 <- function(.x, .y, .f, ..., .default) {
  default_exists <- !missing(.default)

  if (inherits(.f, "formula")) {
    .body <- dimnames(attr(stats::terms(.f), "factors"))[[1]]
    .f <- function(.x, .y, . = .x) {}
    body(.f) <- as.expression(parse(text = .body))
  }

  if (inherits(.f, "function")) {
    mapply(
      function(x, ...) {
        res <- .f(x, ...)
        if ((length(res) == 0) & default_exists) res <- .default
        res
      },
      .x, .y,
      ...,
      SIMPLIFY = FALSE, USE.NAMES = FALSE
    )
  }
}

#' Typed Map
#' @export
map_chr <- function(.x, .f, ..., .default) {
  nm <- names(.x)
  out <- as.character((map(.x, .f, ..., .default = .default)))
  if (length(nm) > 0) set_names(out, nm) else out
}

#' Typed more convenient Map
#' @export
map2_chr <- function(.x, .y, .f, ..., .default) {
  as.character(unlist(map2(.x, .y, .f, ..., .default = .default)))
}

#' Typed Map
#' @export
map_lgl <- function(.x, .f, ..., .default) {
  nm <- names(.x)
  out <- as.logical(unlist(map(.x, .f, ..., .default = .default)))
  if (length(nm) > 0) set_names(out, nm) else out
}

#' Typed more convenient Map
#' @export
map2_lgl <- function(.x, .y, .f, ..., .default) {
  as.logical(unlist(map2(.x, .y, .f, ..., .default = .default)))
}

#' Typed Map
#' @export
map_dbl <- function(.x, .f, ..., .default) {
  nm <- names(.x)
  out <- as.double(unlist(map(.x, .f, ..., .default = .default)))
  if (length(nm) > 0) set_names(out, nm) else out
}

#' Typed more convenient Map
#' @export
map2_dbl <- function(.x, .y, .f, ..., .default) {
  as.double(unlist(map2(.x, .y, .f, ..., .default = .default)))
}

#' Typed Map
#' @export
map_int <- function(.x, .f, ..., .default) {
  nm <- names(.x)
  out <- as.integer(unlist(map(.x, .f, ..., .default = .default)))
  if (length(nm) > 0) set_names(out, nm) else out
}

#' Typed more convenient Map
#' @export
map2_int <- function(.x, .y, .f, ..., .default) {
  as.integer(unlist(map2(.x, .y, .f, ..., .default = .default)))
}

#' Typed more convenient Map
#' @export
map_df <- function(.x, .f, ..., .id = NULL) {
  res <- map(.x, .f, ...)
  out <- bind_rows(res, .id = .id)
  out
}

#' Typed more convenient Map
#' @export
map_dfr <- map_df

#' Typed more convenient Map
#' @export
map_dfc <- function(.x, .f, ...) {
  res <- map(.x, .f, ...)
  out <- bind_cols(res)
  out
}

#' Typed more convenient Map
#' @export
map2_df <- function(.x, .y, .f, ..., .id = NULL) {
  res <- map2(.x, .y, .f, ...)
  out <- bind_rows(res, .id = .id)
  out
}

#' Typed more convenient Map
#' @export
map2_dfc <- function(.x, .y, .f, ...) {
  res <- map2(.x, .y, .f, ...)
  out <- bind_cols(res)
  out
}

#' More convenient do.call(rbind.data.frame, …)
#' @export
bind_rows <- function(..., .id = NULL) {
  res <- list(...)

  if (length(res) == 1) res <- res[[1]]

  cols <- unique(unlist(lapply(res, names), use.names = FALSE))

  if (!is.null(.id)) {
    inthere <- cols[.id %in% cols]
    if (length(inthere) > 0) {
      .id <- make.unique(c(inthere, .id))[2]
    }
  }

  id_vals <- if (is.null(names(res))) 1:length(res) else names(res)

  idx <- 1
  do.call(
    rbind.data.frame,
    lapply(res, function(.x) {
      x_names <- names(.x)
      moar_names <- setdiff(cols, x_names)
      if (length(moar_names) > 0) {
        for (i in 1:length(moar_names)) {
          .x[[moar_names[i]]] <- rep(NA, length(.x[[1]]))
        }
      }
      if (!is.null(.id)) {
        .x[[.id]] <- id_vals[idx]
        idx <<- idx + 1
      }
      .x
    })
  ) -> out

  rownames(out) <- NULL

  out
}

#' More convenient cbind.data.frame
#' @export
bind_cols <- function(...) {
  res <- list(...)

  row_mismatch <- lapply(res, nrow) != nrow(res[[1]])

  if (any(row_mismatch)) {
    first_mismatch_pos <- which(row_mismatch)[1]
    stop(paste0(
      "Argument ", first_mismatch_pos,
      " must be length ", nrow(res[[1]]),
      ", not ", nrow(res[[first_mismatch_pos]])
    ))
  }

  if (length(res) == 1) res <- res[[1]]

  col_names <- unlist(lapply(res, names), use.names = FALSE)
  col_names <- make.unique(col_names, sep = "")

  out <- do.call(cbind.data.frame, res)

  names(out) <- col_names
  rownames(out) <- NULL

  out
}

#' More convenient try
#' @export
safely <- function(.f, otherwise = NULL, quiet = TRUE) {
  function(...) capture_error(.f(...), otherwise, quiet)
}

#' More convenient suppressWarmings
#' @export
quietly <- function(.f) {
  function(...) capture_output(.f(...))
}

#' More convient try
#' @export
possibly <- function(.f, otherwise, quiet = TRUE) {
  force(otherwise)
  function(...) {
    tryCatch(.f(...),
      error = function(e) {
        if (!quiet) {
          message("Error: ", e$message)
        }
        otherwise
      },
      interrupt = function(e) {
        stop("Terminated by user", call. = FALSE)
      }
    )
  }
}

#' More convenient capture.output
#' @export
capture_error <- function(code, otherwise = NULL, quiet = TRUE) {
  tryCatch(
    list(result = code, error = NULL),
    error = function(e) {
      if (!quiet) {
        message("Error: ", e$message)
      }

      list(result = otherwise, error = e)
    },
    interrupt = function(e) {
      stop("Terminated by user", call. = FALSE)
    }
  )
}

#' More convenient capture.output
#' @export
capture_output <- function(code) {
  warnings <- character()
  wHandler <- function(w) {
    warnings <<- c(warnings, w$message)
    invokeRestart("muffleWarning")
  }

  messages <- character()
  mHandler <- function(m) {
    messages <<- c(messages, m$message)
    invokeRestart("muffleMessage")
  }

  temp <- file()
  sink(temp)
  on.exit({
    sink()
    close(temp)
  })

  result <- withCallingHandlers(
    code,
    warning = wHandler,
    message = mHandler
  )

  output <- paste0(readLines(temp, warn = FALSE), collapse = "\n")

  list(
    result = result,
    output = output,
    warnings = warnings,
    messages = messages
  )
}

#' Less noisy lapply
#' @export
walk <- function(.x, .f, ...) {
  if (inherits(.f, "function")) {
    for (idx in seq_along(.x)) .f(.x[[idx]], ...)
  } else {
    stop("I'm not sure indexing by name or number makes sense for walk().")
  }
  invisible(.x)
}

#' Less noisy, more convenient lapply
#' @export
walk2 <- function(.x, .y, .f, ...) {
  for (idx in seq_along(.x)) .f(.x[[idx]], .y[[idx]], ...)
  invisible(.x)
}

#' Convenience data frame sorter
#' @export
arrange <- function(data, ..., decreasing = TRUE) {
  cols <- unlist(as.character(substitute(list(...))))[-1]
  data[
    do.call(
      order,
      c(
        lapply(cols, \(.c) data[,.c]),
        list(na.last = NA, decreasing = decreasing)
      )
    ),
  ] -> out
  rownames(out) <- NULL
  out
}

#' Convenience aggregate
#' @export
count <- function(data, ..., .sort = FALSE, .name = "n") {
  cols <- unlist(as.character(substitute(list(...))))[-1]
  do.call(
    base::table,
    c(
      lapply(cols, \(.c) data[,.c]),
      list(
        dnn = cols
      )
    )
  ) |>
    as.data.frame.table(
      responseName = .name
    ) -> .d
  .sort <- tolower(.sort[1])
  if (.sort %in% c("true", "desc")) {
    .d <- .d[order(.d[[.name]], decreasing = .sort =="desc"),]
    rownames(.d) <- NULL
  }
  .d[.d[[.name]] > 0,]
}

#' Convenience subset
#' @export
dfilter <- base::subset

#' Convenience extract
#' @export
dselect <- function(data, ...) {
  cols <- unlist(as.character(substitute(list(...))))[-1]
  data[, cols, drop=FALSE]
}
