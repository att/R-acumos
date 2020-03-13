## Licensed under the Apache License, Version 2.0 (the "License");
## you may not use this file except in compliance with the License.
## You may obtain a copy of the License at
##
## http://www.apache.org/licenses/LICENSE-2.0
##
## Unless required by applicable law or agreed to in writing, software
## distributed under the License is distributed on an "AS IS" BASIS,
## WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
## See the License for the specific language governing permissions and
## limitations under the License.

## collect dependencies - i.e. loaded packages and their versions
pkg.deps <- function() {
   p <- loadedNamespaces()
   base <- c("compiler", "graphics", "tools", "utils", "grDevices", "stats", 
	     "datasets", "methods", "base", "grid", "parallel", "splines", "R",
             "stats4", "tcltk", "R") ## check for Priority: base
   p <- unique(p[! p %in% base])
   np <- p
   p <- character()
   ip <- installed.packages()
   ## iterate recursively until no new deps are detected
   while (length(np) != length(p)) {
       p <- np
       m <- na.omit(match(p, rownames(ip)))
       xp <- unique(gsub(" .*","",unlist(strsplit(c(ip[m,"Depends"], ip[m,"LinkingTo"], ip[m, "Imports"]), ", *"))))
       np <- na.omit(unique(c(xp, p)))
       np <- np[! np %in% base]
   }
   p <- unique(gsub("\\s+","",gsub("[ \t(]+.*", "", p))) ## remove versions and whitespaces
   l <- lapply(p, function(o) { d=packageDescription(o); d=d[c("Package", "Version")]; names(d)=c("name","version"); d })
   list(l)
}

## fetch type info from a function
fetch.types <- function(f, default.in=c(x="character"), default.out=c(x="character")) {
   args <- formals(f)
   in. <- if ("inputs" %in% names(args)) eval(args$inputs, environment(f)) else default.in
   out. <- if ("outputs" %in% names(args)) eval(args$outputs, environment(f)) else default.out
   list(inputs=in., outputs=out.)
}

## compose a component
compose <- function(predict, transform, fit, generate, service, initialize, aux=list(), name="R Component", file="component.amc") {
    dir <- tempfile("acumos-component")
    if (!all(dir.create(dir))) stop("unable to create demporary directory in `",dir,"' to assemble the component bundle")
    
    meta <- list(schema="acumos.schema.model:0.5.0", name=name,
         runtime=list(list(name="r", version="1.0",
		      dependencies = c(list(R=paste(R.version$major, R.version$minor, sep='.')), packages=I(pkg.deps())))),
         methods=list()
    )
    comp <- list(aux = aux, packages = loadedNamespaces())
    proto <- 'syntax = "proto2";\n'
    if (!missing(predict)) {
        comp$predict <- predict
        sig <- fetch.types(predict)
	meta$methods$predict = list(description="predict", input="predictInput", output="predictOutput")
        proto <- c(proto, protoDefine("predictInput", sig$inputs), protoDefine("predictOutput", sig$outputs), protoService("predict"))
    }
    if (!missing(transform)) {
        comp$transform <- transform
        sig <- fetch.types(transform)
	meta$methods$transform = list(description="transform", input="transformInput", output="transformOutput")
        proto <- c(proto, protoDefine("transformInput", sig$inputs), protoDefine("transformOutput", sig$outputs), protoService("transform"))
    }
    if (!missing(fit)) {
        comp$fit <- fit
        sig <- fetch.types(fit)
	meta$methods$fit = list(description="fit", input="fitInput", output="fitOutput")
        proto <- c(proto, protoDefine("fitInput", sig$inputs), protoDefine("fitOutput", sig$outputs), protoService("fit"))
    }
    if (!missing(generate)){
        comp$generate <- generate
        sig <- fetch.types(generate)
	meta$methods$generate = list(description="generate", input="generateInput", output="generateOutput")
        proto <- c(proto, protoDefine("generateInput", sig$inputs), protoDefine("generateOutput", sig$outputs))
    }
    if (!missing(service)) comp$http.service <- service
    if (!missing(initialize)) comp$initialize <- initialize
    if (length(meta$methods) < 1L) warning("No methods defined - the component won't do anything")
    saveRDS(comp, file=file.path(dir, "component.bin"))
    writeLines(jsonlite::toJSON(meta, auto_unbox=TRUE), file.path(dir, "meta.json"))
    writeLines(proto, file.path(dir, "component.proto"))
    ## -j ignores paths (is it portable in Widnows?)
    if (file.exists(file) && unlink(file)) stop("target file already exists and cannot be removed")
    zip(file, c(file.path(dir, "component.bin"), file.path(dir, "meta.json"), file.path(dir, "component.proto")), extras="-j")
    unlink(dir, TRUE)
    invisible(meta)
}

type2proto <- function(x) sapply(x, function(o) {
    switch(o,
           character = "string",
	   integer = "int32",
	   numeric = "double",
	   raw = "bytes",
	   stop("unsupported type ", o)) })

## proto has a more restricted definiton of identifiers so we have to work around that
## by introducing a special quoting scheme
pQ <- function(x) gsub(".", "_o", gsub("_", "_X", x, fixed=TRUE), fixed=TRUE)
pU <- function(x) gsub("_X", "_", gsub("_o", ".", x, fixed=TRUE), fixed=TRUE)

protoDefine <- function(name, types) {
   paste0("message ", name, " {\n",
          paste0("\trepeated ", type2proto(types), " ", pQ(names(types)), " = ", seq.int(types), ";", collapse="\n"),
          "\n}\n")
}

protoService <- function(name, inputType = paste0(name, "Input"), outputType = paste0(name, "Output"))
    paste0("service ", name, "_service {\n\trpc ", name, " (", inputType, ") returns (", outputType, ");\n}\n\n")

.dinfo <- function(level, ..., exp) {
    cd <- Sys.getenv("ACUMOS_DEBUG")
    if (nzchar(cd) && as.integer(cd) >= level) {
        writeLines(paste0(Sys.getpid(), "/", as.numeric(Sys.time()),": ", ...), stderr())
        if (!missing(exp)) writeLines(capture.output(exp), stderr())
    }
}

run <- function(where=getwd(), file="component.amc", runtime="runtime.json", init.only=FALSE) {
    file <- path.expand(file)
    .dinfo(1L, "INFO: starting component in '", where,"', archive:", file, ", runtime:", runtime)
    if (dir.exists(file)) {
        .dinfo(2L, "INFO: component is a directory, assuming unpacked content")
        dir <- file
    } else {
        dir <- tempfile("acumos-runtime")
        dir.create(dir)
        on.exit(unlink(dir, TRUE))
        unzip(file, exdir=dir)
        .dinfo(2L, "INFO: component unpacked in ", dir)
    }
    metadata <- file.path(dir, "meta.json")
    payload <- file.path(dir, "component.bin")
    proto <- file.path(dir, "component.proto")
    c.files <- c(metadata, payload, proto)
    ok <- file.exists(c.files)
    if (!all(ok)) stop(paste0("invalid archive (missing ",
                              paste(basename(c.files[!ok]), collapse=", "),
                              ")"))
    .GlobalEnv$.http.request <- function(...) {
        setwd(where)
        acumos.http(...)
    }
    .GlobalEnv$meta <- jsonlite::fromJSON(readLines(metadata), F)
    .dinfo(2L, "INFO: loaded metadata:", exp=print(.GlobalEnv$meta))
    .GlobalEnv$comp <- readRDS(payload)
    .dinfo(2L, "INFO: components:", exp=str(.GlobalEnv$comp))
    aux <- .GlobalEnv$comp$aux
    .dinfo(2L," INFO: populating global env with aux vars: ", exp=print(names(aux)))
    for (i in names(aux)) .GlobalEnv[[i]] <- aux[[i]]
    aux <- .GlobalEnv$comp$aux <- NULL
    rt <- .GlobalEnv$runtime <- if (is.list(runtime)) runtime else jsonlite::fromJSON(readLines(runtime), FALSE)
    .dinfo(2L, "INFO: loading runtime: ", exp=print(rt))
    .GlobalEnv$state <- new.env(parent=.GlobalEnv)
    RProtoBuf::readProtoFiles(proto)
    if (length(.GlobalEnv$comp$packages)) {
        .dinfo(2L, "INFO: loading packages: ", exp=print(.GlobalEnv$comp$packages))
        for (pkg in .GlobalEnv$comp$packages) library(pkg, character.only=TRUE)
    }
    if (is.function(.GlobalEnv$comp$initialize)) {
        .dinfo(1L, "INFO: calling initialize()")
        .GlobalEnv$comp$initialize()
    }
    if (init.only) return(TRUE)
    if (is.function(.GlobalEnv$comp$generate)) {
        .dinfo(1L, "INFO: calling generate()")
	.GlobalEnv$comp$generate()
    } else {
       	if (is.null(rt$input_port)) stop("input port is missing in the runtime")
        .dinfo(1L, "INFO: starting HTTP server on port ", rt$input_port)
    	Rserve::run.Rserve(http.port=rt$input_port, http.raw.body=TRUE, qap=FALSE)
    }
}

send.msg <- function(url, payload, response=FALSE) {
    .dinfo(3L, "INFO: POST to ", url)
    .dinfo(4L, "INFO: payload: ", exp=print(payload))
    r <- tryCatch(httr::POST(url, body=payload),
                  error=function(e) stop("ERROR: failed to send data to ",url," (from component ", meta$name,"): ", as.character(e)))
    if (isTRUE(response)) return(r)
    if (identical(r$status_code, 200L)) TRUE else {
         warning("POST to ", url, " was not successful: ", rawToChar(r$content))
         FALSE
    }
}

data2msg <- function(data, output) {
    res.msg <- RProtoBuf::P(output)$new()
    .dinfo(4L, "INFO: data2msg: ", exp=str(data))
    if (is.list(data) && !is.null(names(data))) {
        for (n in names(data)) res.msg[[pQ(n)]] <- data[[n]]
    } else res.msg[[1]] <- data
    res.msg$serialize(NULL)
}

msg2data <- function(msg, input) {
    schema <- RProtoBuf::P(input)
    .dinfo(4L, "INFO: msg2data input: ", exp=print(msg))
    data <- schema$read(msg)
    n <- names(data)
    data <- lapply(n, function(o) data[[o]])
    names(data) <- pU(n)
    .dinfo(4L, "INFO: msg2data result: ", exp=str(data))
    data
}

acumos.http <- function(path, query, body, headers) {
    fn <- NULL
    fn.meta <- NULL
    .dinfo(2L, "INFO: handing HTTP ", path, ", query ", query)
    .dinfo(4L, "INFO: state: meta: ", exp=str(meta))
    .dinfo(4L, "INFO: state: comp: ", exp=str(comp))
    fn.type <- "<unknown>"
    if (isTRUE(grepl("^/predict", path))) {
       fn <- comp$predict
       fn.meta <- meta$methods$predict
       fn.type <- "predict"
    }
    if (isTRUE(grepl("^/transform", path))) {
       fn <- comp$transform
       fn.meta <- meta$methods$transform
       fn.type <- "transform"
    }
    if (isTRUE(grepl("^/fit", path))) {
       fn <- comp$fit
       fn.meta <- meta$methods$fit
       fn.type <- "fit"
    }
    .dinfo(3L, "INFO: handler type: ", fn.type, ", formats: ", exp=str(fn.meta))
    if (is.null(fn)) {
        if (is.function(comp$http.service)) return(comp$http.service(path, query, body, headers))
        return("ERROR: unsupported API call")
    }
    if (!is.function(fn)) return(paste0("ERROR: this component doesn't support ", fn.type, "()"))
    if (is.null(fn.meta$input)) return(paste0("ERROR: ", fn.type, "() schema is missing input type specification"))
    tryCatch({
        res <- do.call(fn, msg2data(body, fn.meta$input))
        if (!is.null(res) && !is.null(fn.meta$output)) {
            msg <- data2msg(res, fn.meta$output)
            for (url in runtime$output_url)
	   	send.msg(url, msg)
            if (isTRUE(runtime$data_response))
                return(list(msg, "application/octet-stream"))
        }
        list("OK", "text/plain")
    }, error=function(e) paste("ERROR: in execution: ", as.character(e)))
}

auth <- function(url, user, password) {
    auth_req = list(request_body=list(password=as.character(password)[1L], username=as.character(user)[1L]))
    res <- POST(url, body=jsonlite::toJSON(auth_req, auto_unbox=TRUE), add_headers(`Content-type` = "application/json"))
    if (status_code(res) < 400L) { ## OK
        info <- jsonlite::fromJSON(rawToChar(res$content))
        if (is.null(info$jwtToken))
            stop("jwtToken is missing in the authorization response: ", rawToChar(res$content))
        info$jwtToken
    } else stop("Authentiaction request failed: ", rawToChar(res$content))
}

push <- function(url, file="component.amc", token, create=TRUE, license, headers, ...) {
    ## FIXME: the server currently accepts only multiplart form
    ## with the uncompressed contents - until the server is fixed to
    ## support the component bundle properly we have to unpack and push
    dir <- tempfile("acumos-push")
    dir.create(dir)
    on.exit(unlink(dir, TRUE))
    unzip(file, exdir=dir)
    metadata <- file.path(dir, "meta.json")
    payload <- file.path(dir, "component.bin")
    proto <- file.path(dir, "component.proto")
    addSource<-file.exists(file.path(dir, "component.R"))
    if(addSource){
      source <- upload_file(file.path(dir, "component.R"), type = "text/plain; charset=UTF-8")
    }else{
      source <- NULL
    }
    headers <- if (missing(headers)) list() else as.list(headers)
    headers[["Content-Type"]] <- "multipart/form-data"
    headers[["isCreateMicroservice"]] <- if (isTRUE(create)) "true" else "false"
    if (!missing(token)) headers$Authorization <- token
    body <- list(
      metadata = upload_file(metadata, type = "application/json; charset=UTF-8"),
      schema = upload_file(proto, type = "text/plain; charset=UTF-8"),
      model = upload_file(payload, type = "application/octet"),
      source = source)
    body <- body[!sapply(body, is.null)]
    aux <- list(...)
    if (length(names(aux))) for (i in names(aux)) body[[i]] <- aux[[i]]
    if (!missing(license)) {
        license <- path.expand(license)
	if (!file.exists(license))
	   stop("specified license file `", license, "` does not exist")
	system(paste("cp",shQuote(license), shQuote(file.path(dir, "license.json"))))
    }
    if (file.exists(file.path(dir, "license.json"))) body$license <- upload_file(file.path(dir, "license.json"), "text/plain")
    req <- POST(url, body=body,
                do.call(httr::add_headers, headers), encode="multipart")
    if (http_error(req)) stop("HTTP error in the POST request: ", content(req))
    if (content(req)$status=="SUCCESS") {
                cat("Model pushed successfully to :",url,"\n")
                if(headers[["isCreateMicroservice"]]=="true") {cat("Acumos model docker image successfully created :",content(req)$dockerImageUri,"\n")}
		}
    invisible(content(req))
}
