#    Copyright (c) 2020 Orange
#
#    Licensed under the Apache License, Version 2.0 (the "License"); you may
#    not use this file except in compliance with the License. You may obtain
#    a copy of the License at
#
#         http://www.apache.org/licenses/LICENSE-2.0
#
#    Unless required by applicable law or agreed to in writing, software
#    distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
#    WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
#    License for the specific language governing permissions and limitations
#    under the License.
#
#' Compose a Acumos microservice from its source file
#' 
#' \code{composeFromSource} generates everything necessary to create a Acumos microservice directly 
#' from a specifically written R file, representing the component source.
#' 
#' @param file string, name of component source file (an R script)
#' @param name 	string, name of the component
#' @param outputfile string, filename for the resulting component file
#' @param addSource boolean, to add source file to the (ZIP) bundle or not
#'
#' @return Structure describing the component (parsed content of the JSON description).
#'
#' @note
#' A regular component source file is an R script in which at least one of the three following 
#' functions are defined: \code{acumos_predict}, \code{acumos_transform} or \code{acumos_fit}. 
#' They respectively correspond to the functions \code{predict}, \code{transform} and \code{fit} 
#' of \code{\link{compose}}. In that script, if the functions \code{acumos_generate}, \code{acumos_service} 
#' or \code{acumos_initialize} are defined, they will also correspond to the other function 
#' type arguments of \code{\link{compose}}, namely \code{generate}, \code{service} and \code{initialize}.
#' 
#' All the R objects defined in that script are included as auxiliary objects that are to be passed to 
#' the global workspace of the component. They will fill the \code{aux} argument of \code{\link{compose}}.
#' @seealso \code{\link{compose}}
#' 
#' @examples 
#' # see an example source file in:
#' print(system.file("examples","example_0/", package = "acumos"))
#' # compose from acumos.R
#' example_source<-system.file("examples","example_0","acumos.R", package = "acumos")
#' composeFromSource(
#'   file=example_source,
#'   outputfile = "acumos_bundle_example_0.zip"
#' )
#' 
#' @author Alassane Samba
#' 
#' @export 
composeFromSource<-function(file="acumos.R", 
                          name = "R Component", outputfile = "component.zip", addSource=T){
  comp<-new.env()
  exprs <- parse(file)
  tryCatch(
    {
      eval(exprs, envir= comp)
    },
    error=function(cond) {
      message(paste0("Please verify the script '", file, "' works. 
                     Please refer to the package documentation: help(package='acumos')"))
      message("Error message while executing the file:")
      stop(cond)
    }
  ) 
  verbs<-ls(envir = comp)[ls(envir = comp)%in%
                            paste(c("acumos"),
                                  c("predict","transform","fit","generate","service","initialize"), 
                                  sep = "_")]
  aux_names<-ls(envir = comp)[!ls(envir = comp)%in%verbs]
  funcs<-mget(verbs, envir = comp)
  names(funcs)<-gsub(x=names(funcs),pattern = "acumos_", replacement = "")
  do.call(acumos::compose, c(funcs, aux = list(mget(aux_names, envir = comp)), 
                             name = name, file = outputfile))
  dir <- tempfile("acumos-tmp")
  dir.create(dir)
  on.exit(unlink(dir, TRUE))
  file.copy(file,file.path(dir,"component.R"))
  if(addSource) zip(zipfile = outputfile, files = file.path(dir,"component.R"), extras = "-j")
}

#' Push a component into the Acumos repository from its source file
#'
#' push pushes a component into the Acumos repository using the component source file (R file).
#' 
#' @param url URL for the POST request
#' @param file string, name of component source file (an R script)
#' @param name string, name of the component
#' @param addSource boolean, to add source file to the (ZIP) bundle or not
#' @param token token obtained from auth (optional)
#' @param create logical, isCreateMicroservice parameter, see Acumos onboarding documentation
#' @param license optional string, name of a file to supply as the license. If not specified push() 
#' will also try to locate a license.json file in the component bundle if present.
#' @param headers optional, named list or named character vector of HTTP headers that are to be added 
#' to the request. NOTE: the meaning of optional headers depends on the onboarding server so consult 
#' the documentation of the onboarding server for supported additional headers and their meaning.
#' @param ... optional, named list or named character vector of HTTP headers that are to be added to 
#' the request. NOTE: the meaning of optional headers depends on the onboarding server so consult the 
#' documentation of the onboarding server for supported additional headers and their meaning.
#'
#' @return invisibly, result of the request (may change in the future)
#'
#' @note 
#' A regular component source file is an R script in which at least one of the three following 
#' functions are defined: \code{acumos_predict}, \code{acumos_transform} or \code{acumos_fit}. 
#' They respectively correspond to the functions \code{predict}, \code{transform} and \code{fit} 
#' of \code{\link{compose}}. In that script, if the functions \code{acumos_generate}, \code{acumos_service} 
#' or \code{acumos_initialize} are defined, they will also correspond to the other function 
#' type arguments of \code{\link{compose}}, namely \code{generate}, \code{service} and \code{initialize}.
#' 
#' All the R objects defined in that script are included as auxiliary objects that are to be passed to 
#' the global workspace of the component. They will fill the \code{aux} argument of \code{\link{compose}}.
#' 
#' @seealso \code{\link{push}}
#'
#' @export
#'
pushFromSource<-function(url, file, name = "R Component", addSource=T, token, create=TRUE, license,
                         headers, ...){
  # create temporary directory
  dir <- tempfile("acumos-tmp")
  dir.create(dir)
  on.exit(unlink(dir, TRUE))
  # compose
  composeFromSource(file=file, outputfile = file.path(dir,"acumos-app.zip"), name=name, addSource=addSource)
  # run
  push(url=url, file = file.path(dir,"acumos-app.zip"), 
       token=token, create=create, license=license, headers=headers, ...)
}
runFromSource<-function(file="acumos.R",where=getwd(),
                        runtime=list(input_port=3330, data_response=TRUE), init.only=FALSE){
  # create temporary directory
  dir <- tempfile("acumos-tmp")
  dir.create(dir)
  on.exit(unlink(dir, TRUE))
  # compose
  composeFromSource(file=file, outputfile = file.path(dir,"acumos-app.zip"), addSource=F)
  # run
  acumos:::run(file=file.path(dir,"acumos-app.zip"),
               runtime=runtime, init.only=init.only, where=where)
}
