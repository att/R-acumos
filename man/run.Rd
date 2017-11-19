\name{run}
\alias{run}
\alias{data2msg}
\alias{msg2data}
\alias{send.msg}
\title{
  Run-time tools for Acumos
}
\description{
  The following functions are not exported and not intended to be used
  by users, but are useful for Acumos developers, testing and used by
  the platform itself.
}
\usage{
run(where = getwd(), file = "component.amc", runtime = "runtime.json",
    init.only = FALSE)
data2msg(data, output)
msg2data(msg, input)
send.msg(url, payload)
}
\arguments{
  \item{where}{directory in which the component will be run}
  \item{file}{path to the model component file (as created by
    \code{\link{compose}()}}
  \item{payload}{raw vector, message to send - typically constructed by
    \code{data2msg}}
  \item{runtime}{either path to the runtime JSON file or a structure
    corresponding to the parsed payload of the file}
  \item{init.only}{boolean, if \code{TRUE} then the runtime is setup but
    the actual server/generator is not executed.}
  \item{data}{data to wrap into a message - it is expected to be a list
    (hence a data frame qualifies)}
  \item{output}{name of the proto message type to use}
  \item{msg}{raw vector containing the message}
  \item{input}{name of the proto message type to use}
  \item{url}{string, URL to send the message to}
}
\details{
  \code{run} loads and runs the component by providing a service
  endpoint on localhost and post specified by the \code{input_port}
  property in the \code{runtime} object.
  
  The component file is expected to be created by the
  \code{\link{compose}} function. \code{runtime}
  defines the run-time properties such as input port and
  outputs.

  \code{data2msg} performs the conversion of native types to the binary
  message for communication with other Acumos components.

  \code{msg2data} converts a Acumos message to data according to the
  provided schema.
}
\value{
  The return value of \code{run} is undefined since it is executed for
  its side-effect of providing the service and may never return.

  \code{data2msg} returns a raw vector constituting the message

  \code{msg2data} returns the data repesented by the message
}
\author{
  Simon Urbanek
}
\note{
  The \code{.proto} file containing the schema definitions must be
  loaded before \code{data2msg()} and \code{msg2data()} are used, e.g.,
  by the virtue of being inside the \code{run()} function or explicitly
  using \code{RProtoBuf::readProtoFiles(proto)}.

  The internal Acumos communication protocol is subject to change and
  all of the above functions are hidden.
}
\seealso{
\code{\link{compose}}
}
\examples{
  library(acumos)
  ## compose a generator component that feeds data into the pipeline
  ## row by row. Note that the protocol supports data frame chunks, so
  ## it is possible to feed mupltiple rows at once
  compose(generate=function(..., outputs=lapply(iris[-5], class))
               for (i in seq.int(nrow(iris)))
                   acumos:::send.msg(runtime$output_url,
                       acumos:::data2msg(iris[i,-5], meta$methods$generate$output)),
          file="generator.amc")

  ## compose a simple RF component and run it
  library(randomForest)
  compose(predict=function(..., inputs=lapply(iris[-5], class)) print(as.character(predict(rf, as.data.frame(list(...))))),
          aux = list(rf = randomForest(Species ~ ., data=iris)),
          name="Random Forest")
  p <- parallel::mcparallel(acumos:::run(runtime=list(input_port=8101)))

  Sys.sleep(.5) ## wait a bit for the server to start

  ## run the generator component to feed the above
  acumos:::run(file="generator.amc", runtime=list(output_url="http://127.0.0.1:8101/predict"))

  ## shut down the RF component
  parallel:::mckill(p)
  parallel:::mccollect(p)
  parallel:::mccollect(p)
}
\keyword{interface}
