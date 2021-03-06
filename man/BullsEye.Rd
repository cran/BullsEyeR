% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BullsEyeR.R
\name{BullsEye}
\alias{BullsEye}
\title{Topic Modelling}
\usage{
BullsEye(ds, spvar = 0.99, myStopWords = NULL, tno = 20, seedno = 12345,
  stemvar = 0)
}
\arguments{
\item{ds}{a character vector of text documents}

\item{spvar}{a sparsity variable which defaults to 0.99}

\item{myStopWords}{a character vector of custom stop words which defaults to NULL}

\item{tno}{a number of topics to be used to model text using LDA approach which defaults to 20}

\item{seedno}{seed which defaults to 12345}

\item{stemvar}{a variable indicating stemming to be performed or not which defaults to '0' meaning no stemming}
}
\value{
A dataframe with index of empty rows  and topic terms.
}
\description{
BullsEye runs intial preprocessing, removes custom stop words and runs LDA with selected number of topics.
}
\examples{
\dontrun{
# Run it and see for yourself
}
data.tmp<-read.csv(system.file("ext", "testdata.csv", package="BullsEyeR"))
ds<-as.character(data.tmp$Story[1:2])
stopwords<-c("sallin","hannah","company","number","started","unlike")
BullsEye(ds=ds,spvar=0.99,myStopWords=stopwords,tno=20,seedno=12345,stemvar=0)
}
\seealso{
\code{\link[ldatuning]{FindTopicsNumber}}
}
