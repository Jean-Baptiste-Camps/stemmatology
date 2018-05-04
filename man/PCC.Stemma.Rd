\encoding{utf8}
\name{PCC.Stemma}
\alias{PCC.Stemma}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
PCC.Stemma: Building the Stemma Codicum
}
\description{The \code{PCC.Stemma} function calls successively the functions PCC.disagreement, PCC.buildgroup and PCC.reconstructModel to build a stemma codicum of the tradition studied. By default, it stops when less than four manuscripts are to be compared, as the possibility of errors becomes high. The user is however able to ask the algorithm its final answer for those last manuscripts.
}
\usage{
PCC.Stemma(x, omissionsAsReadings = FALSE, limit = 0, recoverNAs= TRUE, layout_as_stemma = FALSE, ask = TRUE, verbose = FALSE)
}
\arguments{
  \item{x}{a numeric matrix, with witnesses in columns, variant locations in rows, and readings coded by a number; or a character matrix, with witnesses in columns, variant locations in rows, and, in each cell, one or several readings, coded by numbers and separated by a comma (e.g. '1,2,3', if the witness has three different readings).
}
  \item{omissionsAsReadings}{
     logical; if \code{TRUE}, omissions are treated as variant readings. They are taken into account in determining conflicts between variant locations or in computing severe disagreements between witnesses).
     
     Default: \code{FALSE}.
}
  \item{limit}{
The maximum number of severe disagreements expected for witnesses to be in the same group. 

    Default: \code{0}.
}
 \item{recoverNAs}{
  logical; if \code{TRUE}, when an actual witness or reconstructed subarchetype is identified to the reconstructed model of a group, every NA it has is recovered by taking the value of the reconstructed model; if \code{FALSE}, their NAs values are kept. 
  Default: \code{TRUE}.
}
\item{layout_as_stemma}{logical; if TRUE, the witnesses will be placed vertically
according to the distance from their parent, as per the function \code{\link{layout_as_stemma}} (experimental!)
Default: FALSE
}
\item{ask}{
  logical; if FALSE, decisions will be made without asking the user for input.
  Default: TRUE
  }
  \item{verbose}{
  logical; if FALSE, the function will only return the results, without information
  on the operations.
  Default: FALSE
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{The function returns either a single object of class \code{"pccStemma"}, or a list containing several objects of class \code{"pccStemma"} (if multiple stemmatas were drawn)
}
\references{
Jean-Baptiste Camps et Florian Cafiero, « Genealogical variant locations and simplified stemma: a test case », in \emph{Analysis of Ancient and Medieval Texts and Manuscripts: Digital Approaches}, dir. Tara Andrews & Caroline Macé, Turnhout, 2015, p. 69‑93 (Lectio, 1), \url{http://halshs.archives-ouvertes.fr/halshs-01435633/}.

Eric Poole, « L’analyse stemmatique des textes documentaires », in \emph{La pratique des ordinateurs dans la critique des textes}, Paris, 1979, p. 151‑161

Eric Poole, « The Computer in Determining Stemmatic Relationships », \emph{Computers and the Humanities}, 8-4 (1974), p. 207‑16.
}
\author{
Jean-Baptiste Camps (\email{jbcamps@hotmail.com}) & Florian Cafiero (\email{florian.cafiero@polytechnique.edu})
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{PCC.disagreement}}, \code{\link{PCC.buildGroup}}, \code{\link{PCC.reconstructModel}}, \code{\link{layout_as_stemma}}.
}
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{stemma}
\keyword{stemmatology}
