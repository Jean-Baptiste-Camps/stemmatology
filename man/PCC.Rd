\name{PCC}
\alias{PCC}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{PCC (Poole-Camps-Cafiero) stemmatological method}
\description{%
    Global shell for all the PCC functions, both exploratory and stemma-building. This command successively executes PCC.Exploratory and PCC.Stemma, while asking user for input when necessary.
}
\usage{
PCC(x, omissionsAsReadings = FALSE, alternateReadings = FALSE, limit = 0, recoverNAs = TRUE, pauseAtPlot = FALSE, interactive = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
  if \code{alternateReadings = FALSE} (default), a numeric matrix, with witnesses in columns, variant locations in rows, and readings coded by a number;
  if \code{alternateReadings = TRUE}, a character matrix, with witnesses in columns, variant locations in rows, and, in each cell, one or several readings, coded by numbers and separated by a comma (e.g. '1,2,3', if the witness has three different readings). 
}
  \item{omissionsAsReadings}{
     logical; if \code{TRUE}, omissions are treated as variant readings. They are taken into account in determining conflicts between variant locations or in computing severe disagreements between witnesses).%; if \code{FALSE}, they are treated only as omissions.  
     Default: \code{FALSE}.
}
  \item{alternateReadings}{
  logical; if \code{TRUE}, a witness can have multiple variants for a single variant location (contaminated manuscripts, editio variorum, …), encoded as comma-separated values. %; if \code{FALSE}, only one reading per witness is allowed. 
  Default: \code{FALSE}.
  }
  \item{limit}{The maximum number of severe disagreements expected for witnesses to be in the same group. Default: \code{0}.
  %%%TODO(JBC): Implémenter cette option !!!
}
  \item{recoverNAs}{
  logical; if \code{TRUE}, when an actual witness or reconstructed subarchetype is identified to the reconstructed model of a group, every NA it has is recovered by taking the value of the reconstructed model; if \code{FALSE}, their NAs values are kept. Default: \code{TRUE}.
}
  \item{pauseAtPlot}{logical; if \code{TRUE}, the algorithms stops at each plot during the execution of \code{PCC.contam} (by setting graphical parameter \code{ask = TRUE}). Default: \code{FALSE}.
}
  \item{interactive}{logical; if \code{TRUE} (recommended), the algorithm stops each time user input is needed; if \code{FALSE}, it tries to guess an appropriate value. Default: \code{TRUE}.
}
}
\details{
This function provides a single entry to all the algorithms used in the PCC method. It successively calls \code{\link{PCC.Exploratory}} and  \code{\link{PCC.Stemma}}.
The algorithmic principles of the PCC method are described in Camps & Cafiero 2015. It builds on the propositions of Poole 1974, 1979. 

In a first stage, problematic configurations in the traditions (i.e. configurations that cannot be linked to a normal genealogy, without contamination or polygenesis) are identified by crossing every possible pair of variant locations, and are then plotted as a network. When the most unreliable variant locations (i.e. unreliable) are identified, different methods for eliminating them are offered.

In a second time, a stemma is iteratively built, using the variant locations selected in the first stage. At each step, witness with no severe disagreements (i.e. disagreements between two witnesses, on two readings both shared with at least one other
witness, cf. \emph{Trennfehler}, \emph{errores separativi}) are grouped together. A model is then reconstructed for each group, and either identified to a witness of the group or to an hypothetic subarchetype.

The option \code{recoverNAs=TRUE} is a novelty not described in the original paper (Camps & Cafiero 2015). 

For more information about the underlying principles behind the method applied here, particulary the distinction between severe and benigne disagreement, the different status given to readings, omissions and \emph{lacunae}, the notion of conflict between variant locations or the way the stemma is built, see the \emph{references} section. 
}

\value{
The function returns either a single object of class \code{"pccStemma"}, or a list containing several objects of class \code{"pccStemma"} (if multiple stemmatas were drawn); 
see \code{\link{PCC.Stemma}}.
}
\references{
Jean-Baptiste Camps et Florian Cafiero, « Genealogical variant locations and simplified stemma: a test case », in \emph{Analysis of Ancient and Medieval Texts and Manuscripts: Digital Approaches}, dir. Tara Andrews & Caroline Macé, Turnhout, 2015, p. 69‑93 (Lectio, 1), \url{http://halshs.archives-ouvertes.fr/halshs-01435633/}.

Eric Poole, « L’analyse stemmatique des textes documentaires », in \emph{La pratique des ordinateurs dans la critique des textes}, Paris, 1979, p. 151‑161

Eric Poole, « The Computer in Determining Stemmatic Relationships », \emph{Computers and the Humanities}, 8-4 (1974), p. 207‑16.
}
\author{
Jean-Baptiste Camps (\email{jbcamps@hotmail.com}) & Florian Cafiero
}
\note{
%%%
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{PCC.Exploratory}}, \code{\link{PCC.Stemma}}.
}
\examples{
load(fournival)
PCC(fournival)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{stemmatology}
