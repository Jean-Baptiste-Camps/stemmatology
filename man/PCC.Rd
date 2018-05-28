\encoding{utf8}
\name{PCC}
\alias{PCC}
\title{PCC (Poole-Camps-Cafiero) stemmatological method}
\description{%
    Global shell for all the PCC functions, both exploratory and stemma-building. This command successively executes PCC.Exploratory and PCC.Stemma, while asking user for input when necessary.
}
\usage{
PCC(x, omissionsAsReadings = FALSE, alternateReadings = FALSE, limit = 0, 
    recoverNAs = TRUE, layout_as_stemma = FALSE, pauseAtPlot = FALSE, 
    ask = TRUE, threshold = NULL, verbose = FALSE)
}
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
}
  \item{recoverNAs}{
  logical; if \code{TRUE}, when an actual witness or reconstructed subarchetype is identified to the reconstructed model of a group, every NA it has is recovered by taking the value of the reconstructed model; if \code{FALSE}, their NAs values are kept. Default: \code{TRUE}.
}
\item{layout_as_stemma}{logical; if TRUE, the witnesses will be placed vertically
according to the distance from their parent, as per the function \code{\link{layout_as_stemma}} (experimental!)
Default: FALSE
}
  \item{pauseAtPlot}{logical; if \code{TRUE}, the algorithms stops at each plot during the execution of \code{PCC.contam}. Default: \code{FALSE}.
}
\item{ask}{
  logical; if \code{FALSE}, decisions will be made without asking the user for input.
  Default: \code{TRUE}
  }
    \item{threshold}{numeric; the centrality threshold above which variant locations are considered to be over-conflicting. Used only with \code{ask = FALSE}.}
  \item{verbose}{
  logical; if \code{FALSE}, the function will only return the results, without information on the operations.
  Default: \code{FALSE}.
  }
}
\details{
This function provides a single entry to all the algorithms used in the PCC method. It successively calls \code{\link{PCC.Exploratory}} and  \code{\link{PCC.Stemma}}.
The algorithmic principles of the PCC method are described in Camps & Cafiero 2015. It builds on the propositions of Poole 1974, 1979. 

In a first stage, problematic configurations in the traditions (i.e. configurations that cannot be linked to a normal genealogy, without contamination or polygenesis) are identified by crossing every possible pair of variant locations, and are then plotted as a network. When the most unreliable variant locations (i.e. unreliable) are identified, different methods for eliminating them are offered.

In a second time, a stemma is iteratively built, using the variant locations selected in the first stage. At each step, witness with no severe disagreements (i.e. disagreements between two witnesses, on two readings both shared with at least one other
witness, cf. \emph{Trennfehler}, \emph{errores separativi}) are grouped together. A model is then reconstructed for each group, and either identified to a witness of the group or to an hypothetical subarchetype.

The option \code{recoverNAs=TRUE} is a novelty not described in the original paper (Camps & Cafiero 2015). 

For more information about the underlying principles behind the method applied here, particularly the distinction between severe and benign disagreement, the different status given to readings, omissions and \emph{lacunae}, the notion of conflict between variant locations or the way the stemma is built, see the \emph{references} section. 
}

\value{
The function returns either a single object of class \code{"pccStemma"}, or a list containing several objects of class \code{"pccStemma"} (if multiple stemmata were drawn); 
see \code{\link{PCC.Stemma}}.
}
\references{
Camps, Jean-Baptiste, and Florian Cafiero. ‘Stemmatology: An R Package for the Computer-Assisted Analysis of Textual Traditions’. \emph{Proceedings of the Second Workshop on Corpus-Based Research in the Humanities (CRH-2)}, edited by Andrew U. Frank et al., 2018, pp. 65–74, \url{https://halshs.archives-ouvertes.fr/hal-01695903v1}.

Camps, Jean-Baptiste, and Florian Cafiero. ‘Genealogical Variant Locations and Simplified Stemma: A Test Case’. \emph{Analysis of Ancient and Medieval Texts and Manuscripts: Digital Approaches}, edited by Tara Andrews and Caroline Macé, Brepols, 2015, pp. 69–93, \url{https://halshs.archives-ouvertes.fr/halshs-01435633}, DOI: \href{http://dx.doi.org/10.1484/M.LECTIO-EB.5.102565}{10.1484/M.LECTIO-EB.5.102565}.

Poole, Eric. ‘L’analyse stemmatique des textes documentaires’. \emph{La pratique des ordinateurs dans la critique des textes}, Paris, 1979, p. 151-161.

Poole, Eric, ‘The Computer in Determining Stemmatic Relationships’. \emph{Computers and the Humanities}, 8-4 (1974), p. 207-16.
}
\author{
Jean-Baptiste Camps (\email{jbcamps@hotmail.com}) & Florian Cafiero(\email{florian.cafiero@polytechnique.edu})
}

\seealso{
\code{\link{PCC.Exploratory}}, \code{\link{PCC.Stemma}}.
}
\examples{
# Load data
data("fournival")
# or alternatively, import it
# fournival = import.TEIApparatus(file = "myFournival.xml", 
#    appTypes = c("substantive"))

# Analyse it with the PCC functions
\dontshow{
# subsetting for exec time
fournival = fournival[40:200,c(-5,-7,-8)]
}
# Non interactive mode
PCC(fournival, ask = FALSE, threshold = 0.06)

\dontrun{
# Interactive mode
PCC(fournival)
}

}

\keyword{stemmatology}
