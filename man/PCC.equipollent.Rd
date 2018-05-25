\encoding{utf8}
\name{PCC.equipollent}
\alias{PCC.equipollent}
\title{
PCC Exploratory Methods: Extracting Competing Genealogies
}
\description{A single table of variant locations can sometimes 
reflect different competing genealogies, due to contamination, either for a single manuscript, or for the whole tradition. \code{PCC.equipollent} identifies the variant locations without internal conflicts, and allows to create separate databases for each internally consistent configuration.
}
\usage{
PCC.equipollent(x, ask = TRUE, scope = NULL, wits = NULL, verbose = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A \code{PCC.conflicts} object.
}
\item{ask}{
  logical; if \code{FALSE}, decisions will be made without asking the user for input.
  Default: \code{TRUE}. With \code{FALSE}, it is mandatory to specify \code{scope} 
  and \code{wits}.
  }
  \item{scope}{should the inconsistent variant locations be neutralised for the
  whole tradition (\code{"T"}) or only some witnesses (\code{"W"})?
  Use only with \code{ask = FALSE}.
  }
  \item{wits}{a vector containing the names of the witnesses for which 
  inconsistent variant locations should be neutralised.
  Use only with \code{ask = FALSE} and \code{scope = 'W'}.
  }
  \item{verbose}{
  logical; if \code{FALSE}, the function will only return the results, without information on the operations.
  Default: \code{FALSE}.
  }
}
\details{
Some over-conflicting variant locations can be algorithmically ruled out for the building of a stemma (see \code{\link{PCC.conflicts}}, \code{\link{PCC.overconflicting}} and \code{\link{PCC.elimination}}). Yet, in some cases, choosing between conflicting variables is algorithmically undecidable.
This might be due sometimes to contamination (see \code{\link{PCC.contam}}). \code{PCC.equipollent} helps addressing such cases. 
It tries to assess, first, the sets of variant locations that are internally consistent (no conflict among themselves), and then, creates as many different databases as sets were found. In creating these new databases, the variant location that have conflicting information with the current set are either fully neutralised (\code{scope = "T"}) or neutralised only for some witnesses (\code{scope = "W"}).
}
\value{
An object of class pccEquipollent, a list containing
    \item{databases}{a list with all alternative databases that have been created, if any}
    \item{notInConflict}{a list with the set(s) of VL without internal conflicts}
}
\references{
Camps, Jean-Baptiste, and Florian Cafiero. ‘Stemmatology: An R Package for the Computer-Assisted Analysis of Textual Traditions’. \emph{Proceedings of the Second Workshop on Corpus-Based Research in the Humanities (CRH-2)}, edited by Andrew U. Frank et al., 2018, pp. 65–74, \url{https://halshs.archives-ouvertes.fr/hal-01695903v1}.

Camps, Jean-Baptiste. ‘Detecting Contaminations in Textual Traditions
Computer Assisted and Traditional Methods’.  Leeds, International Medieval Congress, 2013, unpublished paper, \url{https://www.academia.edu/3825633/Detecting_Contaminations_in_Textual_Traditions_Computer_Assisted_and_Traditional_Methods}.

Camps, Jean-Baptiste, and Florian Cafiero. ‘Genealogical Variant Locations and Simplified Stemma: A Test Case’. \emph{Analysis of Ancient and Medieval Texts and Manuscripts: Digital Approaches}, edited by Tara Andrews and Caroline Macé, Brepols, 2015, pp. 69–93, \url{https://halshs.archives-ouvertes.fr/halshs-01435633}, DOI: \href{http://dx.doi.org/10.1484/M.LECTIO-EB.5.102565}{10.1484/M.LECTIO-EB.5.102565}.
}
\author{
Jean-Baptiste Camps & Florian Cafiero
}
\section{Warning}{
This function is still experimental, and will work optimally only for simple cases, where competing genealogies can be easily identified.
}

\seealso{
\code{\link{PCC.Exploratory}}, \code{\link{PCC.conflicts}}, \code{\link{PCC.overconflicting}}, \code{\link{PCC.elimination}}, \code{\link{PCC.contam}}.
}
\examples{
# load data
data("fournival")
\donttest{
# look for conflicts
y = PCC.conflicts(fournival)
# identify and eliminate overconflicting VL
y = PCC.overconflicting(y, ask = FALSE, threshold = 0.06)
y = PCC.elimination(y)
# look for further conflicts
y = PCC.conflicts(y)
}
\dontshow{# subsetting for time gain
y = fournival[1:25,]
y = PCC.conflicts(y)}
# and now, create configurations for competing genealogies
# for instance, for one witness
newDB = PCC.equipollent(y, ask = FALSE, scope = "W", wits = "D")

# Alternatively, you can create them for the whole tradition
newDB = PCC.equipollent(y, ask = FALSE, scope = "T")
# or for several witnesses
newDB = PCC.equipollent(y, ask = FALSE, scope = "W", wits = c("A", "D"))

# and then you proceed to create one or several stemmata, e.g.
# PCC.Stemma(newDB$databases[[1]], ask = FALSE)
}
\keyword{stemmatology}
\keyword{manip}
