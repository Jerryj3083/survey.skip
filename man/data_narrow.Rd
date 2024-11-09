\name{data_narrow}
\alias{data_narrow}

\title{
Reducing survey questions columns
}
\description{Given a subset of respondents or rows, drop all questions where all responses are not asked.
}
\usage{
data_narrow(x, verbose = FALSE)
}

\arguments{
  \item{x}{survey_sp, the object obtained from the data_sm function
}
  \item{verbose}{list, containing a subset of the data and shadow matrix 

}
}
\details{
After a subset of rows have filtered using the filter fuction, the columns of the remaining survey data and shadow matrix are removed if all responses are NA from skipped questions.
}
\value{
Returns a list containing the reduced survey data and shadow matrix
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Jerry Jiang
}
\keyword{manip}