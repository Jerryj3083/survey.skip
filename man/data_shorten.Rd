\name{data_shorten}
\alias{data_shorten}

\title{
Reducing survey questions rows
}
\description{
Given a subset of questions, remove the rows where all responses are not asked.
}
\usage{
data_shorten(x, verbose = FALSE)
}

\arguments{
  \item{x}{survey_sp, the object obtained from the data_sm function
}
  \item{verbose}{list, containing a subset of the data and shadow matrix 
}
}
\details{
After a subset of columns have selected using the select fuction, the rows of the remaining survey data and shadow matrix are removed if all responses are NA from skipped questions.
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