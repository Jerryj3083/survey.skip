\name{sm_plot}
\alias{sm_plot}

\title{
Coloured shadow matrix
}
\description{
This function creates a coloured representation of the shadow matrix. It uses the survey_sp object to obtain the shadow matrix of the survey data. It is a one to one representation of the shadow matrix where each cell is coloured green if the question was answered, blue if the question was skipped, red if the question was not answered and orange if computed. 
}
\usage{
sm_plot(data)
}

\arguments{
  \item{data}{survey_sp, the object obtained from the data_sm function
%%     ~~Describe \code{data} here~~
}
}

\value{
Created a graphical representation of the shadow matrix
}

\author{
Jerry Jiang
}
\keyword{aplot}