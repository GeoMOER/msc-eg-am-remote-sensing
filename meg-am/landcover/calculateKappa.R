#  Function: Calculate kappa indices and disagreements
#  This function is part of the GitHub repository
#                     environmentalinformatics-marburg/Rsenal
#
#  Copyright (C) 2013 Hanna Meyer
#
#  This program is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 3 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
#  Please send any comments, suggestions, criticism, or (for our sake) bug
#  reports to admin@environmentalinformatics-marburg.de

calculateKappa=function(ct){
  KappaResult=matrix()
  ct=ct/sum(ct)#percent of pixels 
  cmax=nrow(ct)#number of categories
  #Fraction of Agreement:
  PA=0
  for (i in 1:cmax) {
    PA=PA+ct[i,i]
  }
  #Expected Fraction of Agreement subject to the observed distribution:
  PE=0
  for (i in 1:cmax) {
    PE=PE+sum(ct[i,])*sum(ct[,i])
  }
  #Maximum  Fraction  of  Agreement  subject  to  the  observed  distribution:
  PMax=0
  for (i in 1:cmax) {
    PMax=PMax+min(sum(ct[i,]),sum(ct[,i]))
  }  
  #Kappa Index:
  K=(PA-PE)/(1-PE)
  #Kappa of location:
  Kloc=(PA-PE)/(PMax-PE)
  #Kappa of histogram:
  Khisto=(PMax-PE)/(1-PE)
  #chance agreement:
  CA=100*min((1/cmax),PA,PE)
  #quantity agreement:
  QA=ifelse(min((1/cmax),PE,PA)==(1/cmax),100*min((PE-1/cmax),PA-1/cmax),0)
  #allocation agreement:
  AA=100*max(PA-PE,0)
  #allocation disagreement:
  AD=100*(PMax-PA)
  #quantity disagreement:
  QD=100*(1-PMax)
  KappaResult=cbind(K,Kloc,Khisto,CA,QA,AA,AD,QD)	
  return (KappaResult)
}

install.packages("D:/development/released/github/Rsenal", repos = NULL, type="source")
