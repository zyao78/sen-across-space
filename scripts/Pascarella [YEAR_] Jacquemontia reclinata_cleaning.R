
Crandon_Park_2000<- readxl::read_xlsx("~/Downloads/JacquemontiareclinataMatrices2000_2010JPascarella.xlsx", sheet = 3, col_names = F)
South_Beach_2000<- readxl::read_xlsx("~/Downloads/JacquemontiareclinataMatrices2000_2010JPascarella.xlsx", sheet = 4, col_names = F)
Crandon_Park_2001<- readxl::read_xlsx("~/Downloads/JacquemontiareclinataMatrices2000_2010JPascarella.xlsx", sheet = 5, col_names = F)
South_Beach_2001<- readxl::read_xlsx("~/Downloads/JacquemontiareclinataMatrices2000_2010JPascarella.xlsx", sheet = 6, col_names = F)
Crandon_Park_2002<- readxl::read_xlsx("~/Downloads/JacquemontiareclinataMatrices2000_2010JPascarella.xlsx", sheet = 7, col_names = F)
South_Beach_2002<- readxl::read_xlsx("~/Downloads/JacquemontiareclinataMatrices2000_2010JPascarella.xlsx", sheet = 8, col_names = F)
Crandon_Park_2003<- readxl::read_xlsx("~/Downloads/JacquemontiareclinataMatrices2000_2010JPascarella.xlsx", sheet = 9, col_names = F)
South_Beach_2003<- readxl::read_xlsx("~/Downloads/JacquemontiareclinataMatrices2000_2010JPascarella.xlsx", sheet = 10, col_names = F)
Crandon_Park_2004<- readxl::read_xlsx("~/Downloads/JacquemontiareclinataMatrices2000_2010JPascarella.xlsx", sheet = 11, col_names = F)
South_Beach_2004<- readxl::read_xlsx("~/Downloads/JacquemontiareclinataMatrices2000_2010JPascarella.xlsx", sheet = 12, col_names = F)
Crandon_Park_2005<- readxl::read_xlsx("~/Downloads/JacquemontiareclinataMatrices2000_2010JPascarella.xlsx", sheet = 13, col_names = F)
South_Beach_2005<- readxl::read_xlsx("~/Downloads/JacquemontiareclinataMatrices2000_2010JPascarella.xlsx", sheet = 14, col_names = F)
Crandon_Park_2006<- readxl::read_xlsx("~/Downloads/JacquemontiareclinataMatrices2000_2010JPascarella.xlsx", sheet = 15, col_names = F)
South_Beach_2006<- readxl::read_xlsx("~/Downloads/JacquemontiareclinataMatrices2000_2010JPascarella.xlsx", sheet = 16, col_names = F)
Crandon_Park_2007<- readxl::read_xlsx("~/Downloads/JacquemontiareclinataMatrices2000_2010JPascarella.xlsx", sheet = 17, col_names = F)
South_Beach_2007<- readxl::read_xlsx("~/Downloads/JacquemontiareclinataMatrices2000_2010JPascarella.xlsx", sheet = 18, col_names = F)
Crandon_Park_2008<- readxl::read_xlsx("~/Downloads/JacquemontiareclinataMatrices2000_2010JPascarella.xlsx", sheet = 19, col_names = F)
South_Beach_2008<- readxl::read_xlsx("~/Downloads/JacquemontiareclinataMatrices2000_2010JPascarella.xlsx", sheet = 20, col_names = F)
Crandon_Park_2009<- readxl::read_xlsx("~/Downloads/JacquemontiareclinataMatrices2000_2010JPascarella.xlsx", sheet = 21, col_names = F)
South_Beach_2009<- readxl::read_xlsx("~/Downloads/JacquemontiareclinataMatrices2000_2010JPascarella.xlsx", sheet = 22, col_names = F)

# get into an array
CP <- array(data = NA, dim = c(5,5,10))
mats <- list(Crandon_Park_2000,Crandon_Park_2001,Crandon_Park_2002,Crandon_Park_2003,Crandon_Park_2004,Crandon_Park_2005,Crandon_Park_2006,Crandon_Park_2007,Crandon_Park_2008,Crandon_Park_2009)
for (i in 1:10) {
  CP[,,i] <- as.matrix(as.data.frame(mats[[i]]))
}

SB <- array(data = NA, dim = c(5,5,10))
mats <- list(South_Beach_2000,South_Beach_2001,South_Beach_2002,South_Beach_2003,South_Beach_2004,South_Beach_2005,South_Beach_2006,South_Beach_2007,South_Beach_2008,South_Beach_2009)
for (i in 1:10) {
  SB[,,i] <- as.matrix(as.data.frame(mats[[i]]))
}

# average








