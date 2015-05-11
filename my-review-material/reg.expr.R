r can read any text file using readLines() or scan()

stopifnot(identical(paste ("A", 1:6, sep = ""),
                    paste0("A", 1:6)))

Encoding(x)
Read or set the declared encodings for a character vector.

is.character
as.character
substring(x, first,last)

substr("abcdef", 2, 4)
[1] "bcd"

substring("abcdef", 1:6, 1:6)
[1] "a" "b" "c" "d" "e" "f"

substr(rep("abcdef", 4), 1:4, 4:5)
[1] "abcd" "bcde" "cd"   "de"  
** recycle

strsplit(x,split)
split is character vector to use for splitting
If empty matches occur, in particular if split has length 0, 
x is split into single characters. If split has length greater than 1, 
it is re-cycled along x.

> strsplit("a.b.c", ".")
[[1]]
[1] "" "" "" "" ""

> strsplit("a.b.c", "[.]")
[[1]]
[1] "a" "b" "c"
> strsplit("a.b.c", ".", fixed = TRUE)
[[1]]
[1] "a" "b" "c"

fixed: if true, match split exactly

str <- "Now is the time      "
sub(" +$", "", str)
[1] "Now is the time"
sub("[[:space:]]+$", "", str)
[1] "Now is the time"

x <- "R Tutorial"
gsub("ut","ot",x)
[1] "R Totorial"




Replace the first occurrence of a pattern with sub or replace all occurrences with gsub.

> x <- c("This is a sentence about axis",
         +        "A second pattern is also listed here")
> sub("is", "XY", x)
[1] "ThXY is a sentence about axis"       
[2] "A second pattern XY also listed here"
> gsub("is", "XY", x)
[1] "ThXY XY a sentence about axXY"       
[2] "A second pattern XY also lXYted here"
> 

  pattern – A pattern to search for, which is assumed to be a regular expression. 
Use an additional argument fixed=TRUE to look for a pattern without using regular expressions.
  
  

The grep function takes your regex as the first argument, 
and the input vector as the second argument. If you pass value=FALSE 
or omit the value parameter then grep returns a new vector with the indexes 
of the elements in the input vector that could be (partially) matched by the regular expression.
If you pass value=TRUE, then grep returns a vector with copies of the actual elements
in the input vector that could be (partially) matched.

> grep("a+", c("abc", "def", "cba a", "aa"), perl=TRUE, value=FALSE)
[1] 1     3       4
> grep("a+", c("abc", "def", "cba a", "aa"), perl=TRUE, value=TRUE)
[1] "abc" "cba a" "aa"

The grepl function takes the same arguments as the grep function, 
except for the value argument, which is not supported. 
grepl returns a logical vector with the same length as the input vector. 
Each element in the returned vector indicates whether the regex could find a match 
in the corresponding string element in the input vector.

> grepl("a+", c("abc", "def", "cba a", "aa"), perl=TRUE)
[1] TRUE  FALSE TRUE  TRUE



If a value is specified for collapse, the values in the result are then concatenated 
into a single string, with the elements being separated by the value of collapse.

speechMo <- sapply(n, "[",1)
"["截取

for list use [[]]
unlist




midterm3 review

unexclaim <- function(x) {
  return(gsub('!',".",x))
}

updateDate<- function(dates, old.yr){
  index = grep(old.yr,dates)
  new.dates = gsub(old.yr,'2015',dates[index])
  return(new.dates)
}

countcatdog<- function(chvec){
  
  chvec <- tolower(chvec)
  
  num.cat <- 0
  num.dog <- 0
  ind.cat <- gregexpr("cat", chvec)[[1]]
  ind.dog <- gregexpr("dog", chvec)[[1]]
  
  if(ind.cat[1] != -1) num.cat <- length(ind.cat)
  if(ind.dog[1] != -1) num.dog <- length(ind.dog)
  counts <- c(cat = num.cat, dog = num.dog)
  return(counts)
}


paste0(..., collapse) is equivalent to paste(..., sep = "", collapse), slightly more efficiently.

minchin.split <- unlist(strsplit(tolower(minchin), " "))
" " split by words   
"" split by characters

phrases <- c("coat", "cat", "ct", "mat", "Sat!", "Now?", "match", "How much? $10", "7 cats", "ratatatcat", "atatatatatatatatat")
text1 = phrases[grep("at",phrases)]

text2 = phrases[grep("at$",phrases)]

text3 = phrases[grep("(at){2,}",phrases)]

tests = paste("test",1:200, sep="")

tests.all = paste(tests,"",collapse = "")

minchin <- "And try as hard as I like, A small crack appears In my diplomacy-dike. By definition, I begin, Alternative Medicine, I continue Has either not been proved to work, Or been proved not to work. You know what they call alternative medicine That's been proved to work? Medicine."

minchin.split= unlist(strsplit(tolower(minchin)," "))





cats = c("diplocat", "Hi cat", "mat", "at", "t!", "ct")

grep("\\<(cat|at|t)\\>", cats)


