## Constructeur
point <- function(v) {
    v <- as.numeric(v)
    
    stopifnot(identical(length(v), 2L))
    
    structure(v, class = c("point", class(v)))
}

## Méthode print
print.point <- function(x, ...)
    cat("x =", x[1], "& y =", x[2], "\n")

## Méthode norme
norme <- function(x, ...)
    UseMethod("norme")

norme.point <- function(x)
    as.numeric(sqrt(crossprod(x)))

## Est-ce que mes points sont alignés?
isaligned.point <- function(...) {
    points <- list(...)
    
    isTRUE(length(points) <= 2) && return(TRUE)
    
    fit <- crossprod(solve(cbind(1, c(
        points[[1]][1], points[[2]][1]
    ))),
    c(points[[1]][2], points[[2]][2]))
    
    for (p in points[-(1:2)]) {
        pred <- crossprod(c(1, p[1]), fit)[1]
        
        isTRUE(all.equal(pred, p[2])) || return(FALSE)
    }
    
    TRUE
}

isaligned <- function(...)
    UseMethod("isaligned")

## Point marqué
pointM <- function(v, mark = NULL) {
    stopifnot(is.null(mark) ||
                  (is.character(mark) &&
                       identical(length(mark), 1L)))
    
    p <- point(v)
    attr(p, "mark") <- mark
    
    structure(p, class = c("pointM", class(p)))
}

## Méthode mark
mark.pointM <- function(point)
    print(attr(point, "mark"))

mark <- function(point)
    UseMethod("mark")

## Méthode mark<-
`mark<-.pointM` <- function(p, v) {
    attr(p, "mark") <- v
    p
}

`mark<-` <- function(p, v, ...)
    UseMethod("mark<-")
