// Finding a single tour on a "mega" board
//=========================================

object KTScala {

type Pos = (Int, Int)    // a position on a chessboard 
type Path = List[Pos]  // a path...a list of positions

//clockwise rotation
val xMoves = List(1,2,2,1,-1,-2,-2,-1)
val yMoves = List(2,1,-1,-2,-2,-1,1,2)
val moves = xMoves zip yMoves

def is_legal(dim: Int, path: Path, x: Pos) : Boolean = {
   (x._1 >= 0 && x._1 < dim) && (x._2 >= 0 && x._2 < dim) && (!path.contains(x))
}


def legal_moves(dim: Int, path: Path, x: Pos) : List[Pos] = {
  moves.map(y => (
    if(is_legal(dim, path,((y._1+x._1),(y._2+x._2)))){
      Some(y._1+x._1 ,y._2+x._2)
      }
      else{
        None
        })).flatten

} 


def ordered_moves(dim: Int, path: Path, x: Pos) : List[Pos] = {
    val movesLegal = legal_moves(dim, path, x)
    val sorted = movesLegal.sortBy(legal_moves(dim, x::path , _ ).size)
    sorted
}

def first(xs: List[Pos], f: Pos => Option[Path]): Option[Path] = {
  if (xs.length == 0) None
  else {
    val tempFirst = f(xs.head)
    if (tempFirst == None) first(xs.drop(1), f) else tempFirst
  }
}

def sqr(x: Int) = x * x


def helper(dim: Int, path: Path, visited: List[Path] ) : Option[Path] = {
  if (visited==Nil){
    None
  }
  else if(visited.head.size == sqr(dim)){
    Some(visited.head)
  }
  else{
    val ordered = ordered_moves(dim, visited.head, visited.head.head).map(_::visited.head)
      helper(dim, path, ordered)

  }
}

def tour_on_mega_board(dim: Int, path: Path) : Option[Path] = {
  helper(dim, path, path::Nil)
}

}

