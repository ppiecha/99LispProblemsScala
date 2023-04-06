/*
* P12 (**) Decode a run-length encoded list.
Given a run-length code list generated as specified in problem P11. Construct its uncompressed version.
* List((4,1), (1,2), (2,3), (2,1), (1,4), (4,5)) => List(1, 1, 1, 1, 2, 3, 3, 1, 1, 4, 5, 5, 5, 5)
* */

def decode(list: List[(Int, Any)]): List[Any] = {
  list.flatMap{ case (len, elem) => List.fill(len)(elem) }
}

decode(List((4,1), (1,2), (2,3), (2,1), (1,4), (4,5)))
assert(decode(List((4,1), (1,2), (2,3), (2,1), (1,4), (4,5))) == List(1, 1, 1, 1, 2, 3, 3, 1, 1, 4, 5, 5, 5, 5))