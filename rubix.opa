/** Rubix elementary moves */

type Face.t = {top}
           or {bottom}
           or {front}
           or {rear}
           or {left}
           or {right}

type Move.t = { Face.t f, int i }
// +1 move is the clockwise rotation when one observer looks at the cube right to the given 'face'.

module Move {

top = {f:{top}, i: 1}
bottom = {f:{bottom}, i: 1}
front = {f:{front}, i: 1}
rear = {f:{rear}, i: 1}
left = {f:{left}, i: 1}
right = {f:{right}, i: 1}

antitop = {f:{top}, i: -1}
antibottom = {f:{bottom}, i: -1}
antifront = {f:{front}, i: -1}
antirear = {f:{rear}, i: -1}
antileft = {f:{left}, i: -1}
antiright = {f:{right}, i: -1}

elementary_moves = [ top, bottom, front, rear, left, right ]

simple_moves = [
  top, bottom, front, rear, left, right,
  antitop, antibottom, antifront, antirear, antileft, antiright
]  

function mod4(x) {
  if (x >= 0) { mod(x, 4) } else { 3 - mod(-x, 4) }
}

function Move.t power(Move.t {~f, ~i}, int n) {
    {~f, i:(mod4(i*n + 1) - 1)}
}

function normalize(Move.t m) {
  power(m, 1)
}

function inverse(Move.t m) {
  power(m, -1)
}

}

type Formula.t = list(Move.t)
// first move to be applied is in head position

module Formula {

// add a move first, with normalization
function add_first(Move.t m, Formula.t x) {
  match ((m, x)) {
    case ({f:_, i:0}, x): x

    case (m, []): [m]

    case ({~f, ~i}, [{f:g, i:j} | y]): if (f==g) { add_first({~f, i:(i+j)}, y) } else {  [Move.normalize(m) | x] }

    case (m, x): [Move.normalize(m) | x]
  }
}

function add_last(Formula.t x, Move.t m) {
  List.rev(add_first(m, List.rev(x))) //TODO: optimized version
}

function normalize(Formula.t x) {
  compose(x, [])
}

// inversion
function inverse(Formula.t x) {
  List.rev_map(Move.inverse, x)
}

function compose(Formula.t x, Formula.t y) {
  List.fold(add_first, List.rev(x), y)
}

}

module Face {

function Face.t apply_elementary_move(Face.t f, Face.t m) {
  match ((m, f)) { // move, face
    case ({top}, {left}): {rear}
    case ({top}, {rear}): {right}
    case ({top}, {right}): {front}
    case ({top}, {front}): {left}

    case ({bottom}, {left}): {front}
    case ({bottom}, {front}): {right}
    case ({bottom}, {right}): {rear}
    case ({bottom}, {rear}): {left}

    case ({front}, {top}): {right}
    case ({front}, {right}): {bottom}
    case ({front}, {bottom}): {left}
    case ({front}, {left}): {top}

    case ({rear}, {top}): {left}
    case ({rear}, {left}): {bottom}
    case ({rear}, {bottom}): {right}
    case ({rear}, {right}): {top}

    case ({right}, {top}): {rear}
    case ({right}, {rear}): {bottom}
    case ({right}, {bottom}): {front}
    case ({right}, {front}): {top}

    case ({left}, {top}): {front}
    case ({left}, {front}): {bottom}
    case ({left}, {bottom}): {rear}
    case ({left}, {rear}): {top}

    case _: f
  } 
}

function Face.t apply_move(Face.t f, Move.t {f:fm, ~i}) {
// TODO: memoize values
  match (Move.mod4(i)) {
  case 0: f
  case 1: apply_elementary_move(f, fm)
  case 2: apply_elementary_move(apply_elementary_move(f, fm), fm)
  case 3: apply_elementary_move(apply_elementary_move(apply_elementary_move(f, fm), fm), fm)
  case _: error("Face.apply_move: Illegal move")
  }
}

}

type Vertex.loc = (Face.t, Face.t, Face.t)

module Vertex {

function Vertex.loc apply_move(Vertex.loc (f1, f2, f3) as loc, Move.t m) {
  if (m.f == f1 || m.f == f2 || m.f == f3) {
    (Face.apply_move(f1, m), Face.apply_move(f2, m), Face.apply_move(f3, m))
  } else loc
}

function distance(Vertex.loc (f11, f12, f13) as v1, Vertex.loc (f21, f22, f23) as v2) {
  if (v1 == v2) { 0 }
  else { 1 + list_distance(List.sort([f11,f12,f13]), List.sort([f21,f22,f23])) }
}

}

type Edge.loc = (Face.t, Face.t)

module Edge {

function Edge.loc apply_move(Edge.loc (f1, f2) as loc, Move.t m) {
  if (m.f == f1 || m.f == f2) {
    (Face.apply_move(f1, m), Face.apply_move(f2, m))
  } else loc
}

function distance(Edge.loc (f11, f12) as e1, Edge.loc (f21, f22) as e2) {
  if (e1 == e2) { 0 }
  else { 1 + list_distance(List.sort([f11,f12]), List.sort([f21,f22])) }
}

}

type Cube.state = { intmap(Vertex.loc) vertices, intmap(Edge.loc) edges}

module Cube {

initial =
  function rotate_face(f) {
     Face.apply_elementary_move(f, {right})
  }
  function rotate_vertex((f1,f2,f3)) {
    (rotate_face(f1), rotate_face(f2), rotate_face(f3))
  }
  function rotate_edge((f1,f2)) {
    (rotate_face(f1), rotate_face(f2))
  }
  v1 = ({top}, {right}, {front})
  v2 = ({top}, {front}, {left})
  v3 = rotate_vertex(v1)
  v4 = rotate_vertex(v2)
  v5 = rotate_vertex(v3)
  v6 = rotate_vertex(v4)
  v7 = rotate_vertex(v5)
  v8 = rotate_vertex(v6)

  e1 = ({top}, {front})
  e2 = ({top}, {right})
  e3 = ({top}, {left})
  e4 = rotate_edge(e1)
  e5 = rotate_edge(e2)
  e6 = rotate_edge(e3)
  e7 = rotate_edge(e4)
  e8 = rotate_edge(e5)
  e9 = rotate_edge(e6)
  e10 = rotate_edge(e7)
  e11 = rotate_edge(e8)
  e12 = rotate_edge(e9)

  { vertices: list_to_intmap([ v1, v2, v3, v4, v5, v6, v7, v8 ]),
    edges: list_to_intmap([ e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12 ])
  }

function apply_move(Cube.state cube, Move.t move) {
  { vertices: Map.map(Vertex.apply_move(_, move), cube.vertices),
    edges: Map.map(Edge.apply_move(_, move), cube.edges),
  }
}

function vertex_distance(Cube.state cube1, Cube.state cube2) {
  Map.fold(function(i, v, n) {
    n + Vertex.distance(v,
          match(Map.get(i, cube2.vertices)) { //TODO: use some Map.fold2
            case {none}: error("Invalid cube state")
            case {~some}:some
         })}, cube1.vertices, 0)
}

function edge_distance(Cube.state cube1, Cube.state cube2) {
  Map.fold(function(i, v, n) {
    n + Edge.distance(v,
          match(Map.get(i, cube2.edges)) { //TODO: use some Map.fold2
            case {none}: error("Invalid cube state")
            case {~some}:some
         })}, cube1.edges, 0)
}

function distance(Cube.state cube1, Cube.state cube2) {
  vertex_distance(cube1, cube2) + edge_distance(cube1, cube2)
}

}



// library

function list_to_intmap(l) {
  n = List.length(l);
  List.foldi(function(i,x,m){ Map.add(n-i+1, x, m) }, l, Map.empty)
}

//caution: assume list of equal length
function list_distance(l1, l2) {
  List.fold2(function(x, y, n) { if (x==y) n else n+1 }, l1, l2, 0)
}
