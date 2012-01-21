import stdlib.themes.bootstrap
import stdlib.widgets.bootstrap

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
  if (x >= 0) { mod(x, 4) } else { 4 - mod(-x, 4) }
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

function random_simple() {
  list_pick_random(simple_moves)
}

function to_string(Move.t {~f, ~i}) {
  Face.to_short_string(f)^(if (i != 1) "^{i}" else "")
}

function to_html(Move.t {~f, ~i}) {
  <>{Face.to_short_string(f)}{if (i != 1) <sup>{i}</sup> else <></>}</>
}

}

type Formula.t = list(Move.t)
// first move to be applied is in head position

module Formula {

Formula.t empty = []

// add a move first, with normalization
function add_first(Move.t m0, Formula.t x) {
  match ((Move.normalize(m0), x)) {
    case ({f:_, i:0}, x): x

    case (m, []): [m]

    case ({~f, ~i} as m, [{f:g, i:j} | y]): if (f==g) { add_first({~f, i:(i+j)}, y) } else {  [m | x] }

    case (m, x): [m | x]
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

function rev_compose(Formula.t x, Formula.t y) {
  List.fold(add_first, x, y)
}

function compose(Formula.t x, Formula.t y) {
  rev_compose(List.rev(x), y)
}

// TODO should be local
  function random_aux(acc, n) {
    if (n<=0) acc
    else random_aux(add_first(Move.random_simple(), acc), n-1)
  }


function random(n) {
  random_aux(empty, n)
}

function to_html(Formula.t f) {
  <span class=formula>{if (f == empty) {[<>&empty;</>]} else {List.map(Move.to_html, f)}}</span>
}

}

module Face {

  function to_string(Face.t f) {
    match(f) {
      case {top}: "top"
      case {bottom}: "bottom"
      case {front}: "front"
      case {rear}: "rear"
      case {left}: "left"
      case {right}: "right"
    }
  }

  function to_short_string(Face.t f) {
    match(f) {
      case {top}: "t"
      case {bottom}: "b"
      case {front}: "f"
      case {rear}: "k" //back
      case {left}: "l"
      case {right}: "r"
    }
  }

  function to_color(Face.t f) {
    match(f) {
      case {top}: Color.red
      case {bottom}: Color.orange
      case {front}: Color.blue
      case {rear}: Color.green
      case {left}: Color.yellow
      case {right}: Color.white
    }
  }

function to_Css_background(Face.t f) {
  Css_build.background_color(to_color(f))
}


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

type Vertex.t = (Face.t, Face.t, Face.t)

module Vertex {

function Vertex.t apply_move(Vertex.t (f1, f2, f3) as v, Move.t m) {
  if (m.f == f1 || m.f == f2 || m.f == f3) {
    (Face.apply_move(f1, m), Face.apply_move(f2, m), Face.apply_move(f3, m))
  } else v
}

function distance(Vertex.t (f11, f12, f13) as v1, Vertex.t (f21, f22, f23) as v2) {
  if (v1 == v2) { 0 }
  else { 1 + list_distance(List.sort([f11,f12,f13]), List.sort([f21,f22,f23])) }
}

function name_to_html(Vertex.t (f1, f2, f3)) {
   <span>{Face.to_string(f1)}, {Face.to_string(f2)}, {Face.to_string(f3)}</span>
}

function color_to_html(Vertex.t (f1, f2, f3)) {
   <><span class="square" style="background:{Face.to_Css_background(f1)}"/><span class="square" style="background:{Face.to_Css_background(f2)}"/><span class="square" style="background:{Face.to_Css_background(f3)}"/></>
}

}

type Edge.t = (Face.t, Face.t)

module Edge {

function Edge.t apply_move(Edge.t (f1, f2) as e, Move.t m) {
  if (m.f == f1 || m.f == f2) {
    (Face.apply_move(f1, m), Face.apply_move(f2, m))
  } else e
}

function distance(Edge.t (f11, f12) as e1, Edge.t (f21, f22) as e2) {
  if (e1 == e2) { 0 }
  else { 1 + list_distance(List.sort([f11,f12]), List.sort([f21,f22])) }
}

function name_to_html(Edge.t (f1, f2)) {
   <span>{Face.to_string(f1)}, {Face.to_string(f2)}</span>
}

function color_to_html(Edge.t (f1, f2)) {
   <><span class="square" style="background:{Face.to_Css_background(f1)}"/><span class="square" style="background:{Face.to_Css_background(f2)}"/></>
}

}

type Cube.t = { intmap(Vertex.t) vertices, intmap(Edge.t) edges}

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

function apply_move(Cube.t cube, Move.t move) {
  { vertices: Map.map(Vertex.apply_move(_, move), cube.vertices),
    edges: Map.map(Edge.apply_move(_, move), cube.edges),
  }
}

function apply_formula(Cube.t cube, Formula.t f) {
  List.fold(function(m,c) { apply_move(c,m) }, f, cube)
}

function get_vertex(Cube.t cube, i) {
   if (i < 1 || i > 8) error("Invalid vertex number");
   match(Map.get(i, cube.vertices)) {
     case {none}: error("Invalid cube state")
     case {~some}:some
   }
}

function get_edge(Cube.t cube, i) {
   if (i < 1 || i > 12) error("Invalid edge number");
   match(Map.get(i, cube.edges)) {
     case {none}: error("Invalid cube state")
     case {~some}:some
   }
}

function vertex_distance(Cube.t cube1, Cube.t cube2) {
  Map.fold(   //TODO: use some Map.fold2
    function(i, v, n) {
       n + Vertex.distance(v, get_vertex(cube2, i))
    }, cube1.vertices, 0)
}

function edge_distance(Cube.t cube1, Cube.t cube2) {
  Map.fold(   //TODO: use some Map.fold2
    function(i, v, n) {
       n + Edge.distance(v, get_edge(cube2, i))
    }, cube1.edges, 0)
}

function distance(Cube.t cube1, Cube.t cube2) {
  vertex_distance(cube1, cube2) + edge_distance(cube1, cube2)
}

function to_html(Cube.t cube) {
  <div>
    {if (cube == initial) WBootstrap.Label.make("Solved", {success}) else WBootstrap.Label.make("Scrambled: {Cube.distance(initial, cube)}", {important})}
    <h2>Vertices</h2>
    <table>
    {List.rev(Map.fold(function(i, v, list(xhtml) lh) {
        [ <tr><td>{i}</td><td>{Vertex.color_to_html(get_vertex(initial,i))}</td><td>{Vertex.name_to_html(v)}</td></tr> | lh]}, cube.vertices, []))}
    </table>
    <h2>Edges</h2>
    <table>
    {List.rev(Map.fold(function(i, e, list(xhtml) lh) {
        [ <tr><td>{i}</td><td>{Edge.color_to_html(get_edge(initial,i))}</td><td>{Edge.name_to_html(e)}</td></tr> | lh]}, cube.edges, []))}
    </table>
  </div>
}

}



/* --- library --- */

function list_to_intmap(l) {
  n = List.length(l);
  List.foldi(function(i,x,m){ Map.add(n-i, x, m) }, l, Map.empty)
}

//caution: assume list of equal length
function list_distance(l1, l2) {
  List.fold2(function(x, y, n) { if (x==y) n else n+1 }, l1, l2, 0)
}

function list_pick_random(l) {
  n = List.length(l);
  i = Random.int(n);
  List.unsafe_nth(i, l)
}

/* ---- */

client module Display {

  cube = Reference.create(Cube.initial)

  history = Reference.create(Formula.empty) // reversed formula
  
//  container_id = #display

  function install() {
    function mkcmd(m) {
      WBootstrap.Button.make({button: Move.to_html(m), callback:function(_){apply_move(m)}}, [])
    }
    refresh();
    #commands = <div>{List.map(mkcmd, Move.simple_moves)}</div> 
  }

  function refresh() {
    #cube = Cube.to_html(Reference.get(cube));
    #history = Formula.to_html(List.rev(Reference.get(history)))
  }

  function apply_move(m) {
    Reference.update(cube, Cube.apply_move(_, m))
    Reference.update(history, Formula.add_first(m, _))
    refresh()
  }

  function apply_formula(f) {
    Reference.update(cube, Cube.apply_formula(_, f))
    Reference.update(history, Formula.rev_compose(f, _))
    refresh()
  }
}

function page() {
   WBootstrap.Layout.fixed(
      <div onready={function(_){Display.install()}}>
      <h1>My cube</h1>
      <div id=cube />
      <h2>History</h2>
      <div id=history/>
      <h2>Commands</h2>
      <div id=commands />
      </div>
   )
}

Server.start(
  Server.http,
  [ {resources: @static_resource_directory("resources")}
  , {register: ["resources/rubix.css"]}
  , {title: "Opa rubix", page:page }
  ]
)
