import stdlib.themes.bootstrap
import stdlib.widgets.bootstrap

/** Rubix elementary moves */

type Face.t = {up}
           or {down}
           or {front}
           or {back}
           or {left}
           or {right}

type Move.t = { Face.t f, int i }
// +1 move is the clockwise rotation when one observer looks at the cube right to the given 'face'.

module Move {

up = {f:{up}, i: 1}
down = {f:{down}, i: 1}
front = {f:{front}, i: 1}
back = {f:{back}, i: 1}
left = {f:{left}, i: 1}
right = {f:{right}, i: 1}

antiup = {f:{up}, i: -1}
antidown = {f:{down}, i: -1}
antifront = {f:{front}, i: -1}
antiback = {f:{back}, i: -1}
antileft = {f:{left}, i: -1}
antiright = {f:{right}, i: -1}

elementary_moves = [ up, down, front, back, left, right ]

simple_moves = [
  up, down, front, back, left, right,
  antiup, antidown, antifront, antiback, antileft, antiright
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
      case {up}: "Up"
      case {down}: "Down"
      case {front}: "Front"
      case {back}: "Back"
      case {left}: "Left"
      case {right}: "Right"
    }
  }

  function to_short_string(Face.t f) {
    match(f) {
      case {up}: "U"
      case {down}: "D"
      case {front}: "F"
      case {back}: "B"
      case {left}: "L"
      case {right}: "R"
    }
  }

  function to_color(Face.t f) {
    match(f) {
      case {up}: Color.blue
      case {down}: Color.green
      case {front}: Color.yellow
      case {back}: Color.white
      case {left}: Color.orange
      case {right}: Color.red
    }
  }

function to_Css_background(Face.t f) {
  Css_build.background_color(to_color(f))
}


function Face.t apply_elementary_move(Face.t f, Face.t m) {
  match ((m, f)) { // move, face
    case ({up}, {left}): {back}
    case ({up}, {back}): {right}
    case ({up}, {right}): {front}
    case ({up}, {front}): {left}

    case ({down}, {left}): {front}
    case ({down}, {front}): {right}
    case ({down}, {right}): {back}
    case ({down}, {back}): {left}

    case ({front}, {up}): {right}
    case ({front}, {right}): {down}
    case ({front}, {down}): {left}
    case ({front}, {left}): {up}

    case ({back}, {up}): {left}
    case ({back}, {left}): {down}
    case ({back}, {down}): {right}
    case ({back}, {right}): {up}

    case ({right}, {up}): {back}
    case ({right}, {back}): {down}
    case ({right}, {down}): {front}
    case ({right}, {front}): {up}

    case ({left}, {up}): {front}
    case ({left}, {front}): {down}
    case ({left}, {down}): {back}
    case ({left}, {back}): {up}

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

type Corner.t = (Face.t, Face.t, Face.t)

module Corner {

function Corner.t apply_move(Corner.t (f1, f2, f3) as v, Move.t m) {
  if (m.f == f1 || m.f == f2 || m.f == f3) {
    (Face.apply_move(f1, m), Face.apply_move(f2, m), Face.apply_move(f3, m))
  } else v
}

function distance(Corner.t (f11, f12, f13) as v1, Corner.t (f21, f22, f23) as v2) {
  if (v1 == v2) { 0 }
  else { 1 + list_distance(List.sort([f11,f12,f13]), List.sort([f21,f22,f23])) }
}

function name_to_html(Corner.t (f1, f2, f3)) {
   <span>{Face.to_string(f1)}, {Face.to_string(f2)}, {Face.to_string(f3)}</span>
}

function color_to_html(Corner.t (f1, f2, f3)) {
   <><span class="square" style="background:{Face.to_Css_background(f1)}"/><span class="square" style="background:{Face.to_Css_background(f2)}"/><span class="square" style="background:{Face.to_Css_background(f3)}"/></>
}

function permutations((x,y,z)) {
  [(x,y,z), (x,z,y), (y,z,x), (y,x,z), (z,x,y), (z,y,x)]
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

function permutations((x,y)) {
  [(x,y), (y,x)]
}

}

type Cube.t = { intmap(Corner.t) corners, intmap(Edge.t) edges}

module Cube {

initial =
/*  function rotate_face(f) {
     Face.apply_elementary_move(f, {right})
  }
  function rotate_corner((f1,f2,f3)) {
    (rotate_face(f1), rotate_face(f2), rotate_face(f3))
  }
  function rotate_edge((f1,f2)) {
    (rotate_face(f1), rotate_face(f2))
  }
  v1 = ({up}, {right}, {front})
  v2 = ({up}, {front}, {left})
  v3 = rotate_corner(v1)
  v4 = rotate_corner(v2)
  v5 = rotate_corner(v3)
  v6 = rotate_corner(v4)
  v7 = rotate_corner(v5)
  v8 = rotate_corner(v6)

  e1 = ({up}, {front})
  e2 = ({up}, {right})
  e3 = ({up}, {left})
  e4 = rotate_edge(e1)
  e5 = rotate_edge(e2)
  e6 = rotate_edge(e3)
  e7 = rotate_edge(e4)
  e8 = rotate_edge(e5)
  e9 = rotate_edge(e6)
  e10 = rotate_edge(e7)
  e11 = rotate_edge(e8)
  e12 = rotate_edge(e9)
*/

  v1 = ({up}, {right}, {front})
  v2 = ({up}, {front}, {left})
  v3 = ({back}, {right}, {up})
  v4 = ({back}, {up}, {left})
  v5 = ({down}, {right}, {back})
  v6 = ({down}, {back}, {left})
  v7 = ({front}, {right}, {down})
  v8 = ({front}, {down}, {left})

  e1 = ({up}, {front})
  e2 = ({up}, {right})
  e3 = ({up}, {left})
  e4 = ({back}, {up})
  e5 = ({back}, {right})
  e6 = ({back}, {left})
  e7 = ({down}, {back})
  e8 = ({down}, {right})
  e9 = ({down}, {left})
  e10 = ({front}, {down})
  e11 = ({front}, {right})
  e12 = ({front}, {left})

  { corners: list_to_intmap([ v1, v2, v3, v4, v5, v6, v7, v8 ]),
    edges: list_to_intmap([ e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12 ])
  }

function apply_move(Cube.t cube, Move.t move) {
  { corners: Map.map(Corner.apply_move(_, move), cube.corners),
    edges: Map.map(Edge.apply_move(_, move), cube.edges),
  }
}

function apply_formula(Cube.t cube, Formula.t f) {
  List.fold(function(m,c) { apply_move(c,m) }, f, cube)
}

function get_corner(Cube.t cube, i) {
   if (i < 1 || i > 8) error("Invalid corner number");
   map_unsafe_get(i, cube.corners, "Invalid cube state")
}

function get_edge(Cube.t cube, i) {
   if (i < 1 || i > 12) error("Invalid edge number");
   map_unsafe_get(i, cube.edges, "Invalid cube state")
}

function corner_distance(Cube.t cube1, Cube.t cube2) {
  Map.fold(   //TODO: use some Map.fold2
    function(i, v, n) {
       n + Corner.distance(v, get_corner(cube2, i))
    }, cube1.corners, 0)
}

max_corner_distance = 8*4

function edge_distance(Cube.t cube1, Cube.t cube2) {
  Map.fold(   //TODO: use some Map.fold2
    function(i, v, n) {
       n + Edge.distance(v, get_edge(cube2, i))
    }, cube1.edges, 0)
}

max_edge_distance = 12*3

function distance(Cube.t cube1, Cube.t cube2) {
  corner_distance(cube1, cube2) + edge_distance(cube1, cube2)
}

max_distance = max_corner_distance + max_edge_distance

function to_html(Cube.t cube) {
  <div>
    {if (cube == initial) WBootstrap.Label.make("Solved", {success}) else WBootstrap.Label.make("Scrambled: {Cube.distance(initial, cube)}", {important})}
    <h2>Corners</h2>
    <table>
    {List.rev(Map.fold(function(i, v, list(xhtml) lh) {
        [ <tr><td>{i}</td><td>{Corner.color_to_html(get_corner(initial,i))}</td><td>{Corner.name_to_html(v)}</td></tr> | lh]}, cube.corners, []))}
    </table>
    <h2>Edges</h2>
    <table>
    {List.rev(Map.fold(function(i, e, list(xhtml) lh) {
        [ <tr><td>{i}</td><td>{Edge.color_to_html(get_edge(initial,i))}</td><td>{Edge.name_to_html(e)}</td></tr> | lh]}, cube.edges, []))}
    </table>
  </div>
}

}

type Facelet.t = {Face.t f, int n}

type Facelet.maps = {
   map(Corner.t, (int,int,int)) corners,
   map(Edge.t, (int,int)) edges
}

module Facelet {

numbers =
  v1 = (9, 1, 3) //({up}, {right}, {front})
  v2 = (7, 1, 3) //({up}, {front}, {left})
  v3 = (1, 3, 3) //({back}, {right}, {up})
  v4 = (3, 1, 3) //({back}, {up}, {left})
  v5 = (9, 9, 7) //({down}, {right}, {back})
  v6 = (7, 7, 9) //({down}, {back}, {left})
  v7 = (9, 7, 3) //({front}, {right}, {down})
  v8 = (7, 1, 9) //({front}, {down}, {left})

  e1 = (8, 2) //({up}, {front})
  e2 = (6, 2) //({up}, {right})
  e3 = (4, 2) //({up}, {left})
  e4 = (2, 2) //({back}, {up})
  e5 = (4, 6) //({back}, {right})
  e6 = (6, 4) //({back}, {left})
  e7 = (8, 8) //({down}, {back})
  e8 = (6, 8) //({down}, {right})
  e9 = (4, 8) //({down}, {left})
  e10 = (8, 2) //({front}, {down})
  e11 = (6, 4) //({front}, {right})
  e12 = (4, 6) //({front}, {left})

  { corners: list_to_intmap([ v1, v2, v3, v4, v5, v6, v7, v8 ]),
    edges: list_to_intmap([ e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12 ])
  }

Facelet.maps maps =
  function add_corner_facelets(Corner.t c0, (int,int,int) n0, m0) {
    List.fold2(Map.add, Corner.permutations(c0), Corner.permutations(n0), m0)
  }
  function add_edge_facelets(Edge.t e0, (int,int) n0, m0) {
    List.fold2(Map.add, Edge.permutations(e0), Edge.permutations(n0), m0)
  }
  corners =
    Map.fold(   //TODO: use some Map.fold2
      function(i, x, m) {
         add_corner_facelets(x, map_unsafe_get(i, numbers.corners, "Invalid corner table"), m)
      }, Cube.initial.corners, Map.empty)
  edges =
    Map.fold(   //TODO: use some Map.fold2
      function(i, x, m) {
         add_edge_facelets(x, map_unsafe_get(i, numbers.edges, "Invalid corner table"), m)
      }, Cube.initial.edges, Map.empty)
  { ~corners, ~edges }

function corner(Corner.t c) {
  map_unsafe_get(c, maps.corners, "Invalid corner map")
}

function edge(Edge.t e) {
  map_unsafe_get(e, maps.edges, "Invalid edge map")
}

}



/* --- library --- */

function list_to_intmap(l) {
  List.foldi(function(i,x,m){ Map.add(i+1, x, m) }, l, Map.empty)
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

function map_unsafe_get(k, m, message) {
   match(Map.get(k, m)) {
     case {none}: error(message)
     case {~some}:some
   }
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
