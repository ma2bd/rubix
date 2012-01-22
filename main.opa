import stdlib.themes.bootstrap
import stdlib.widgets.bootstrap

client module Display {

  function cubies_to_html(Cube.t cube) {
    <div>
      {if (cube == Cube.initial) WBootstrap.Label.make("Solved", {success}) else WBootstrap.Label.make("Scrambled: {Cube.complexity(cube)}/{Cube.max_distance}", {important})}
      {WBootstrap.Grid.row([
        {span:6, offset:none, content:<h2>Corners</h2>},
        {span:6, offset:none, content:<h2>Edges</h2>}])}
      {WBootstrap.Grid.row([
        {span:6, offset:none, content:
          <table>
      {List.rev(Map.fold(function(i, v, list(xhtml) lh) {
          [ <tr><td>{i}</td><td>{Corner.color_to_html(Cube.get_corner(Cube.initial,i))}</td><td>{Corner.name_to_html(v)}</td></tr> | lh]}, cube.corners, []))}
         </table>},
        {span:6, offset:none, content:
         <table>
      {List.rev(Map.fold(function(i, e, list(xhtml) lh) {
          [ <tr><td>{i}</td><td>{Edge.color_to_html(Cube.get_edge(Cube.initial,i))}</td><td>{Edge.name_to_html(e)}</td></tr> | lh]}, cube.edges, []))}
        </table>}])}
    </div>
  }

//  container_id = #display

  function facelet_id(fc) {
    "id_"^(Facelet.to_string(fc))
  }

  function set_facelet_color(f, c) {
    Dom.set_style(Dom.select_id(facelet_id(f)), [{background:Face.to_Css_background(c)}])
  }

  function install() {
    function mkcmd(m) {
      <td>{WBootstrap.Button.make({button: Move.to_html(m), callback:function(_){CubeState.apply_move(m)}}, []) |> Xhtml.update_class("formula", _)}</td>
    }
    function mksimulatecmd(m) {
      <td>{WBootstrap.Label.make("", {notice}) |> Xhtml.add_id(some("id_simu_{Move.to_string(m)}"), _)}</td>
    }
    function mkface(f) {    // TODO use a table ?
      function fid(n) { facelet_id({~f, ~n}) }
      function td(n) { <td class=square id={fid(n)}>{Facelet.to_string({~f, ~n})}</td> }
      <h3>{Face.to_string(f)}</h3>
      <table onclick={function(_) { CubeState.apply_move({~f, i:1}) }}>
        <tr>{list(xhtml) [td(1), td(2), td(3)]}</tr>
        <tr>{list(xhtml) [td(4), td(5), td(6)]}</tr>
        <tr>{list(xhtml) [td(7), td(8), td(9)]}</tr>
      </table>
    }
    #commands = <table>
       <tr>{List.map(mkcmd, Move.simple_moves)}</tr>
       <tr>{List.map(mksimulatecmd, Move.simple_moves)}</tr>
       </table>
    #facelets =
       <div>
  {WBootstrap.Grid.row([{span:3, offset:some(3), content:mkface({up})}])}
  {WBootstrap.Grid.row([
     {span:3, offset:none, content:mkface({left})},
     {span:3, offset:none, content:mkface({front})},
     {span:3, offset:none, content:mkface({right})},
     {span:3, offset:none, content:mkface({back})}
])}
  {WBootstrap.Grid.row([{span:3, offset:some(3), content:mkface({down})}])}
       </div>
    // last color the center facelets
    List.iter(function(f) { set_facelet_color({~f, n:5}, f)}, Face.all)
  }

  function refresh_corner_facelets(int i, Corner.t (f1,f2,f3) as c) {
    (n1, n2, n3) = Facelet.corner_numbers(c)
    (c1, c2, c3) = Cube.get_corner(Cube.initial, i)
    set_facelet_color({f:f1, n:n1}, c1);
    set_facelet_color({f:f2, n:n2}, c2);
    set_facelet_color({f:f3, n:n3}, c3);
  }

  function refresh_edge_facelets(int i, Edge.t (f1,f2) as e) {
    (n1, n2) = Facelet.edge_numbers(e)
    (c1, c2) = Cube.get_edge(Cube.initial, i)
    set_facelet_color({f:f1, n:n1}, c1);
    set_facelet_color({f:f2, n:n2}, c2);
  }

  function refresh_simulate_cmd(cube, m) {
     #{"id_simu_{Move.to_string(m)}"} = <>{Cube.complexity(Cube.apply_move(cube, m))}</>
  }

  function refresh(cube, history) {
    #cubies = cubies_to_html(cube);
    Map.iter(refresh_corner_facelets, cube.corners);
    Map.iter(refresh_edge_facelets, cube.edges);
    #history = Formula.to_html(List.rev(history));
    List.iter(refresh_simulate_cmd(cube, _), Move.simple_moves)
  }

}


client module CubeState {

  cube = Reference.create(Cube.initial)

  history = Reference.create(Formula.empty) // reversed formula

  function apply_move(m) {
    Reference.update(cube, Cube.apply_move(_, m));
    Reference.update(history, Formula.add_first(m, _));
    Display.refresh(Reference.get(cube), Reference.get(history))
  }

  function apply_formula(f) {
    Reference.update(cube, Cube.apply_formula(_, f));
    Reference.update(history, Formula.rev_compose(f, _));
    Display.refresh(Reference.get(cube), Reference.get(history))
  }

  function install() {
    Display.install();
    Display.refresh(Reference.get(cube), Reference.get(history))
  }
}

function page() {
   WBootstrap.Layout.fixed(
      <div onready={function(_){CubeState.install()}}>
      <h1>My cube</h1>
      <div id="cubies" />
      <h2>Faces</h2>
      <div id="facelets" />
      <h2>History</h2>
      <code id="history" />
      <h2>Commands</h2>
      <div id="commands" />
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
