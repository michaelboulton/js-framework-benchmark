open! Core_kernel;
open Incr_dom;
open Elements;

let glyph_icon =
  <span className="glyphicon glyphicon-remove" ariaHidden=true />;

let createElement =
    (~onSelect, ~onRemove, ~id, ~selected, ~label, ~children as _, _) => {
  <tr className={selected ? "danger" : ""}>
    <td className="col-md-1"> {id |> string_of_int |> Vdom.Node.text} </td>
    <td className="col-md-4">
      <a onClick=onSelect> {label |> Vdom.Node.text} </a>
    </td>
    <td className="col-md-1"> <a onClick=onRemove> glyph_icon </a> </td>
    <td className="col-md-6" />
  </tr>;
};
