open! Core_kernel;
open Incr_dom;
open Elements;
open Util;

module TableT =
  Incr_dom_partial_render.Table.Make(
    Int,
    Int,
    Incr_dom_partial_render.Table.Default_sort_spec,
  );

/***********************************/

module Model = {
  [@deriving (fields, compare)]
  type t = {
    selected: ref(RowItem.t),
    data: Int.Map.t(RowItem.t),
    table: TableT.Model.t,
  };

  module Updates = {
    let create_some = (model, n) => {
      let data = Util.build_data(n);
      {...model, data};
    };

    let add_some = (model, n) => {
      let data = Util.build_data(n);

      let merge = (~key as _) =>
        fun
        | `Both(_, _) => failwith("Unexpected duplicate")
        | `Left(a)
        | `Right(a) => Some(a);

      {...model, data: Int.Map.merge(model.data, data, ~f=merge)};
    };

    let update_every_10 = model => {
      let data = Int.Map.fold(model.data, ~init=model.data, ~f=exclaim);
      {...model, data};
    };

    let select = (model, idx) => {
      let itm = Int.Map.find_exn(model.data, idx);

      let data =
        model.data
        |> Int.Map.set(_, ~key=idx, ~data={...itm, selected: true})
        |> (
          switch (Int.Map.find(model.data, model.selected^.id)) {
          | None => ident
          | Some(old_itm) =>
            Int.Map.set(
              _,
              ~key=old_itm.id,
              ~data={...old_itm, selected: false},
            )
          }
        );
      {...model, data, selected: ref(itm)};
    };

    let swap_rows = model =>
      if (Int.Map.length(model.data) > 998) {
        let idx_1 = (Int.Map.min_elt_exn(model.data) |> fst) + 1;
        let idx_2 = (Int.Map.max_elt_exn(model.data) |> fst) - 1;

        let elem_1 = Int.Map.find_exn(model.data, idx_1);
        let elem_2 = Int.Map.find_exn(model.data, idx_2);
        let data =
          model.data
          |> Int.Map.set(_, ~key=idx_2, ~data=elem_1)
          |> Int.Map.set(_, ~key=idx_1, ~data=elem_2);
        {...model, data};
      } else {
        model;
      };

    let remove_item = (model, idx) => {
      let data = Int.Map.remove(model.data, idx);
      {...model, data};
    };
  };

  let cutoff = (t1, t2) => compare(t1, t2) == 0;
};

module Action = {
  [@deriving sexp]
  type t =
    | RUN
    | RUNLOTS
    | ADD
    | UPDATEEVERYTENTH
    | SELECT(int)
    | REMOVE(int)
    | CLEAR
    | SWAPROWS
    | TableAction(TableT.Action.t);

  let should_log = _ => is_debug;
};

module State = {
  type t = unit;
};

/***********************************/

// 1 column, no header
let columns = [(0, TableT.Column.create(~header=<div />, ()))];

let view_row = (~inject, ~row_id, ~row: Incr.t(RowItem.t)) => {
  module Rn_spec = Incr_dom_partial_render.Row_node_spec;
  open Incr.Let_syntax;
  open Action;

  let sender = (action, _) => inject(action);
  let%bind item = row;

  let node =
    <Row
      onSelect={sender(SELECT(row_id))}
      onRemove={sender(REMOVE(row_id))}
      item
    />;

  // 1 column => 1 cell
  let cells = [Rn_spec.{Cell.attrs: [], node}];

  return(Rn_spec.{row_attrs: [], cells});
};

let render_row = (inject, ~row_id, ~row) => view_row(~inject, ~row_id, ~row);

let create_table = (model: Incr.t(Model.t), ~old_model, ~inject) => {
  open Incr.Let_syntax;
  let rows = Incr.map(model, ~f=m => Model.data(m))
  and render_row = render_row(inject)
  and table_model = model >>| Model.table
  and old_table_model = old_model >>| Model.table >>| Option.some
  and columns = columns |> Incr.const;
  TableT.create(
    table_model,
    ~old_model=old_table_model,
    ~rows,
    ~columns,
    ~render_row,
  );
};

let empty: Model.t = {
  data: Int.Map.empty,
  selected: ref(RowItem.{id: 1, label: "", selected: false}),
  table: create_table(),
};

// let apply_action = (table) => {

//   let%map table_apply_action = table >>| Component.apply_action;

// let impl = (model, action, _, ~schedule_action as _) =>{
//   switch ((action: Action.t)) {
//   | RUN => Model.Updates.create_some(model, 1000)
//   | RUNLOTS => Model.Updates.create_some(model, 10000)
//   | ADD => Model.Updates.add_some(model, 1000)
//   | UPDATEEVERYTENTH => Model.Updates.update_every_10(model)
//   | SELECT(item) => Model.Updates.select(model, item)
//   | SWAPROWS => Model.Updates.swap_rows(model)
//   | REMOVE(item) => Model.Updates.remove_item(model, item)
//   | CLEAR => Model.empty
//   | TableAction(a)=>{
//   }
//   };

// }}

let update_visibility = m => m;

let on_startup = (~schedule_action as _, _) => Async_kernel.return();

let on_display = (~old as _, _, _) => ();

let view = (model: Incr.t(Model.t), ~inject) => {
  open Incr.Let_syntax;
  open Action;

  let sender = (action, _) => inject(action);

  let%map rows =
    Incr.Map.mapi'(
      model >>| Model.data,
      ~f=(~key as rowid, ~data as item) => {
        let%map item = item;

        <Row
          onSelect={sender(SELECT(rowid))}
          onRemove={sender(REMOVE(rowid))}
          item
        />;
      },
    );

  <div className="container">
    <Jumbotron
      run={sender(RUN)}
      runLots={sender(RUNLOTS)}
      add={sender(ADD)}
      update={sender(UPDATEEVERYTENTH)}
      clear={sender(CLEAR)}
      swapRows={sender(SWAPROWS)}
    />
    <table className="table table-hover table-striped test-data">
      <tbody> ...{Int.Map.data(rows)} </tbody>
    </table>
    <span className="preloadicon glyphicon glyphicon-remove" ariaHidden=true />
  </div>;
};

let create = (model: Incr.t(Model.t), ~old_model as _, ~inject) => {
  open Incr.Let_syntax;
  let%map apply_action = {
    let%map model = model;
    apply_action(model);
  }
  and view = view(model, ~inject)
  and model = model;

  Component.create(~apply_action, model, view);
};
