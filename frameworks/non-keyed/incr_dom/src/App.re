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
    selected: option(RowItem.t),
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
      open Option.Let_syntax;

      let data =
        model.data
        |> Int.Map.set(_, ~key=idx, ~data={...itm, selected: true})
        |> (
          switch (model.selected >>= (a => Int.Map.find(model.data, a.id))) {
          | None => ident
          | Some(old_itm) =>
            Int.Map.set(
              _,
              ~key=old_itm.id,
              ~data={...old_itm, selected: false},
            )
          }
        );
      {...model, data, selected: Some(itm)};
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

// 1 column
let columns = {
  let header = <div> {Vdom.Node.text("jisfd")} </div>;
  [(0, TableT.Column.create(~header, ()))];
};

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
  and inject_table_action = a => inject(Action.TableAction(a))
  and columns = columns |> Incr.const;

  // FIXME: Split this out into a element?
  TableT.create(
    table_model,
    ~old_model=old_table_model,
    ~rows,
    ~columns,
    ~render_row,
    ~inject=inject_table_action,
    ~attrs=[
      Vdom.Attr.classes([
        "table",
        "table-hover",
        "table-striped",
        "test-data",
      ]),
    ],
  );
};

let init: unit => Model.t =
  () => {
    let height_guess = 30.;

    let table =
      TableT.Model.create(
        ~scroll_margin=Incr_dom_partial_render.Table.Margin.uniform(5.),
        ~scroll_region=Element("table-container"),
        ~float_header=Edge,
        ~float_first_col=Px_from_edge(-1),
        ~height_guess,
        (),
      );

    {data: Int.Map.empty, selected: None, table};
  };

let apply_action = (table, model: Incr.t(Model.t)) => {
  open Incr.Let_syntax;

  let%map apply_table_action_ = table >>| Component.apply_action
  and model = model;

  (action: Action.t, _, ~schedule_action) => {
    let schedule_table_action = a => {
      schedule_action(Action.TableAction(a));
    };
    let apply_table_action = action => {
      apply_table_action_(action, (), ~schedule_action=schedule_table_action);
    };

    switch ((action: Action.t)) {
    | RUN => Model.Updates.create_some(model, 1000)
    | RUNLOTS => Model.Updates.create_some(model, 10000)
    | ADD => Model.Updates.add_some(model, 1000)
    | UPDATEEVERYTENTH => Model.Updates.update_every_10(model)
    | SELECT(item) => Model.Updates.select(model, item)
    | SWAPROWS => Model.Updates.swap_rows(model)
    | REMOVE(item) => Model.Updates.remove_item(model, item)
    | CLEAR => init()
    | TableAction(a) => {...model, table: apply_table_action(a)}
    };
  };
};

let update_visibility = (table, model: Incr.t(Model.t)) => {
  open Incr.Let_syntax;
  let%map model = model
  and table = table;

  let impl = (~schedule_action) => {
    let update_table = a => schedule_action(Action.TableAction(a));
    let table =
      Component.update_visibility(table, ~schedule_action=update_table);

    {...model, table};
  };

  impl;
};

let on_startup = (~schedule_action as _, _) => Async_kernel.return();

let on_display = (~old as _, _, _) => ();

let view = (~inject) => {
  open Incr.Let_syntax;
  open Action;

  let sender = (action, _) => inject(action);

  let onScroll = _ => Vdom.Event.Viewport_changed;

  (table: Incr.t(TableT.t(RowItem.t)), _model: Incr.t(Model.t)) => {
    let%map table = table >>| Component.view;

    /* FIXME: Sort out the onscroll attribute so this can be done as an element */

    <div className="container">
      <Jumbotron
        run={sender(RUN)}
        runLots={sender(RUNLOTS)}
        add={sender(ADD)}
        update={sender(UPDATEEVERYTENTH)}
        clear={sender(CLEAR)}
        swapRows={sender(SWAPROWS)}
      />
      <div key="table" id="table-container" onScroll> table </div>
      <span
        className="preloadicon glyphicon glyphicon-remove"
        ariaHidden=true
      />
    </div>;
  };
};

let create = (model: Incr.t(Model.t), ~old_model, ~inject) => {
  open Incr.Let_syntax;

  let table = create_table(model, ~old_model, ~inject);

  let%map apply_action = apply_action(table, model)
  and update_visibility = update_visibility(table, model)
  and view = view(table, model, ~inject)
  and model = model;

  Component.create(~apply_action, ~update_visibility, model, view);
};
